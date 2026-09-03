# Session capture for the "Open in Align web" handoff (DS-396).
#
# A figure's code runs here as a child of globalenv(), so it can lean on
# anything the user built at the console — the demo volcano region reads `df`,
# `top_hits`, `fc_cutoff`, all defined above its marker. Align web has no such
# session: the same code resolves `df` to stats::df and ggplot fails with
# "`data` cannot be a function". Rather than asking the user to make every
# figure self-contained first, the handoff captures the slice of the session
# the code actually needs — objects it references that live in globalenv(),
# transitively through user-defined functions — plus the attached packages its
# other symbols come from, into ONE .rds the web runtime loads before
# evaluating the code. Deterministic and synchronous, same stance as
# annotate.R: no AI in the loop. RDS rather than CSV because it is exact
# (tibbles, factors, dates, functions) and base R on both ends.

# Packages every R has attached; `library()`-ing them on web would be noise.
.ALIGN_BASE_PKGS <- c("base", "stats", "utils", "graphics", "grDevices", "methods", "datasets")

# Ceilings. A single object over the first is almost certainly not something
# the user meant to ship in a document; the second is @align/core's
# DOC_EMBED_MAX_BYTES — a bigger blob would be refused at save time anyway, so
# refuse here with a reason the user can read instead.
.ALIGN_CAPTURE_OBJECT_MAX <- 50 * 1024^2
.ALIGN_CAPTURE_TOTAL_MAX <- 5 * 1024^2

#' Symbols an expression reads without defining.
#'
#' Assignment targets and function formals are excluded; `font_scale` is
#' excluded because the render contract injects it on both surfaces. Column
#' names used under non-standard evaluation (`aes(x = log2FC)`) come through
#' too — they resolve to nothing in globalenv() and no package, so the caller
#' drops them.
#' @param exprs An `expression` (from `parse`) or a single language object.
.align_free_symbols <- function(exprs) {
  assigned <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    if (is.symbol(fn)) {
      nm <- as.character(fn)
      if (nm %in% c("<-", "=", "<<-") && length(e) >= 2 && is.symbol(e[[2]])) {
        assigned <<- c(assigned, as.character(e[[2]]))
      }
      if (nm == "function" && length(e) >= 2 && !is.null(e[[2]])) {
        assigned <<- c(assigned, names(e[[2]]))
      }
    }
    args <- as.list(e)
    for (i in seq_along(args)) {
      # `x[, 1]` carries an empty-symbol argument that errors on first touch;
      # nothing to read there, so skip it rather than special-case it.
      tryCatch(walk(args[[i]]), error = function(err) NULL)
    }
  }
  if (is.expression(exprs)) for (ex in exprs) walk(ex) else walk(exprs)
  setdiff(all.names(exprs, unique = TRUE), c(assigned, "font_scale"))
}

#' Why an object can't ride in an .rds, or NULL when it can.
.align_uncapturable <- function(obj) {
  if (is.environment(obj)) return("environments can't be saved")
  if (inherits(obj, "connection")) return("connections can't be saved")
  if (typeof(obj) == "externalptr") return("external pointers can't be saved")
  size <- tryCatch(as.numeric(utils::object.size(obj)), error = function(e) NA_real_)
  if (!is.na(size) && size > .ALIGN_CAPTURE_OBJECT_MAX) return("larger than 50 MB")
  NULL
}

#' Attached, non-base packages that supply any of `syms`. Only the search
#' path counts — that is what "the code worked here" relied on.
.align_symbol_packages <- function(syms) {
  pkgs <- character()
  for (s in syms) {
    where <- tryCatch(utils::find(s), error = function(e) character())
    where <- where[startsWith(where, "package:")]
    if (length(where)) pkgs <- c(pkgs, sub("^package:", "", where[1]))
  }
  setdiff(unique(pkgs), .ALIGN_BASE_PKGS)
}

#' Capture the slice of the live session that `code` depends on.
#'
#' @param code R source of one figure.
#' @return A list. `found = TRUE` carries `contentBase64` (the .rds of
#'   `list(packages = <chr>, objects = <named list>)`), `bytes`, `objects`,
#'   `packages`, `skipped`. `found = FALSE` carries `reason` when something
#'   prevented a capture, or just empty `objects`/`packages` when the code
#'   needs nothing from the session. Errors are data, never conditions.
align_capture_session <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(list(found = FALSE, reason = "The figure's code doesn't parse."))

  objects <- list()
  skipped <- list()
  external <- character()
  seen <- character()
  queue <- .align_free_symbols(exprs)
  while (length(queue)) {
    s <- queue[1]
    queue <- queue[-1]
    if (s %in% seen) next
    seen <- c(seen, s)
    if (!exists(s, envir = globalenv(), inherits = FALSE)) {
      external <- c(external, s)
      next
    }
    obj <- get(s, envir = globalenv(), inherits = FALSE)
    why <- .align_uncapturable(obj)
    if (!is.null(why)) {
      skipped[[length(skipped) + 1]] <- list(name = s, reason = why)
      next
    }
    objects[[s]] <- obj
    # A user-defined helper drags its own dependencies along, to a fixpoint.
    if (is.function(obj) && !is.primitive(obj)) {
      inner <- setdiff(.align_free_symbols(body(obj)), names(formals(obj)))
      queue <- c(queue, inner)
    }
  }
  packages <- .align_symbol_packages(external)

  if (!length(objects) && !length(packages)) {
    return(list(found = FALSE, objects = list(), packages = list(), skipped = skipped))
  }

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(packages = packages, objects = objects), tmp)
  bytes <- as.numeric(file.size(tmp))
  if (bytes > .ALIGN_CAPTURE_TOTAL_MAX) {
    return(list(
      found = FALSE,
      reason = sprintf("The session objects this figure needs come to %.1f MB; a document can carry 5 MB.", bytes / 1024^2),
      objects = as.list(names(objects)), packages = as.list(packages), skipped = skipped
    ))
  }
  # as.list() so a single name still serialises as a JSON array (auto_unbox).
  list(
    found = TRUE,
    contentBase64 = gsub("[\r\n]", "", jsonlite::base64_enc(readBin(tmp, "raw", bytes))),
    bytes = bytes,
    objects = as.list(names(objects)),
    packages = as.list(packages),
    skipped = skipped
  )
}
