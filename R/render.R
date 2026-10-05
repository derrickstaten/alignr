# Render core for the Align RStudio addin (ALI-186 T1).
#
# Near-verbatim port of the web app's renderRVisualization contract
# (@align/core render recipe): svgstring device -> user code assigns `p` in a
# child of globalenv() -> .align_render dispatch -> dev.off() -> scalar SVG
# string. Evaluating in a child of globalenv() is the product thesis: the
# user's session data frames are in scope with no upload step.
#
# Two invariants proven load-bearing by the ALI-179 spike:
#  - The graphics device MUST close even when user code throws — a leaked
#    device makes every subsequent render draw into the dead one.
#  - The result MUST be coerced to a length-1 character — svglite/systemfonts
#    can return a character vector while the font cache warms up.

#' Render Align visualization source to an SVG string.
#'
#' @param code R source; the Align contract is that it assigns a plot to `p`
#'   (ggplot object, grob, or zero-arg function).
#' @param width_inches,height_inches Device size in inches.
#' @param font_scale Exposed to user code as `font_scale`, same as the web app.
#' @return list(svg = <string>|NULL, error = <string>|NULL) — errors are data,
#'   never conditions, so the HTTP layer can pass them straight through.
#' Plot-like values the dispatch will draw — kept identical to
#' lib/webr-render.ts's .align_renderable so a figure renders the same in the
#' plugin and on the web.
.align_renderable <- function(x) {
  !is.null(x) && (
    is.function(x) ||
    inherits(x, c("grob", "gTree", "gList", "gg", "ggplot", "Heatmap",
                  "HeatmapList", "trellis", "recordedplot"))
  )
}

align_render_svg <- function(code, width_inches = 6, height_inches = 4, font_scale = 1) {
  devices_before <- grDevices::dev.cur()
  # File-backed svglite rather than svgstring(): svglite 2.2.1's svgstring
  # content reader SEGFAULTS when the device closes with zero pages drawn
  # (e.g. user code assigns a non-plot to p, so print() draws nothing) —
  # found by this package's test harness, ALI-186. With a file device,
  # "nothing drawn" is just a missing file we can turn into a clean error.
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)
  svglite::svglite(tmp, width = width_inches, height = height_inches)
  ok <- TRUE
  err <- NULL
  tryCatch({
    env <- new.env(parent = globalenv())
    env$font_scale <- font_scale
    # Expression by expression, keeping the last value: a script figure
    # usually ENDS with its plot (`ggplot(df, ...) + geom_point()`) rather
    # than assigning it to `p`, and that region must render the same as it
    # does when the user runs it (DS-449 live finding — it rendered nothing,
    # with a cryptic error). Same dispatch as the web's WebR wrapper
    # (lib/webr-render.ts, ALI-229): an explicit `p` wins, then a renderable
    # final value, then whatever the code drew on the device itself.
    last <- NULL
    for (expr in parse(text = code)) {
      last <- eval(expr, envir = env)
    }
    if (!exists("p", envir = env, inherits = FALSE) && .align_renderable(last)) {
      env$p <- last
    }
    .align_render <- function(x) {
      if (inherits(x, c("grob", "gTree", "gList"))) {
        grid::grid.draw(x)
      } else if (is.function(x)) {
        x()
      } else {
        print(x)
      }
    }
    .align_render(env$p)
  }, error = function(e) {
    ok <<- FALSE
    err <<- conditionMessage(e)
  })
  # Unconditional close: runs on success AND failure (see header).
  grDevices::dev.off()
  stopifnot(grDevices::dev.cur() == devices_before)
  if (!ok) return(list(svg = NULL, error = err))
  # svglite leaves an empty file, not a missing one, when nothing was drawn
  # — that used to come back as an empty SVG with no error at all.
  if (!file.exists(tmp) || file.size(tmp) == 0) {
    return(list(svg = NULL, error = "The code ran but didn't draw anything — end it with the plot, or assign the plot (ggplot, grob, or function) to `p`."))
  }
  svg <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # Coerce defensively to a length-1 string (spike guard; see header).
  list(svg = paste(as.character(svg), collapse = ""), error = NULL)
}

#' Data frames currently in the user's global environment.
#'
#' Read fresh per call (no snapshotting) — the live-session behavior the spike
#' proved and WebR structurally can't have: a data frame created at the
#' console appears on the next request.
#'
#' @return list of list(name, rows, cols, columns = chr vector)
align_list_session_data <- function() {
  names <- ls(envir = globalenv())
  out <- list()
  for (nm in names) {
    obj <- tryCatch(get(nm, envir = globalenv()), error = function(e) NULL)
    if (is.data.frame(obj)) {
      out[[length(out) + 1]] <- list(
        name = nm,
        rows = nrow(obj),
        cols = ncol(obj),
        columns = as.character(colnames(obj))
      )
    }
  }
  out
}

#' Preview of one session data frame (head only — the canvas never needs full data).
align_preview_session_data <- function(name, n = 20) {
  obj <- tryCatch(get(name, envir = globalenv()), error = function(e) NULL)
  if (!is.data.frame(obj)) return(list(error = paste0("No data frame named '", name, "' in the session.")))
  head_df <- utils::head(obj, n)
  list(
    name = name,
    rows = nrow(obj),
    columns = as.character(colnames(obj)),
    head = lapply(seq_len(nrow(head_df)), function(i) as.list(lapply(head_df[i, , drop = FALSE], function(v) as.character(v))))
  )
}

#' Is `pkg` installed in this session's library paths?
align_package_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}
