# Bundle freshness guard (ALI-269). inst/www is build-on-demand and never
# committed, which means nothing in git prevents serving a bundle older than
# the source being reviewed — the exact failure that made plugin chrome
# unreviewable (DSN-28). The Vite build stamps the bundle with
# build-stamp.json; when the source tree is reachable from getwd() we compare
# that stamp against the newest source mtime and say so out loud. Installed
# use outside the repo has no source tree to compare against, so the check
# silently passes there rather than guessing.

#' Walk up from `start` looking for the align-web repo root, identified by
#' the rstudio-host source dir. Mirrors the demo script's find_repo_root();
#' duplicated because the installed package can't source demo code.
#' @return The root path, or NULL when `start` isn't inside the repo.
.align_find_repo_root <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (dir.exists(file.path(dir, "packages", "rstudio-host", "src"))) return(dir)
    parent <- dirname(dir)
    if (identical(parent, dir)) return(NULL)
    dir <- parent
  }
}

#' Read the bundle's build time from build-stamp.json.
#' @return POSIXct in UTC, or NULL when the stamp is missing/unreadable
#'   (a pre-ALI-269 bundle, or one not produced by the Vite build).
.align_bundle_built_at <- function(www_root) {
  stamp_path <- file.path(www_root, "build-stamp.json")
  if (!file.exists(stamp_path)) return(NULL)
  stamp <- tryCatch(jsonlite::fromJSON(stamp_path), error = function(e) NULL)
  if (is.null(stamp) || !is.character(stamp$builtAt)) return(NULL)
  parsed <- as.POSIXct(
    sub("Z$", "", stamp$builtAt),
    format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
  )
  if (is.na(parsed)) NULL else parsed
}

#' Warn (via message, so it lands in the console mid-align_open() instead of
#' a deferred warning) when the served bundle predates the plugin/core source
#' it was built from. Never fatal: a stale bundle still runs — the point is
#' that nobody reviews one unknowingly.
.align_check_bundle_freshness <- function(www_root) {
  root <- .align_find_repo_root()
  if (is.null(root)) return(invisible(NULL))
  src_dirs <- file.path(root, "packages", c("rstudio-host", "core"), "src")
  src_files <- list.files(src_dirs, recursive = TRUE, full.names = TRUE)
  if (length(src_files) == 0) return(invisible(NULL))
  newest <- max(file.mtime(src_files), na.rm = TRUE)
  built_at <- .align_bundle_built_at(www_root)
  rebuild <- paste0(
    "rebuild with `npm run build --workspace=@align/rstudio-host`, ",
    "then restart the server (align_stop(); align_open())."
  )
  if (is.null(built_at)) {
    message("Align bundle has no build stamp (predates the ALI-269 policy) - ", rebuild)
  } else if (newest > built_at) {
    message(
      "Align bundle is STALE: built ", format(built_at, usetz = TRUE),
      ", but plugin/core source changed ", format(newest, usetz = TRUE),
      " - ", rebuild
    )
  }
  invisible(NULL)
}
