# RStudio addin entry points (ALI-186 T1). Positron runs these through its
# rstudioapi shim ("R: Run RStudio Addin"), so one package covers both IDEs.

#' Open Align in the Viewer pane (or the default browser outside RStudio).
align_open <- function() {
  url <- align_start()
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::viewer(url)
  } else {
    utils::browseURL(url)
  }
  invisible(url)
}

#' Pop the running Align out of the Viewer pane into the system browser —
#' the Viewer is cramped for an 8.5x11 canvas.
align_pop_out <- function() {
  if (is.null(.align_state$url)) stop("Align isn't running — align_open() first.")
  utils::browseURL(.align_state$url)
  invisible(.align_state$url)
}
