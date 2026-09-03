# RDS preview for the plugin's Data tab (DS-397).
#
# The R that turns an .rds into a JSON description lives in @align/core
# (packages/core/src/rds-describe.ts) so the web app's WebR and this package
# evaluate the SAME snippet — the host sends it with each request rather than
# this package carrying a second, hand-synced copy (the marker parser already
# pays that tax; this one doesn't have to). Evaluating a script the pane sends
# is no new capability: /render has always run arbitrary code from the pane.

#' Describe .rds bytes using the snippet the host supplies.
#'
#' @param contentBase64 The file bytes, base64 (how the document stores them).
#' @param script R source defining `.align_describe_rds(path)`.
#' @return list(json = <string>) or list(error = <string>).
align_describe_rds_bytes <- function(contentBase64, script) {
  if (!is.character(contentBase64) || length(contentBase64) != 1 || !nzchar(contentBase64)) {
    return(list(error = "No file bytes."))
  }
  if (!is.character(script) || length(script) != 1 || !nzchar(script)) {
    return(list(error = "No describe script."))
  }
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(jsonlite::base64_dec(contentBase64), tmp)
  env <- new.env(parent = globalenv())
  out <- tryCatch({
    eval(parse(text = script), envir = env)
    env$.align_describe_rds(tmp)
  }, error = function(e) e)
  if (inherits(out, "error")) return(list(error = conditionMessage(out)))
  if (!is.character(out) || length(out) != 1) return(list(error = "The describe script returned no JSON."))
  list(json = out)
}
