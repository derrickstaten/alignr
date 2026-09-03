# Binary export-file writes for the plugin's Export menu (ALI-186, closing
# ALI-186).
#
# Why this needs its own endpoint rather than reusing align_write_doc
# (docs.R): that path is text-only (writeLines(content, useBytes=TRUE)) by
# design — .align files are always UTF-8 JSON. Export formats (PNG/PDF/ZIP/
# PPTX) are binary, so bytes have to cross the JSON-bodied HTTP boundary as
# base64 and land on disk via writeBin, not writeLines. See docs/product/
# rstudio.md RS-2x for why routing through the R working directory (rather
# than a browser download) is the plugin's answer to the Viewer pane's
# webview not reliably supporting synthetic <a download> clicks.

# File names only, any of the four export extensions — deliberately more
# permissive than .align_safe_doc_name's forced .align suffix (docs.R) but
# built the same way: reject separators/traversal so the HTTP layer can't be
# walked out of the working directory.
.align_safe_export_name <- function(name) {
  name <- as.character(name)
  if (length(name) != 1 || is.na(name) || !nzchar(name)) return(NULL)
  if (grepl("[/\\\\]", name) || grepl("\\.\\.", name)) return(NULL)
  if (!grepl("\\.(png|pdf|zip|pptx)$", name, ignore.case = TRUE)) return(NULL)
  name
}

#' Writes a base64-encoded export payload (PNG/PDF/ZIP/PPTX bytes) to the
#' working directory. `content_base64` is produced client-side by the
#' plugin's exportAs implementation (packages/rstudio-host/src/export.ts) —
#' the same bytes @align/core's export functions already produce for the web
#' app's browser download, just base64-transported instead of blob-downloaded.
#' Write-then-rename mirrors align_write_doc's crash-safety (docs.R).
align_write_export <- function(name, content_base64) {
  safe <- .align_safe_export_name(name)
  if (is.null(safe)) return(list(error = "Invalid export file name."))
  if (!is.character(content_base64) || length(content_base64) != 1 || !nzchar(content_base64)) {
    return(list(error = "Empty export content."))
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return(list(error = "The base64enc package is required for export — install.packages(\"base64enc\")."))
  }
  bytes <- tryCatch(base64enc::base64decode(content_base64), error = function(e) NULL)
  if (is.null(bytes)) return(list(error = "Could not decode export content."))
  path <- file.path(getwd(), safe)
  tmp <- paste0(path, ".tmp")
  con <- file(tmp, "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeBin(bytes, con)
  close(con)
  file.rename(tmp, path)
  list(name = safe, bytes = as.numeric(file.info(path)$size))
}
