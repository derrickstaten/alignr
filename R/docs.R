# .align document persistence for the addin (ALI-186 T3).
#
# Documents live as plain .align files (JSON, the shared schema v2.1 — the
# SAME bytes the web app reads/writes) in the session's working directory.
# That's deliberate: getwd() is where R users already keep project artifacts,
# it survives the session, and it makes "open the same file in the web app"
# a file-picker away. Snapshots never contain rendered SVG (schema rule), so
# files stay small.

# File names only — never paths. Anything with a separator or traversal is
# rejected so the HTTP layer can't be walked out of the working directory.
.align_safe_doc_name <- function(name) {
  name <- as.character(name)
  if (length(name) != 1 || is.na(name) || !nzchar(name)) return(NULL)
  if (grepl("[/\\\\]", name) || grepl("\\.\\.", name)) return(NULL)
  if (!grepl("\\.align$", name)) name <- paste0(name, ".align")
  name
}

#' .align files in the working directory, newest first.
align_list_docs <- function() {
  files <- list.files(getwd(), pattern = "\\.align$", full.names = FALSE)
  if (length(files) == 0) return(list())
  info <- file.info(file.path(getwd(), files))
  ord <- order(info$mtime, decreasing = TRUE)
  lapply(ord, function(i) list(
    name = files[i],
    bytes = as.numeric(info$size[i]),
    modifiedAt = format(info$mtime[i], "%Y-%m-%dT%H:%M:%S%z")
  ))
}

#' Raw response for a document read (bypasses jsonlite: the file already IS
#' the JSON — re-encoding a large snapshot would double the work and risk
#' mangling numbers).
align_read_doc <- function(name) {
  safe <- .align_safe_doc_name(name)
  if (is.null(safe) || !file.exists(file.path(getwd(), safe))) {
    return(list(status = 404L,
                headers = list("Content-Type" = "application/json", "Cache-Control" = "no-store"),
                body = jsonlite::toJSON(list(error = "Document not found."), auto_unbox = TRUE)))
  }
  list(
    status = 200L,
    headers = list("Content-Type" = "application/json; charset=utf-8", "Cache-Control" = "no-store"),
    body = readBin(file.path(getwd(), safe), "raw", file.info(file.path(getwd(), safe))$size)
  )
}

#' Write a document. `content` is the snapshot JSON as a string (already
#' serialized by @align/core dehydrate — the R side never re-interprets it).
align_write_doc <- function(name, content) {
  safe <- .align_safe_doc_name(name)
  if (is.null(safe)) return(list(error = "Invalid document name."))
  if (!is.character(content) || length(content) != 1 || !nzchar(content)) {
    return(list(error = "Empty document content."))
  }
  path <- file.path(getwd(), safe)
  # Write-then-rename so a crash mid-write can't truncate an existing doc.
  tmp <- paste0(path, ".tmp")
  writeLines(content, tmp, useBytes = TRUE)
  file.rename(tmp, path)
  list(name = safe, bytes = as.numeric(file.info(path)$size))
}
