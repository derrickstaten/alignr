# ---------------------------------------------------------------------------
# Durable key-value store for the Viewer pane (DS-1033, epic DS-1026).
#
# The pane runs on 127.0.0.1:<random port>, a new origin every align_start(),
# so its localStorage is effectively wiped each session — fine for UI
# preferences, fatal for a cloud document's queue of UNSENT edits, which is
# the one thing that must survive RStudio closing (ALI-275 learned the same
# lesson for the chat credential). This store is where the pane keeps them:
# one file per key under the R user data dir, 0600 like the chat history.
#
# Opaque on purpose: R never interprets a value, so the TypeScript side owns
# the shapes and can change them without an alignr release.
# ---------------------------------------------------------------------------

.align_store_dir <- function() {
  file.path(tools::R_user_dir("alignr", which = "data"), "store")
}

# File name for a key: base64url of its UTF-8 bytes. Collision-free (unlike
# sanitising characters away) and reversible, so a prefix listing can return
# the real keys.
.align_store_file <- function(key) {
  enc <- openssl::base64_encode(charToRaw(enc2utf8(key)))
  enc <- gsub("=+$", "", chartr("+/", "-_", enc))
  file.path(.align_store_dir(), paste0(enc, ".json"))
}

.align_store_key_from_file <- function(file) {
  enc <- sub("\\.json$", "", basename(file))
  enc <- chartr("-_", "+/", enc)
  pad <- (4 - nchar(enc) %% 4) %% 4
  enc <- paste0(enc, strrep("=", pad))
  rawToChar(openssl::base64_decode(enc))
}

#' Every stored key that starts with `prefix`, with its value. The pane loads
#' a document's entries once, before its sync client starts, then keeps a
#' synchronous in-memory mirror (createMirroredStorage on the TS side).
align_store_list <- function(prefix = "") {
  dir <- .align_store_dir()
  if (!dir.exists(dir)) return(list(entries = setNames(list(), character())))
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  out <- list()
  for (f in files) {
    key <- tryCatch(.align_store_key_from_file(f), error = function(e) NULL)
    if (is.null(key) || !startsWith(key, prefix)) next
    out[[key]] <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  if (length(out) == 0) out <- setNames(list(), character())
  list(entries = out)
}

#' Writes `value` under `key`, or removes the key when `value` is NULL.
#' Write-then-rename, so a crash mid-write never leaves a torn queue.
align_store_set <- function(key, value = NULL) {
  if (!is.character(key) || length(key) != 1 || !nzchar(key)) return(list(error = "key required"))
  path <- .align_store_file(key)
  if (is.null(value)) {
    unlink(path)
    return(list(ok = TRUE))
  }
  if (!is.character(value) || length(value) != 1) return(list(error = "value must be a string"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  writeLines(value, tmp, useBytes = TRUE)
  Sys.chmod(tmp, mode = "0600")
  file.rename(tmp, path)
  list(ok = TRUE)
}
