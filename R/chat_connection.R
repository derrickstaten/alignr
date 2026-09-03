# Durable chat-connection storage (ALI-275, Derrick's call 2026-08-21).
#
# The Viewer pane's localStorage is origin-scoped and align_start() binds a
# fresh random port each session, so a token kept only in the browser dies on
# every reload — the user re-pasted their API key each time. The durable home
# is a config file in the user's R config dir (same trust model as
# ~/.Renviron: plaintext on the user's own disk, 0600). The localhost server
# reads it back to the pane; any process that could call this endpoint runs
# as the same user who can read the file directly, so the endpoint adds no
# exposure beyond the file itself. This file is also the intended slot for
# whatever credential replaces the API token when real auth lands.

.align_chat_conn_path <- function() {
  file.path(tools::R_user_dir("alignr", which = "config"), "chat-connection.json")
}

align_get_chat_connection <- function() {
  path <- .align_chat_conn_path()
  if (!file.exists(path)) return(list(configured = FALSE))
  conn <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  if (is.null(conn) || !is.character(conn$serverUrl) || !is.character(conn$token)) {
    return(list(configured = FALSE))
  }
  list(configured = TRUE, serverUrl = conn$serverUrl, token = conn$token)
}

align_set_chat_connection <- function(serverUrl, token) {
  if (!is.character(serverUrl) || length(serverUrl) != 1 || !nzchar(serverUrl) ||
      !is.character(token) || length(token) != 1 || !nzchar(token)) {
    return(list(error = "serverUrl and token are both required."))
  }
  path <- .align_chat_conn_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(list(serverUrl = serverUrl, token = token), auto_unbox = TRUE), path)
  Sys.chmod(path, mode = "0600")
  list(ok = TRUE)
}

align_clear_chat_connection <- function() {
  unlink(.align_chat_conn_path())
  list(ok = TRUE)
}

# ---------------------------------------------------------------------------
# Chat history (chrome round 2, ALI-186): conversations persist in the R
# user data dir, keyed by document id, so the pane's per-port localStorage
# amnesia stops erasing chat history the way it used to erase the API key.
# Whole-blob read/write — plugin chats are small, and one file per doc keeps
# eviction trivial (delete the file).
# ---------------------------------------------------------------------------

.align_chat_history_path <- function(doc_id) {
  safe <- gsub("[^A-Za-z0-9_-]", "_", as.character(doc_id))
  file.path(tools::R_user_dir("alignr", which = "data"), "chat-history", paste0(safe, ".json"))
}

#' Raw conversations JSON for one document ('[]' when none). Returned as an
#' opaque string: the R side never interprets the conversation shape, so the
#' TS side can evolve it without a package release.
align_get_chat_history <- function(doc_id) {
  path <- .align_chat_history_path(doc_id)
  if (!file.exists(path)) return(list(json = "[]"))
  list(json = paste(readLines(path, warn = FALSE), collapse = "\n"))
}

align_set_chat_history <- function(doc_id, json) {
  if (!is.character(json) || length(json) != 1) return(list(error = "history json required"))
  path <- .align_chat_history_path(doc_id)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(json, path, useBytes = TRUE)
  Sys.chmod(path, mode = "0600")
  list(ok = TRUE)
}
