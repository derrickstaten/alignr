# httpuv transport for the Align RStudio addin (ALI-186 T1).
#
# Design constraints carried from the ALI-179 spike:
#  - Validate resources BEFORE binding the port: binding without content made
#    every request 500 in a way that read as "broken server", not "wrong dir".
#  - Never hard-code the port: an abandoned rsession can strand it, and
#    stopAllServers() can't reach across processes. Ask httpuv for a free one.
#  - Cache-Control: no-store on everything — the Viewer pane caches hard.
#  - R errors travel as JSON data ({error: ...}), never as thrown conditions.
#  - httpuv drives itself off later's event loop, so the console stays free —
#    that's what makes the live-session demo possible at all.

.align_state <- new.env(parent = emptyenv())

.align_json <- function(body, status = 200L) {
  list(
    status = status,
    headers = list(
      "Content-Type" = "application/json; charset=utf-8",
      "Cache-Control" = "no-store"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  )
}

.align_read_body <- function(req) {
  raw <- req$rook.input$read()
  if (length(raw) == 0) return(list())
  jsonlite::fromJSON(rawToChar(raw), simplifyVector = TRUE)
}

#' Minimal application/x-www-form-urlencoded query-string parser — httpuv
#' hands us the raw QUERY_STRING and does no parsing of its own.
.align_parse_query <- function(qs) {
  if (is.null(qs) || !nzchar(qs)) return(list())
  qs <- sub("^\\?", "", qs)
  out <- list()
  for (pair in strsplit(qs, "&", fixed = TRUE)[[1]]) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(kv) < 1 || !nzchar(kv[1])) next
    key <- utils::URLdecode(kv[1])
    out[[key]] <- if (length(kv) >= 2) utils::URLdecode(kv[2]) else ""
  }
  out
}

.align_static <- function(www_root, path) {
  rel <- sub("^/", "", path)
  if (rel == "" || rel == "/") rel <- "index.html"
  file <- normalizePath(file.path(www_root, rel), mustWork = FALSE)
  # Path traversal guard: resolved file must stay inside www_root.
  if (!startsWith(file, normalizePath(www_root)) || !file.exists(file)) {
    return(list(status = 404L, headers = list("Cache-Control" = "no-store"), body = "Not found"))
  }
  ext <- tolower(tools::file_ext(file))
  type <- switch(ext,
    html = "text/html; charset=utf-8",
    js = "text/javascript; charset=utf-8",
    mjs = "text/javascript; charset=utf-8",
    css = "text/css; charset=utf-8",
    svg = "image/svg+xml",
    json = "application/json",
    png = "image/png",
    woff2 = "font/woff2",
    "application/octet-stream"
  )
  list(
    status = 200L,
    headers = list("Content-Type" = type, "Cache-Control" = "no-store"),
    body = readBin(file, "raw", file.info(file)$size)
  )
}

.align_app <- function(www_root) {
  list(call = function(req) {
    path <- req$PATH_INFO
    method <- req$REQUEST_METHOD
    tryCatch({
      if (method == "GET" && path == "/ready") {
        return(.align_json(list(ready = TRUE, r = R.version.string)))
      }
      if (method == "GET" && path == "/vars") {
        return(.align_json(list(vars = align_list_session_data())))
      }
      if (method == "GET" && startsWith(path, "/var/")) {
        return(.align_json(align_preview_session_data(utils::URLdecode(sub("^/var/", "", path)))))
      }
      if (method == "POST" && path == "/render") {
        b <- .align_read_body(req)
        return(.align_json(align_render_svg(
          code = b$code,
          width_inches = if (is.null(b$widthInches)) 6 else as.numeric(b$widthInches),
          height_inches = if (is.null(b$heightInches)) 4 else as.numeric(b$heightInches),
          font_scale = if (is.null(b$fontScale)) 1 else as.numeric(b$fontScale)
        )))
      }
      if (method == "GET" && path == "/doc/list") {
        # wd rides along (DS-390): the Open menu shows WHICH directory the
        # listing is scoped to, so an empty list explains itself after a
        # setwd() instead of reading as "my file is gone".
        return(.align_json(list(docs = align_list_docs(), wd = getwd())))
      }
      if (method == "GET" && startsWith(path, "/doc/read/")) {
        return(align_read_doc(utils::URLdecode(sub("^/doc/read/", "", path))))
      }
      if (method == "POST" && path == "/doc/write") {
        b <- .align_read_body(req)
        return(.align_json(align_write_doc(b$name, b$content)))
      }
      if (method == "POST" && path == "/export/write") {
        # Export menu (ALI-186): binary PNG/PDF/ZIP/PPTX bytes,
        # base64-transported since the request body here is JSON text — see
        # export.R's header comment for why this can't reuse /doc/write.
        b <- .align_read_body(req)
        return(.align_json(align_write_export(b$name, b$contentBase64)))
      }
      if (method == "GET" && startsWith(path, "/file/stat")) {
        qs <- .align_parse_query(req$QUERY_STRING)
        return(.align_json(align_stat_file(if (is.null(qs$path)) "" else qs$path)))
      }
      if (method == "GET" && startsWith(path, "/file/read")) {
        qs <- .align_parse_query(req$QUERY_STRING)
        return(.align_json(align_read_file(if (is.null(qs$path)) "" else qs$path)))
      }
      if (method == "GET" && path == "/file/active") {
        return(.align_json(align_active_document_state()))
      }
      if (method == "POST" && path == "/annotate") {
        return(.align_json(align_annotate_selection()))
      }
      if (method == "GET" && path == "/annotate/pending") {
        return(.align_json(align_drain_pending_annotations()))
      }
      if (method == "GET" && path == "/annotate/context") {
        return(.align_json(align_get_editor_context()))
      }
      if (method == "POST" && path == "/apply-edit") {
        b <- .align_read_body(req)
        return(.align_json(align_apply_file_edit(b$filePath, b$markerId, b$code)))
      }
      if (method == "GET" && startsWith(path, "/chat-history/")) {
        return(.align_json(align_get_chat_history(utils::URLdecode(sub("^/chat-history/", "", path)))))
      }
      if (method == "POST" && path == "/chat-history") {
        b <- .align_read_body(req)
        return(.align_json(align_set_chat_history(b$docId, b$json)))
      }
      if (method == "GET" && path == "/chat-connection") {
        return(.align_json(align_get_chat_connection()))
      }
      if (method == "POST" && path == "/chat-connection") {
        b <- .align_read_body(req)
        # An empty body clears — the pane's Disconnect action.
        if (is.null(b$serverUrl) && is.null(b$token)) {
          return(.align_json(align_clear_chat_connection()))
        }
        return(.align_json(align_set_chat_connection(b$serverUrl, b$token)))
      }
      # ── OAuth sign-in (DS-273) ──────────────────────────────────────────
      # The pane can't run the flow itself (no system browser, no durable
      # storage), so it drives these five endpoints and the R side owns the
      # credential. Only /auth/token ever hands a secret to the pane, and it
      # is the short-lived access token — never the refresh token.
      if (method == "GET" && path == "/auth/status") {
        return(.align_json(align_oauth_status()))
      }
      if (method == "POST" && path == "/auth/signin") {
        b <- .align_read_body(req)
        return(.align_json(align_signin(
          b$serverUrl,
          mode = if (identical(b$mode, "code")) "code" else "loopback"
        )))
      }
      if (method == "POST" && path == "/auth/code") {
        b <- .align_read_body(req)
        return(.align_json(align_oauth_submit_code(b$code)))
      }
      if (method == "POST" && path == "/auth/signout") {
        return(.align_json(align_signout()))
      }
      if (method == "GET" && path == "/auth/token") {
        return(.align_json(align_oauth_access_token()))
      }
      if (method == "POST" && path == "/open-web") {
        # Tier A "Open in Align web" (ALI-273): the Viewer pane can't reliably
        # open the system browser itself, so the host asks the R side to.
        # http(s) only — this must never become a general command runner.
        b <- .align_read_body(req)
        url <- as.character(b$url)
        if (length(url) != 1 || !grepl("^https?://", url)) {
          return(.align_json(list(error = "Only http(s) URLs can be opened."), status = 400L))
        }
        utils::browseURL(url)
        return(.align_json(list(ok = TRUE)))
      }
      if (method == "POST" && path == "/file/describe-rds") {
        # DS-397: RDS preview for the Data tab. See describe.R.
        b <- .align_read_body(req)
        return(.align_json(align_describe_rds_bytes(b$contentBase64, b$script)))
      }
      if (method == "POST" && path == "/handoff/capture") {
        # DS-396: the slice of the live session one figure's code depends
        # on, for the host to attach before the handoff save. See handoff.R.
        b <- .align_read_body(req)
        return(.align_json(align_capture_session(as.character(b$code))))
      }
      if (method == "POST" && path == "/install") {
        b <- .align_read_body(req)
        pkg <- as.character(b$name)
        if (!align_package_installed(pkg)) {
          utils::install.packages(pkg)
        }
        return(.align_json(list(installed = align_package_installed(pkg))))
      }
      .align_static(www_root, path)
    }, error = function(e) {
      .align_json(list(error = conditionMessage(e)), status = 500L)
    })
  })
}

#' Start the Align server. Returns the URL.
#'
#' @param www_root Directory holding the built rstudio-host bundle. Defaults to
#'   the installed package's inst/www; a source-tree path works for dev.
#' @param port Optional; a free port is chosen when NULL (see header).
align_start <- function(www_root = system.file("www", package = "alignr"), port = NULL) {
  if (!nzchar(www_root) || !file.exists(file.path(www_root, "index.html"))) {
    stop(
      "Align bundle not found (no index.html in '", www_root, "'). ",
      "Build packages/rstudio-host and copy dist/ into r-package/inst/www ",
      "before starting — refusing to bind a port with nothing to serve."
    )
  }
  .align_check_bundle_freshness(www_root)
  .align_check_release_version(www_root)
  align_stop()
  if (is.null(port)) port <- httpuv::randomPort()
  .align_state$server <- httpuv::startServer("127.0.0.1", port, .align_app(www_root))
  .align_state$url <- sprintf("http://127.0.0.1:%d/", port)
  message("Align server: ", .align_state$url, " (lives with this R session; align_stop() to end)")
  invisible(.align_state$url)
}

#' Stop the Align server, if running.
align_stop <- function() {
  if (!is.null(.align_state$server)) {
    httpuv::stopServer(.align_state$server)
    .align_state$server <- NULL
    .align_state$url <- NULL
  }
  invisible(NULL)
}
