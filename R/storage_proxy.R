# ---------------------------------------------------------------------------
# Presigned storage requests, repeated from the R side (DS-1029 plan step 4,
# DS-1033).
#
# A cloud document's checkpoint, code blobs and data files live in Align's
# R2 bucket and are read and written through presigned URLs. The bucket's CORS
# allows only Align web's origins, and the Viewer pane's origin is
# http://127.0.0.1:<a new port every launch>, which R2's CORS cannot
# wildcard. Derrick chose this proxy over widening the bucket (2026-11-23):
# the pane sends each presigned request to this same-origin endpoint and R
# repeats it server-side, where CORS does not apply.
#
# It is deliberately narrow, so it can't become a way to fetch anything:
#   - only presigned URLs (an X-Amz-Signature in the query) are accepted;
#   - only https to <bucket>.<account>.r2.cloudflarestorage.com (any
#     subdomain of r2.cloudflarestorage.com) — plus a loopback host when
#     ALIGNR_SERVER points this plugin at a local/dev Align (the e2e storage
#     stand-in on 127.0.0.1);
#   - only GET, PUT and HEAD;
#   - the request must carry the x-align-storage-method header, which a page
#     on another origin can't send without a CORS preflight this server never
#     answers.
# ---------------------------------------------------------------------------

#' Whether a presigned storage URL may be proxied (see the file header).
#' @noRd
.align_storage_url_allowed <- function(url, dev_override = nzchar(Sys.getenv("ALIGNR_SERVER", unset = ""))) {
  if (!is.character(url) || length(url) != 1 || !nzchar(url)) return(FALSE)
  m <- regmatches(url, regexec("^(https?)://([^/:?#]+)(:[0-9]+)?(/[^?#]*)?\\?(.*)$", url))[[1]]
  if (length(m) == 0) return(FALSE)
  scheme <- m[2]
  host <- tolower(m[3])
  query <- m[6]
  if (!grepl("(^|&)X-Amz-Signature=", query)) return(FALSE)
  # Virtual-hosted presigned URLs name the bucket in the host:
  # <bucket>.<account>.r2.cloudflarestorage.com (seen live, 2026-11-23).
  if (scheme == "https" && grepl("^([a-z0-9-]+\\.)+r2\\.cloudflarestorage\\.com$", host)) return(TRUE)
  if (isTRUE(dev_override) && host %in% c("127.0.0.1", "localhost")) return(TRUE)
  FALSE
}

#' Repeats one presigned storage request for the pane and returns R2's
#' status, Content-Type and body unchanged (DS-1033).
#' @noRd
.align_storage_proxy <- function(req) {
  method <- toupper(as.character(.align_or(req$HTTP_X_ALIGN_STORAGE_METHOD, "")))
  if (!method %in% c("GET", "PUT", "HEAD")) {
    return(.align_json(list(error = "Missing or unsupported storage method."), status = 400L))
  }
  qs <- .align_parse_query(req$QUERY_STRING)
  url <- qs$u
  if (!.align_storage_url_allowed(url)) {
    return(.align_json(list(error = "Only Align's presigned storage URLs can be fetched."), status = 403L))
  }
  h <- curl::new_handle()
  curl::handle_setopt(h, customrequest = method, followlocation = FALSE, timeout = 300L)
  if (method == "HEAD") curl::handle_setopt(h, nobody = TRUE)
  if (method == "PUT") {
    body <- req$rook.input$read()
    if (is.null(body)) body <- raw(0)
    curl::handle_setopt(h, postfields = body, postfieldsize = length(body))
    type <- req$HTTP_X_ALIGN_STORAGE_CONTENT_TYPE
    curl::handle_setheaders(h, .list = c(
      if (!is.null(type) && nzchar(type)) list("Content-Type" = type) else list("Content-Type" = ""),
      list("Expect" = "")
    ))
  }
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
  if (inherits(res, "error")) {
    return(.align_json(list(error = paste("Couldn't reach Align's storage:", conditionMessage(res))), status = 502L))
  }
  headers <- curl::parse_headers_list(res$headers)
  type <- .align_or(headers[["content-type"]], "application/octet-stream")
  list(
    status = as.integer(res$status_code),
    headers = list("Content-Type" = type, "Cache-Control" = "no-store"),
    body = res$content
  )
}
