# Deployed-release check (DS-404). A released bundle is stamped with the
# production commit it was cut from (build-stamp.json: builtAt, commit,
# version); align_start() asks the Align server which commit it is running
# and says so, once, when they differ. Advisory only — a mismatch never
# blocks starting, because the plugin usually keeps working across releases
# and the message is there to make a stale install explain itself rather
# than fail mysteriously later (schema version, handoff contract, AI API).
#
# Dev bundles (built from a checkout, no `version` in the stamp) skip this:
# the freshness guard in bundle_check.R already covers "your bundle is older
# than your source", which is the only staleness that matters there.

.ALIGN_DEFAULT_SERVER <- "https://alignfigures.com"

#' Read the whole build stamp, or NULL when absent/unparseable.
.align_read_build_stamp <- function(www_root) {
  stamp_path <- file.path(www_root, "build-stamp.json")
  if (!file.exists(stamp_path)) return(NULL)
  tryCatch(jsonlite::fromJSON(stamp_path), error = function(e) NULL)
}

#' Which server to compare against: the one the user signed in to if any,
#' else production. ALIGNR_SERVER overrides both (local testing against a
#' dev server that has no VERCEL_GIT_COMMIT_SHA answers null, i.e. no-op).
.align_release_server <- function() {
  override <- Sys.getenv("ALIGNR_SERVER", unset = "")
  if (nzchar(override)) return(sub("/+$", "", override))
  record <- tryCatch(.align_oauth_load(), error = function(e) NULL)
  if (is.list(record) && is.character(record$serverUrl) && nzchar(record$serverUrl)) {
    return(sub("/+$", "", record$serverUrl))
  }
  .ALIGN_DEFAULT_SERVER
}

#' The deployed commit per GET /api/version, or NULL on any failure. Two
#' second timeout: this runs on every align_start() and must never make a
#' working plugin feel slow because a network is down.
.align_fetch_deployed_commit <- function(server) {
  tryCatch({
    handle <- curl::new_handle(timeout = 2, connecttimeout = 2)
    res <- curl::curl_fetch_memory(paste0(server, "/api/version"), handle = handle)
    if (!identical(as.integer(res$status_code), 200L)) return(NULL)
    body <- jsonlite::fromJSON(rawToChar(res$content))
    if (is.character(body$commit) && length(body$commit) == 1 && nzchar(body$commit)) body$commit else NULL
  }, error = function(e) NULL)
}

#' Same release? Compared on the short sha so a stamp written from a
#' shortened `git rev-parse` still matches a full Vercel sha.
.align_same_release <- function(stamp_commit, deployed_commit) {
  if (!is.character(stamp_commit) || !is.character(deployed_commit)) return(NA)
  a <- substr(stamp_commit, 1, 7)
  b <- substr(deployed_commit, 1, 7)
  nzchar(a) && nzchar(b) && identical(a, b)
}

#' The message for a mismatch, kept pure so the test harness can pin it.
.align_release_mismatch_message <- function(version, server) {
  paste0(
    "Align plugin ", version, " was built for a different Align Web release than ",
    server, " is running. Update with remotes::install_github(\"derrickstaten/alignr\")."
  )
}

.align_check_release_version <- function(www_root) {
  stamp <- .align_read_build_stamp(www_root)
  # No version = dev bundle; no commit = pre-DS-404 stamp. Nothing to compare.
  if (is.null(stamp) || !is.character(stamp$version) || !is.character(stamp$commit)) {
    return(invisible(NULL))
  }
  server <- .align_release_server()
  deployed <- .align_fetch_deployed_commit(server)
  if (is.null(deployed)) return(invisible(NULL))
  if (isFALSE(.align_same_release(stamp$commit, deployed))) {
    message(.align_release_mismatch_message(stamp$version, server))
  }
  invisible(NULL)
}
