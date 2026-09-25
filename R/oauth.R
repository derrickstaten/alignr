# OAuth 2.1 sign-in for the Align plugin's AI chat (DS-273).
#
# Replaces the ALI-198 T4a ritual — mint a personal access token on the web
# Account page, paste it into the Viewer pane — with the standard flow every
# other Align client uses (ALI-224's authorization server: auth code + PKCE).
# The user gets revocation and expiry for free, and the plugin stops holding a
# credential that never expires.
#
# Four constraints shaped everything below:
#
#  - **The Viewer pane cannot host the flow.** Its embedded browser has no
#    real address bar, no cookie jar shared with the user's normal browser,
#    and no reliable window.open. So sign-in opens the SYSTEM browser
#    (utils::browseURL, the same hook /open-web already uses) and the code
#    comes back out-of-band.
#  - **The code comes back by loopback, with copy-paste as the fallback.**
#    A second httpuv server on an ephemeral 127.0.0.1 port is the RFC 8252
#    native-app pattern, and httpuv is already a hard dependency. It cannot
#    work when the browser is on a different machine from the R session
#    (RStudio Server, Posit Workbench), so `mode = "code"` redirects to
#    Align's /oauth/code page instead and the user pastes the code back.
#  - **Nothing blocks the R console.** Both httpuv servers ride later's event
#    loop, so `align_signin()` returns immediately and the callback lands
#    asynchronously; the pane polls /auth/status. An implementation that spun
#    `httpuv::service()` in a while-loop would freeze the user's session for
#    as long as sign-in took, which is exactly the behaviour that makes
#    plugins feel broken.
#  - **Errors travel as data**, never as thrown conditions — the whole
#    package's rule, because every caller here is an HTTP handler.
#
# Token storage: the refresh token goes into the system keyring when the
# `keyring` package is available (Suggests — it is not worth a hard dependency
# and has system requirements on Linux), and otherwise into
# `~/.align/rstudio-oauth.json` at mode 0600. The file fallback is the same
# trust model as ~/.Renviron and as this package's existing
# chat-connection.json: plaintext on the user's own disk, readable only by
# them. It is written under ~/.align rather than R_user_dir() deliberately, so
# a user who wants to revoke locally has one obvious place to delete.

#' Which Align the plugin talks to (DS-452): production, no exceptions, for
#' users — the sign-in card no longer offers a URL. The one override is the
#' ALIGNR_SERVER environment variable (~/.Renviron, or Sys.setenv() before
#' align_open()), for developing against a local dev server; when it is set
#' the host reports `overridden = TRUE` and the pane shows an amber pill so a
#' stale .Renviron can never masquerade as production.
.ALIGN_PRODUCTION_SERVER <- "https://alignfigures.com"

align_server_config <- function() {
  override <- Sys.getenv("ALIGNR_SERVER", unset = "")
  if (nzchar(override)) {
    return(list(serverUrl = sub("/+$", "", override), overridden = TRUE))
  }
  list(serverUrl = .ALIGN_PRODUCTION_SERVER, overridden = FALSE)
}

#' The plugin's OAuth client id. A fixed, public, pre-registered id — see
#' lib/oauth/first-party-clients.ts on the server for why this is not dynamic
#' client registration. It is not a secret; PKCE is what proves possession.
ALIGN_OAUTH_CLIENT_ID <- "align-rstudio-plugin"

#' The one scope the plugin asks for. AI chat only — the plugin has no code
#' that touches documents over MCP, and asking for `align:docs` would be
#' consent it cannot honestly spend.
ALIGN_OAUTH_SCOPE <- "align:ai"

#' Refresh with this much life left on the access token rather than waiting
#' for expiry: a token that expires mid-request surfaces to the user as a chat
#' turn that failed for no reason. Sixty seconds comfortably covers a slow
#' round trip on a bad connection.
.ALIGN_TOKEN_SKEW_SECONDS <- 60

#' How long a started sign-in stays open. Long enough to find the browser
#' window, create an account, and read a consent screen; short enough that an
#' abandoned attempt releases its loopback port rather than holding it for the
#' life of the R session.
.ALIGN_SIGNIN_TIMEOUT_SECONDS <- 300

# In-memory flow + access-token cache. Deliberately not persisted: the access
# token is short-lived and re-derivable from the refresh token, so writing it
# to disk would widen the blast radius of a stolen file for no gain.
.align_oauth <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Small primitives
# ---------------------------------------------------------------------------

#' base64url (RFC 4648 §5) — unpadded, `-`/`_` for `+`/`/`. PKCE and the
#' server's own token format both speak this dialect, and base64's padding
#' would have to be percent-encoded in a query string anyway.
.align_b64url <- function(x) {
  out <- openssl::base64_encode(x)
  out <- gsub("+", "-", out, fixed = TRUE)
  out <- gsub("/", "_", out, fixed = TRUE)
  gsub("=", "", out, fixed = TRUE)
}

#' A PKCE verifier/challenge pair. `openssl::rand_bytes` rather than R's RNG:
#' `sample()` is a Mersenne Twister, which is predictable from previous
#' output, and the verifier is the only thing standing between an intercepted
#' authorization code and a token.
.align_pkce_pair <- function() {
  verifier <- .align_b64url(openssl::rand_bytes(32))
  list(verifier = verifier, challenge = .align_b64url(openssl::sha256(charToRaw(verifier))))
}

#' Null-coalescing helper. Named rather than an operator so it cannot shadow
#' base R 4.4's own `%||%` when the package is loaded under a newer R.
.align_or <- function(a, b) if (is.null(a)) b else a

#' Trailing-slash-free origin, so string concatenation downstream can't
#' produce the double slash that would break an exact redirect_uri match.
.align_normalize_server <- function(server_url) {
  sub("/+$", "", as.character(server_url)[1])
}

#' JSON POST with a parsed body and never a thrown condition. JSON rather than
#' form encoding because Align's token endpoint accepts both and JSON has no
#' ambiguity about how a value was escaped.
.align_post_json <- function(url, payload) {
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "Content-Type" = "application/json", "Accept" = "application/json")
  curl::handle_setopt(
    handle,
    post = TRUE,
    postfields = jsonlite::toJSON(payload, auto_unbox = TRUE),
    timeout = 30L
  )
  res <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(error = paste0("Could not reach ", url, ": ", conditionMessage(res))))
  }
  body <- tryCatch(
    jsonlite::fromJSON(rawToChar(res$content), simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (res$status_code >= 400) {
    detail <- if (!is.null(body$error_description)) body$error_description
              else if (!is.null(body$error)) body$error
              else paste0("HTTP ", res$status_code)
    return(list(error = as.character(detail)[1]))
  }
  if (is.null(body)) return(list(error = "The server returned a response Align could not read."))
  body
}

# ---------------------------------------------------------------------------
# Refresh-token storage
# ---------------------------------------------------------------------------

.align_oauth_file <- function() file.path(path.expand("~"), ".align", "rstudio-oauth.json")

#' Whether the system keyring is usable *right now*. `requireNamespace` is not
#' enough on its own: keyring is installed but unusable on a headless Linux
#' box with no secret service, and finding that out at write time (when the
#' user has just signed in) is far better than at read time (when they are
#' trying to chat).
.align_keyring_available <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) return(FALSE)
  isTRUE(tryCatch({ keyring::key_list(service = "align-rstudio"); TRUE }, error = function(e) FALSE))
}

.ALIGN_KEYRING_SERVICE <- "align-rstudio"

#' Persists the sign-in. The server URL is always written to the JSON file —
#' it is not a secret, and something has to record *which* Align a stored
#' refresh token belongs to. Only the refresh token itself goes to the
#' keyring, with `storage` recording where it went so the reader does not have
#' to guess (and so a user reading the file can see that a secret exists
#' elsewhere rather than concluding the file is corrupt).
.align_oauth_save <- function(server_url, refresh_token) {
  path <- .align_oauth_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  use_keyring <- .align_keyring_available()
  if (use_keyring) {
    ok <- tryCatch({
      keyring::key_set_with_value(.ALIGN_KEYRING_SERVICE, username = server_url, password = refresh_token)
      TRUE
    }, error = function(e) FALSE)
    use_keyring <- ok
  }
  record <- list(
    serverUrl = server_url,
    storage = if (use_keyring) "keyring" else "file",
    refreshToken = if (use_keyring) NULL else refresh_token
  )
  writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null"), path)
  Sys.chmod(path, mode = "0600")
  .align_oauth$storedCache <- list(serverUrl = server_url, refreshToken = refresh_token)
  invisible(TRUE)
}

#' Reads the sign-in back, or NULL. A keyring record whose secret has since
#' been deleted from the keychain reads as signed-out rather than as an error:
#' the honest state is "we have no refresh token", and the user's fix is to
#' sign in again either way.
.align_oauth_load <- function() {
  # Memoised because the pane polls /auth/status roughly once a second while
  # a sign-in is open, and on macOS every keyring read is a Keychain call —
  # cheap once, a visible stall (and, the first time, a permission prompt) if
  # repeated on a poll. The cache is invalidated by the only two things that
  # can change the answer: .align_oauth_save and .align_oauth_clear.
  if (!is.null(.align_oauth$storedCache)) return(.align_oauth$storedCache)
  path <- .align_oauth_file()
  if (!file.exists(path)) return(NULL)
  record <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  if (is.null(record) || !is.character(record$serverUrl)) return(NULL)
  refresh <- record$refreshToken
  if (identical(record$storage, "keyring")) {
    refresh <- tryCatch(
      keyring::key_get(.ALIGN_KEYRING_SERVICE, username = record$serverUrl),
      error = function(e) NULL
    )
  }
  if (!is.character(refresh) || length(refresh) != 1 || !nzchar(refresh)) return(NULL)
  .align_oauth$storedCache <- list(serverUrl = record$serverUrl, refreshToken = refresh)
  .align_oauth$storedCache
}

.align_oauth_clear <- function() {
  path <- .align_oauth_file()
  record <- if (file.exists(path)) tryCatch(jsonlite::fromJSON(path), error = function(e) NULL) else NULL
  if (!is.null(record) && identical(record$storage, "keyring") && is.character(record$serverUrl)) {
    tryCatch(keyring::key_delete(.ALIGN_KEYRING_SERVICE, username = record$serverUrl), error = function(e) NULL)
  }
  unlink(path)
  rm(list = ls(.align_oauth), envir = .align_oauth)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# The flow
# ---------------------------------------------------------------------------

#' Stops and forgets the one-shot loopback listener, if any. Called from every
#' terminal branch (success, error, timeout, sign-out) — an httpuv server left
#' bound would keep answering a redirect for a flow that is over.
.align_release_listener <- function() {
  if (!is.null(.align_oauth$listener)) {
    tryCatch(httpuv::stopServer(.align_oauth$listener), error = function(e) NULL)
    .align_oauth$listener <- NULL
  }
  invisible(NULL)
}

#' Exchanges an authorization code for tokens and persists the result. Shared
#' by the loopback callback and the paste-the-code fallback, so both paths
#' agree about PKCE, storage, and what counts as success.
.align_oauth_exchange <- function(code) {
  pending <- .align_oauth$pending
  if (is.null(pending)) return(list(error = "No sign-in is in progress. Start again from the plugin."))
  body <- .align_post_json(paste0(pending$serverUrl, "/api/oauth/token"), list(
    grant_type = "authorization_code",
    code = code,
    redirect_uri = pending$redirectUri,
    client_id = ALIGN_OAUTH_CLIENT_ID,
    code_verifier = pending$verifier
  ))
  if (!is.null(body$error)) return(list(error = body$error))
  if (!is.character(body$access_token) || !is.character(body$refresh_token)) {
    return(list(error = "The server did not return a usable token pair."))
  }
  .align_oauth_save(pending$serverUrl, body$refresh_token)
  .align_oauth$access <- list(
    token = body$access_token,
    expiresAt = Sys.time() + as.numeric(.align_or(body$expires_in, 3600))
  )
  .align_oauth$pending <- NULL
  .align_release_listener()
  list(ok = TRUE, serverUrl = pending$serverUrl)
}

#' The one-shot loopback app. Answers exactly one path and hands the browser a
#' human-readable page either way — a bare 200 with no body reads, to someone
#' staring at a browser tab, as the flow having silently failed.
.align_callback_app <- function(expected_state) {
  list(call = function(req) {
    if (!identical(req$PATH_INFO, "/callback")) {
      return(list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not found"))
    }
    qs <- .align_parse_query(req$QUERY_STRING)
    result <- if (!is.null(qs$error)) {
      list(error = as.character(.align_or(qs$error_description, qs$error)))
    } else if (!identical(as.character(qs$state), expected_state)) {
      # CSRF defence: a callback whose state does not match the one this
      # session generated was not started by this session.
      list(error = "Sign-in state did not match. Start again from the plugin.")
    } else if (is.null(qs$code)) {
      list(error = "The browser came back without an authorization code.")
    } else {
      .align_oauth_exchange(as.character(qs$code))
    }
    .align_oauth$result <- result
    if (isTRUE(result$ok)) .align_release_listener()
    page <- if (isTRUE(result$ok)) {
      "<h2>Signed in to Align</h2><p>You can close this tab and return to RStudio.</p>"
    } else {
      paste0("<h2>Align sign-in failed</h2><p>", result$error, "</p>")
    }
    list(
      status = if (isTRUE(result$ok)) 200L else 400L,
      headers = list("Content-Type" = "text/html; charset=utf-8", "Cache-Control" = "no-store"),
      body = paste0("<!doctype html><meta charset=utf-8><body style='font-family:sans-serif;padding:3rem'>", page)
    )
  })
}

#' Start an OAuth sign-in against the resolved Align server (DS-452:
#' production unless ALIGNR_SERVER is set — see align_server_config()).
#'
#' @param mode "loopback" (default) binds a one-shot local listener for the
#'   redirect; "code" redirects to Align's /oauth/code page for copy-paste,
#'   which is the only thing that works when the browser and the R session are
#'   on different machines.
#' @return A list with `authorizeUrl` and `mode`, or `error`. Returns
#'   immediately — the callback lands later on the event loop; poll
#'   `align_oauth_status()`.
align_signin <- function(mode = c("loopback", "code")) {
  mode <- match.arg(mode)
  server_url <- .align_normalize_server(align_server_config()$serverUrl)
  if (!nzchar(server_url) || !grepl("^https?://", server_url)) {
    return(list(error = paste0("ALIGNR_SERVER is not an http(s) URL: ", server_url)))
  }
  .align_release_listener()
  pkce <- .align_pkce_pair()
  state <- .align_b64url(openssl::rand_bytes(16))

  redirect_uri <- NULL
  if (mode == "loopback") {
    port <- httpuv::randomPort()
    listener <- tryCatch(
      httpuv::startServer("127.0.0.1", port, .align_callback_app(state)),
      error = function(e) e
    )
    if (inherits(listener, "error")) {
      # Not fatal: this is precisely the situation the copy-paste mode exists
      # for, so say so rather than dead-ending.
      return(list(error = paste0(
        "Could not open a local listener for the sign-in redirect (", conditionMessage(listener),
        "). Try the paste-a-code option instead."
      )))
    }
    .align_oauth$listener <- listener
    redirect_uri <- sprintf("http://127.0.0.1:%d/callback", port)
  } else {
    redirect_uri <- paste0(server_url, "/oauth/code")
  }

  .align_oauth$pending <- list(
    serverUrl = server_url,
    verifier = pkce$verifier,
    state = state,
    redirectUri = redirect_uri,
    startedAt = Sys.time(),
    mode = mode
  )
  .align_oauth$result <- NULL

  query <- paste(
    c(
      "response_type=code",
      paste0("client_id=", utils::URLencode(ALIGN_OAUTH_CLIENT_ID, reserved = TRUE)),
      paste0("redirect_uri=", utils::URLencode(redirect_uri, reserved = TRUE)),
      paste0("scope=", utils::URLencode(ALIGN_OAUTH_SCOPE, reserved = TRUE)),
      paste0("state=", utils::URLencode(state, reserved = TRUE)),
      paste0("code_challenge=", pkce$challenge),
      "code_challenge_method=S256",
      paste0("resource=", utils::URLencode(paste0(server_url, "/api/ai"), reserved = TRUE))
    ),
    collapse = "&"
  )
  authorize_url <- paste0(server_url, "/oauth/authorize?", query)
  tryCatch(utils::browseURL(authorize_url), error = function(e) NULL)
  list(ok = TRUE, mode = mode, authorizeUrl = authorize_url)
}

#' Finish a sign-in by pasting the code from Align's /oauth/code page.
align_oauth_submit_code <- function(code) {
  code <- as.character(code)[1]
  if (!is.character(code) || is.na(code) || !nzchar(trimws(code))) {
    return(list(error = "Paste the code from the Align page."))
  }
  result <- .align_oauth_exchange(trimws(code))
  .align_oauth$result <- result
  result
}

#' Current sign-in state for the pane's poll.
#'
#' Expiring a stale pending flow here, rather than on a timer, is deliberate:
#' a timer would keep the R session's event loop busy for an attempt the user
#' abandoned, and the only observer that cares is this poll.
align_oauth_status <- function() {
  pending <- .align_oauth$pending
  if (!is.null(pending) &&
      as.numeric(difftime(Sys.time(), pending$startedAt, units = "secs")) > .ALIGN_SIGNIN_TIMEOUT_SECONDS) {
    .align_oauth$pending <- NULL
    .align_release_listener()
    .align_oauth$result <- list(error = "Sign-in timed out. Try again.")
    pending <- NULL
  }
  stored <- .align_oauth_load()
  result <- .align_oauth$result
  target <- align_server_config()$serverUrl
  # A sign-in minted by a different Align than the one this plugin now
  # targets (DS-452: a developer flipping ALIGNR_SERVER) must not be reused
  # silently — it reads as signed out, with the reason, until the user signs
  # in again against the resolved server.
  mismatch <- !is.null(stored) && !identical(sub("/+$", "", stored$serverUrl), target)
  list(
    signedIn = !is.null(stored) && !mismatch,
    serverUrl = if (!is.null(stored) && !mismatch) stored$serverUrl else if (!is.null(pending)) pending$serverUrl else NULL,
    pending = !is.null(pending),
    mode = if (!is.null(pending)) pending$mode else NULL,
    error = if (!is.null(result) && !is.null(result$error)) result$error
            else if (mismatch) paste0("You were signed in to ", stored$serverUrl, "; this plugin now targets ", target, ". Sign in again.")
            else NULL
  )
}

#' Forget the sign-in: keyring entry, file, cached access token, and any
#' half-finished flow.
align_signout <- function() {
  .align_release_listener()
  .align_oauth_clear()
  list(ok = TRUE)
}

#' A usable access token, refreshing transparently when the cached one is gone
#' or nearly expired.
#'
#' Returns the token to the Viewer pane, which puts it in the Authorization
#' header of its /api/ai calls. That is a deliberate choice over proxying chat
#' through the R side: /api/ai streams, and re-implementing a streaming proxy
#' in httpuv would add a place for the stream to break for no security gain —
#' the pane and the R session are the same user on the same machine, and the
#' pane can already read the config file's contents through /chat-connection.
#' What the pane never sees is the refresh token, which is the credential
#' worth protecting.
align_oauth_access_token <- function() {
  cached <- .align_oauth$access
  if (!is.null(cached) &&
      as.numeric(difftime(cached$expiresAt, Sys.time(), units = "secs")) > .ALIGN_TOKEN_SKEW_SECONDS) {
    return(list(accessToken = cached$token, serverUrl = .align_oauth_load()$serverUrl))
  }
  stored <- .align_oauth_load()
  if (is.null(stored)) return(list(error = "Not signed in to Align."))

  body <- .align_post_json(paste0(stored$serverUrl, "/api/oauth/token"), list(
    grant_type = "refresh_token",
    refresh_token = stored$refreshToken,
    client_id = ALIGN_OAUTH_CLIENT_ID
  ))
  if (!is.null(body$error)) {
    # A refused refresh is terminal, not transient: the server revokes the
    # whole chain when it sees a replayed token, and retrying with the same
    # dead credential on every chat turn would just keep failing. Clearing it
    # makes the pane show "Sign in" again, which is the actual fix.
    .align_oauth_clear()
    return(list(error = paste0("Align sign-in expired (", body$error, "). Sign in again.")))
  }
  if (!is.character(body$access_token)) return(list(error = "The server did not return an access token."))
  # Rotation: the server issues a new refresh token on every refresh and burns
  # the old one, so failing to store this would break the NEXT refresh.
  if (is.character(body$refresh_token)) .align_oauth_save(stored$serverUrl, body$refresh_token)
  .align_oauth$access <- list(
    token = body$access_token,
    expiresAt = Sys.time() + as.numeric(.align_or(body$expires_in, 3600))
  )
  list(accessToken = body$access_token, serverUrl = stored$serverUrl)
}
