# Headless verification for the alignr package (ALI-186 T1).
# Run:  Rscript tests/test-alignr.R   (from r-package/)
#
# Covers the two spike-proven invariants (device cleanup on error, scalar
# coercion), the live-session read, and a real HTTP round-trip against a
# started server — no RStudio needed.

suppressPackageStartupMessages({
  library(ggplot2)
})
source("R/render.R")
source("R/docs.R")
source("R/tracked_files.R")
source("R/annotate.R")
source("R/apply_edit.R")
source("R/chat_connection.R")
source("R/bundle_check.R")
source("R/version_check.R")
source("R/server.R")
source("R/oauth.R")
source("R/handoff.R")
source("R/describe.R")

passed <- 0L
failed <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (ok) { passed <<- passed + 1L; cat("ok  -", label, "\n") }
  else { failed <<- failed + 1L; cat("FAIL-", label, "\n") }
}

# ── Render core ──────────────────────────────────────────────────────────────
r1 <- align_render_svg('p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(title = "T")')
check("basic ggplot renders SVG", is.null(r1$error) && grepl("<svg", r1$svg, fixed = TRUE))
check("SVG is scalar length-1", length(r1$svg) == 1)

r2 <- align_render_svg('p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()', width_inches = 8, height_inches = 5)
check("explicit dimensions land in the SVG", grepl('width=.8', substr(r2$svg, 1, 300)) || grepl("576", substr(r2$svg, 1, 300)))

r3 <- align_render_svg('p <- font_scale')  # not a plot: print(numeric) draws nothing but must not leak
check("non-plot p returns without throwing", is.null(r3$error) || is.character(r3$error))

devs_before <- grDevices::dev.cur()
r4 <- align_render_svg('stop("boom")')
check("user error returned as data", identical(r4$error, "boom") && is.null(r4$svg))
check("device cleaned up after error", grDevices::dev.cur() == devs_before)

r5 <- align_render_svg('p <- ggplot(nope_not_here, aes(x, y)) + geom_point()')
check("missing-object error is data, not condition", is.character(r5$error))

for (i in 1:5) {
  ri <- align_render_svg('p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()')
  if (length(ri$svg) != 1) { failed <- failed + 1L; cat("FAIL- scalar held on run", i, "\n") }
}
check("5 consecutive renders stayed scalar (atomic-vector guard)", TRUE)

check("font_scale visible to user code",
      is.null(align_render_svg('p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme(text = element_text(size = 10 * font_scale))', font_scale = 2)$error))

# ── Live session read ────────────────────────────────────────────────────────
assign("t1_session_df", data.frame(a = 1:3, b = c("x", "y", "z")), envir = globalenv())
vars <- align_list_session_data()
check("session data frame listed", any(vapply(vars, function(v) v$name == "t1_session_df", logical(1))))
prev <- align_preview_session_data("t1_session_df")
check("session data frame previews", identical(prev$rows, 3L) && identical(prev$columns, c("a", "b")))
check("render sees session data",
      is.null(align_render_svg('p <- ggplot(t1_session_df, aes(a, a)) + geom_point()')$error))

# ── Session capture for the web handoff (DS-396) ────────────────────────────
# The volcano shape: a marker region that reads objects its file defined above
# the marker. Everything the region needs must ride along; nothing else may.
assign("t5_df", data.frame(x = 1:4, y = c(2, 4, 6, 8), g = factor(c("a", "a", "b", "b"))), envir = globalenv())
assign("t5_cut", 1.5, envir = globalenv())
assign("t5_unused", data.frame(z = 1), envir = globalenv())
assign("t5_scale", function(v) v * t5_cut, envir = globalenv())
assign("t5_env", new.env(), envir = globalenv())
cap <- align_capture_session('p <- ggplot(t5_df, aes(x, t5_scale(y), color = g)) + geom_point() + geom_hline(yintercept = t5_cut)')
check("capture finds the session objects the code reads",
      isTRUE(cap$found) && setequal(unlist(cap$objects), c("t5_df", "t5_scale", "t5_cut")))
check("capture leaves unrelated session objects behind", !("t5_unused" %in% unlist(cap$objects)))
check("capture names the attached package the code leans on", "ggplot2" %in% unlist(cap$packages))
check("capture excludes base packages", !any(c("base", "stats") %in% unlist(cap$packages)))
rds_tmp <- tempfile(fileext = ".rds")
writeBin(jsonlite::base64_dec(cap$contentBase64), rds_tmp)
restored <- readRDS(rds_tmp)
check("captured rds round-trips a data frame with its types",
      identical(restored$objects$t5_df, t5_df) && is.factor(restored$objects$t5_df$g))
check("captured rds round-trips a numeric and a function",
      identical(restored$objects$t5_cut, 1.5) && identical(restored$objects$t5_scale(2), 3))
# The web's situation: none of the objects exist until the slice is restored.
rm("t5_df", "t5_cut", "t5_scale", envir = globalenv())
check("without the slice the code fails the way it does on web today",
      is.character(align_render_svg('p <- ggplot(t5_df, aes(x, t5_scale(y), color = g)) + geom_point()')$error))
list2env(restored$objects, envir = globalenv())
check("restored slice renders the code again",
      is.null(align_render_svg('p <- ggplot(t5_df, aes(x, t5_scale(y), color = g)) + geom_point() + geom_hline(yintercept = t5_cut)')$error))
cap_env <- align_capture_session('p <- ggplot(t5_df, aes(x, y)) + geom_point(); e <- t5_env')
check("uncapturable objects are reported, not silently dropped",
      isTRUE(cap_env$found) && any(vapply(cap_env$skipped, function(s) s$name == "t5_env", logical(1))))
# `hp`, not `mpg`: ggplot2 exports a dataset called mpg, and a column name
# that happens to match an attached package's export is carried as that
# package (harmless — library(ggplot2) on web — but it would defeat this check).
cap_none <- align_capture_session('p <- function() plot(mtcars$wt, mtcars$hp)')
check("code needing nothing beyond base R yields no capture file",
      isFALSE(cap_none$found) && is.null(cap_none$contentBase64))
cap_pkg <- align_capture_session('p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()')
check("an attached package alone is still worth carrying", isTRUE(cap_pkg$found) && identical(unlist(cap_pkg$objects), NULL))
cap_local <- align_capture_session('t5_df <- data.frame(x = 1, y = 2)\np <- ggplot(t5_df, aes(x, y)) + geom_point()')
check("a name the code assigns itself is not captured", isFALSE(cap_local$found) || !("t5_df" %in% unlist(cap_local$objects)))
check("font_scale is never captured",
      { assign("font_scale", 2, envir = globalenv()); r <- align_capture_session('p <- ggplot(t5_df, aes(x, y * font_scale)) + geom_point()'); rm("font_scale", envir = globalenv()); !("font_scale" %in% unlist(r$objects)) })
check("unparsable code is a reason, not an error", grepl("doesn't parse", align_capture_session('p <- ggplot(')$reason))
check("empty-argument calls like x[, 1] don't trip the walker",
      isTRUE(align_capture_session('p <- ggplot(t5_df[, 1:2], aes(x, y)) + geom_point()')$found))

# ── HTTP round-trip ──────────────────────────────────────────────────────────
www <- file.path(tempdir(), "align-test-www")
dir.create(www, showWarnings = FALSE)
writeLines("<html><body>align test</body></html>", file.path(www, "index.html"))
url <- align_start(www_root = www)

# httpuv serves off later's event loop, which a same-process blocking client
# would starve (deadlock -> timeout). So: fire curl in a subprocess and pump
# later::run_now() while waiting — the same shape as the real Viewer client.
http_raw <- function(curl_args) {
  outfile <- tempfile()
  done <- paste0(outfile, ".done")
  cmd <- paste("curl -s -o", shQuote(outfile), curl_args, "; touch", shQuote(done))
  system(paste0("(", cmd, ") &"))
  for (i in 1:400) {
    later::run_now(0.05)
    if (file.exists(done)) break
  }
  later::run_now(0.05)
  if (!file.exists(outfile)) return("")
  paste(readLines(outfile, warn = FALSE), collapse = "\n")
}
http_get <- function(path) http_raw(shQuote(paste0(sub("/$", "", url), path)))
ready <- jsonlite::fromJSON(http_get("/ready"))
check("/ready responds", isTRUE(ready$ready))
vars_http <- jsonlite::fromJSON(http_get("/vars"), simplifyVector = FALSE)
check("/vars sees the session over HTTP", any(vapply(vars_http$vars, function(v) v$name == "t1_session_df", logical(1))))
check("static index served", grepl("align test", http_get("/")))
check("path traversal blocked", grepl("Not found", http_get("/../DESCRIPTION")))

post_json <- function(path, body) {
  payload <- tempfile()
  writeLines(as.character(jsonlite::toJSON(body, auto_unbox = TRUE)), payload)
  jsonlite::fromJSON(http_raw(paste(
    "-X POST -H 'Content-Type: application/json' --data-binary", paste0("@", shQuote(payload)),
    shQuote(paste0(sub("/$", "", url), path))
  )))
}
rr <- post_json("/render", list(code = 'p <- ggplot(t1_session_df, aes(a, a)) + geom_point()', widthInches = 6, heightInches = 4, fontScale = 1))
check("POST /render round-trips session data to SVG", is.null(rr$error) && grepl("<svg", rr$svg, fixed = TRUE))
re <- post_json("/render", list(code = 'stop("http boom")'))
check("POST /render returns error as JSON data", identical(re$error, "http boom"))
hc <- post_json("/handoff/capture", list(code = 'p <- ggplot(t1_session_df, aes(a, a)) + geom_point()'))
check("POST /handoff/capture returns the session slice as JSON", isTRUE(hc$found) && "t1_session_df" %in% unlist(hc$objects) && nzchar(hc$contentBase64))
# DS-397: the describe snippet itself is @align/core's and is tested there
# against real R; here the plumbing — bytes in, the host's script run, JSON out.
dr <- post_json("/file/describe-rds", list(
  contentBase64 = hc$contentBase64,
  script = '.align_describe_rds <- function(path) { x <- readRDS(path); paste0("{\\"kind\\":\\"probe\\",\\"n\\":", length(x$objects), "}") }'
))
check("POST /file/describe-rds runs the supplied script against the bytes", identical(jsonlite::fromJSON(dr$json)$n, 1L))
check("POST /file/describe-rds refuses an empty payload", nzchar(post_json("/file/describe-rds", list(contentBase64 = "", script = "x"))$error))

# ── Document persistence over HTTP ───────────────────────────────────────────
docdir <- file.path(tempdir(), "align-docs")
dir.create(docdir, showWarnings = FALSE)
oldwd <- setwd(docdir)
snapshot_json <- '{"schemaVersion":2.1,"meta":{"id":"t3","name":"T3 doc"},"pages":[]}'
w <- post_json("/doc/write", list(name = "t3-test", content = snapshot_json))
check("doc write returns normalized name", identical(w$name, "t3-test.align"))
check("doc file exists on disk", file.exists(file.path(docdir, "t3-test.align")))
lst <- jsonlite::fromJSON(http_get("/doc/list"), simplifyVector = FALSE)
check("doc list includes the file", any(vapply(lst$docs, function(d) d$name == "t3-test.align", logical(1))))
rd <- http_get("/doc/read/t3-test.align")
check("doc read round-trips exact bytes", identical(jsonlite::fromJSON(rd)$meta$name, "T3 doc"))
bad <- post_json("/doc/write", list(name = "../evil", content = "x"))
check("doc name traversal rejected", identical(bad$error, "Invalid document name."))
missing <- jsonlite::fromJSON(http_get("/doc/read/nope.align"))
check("missing doc reads as JSON error", identical(missing$error, "Document not found."))

# ── Tracked-file stat/read over HTTP (ALI-198 T1) ────────────────────────────
dir.create(file.path(docdir, "R"), showWarnings = FALSE)
tracked_path <- "R/plots.R"
writeLines(c(
  "# Figure: Sales by Region [fig:abc123] ----",
  "p <- ggplot(df, aes(x, y)) + geom_point()"
), file.path(docdir, tracked_path))

stat1 <- jsonlite::fromJSON(http_get(paste0("/file/stat?path=", utils::URLencode(tracked_path, reserved = TRUE))))
check("file stat reports existence", isTRUE(stat1$exists))
check("file stat reports an mtime", is.character(stat1$mtime) && nzchar(stat1$mtime))

read1 <- jsonlite::fromJSON(http_get(paste0("/file/read?path=", utils::URLencode(tracked_path, reserved = TRUE))))
check("file read returns the marker line", grepl("fig:abc123", read1$content, fixed = TRUE))

Sys.sleep(1.1)  # mtime resolution is whole seconds on some filesystems
writeLines(c(
  "# Figure: Sales by Region [fig:abc123] ----",
  "p <- ggplot(df, aes(x, y)) + geom_point() + theme_minimal()"
), file.path(docdir, tracked_path))
stat2 <- jsonlite::fromJSON(http_get(paste0("/file/stat?path=", utils::URLencode(tracked_path, reserved = TRUE))))
check("file stat mtime changes after a rewrite", !identical(stat1$mtime, stat2$mtime))

stat_missing <- jsonlite::fromJSON(http_get("/file/stat?path=R/nope.R"))
check("stat of missing file reports exists=FALSE", isFALSE(stat_missing$exists))
stat_traversal <- jsonlite::fromJSON(http_get(paste0("/file/stat?path=", utils::URLencode("../evil.R", reserved = TRUE))))
check("stat path traversal reports exists=FALSE, not an escape", isFALSE(stat_traversal$exists))
read_traversal <- jsonlite::fromJSON(http_get(paste0("/file/read?path=", utils::URLencode("../evil.R", reserved = TRUE))))
check("read path traversal rejected", identical(read_traversal$error, "File not found."))

# ── Live-buffer read (ALI-198 T1, revised: never save on the user's behalf) ──
check("align_active_document_state degrades without rstudioapi",
      isFALSE(align_active_document_state()$available))
active_http <- jsonlite::fromJSON(http_get("/file/active"))
check("GET /file/active degrades over HTTP too", isFALSE(active_http$available))

# ── "Annotate for Align" mechanical primitives (ALI-198 T2) ─────────────────
check(".align_rel_from_abs resolves a path inside the working dir",
      identical(.align_rel_from_abs(file.path(docdir, "R", "plots.R")), "R/plots.R") ||
      identical(.align_rel_from_abs(file.path(docdir, "R", "plots.R")), "R\\plots.R"))
check(".align_rel_from_abs rejects a path outside the working dir",
      is.null(.align_rel_from_abs(file.path(tempdir(), "elsewhere.R"))))

check("default marker label starts at Figure 1", identical(.align_default_marker_label(""), "Figure 1"))
check("default marker label counts existing markers",
      identical(.align_default_marker_label("# Figure: A [fig:aaa111] ----\n# Figure: B [fig:bbb222] ----\n"), "Figure 3"))

check("marker line matches the TS-side syntax",
      identical(.align_marker_line("Sales by Region", "a1b2c3"), "# Figure: Sales by Region [fig:a1b2c3] ----"))
check("marker line strips brackets from the label",
      identical(.align_marker_line("Sales [Q1]", "a1b2c3"), "# Figure: Sales Q1 [fig:a1b2c3] ----"))

unmarked_src <- paste(
  "df <- data.frame(x = 1, y = 2)",
  "p <- ggplot2::ggplot(df) # not yet tracked",
  "# Figure: Tracked [fig:abc123] ----",
  "p2 <- ggplot2::ggplot(df) # already tracked",
  sep = "\n"
)
hits <- align_scan_unmarked_plots(unmarked_src)
check("unmarked plotting code is flagged", length(hits) == 1 && hits[[1]]$line == 2)
check("a hit carries a click-ready line range (DS-423)",
      identical(hits[[1]]$startLine, 2L) && identical(hits[[1]]$endLine, 2L))

# DS-423: a figure is a whole top-level expression, however many lines it
# spans — the volcano shape from the Desktop test script.
chain_src <- paste(
  "library(ggplot2)",
  "",
  "df <- read.csv(\"volcano.csv\")",
  "fc_cutoff <- 1",
  "",
  "p <- ggplot(df, aes(x = log2FC, y = negLogP)) +",
  "  geom_point(alpha = 0.6) +",
  "  geom_vline(xintercept = c(-fc_cutoff, fc_cutoff)) +",
  "  theme(",
  "    legend.position = \"bottom\"",
  "  )",
  "",
  "p",
  sep = "\n"
)
chain_hits <- align_scan_unmarked_plots(chain_src)
check("a multi-line ggplot chain is ONE figure spanning its whole expression",
      length(chain_hits) == 1 && chain_hits[[1]]$startLine == 6 && chain_hits[[1]]$endLine == 11)
check("the hit's snippet is the expression's first line",
      identical(chain_hits[[1]]$snippet, "p <- ggplot(df, aes(x = log2FC, y = negLogP)) +"))

two_src <- paste(
  "x <- 1:10",
  "hist(x)",
  "p <- ggplot2::ggplot(data.frame(x)) +",
  "  ggplot2::geom_histogram()",
  sep = "\n"
)
two_hits <- align_scan_unmarked_plots(two_src)
check("base-R plots and ggplot chains are both found, as separate figures",
      length(two_hits) == 2 && two_hits[[1]]$startLine == 2 && two_hits[[2]]$startLine == 3 && two_hits[[2]]$endLine == 4)

marked_chain_src <- paste(
  "# Figure: One [fig:aaa111] ----",
  "p <- ggplot2::ggplot(df) +",
  "  ggplot2::geom_point()",
  sep = "\n"
)
check("an expression inside an existing marker region is not offered again",
      length(align_scan_unmarked_plots(marked_chain_src)) == 0)

comment_src <- "# select the `p <- ggplot(...)` block and click Add"
check("prose in a comment is never a figure", length(align_scan_unmarked_plots(comment_src)) == 0)

broken_src <- paste(
  "p <- ggplot2::ggplot(df) +",
  "  ggplot2::geom_point(",       # unbalanced — mid-edit typo
  "x <- 1",
  sep = "\n"
)
broken_hits <- align_scan_unmarked_plots(broken_src)
check("an unparseable file falls back to per-line hits instead of going blank",
      length(broken_hits) >= 1 && broken_hits[[1]]$startLine == 1 && broken_hits[[1]]$endLine == 1)

# No RStudio session in this headless harness — every entry point must
# degrade to a clean error/unavailable rather than throwing.
check("align_annotate_selection degrades without rstudioapi",
      identical(align_annotate_selection()$error, "RStudio API not available."))
check("align_annotate_range degrades without rstudioapi",
      identical(align_annotate_range(6, 11)$error, "RStudio API not available."))
fake_ctx <- list(id = "x", path = "/wd/volcano.R", contents = c(
  "library(ggplot2)",
  "df <- read.csv(\"volcano.csv\")",
  "# Figure: Figure 1 [fig:w661mr] ----",
  "p <- ggplot(df)",
  "p"
))
whole_file <- .align_annotate_rows(fake_ctx, "volcano.R", 1, 5)
check("a selection containing an existing marker is refused, naming the figure",
      identical(whole_file$error, "That selection already contains a tracked figure (Figure 1). Select just the plot code."))
adopt <- .align_annotate_rows(fake_ctx, "volcano.R", 3, 5)
check("a selection STARTING on a marker adopts it instead of refusing",
      isTRUE(adopt$reused) && identical(adopt$markerId, "w661mr") && identical(adopt$sourceCode, "p <- ggplot(df)\np"))
check("align_annotate_range refuses a missing range before touching rstudioapi",
      identical(align_annotate_range(NULL, NULL)$error, "A line range is required."))
check("align_get_editor_context degrades without rstudioapi",
      isFALSE(align_get_editor_context()$available))

ann_http <- jsonlite::fromJSON(http_get("/annotate/context"))
check("GET /annotate/context degrades over HTTP too", isFALSE(ann_http$available))
ann_post <- post_json("/annotate", list())
check("POST /annotate degrades over HTTP too", identical(ann_post$error, "RStudio API not available."))
ann_range <- post_json("/annotate", list(startLine = 6, endLine = 11))
check("POST /annotate with a line range routes to the range entry (DS-423)",
      identical(ann_range$error, "RStudio API not available."))

pending_empty <- jsonlite::fromJSON(http_get("/annotate/pending"), simplifyVector = FALSE)
check("pending-annotation queue starts empty", length(pending_empty$annotations) == 0)
.align_state$pending_annotations <- list(list(filePath = "R/plots.R", markerId = "abc123", label = "Figure 1", sourceCode = "p <- 1"))
pending_one <- jsonlite::fromJSON(http_get("/annotate/pending"), simplifyVector = FALSE)
check("pending-annotation queue drains a queued entry", length(pending_one$annotations) == 1 &&
      identical(pending_one$annotations[[1]]$markerId, "abc123"))
pending_drained <- jsonlite::fromJSON(http_get("/annotate/pending"), simplifyVector = FALSE)
check("pending-annotation queue is read-once", length(pending_drained$annotations) == 0)

# ── Marker region line-finding (ALI-198 T4c) ────────────────────────────────
lines_single <- c(
  "df <- data.frame(x = 1)",
  "# Figure: Sales [fig:abc123] ----",
  "p <- ggplot2::ggplot(df)",
  "p <- p + ggplot2::geom_point()"
)
r1 <- .align_find_marker_region(lines_single, "abc123")
check("single marker: region runs to EOF", isTRUE(r1$found) && r1$startLine == 3 && r1$endLine == 4 && !isTRUE(r1$duplicate))

lines_two <- c(
  "# Figure: First [fig:aaa111] ----",
  "p <- ggplot2::ggplot(df)",
  "# Figure: Second [fig:bbb222] ----",
  "p2 <- ggplot2::ggplot(df2)",
  "p2 <- p2 + ggplot2::geom_line()"
)
r2 <- .align_find_marker_region(lines_two, "aaa111")
check("two markers: first region bounded by the second's start", r2$startLine == 2 && r2$endLine == 2)
r3 <- .align_find_marker_region(lines_two, "bbb222")
check("two markers: last region runs to EOF", r3$startLine == 4 && r3$endLine == 5)

r4 <- .align_find_marker_region(lines_two, "nope")
check("unknown marker id: not found", !isTRUE(r4$found))

lines_dup <- c(
  "# Figure: One [fig:dup1] ----",
  "p <- 1",
  "# Figure: Two [fig:dup1] ----",
  "p <- 2"
)
r5 <- .align_find_marker_region(lines_dup, "dup1")
check("duplicate marker id: flagged, first match used", isTRUE(r5$found) && isTRUE(r5$duplicate) && r5$startLine == 2 && r5$endLine == 2)

lines_empty_eof <- c("df <- data.frame(x = 1)", "# Figure: Empty [fig:e1] ----")
r6 <- .align_find_marker_region(lines_empty_eof, "e1")
check("marker as literal last line: empty region, startLine just past endLine", isTRUE(r6$found) && r6$startLine == r6$endLine + 1)

# align_apply_file_edit degrades cleanly without a live RStudio session —
# the modifyRange/document_range path itself can't be exercised headlessly
# (rstudioapi::isAvailable() is FALSE here), so this only proves the guard.
check("align_apply_file_edit degrades without rstudioapi",
      identical(align_apply_file_edit("R/plots.R", "abc123", "p <- 1")$error, "RStudio API not available."))

apply_http <- post_json("/apply-edit", list(filePath = "R/plots.R", markerId = "abc123", code = "p <- 1"))
check("POST /apply-edit degrades over HTTP too", identical(apply_http$error, "RStudio API not available."))

setwd(oldwd)

align_stop()
check("server stops cleanly", is.null(.align_state$server))


# ── OAuth sign-in (DS-273) ───────────────────────────────────────────────────
# Everything here runs offline. The one thing that genuinely cannot be
# exercised without a server and a browser is the happy path — so what is
# pinned instead is the shape of the request we would send, the crypto, the
# storage round trip, and every failure branch, since those are the ones that
# would otherwise only be discovered by a user mid-sign-in.

oauth_dir <- file.path(tempdir(), "align-oauth-test")
dir.create(oauth_dir, showWarnings = FALSE, recursive = TRUE)
# Overridden rather than pointed at a fake HOME so the test can never touch
# the real ~/.align, and never touch a real keychain on a machine that has
# `keyring` installed.
.align_oauth_file <- function() file.path(oauth_dir, "rstudio-oauth.json")
.align_keyring_available <- function() FALSE
# align_signin() ends by handing the URL to the system browser. Stubbed out
# here or `Rscript tests/test-alignr.R` would pop a browser tab open on
# whoever ran it.
options(browser = function(url) invisible(NULL))

check("base64url output is unpadded and URL-safe",
      !grepl("[+/=]", .align_b64url(as.raw(0:255))))

pkce <- .align_pkce_pair()
check("PKCE challenge is base64url(SHA-256(verifier))",
      identical(pkce$challenge, .align_b64url(openssl::sha256(charToRaw(pkce$verifier)))))
check("PKCE verifier is fresh per call", !identical(pkce$verifier, .align_pkce_pair()$verifier))

check("server URL loses its trailing slashes",
      identical(.align_normalize_server("https://align.app///"), "https://align.app"))

.align_oauth_clear()
check("no stored sign-in reads as signed out", is.null(.align_oauth_load()))
check("access token without a sign-in is an error, not a throw",
      identical(align_oauth_access_token()$error, "Not signed in to Align."))

.align_oauth_save("https://align.app", "align_rt_secret")
.align_oauth$storedCache <- NULL  # force a real read back off disk
stored <- .align_oauth_load()
check("refresh token round-trips through the file store",
      identical(stored$serverUrl, "https://align.app") && identical(stored$refreshToken, "align_rt_secret"))
check("the token file is 0600 — no other local user may read it",
      identical(substr(as.character(file.mode(.align_oauth_file())), 1, 3), "600"))
check("status reports the stored sign-in", isTRUE(align_oauth_status()$signedIn))
invisible(align_signout())
check("sign-out forgets the stored sign-in",
      !isTRUE(align_oauth_status()$signedIn) && !file.exists(.align_oauth_file()))

check("a non-URL server is refused before anything is opened",
      is.character(align_signin("not a url")$error))

signin <- align_signin("http://127.0.0.1:1/", mode = "loopback")
check("loopback sign-in returns an authorize URL immediately", is.character(signin$authorizeUrl))
auth_url <- signin$authorizeUrl
check("authorize URL carries PKCE S256", grepl("code_challenge_method=S256", auth_url, fixed = TRUE))
check("authorize URL names the pre-registered client",
      grepl("client_id=align-rstudio-plugin", auth_url, fixed = TRUE))
check("authorize URL asks for align:ai and nothing else",
      grepl("scope=align%3Aai", auth_url, fixed = TRUE) && !grepl("align%3Adocs", auth_url))
check("authorize URL names the AI resource as its audience",
      grepl("resource=http%3A%2F%2F127.0.0.1%3A1%2Fapi%2Fai", auth_url, fixed = TRUE))
check("loopback redirect_uri is a 127.0.0.1 /callback on a bound port",
      grepl("redirect_uri=http%3A%2F%2F127.0.0.1%3A[0-9]+%2Fcallback", auth_url))
check("a listener is bound while the flow is open", !is.null(.align_oauth$listener))
check("status reports the flow as pending", isTRUE(align_oauth_status()$pending))

# The callback handler, driven directly — httpuv would deliver exactly this.
callback_app <- .align_callback_app(.align_oauth$pending$state)
fake_req <- function(qs) list(PATH_INFO = "/callback", QUERY_STRING = qs)
res <- callback_app$call(fake_req("state=wrong&code=align_ac_x"))
check("a mismatched state is refused (CSRF)",
      res$status == 400L && grepl("state did not match", .align_oauth$result$error))
res <- callback_app$call(fake_req(paste0("state=", .align_oauth$pending$state, "&error=access_denied&error_description=User%20said%20no")))
check("a user-cancelled sign-in surfaces the server's reason",
      identical(.align_oauth$result$error, "User said no"))
# A real code against a server that isn't there: the exchange must come back
# as data, since this handler runs inside httpuv where a throw is a 500.
res <- callback_app$call(fake_req(paste0("state=", .align_oauth$pending$state, "&code=align_ac_x")))
check("an unreachable token endpoint is an error, not a throw",
      is.character(.align_oauth$result$error) && grepl("Could not reach", .align_oauth$result$error))

check("pasting an empty code is refused", is.character(align_oauth_submit_code("")$error))

invisible(align_signout())
check("sign-out releases the loopback listener", is.null(.align_oauth$listener))
check("pasting a code with no flow in progress is refused",
      grepl("No sign-in is in progress", align_oauth_submit_code("align_ac_x")$error))

# ── Deployed-release check (DS-404) ─────────────────────────────────────────
check("same release on matching short shas", isTRUE(.align_same_release("abcdef0123456789", "abcdef0")))
check("different release on differing shas", isFALSE(.align_same_release("abcdef0123456789", "1234567890")))
check("NA when a sha is missing", is.na(.align_same_release(NULL, "abcdef0")))
stamp_dir <- tempfile("www"); dir.create(stamp_dir)
writeLines('{"builtAt":"2026-09-02T00:00:00.000Z","commit":"abcdef0123456789","version":"2026.9.2"}', file.path(stamp_dir, "build-stamp.json"))
st <- .align_read_build_stamp(stamp_dir)
check("stamp reads version and commit", identical(st$version, "2026.9.2") && identical(st$commit, "abcdef0123456789"))
check("missing stamp reads as NULL", is.null(.align_read_build_stamp(tempfile("nowhere"))))
check("mismatch message names the install line", grepl('install_github("derrickstaten/alignr")', .align_release_mismatch_message("2026.9.2", "https://alignfigures.com"), fixed = TRUE))
Sys.setenv(ALIGNR_SERVER = "https://example.invalid/")
check("ALIGNR_SERVER overrides and is trimmed", identical(.align_release_server(), "https://example.invalid"))
check("unreachable server is a silent NULL", is.null(.align_fetch_deployed_commit("https://example.invalid")))
check("dev bundle (no version) skips the check silently", is.null(.align_check_release_version(tempfile("nowhere"))))
Sys.unsetenv("ALIGNR_SERVER")

cat(sprintf("\n%d passed, %d failed\n", passed, failed))
if (failed > 0) quit(status = 1)
