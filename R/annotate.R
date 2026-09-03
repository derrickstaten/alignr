# "Annotate for Align" — the zero-AI marker-insertion primitive (ALI-198 T2).
#
# Two entry points share this file's logic, per the ticket's two-tier design:
#  - align_addin_annotate(): RStudio Addins-menu binding, runs synchronously
#    in the R console with no direct line to the browser session, so its
#    result is queued (align_drain_pending_annotations) for the browser's
#    poll to pick up.
#  - POST /annotate (server.R): the chat-panel button's entry point, called
#    directly from the browser and answered synchronously.
#
# Mechanical only: default labels come from counting existing markers, never
# an LLM call (Round 4.B — "must work with zero AI calls" is a hard
# requirement, not a v1 shortcut).

#' Absolute path -> path relative to getwd(), or NULL if outside it. Inverse
#' of tracked_files.R's .align_safe_rel_path (that resolves relative -> safe
#' absolute; this resolves rstudioapi's absolute editor path -> the relative
#' form a Visualization's fileSource stores).
.align_rel_from_abs <- function(abs_path) {
  root <- normalizePath(getwd())
  full <- suppressWarnings(normalizePath(abs_path, mustWork = FALSE))
  if (!startsWith(full, root)) return(NULL)
  rel <- substring(full, nchar(root) + 1)
  rel <- sub("^[/\\\\]+", "", rel)
  if (!nzchar(rel)) return(NULL)
  rel
}

.align_generate_marker_id <- function() {
  paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
}

#' Mechanical default label: "Figure N" where N is one past however many
#' markers the file already has. No inference, no LLM — just a counter.
.align_default_marker_label <- function(content) {
  lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
  count <- sum(grepl("^\\s*#\\s*Figure:", lines))
  paste0("Figure ", count + 1)
}

#' Same marker syntax as file-region-utils.ts's buildMarkerLine — kept in
#' sync by hand since one side is R, the other TS; ALI-198 T3 durability
#' work is the natural place to add a cross-language format test if these
#' ever drift.
.align_marker_line <- function(label, marker_id) {
  safe_label <- gsub("[][]", "", label)
  safe_label <- trimws(safe_label)
  if (!nzchar(safe_label)) safe_label <- "Untitled"
  sprintf("# Figure: %s [fig:%s] ----", safe_label, marker_id)
}

#' Regex/substring scan for plotting-shaped code that isn't already inside a
#' marked region (Round 4.C) — the proactive nudge's entire detection
#' mechanism, deliberately zero LLM calls. False positives are fine (it's a
#' nudge, not an auto-apply); false negatives just mean a missed nudge.
align_scan_unmarked_plots <- function(content) {
  lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return(list())

  marker_lines <- grep("^\\s*#\\s*Figure:.*\\[fig:[A-Za-z0-9_-]+\\]\\s*-{4,}\\s*$", lines)
  in_marked_region <- logical(length(lines))
  if (length(marker_lines) > 0) {
    for (i in seq_along(marker_lines)) {
      start <- marker_lines[i]
      end <- if (i < length(marker_lines)) marker_lines[i + 1] - 1 else length(lines)
      in_marked_region[start:end] <- TRUE
    }
  }

  hint_pattern <- "ggplot\\(|geom_|\\bp\\s*<-|\\bp\\s*=[^=]"
  hits <- list()
  for (i in seq_along(lines)) {
    if (in_marked_region[i]) next
    # Comment lines never contain runnable plotting code — prose like
    # "select the `p <- ...` block" in a file header used to trigger the
    # nudge (ALI-273 live finding). Strip a trailing comment too, so
    # `x <- 1  # then ggplot(...)` doesn't hit on the comment half.
    code <- sub("#.*$", "", lines[i])
    if (grepl(hint_pattern, code, perl = TRUE)) {
      hits[[length(hits) + 1]] <- list(line = i, snippet = trimws(lines[i]))
    }
  }
  hits
}

#' Wraps the current RStudio editor selection in an Align figure marker —
#' the shared "Annotate for Align" primitive. Errors travel as data
#' (list(error=)), same contract as the rest of this package, since both
#' call sites (addin dialog, HTTP response) need to show them without a
#' thrown condition to catch.
align_annotate_selection <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(error = "RStudio API not available."))
  }
  ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
  if (is.null(ctx) || !nzchar(ctx$path)) {
    return(list(error = "Save the active file before annotating — Align tracks files on disk, not unsaved buffers."))
  }

  rel_path <- .align_rel_from_abs(ctx$path)
  if (is.null(rel_path)) {
    return(list(error = "The active file is outside the R working directory."))
  }

  sel <- ctx$selection[[1]]
  selected_text <- if (!is.null(sel)) sel$text else ""
  if (!nzchar(trimws(selected_text))) {
    return(list(error = "Select the code to annotate first."))
  }

  # Reuse an existing marker instead of stacking a second one (ALI-270
  # follow-up): a reload leaves markers in the file with no viz tracking
  # them, and re-annotating the same block must adopt the existing id, not
  # duplicate the line. Adopt when the selection's first line IS a marker
  # line, or the nearest non-blank line above the selection is one.
  marker_re <- "^\\s*#\\s*Figure:\\s*(.*?)\\s*\\[fig:([A-Za-z0-9_-]+)\\]\\s*-{4,}\\s*$"
  sel_start_row <- sel$range$start[[1]]
  existing_line <- NULL
  if (sel_start_row <= length(ctx$contents) && grepl(marker_re, ctx$contents[sel_start_row])) {
    existing_line <- ctx$contents[sel_start_row]
  } else {
    i <- sel_start_row - 1
    while (i >= 1 && grepl("^\\s*$", ctx$contents[i])) i <- i - 1
    if (i >= 1 && grepl(marker_re, ctx$contents[i])) existing_line <- ctx$contents[i]
  }
  if (!is.null(existing_line)) {
    # The selection may include the marker line itself — the region body a
    # later poll resolves never contains it, so strip it here too.
    body_lines <- Filter(
      function(l) !grepl(marker_re, l),
      strsplit(selected_text, "\r\n|\r|\n")[[1]]
    )
    return(list(
      filePath = rel_path,
      markerId = sub(marker_re, "\\2", existing_line),
      label = sub(marker_re, "\\1", existing_line),
      sourceCode = paste(body_lines, collapse = "\n"),
      reused = TRUE
    ))
  }

  full_content <- paste(ctx$contents, collapse = "\n")
  marker_id <- .align_generate_marker_id()
  label <- .align_default_marker_label(full_content)
  marker_line <- .align_marker_line(label, marker_id)

  # Insert directly above the selection's start line — the selection itself
  # is left untouched, so its text (returned below) still matches what ends
  # up as the region body once the poll re-scans the file.
  start_row <- sel$range$start[[1]]
  insert_at <- rstudioapi::document_position(start_row, 1)
  rstudioapi::insertText(insert_at, paste0(marker_line, "\n"), id = ctx$id)
  # Deliberately no documentSave() here — Align never saves a file on the
  # user's behalf, full stop. The marker lives in the live editor buffer;
  # the tracked-file poll reads that buffer directly for whichever file is
  # currently active (align_active_document_state, tracked_files.R) rather
  # than requiring a save to become visible.

  list(filePath = rel_path, markerId = marker_id, label = label, sourceCode = selected_text)
}

#' Active RStudio document's tracking-relevant state (Round 4.A/4.C) — path,
#' whether there's a selection ready to annotate, and unmarked-plot nudge
#' hints. Powers the chat panel shell; zero LLM calls anywhere in it.
align_get_editor_context <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(available = FALSE))
  }
  ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
  if (is.null(ctx) || !nzchar(ctx$path)) {
    return(list(available = TRUE, path = NULL, hasSelection = FALSE, unmarkedPlots = list()))
  }
  rel_path <- .align_rel_from_abs(ctx$path)
  sel <- ctx$selection[[1]]
  has_selection <- !is.null(sel) && nzchar(trimws(sel$text))
  content <- paste(ctx$contents, collapse = "\n")
  list(
    available = TRUE,
    path = rel_path,
    hasSelection = has_selection,
    unmarkedPlots = align_scan_unmarked_plots(content)
  )
}

#' RStudio Addins-menu entry point. See file header for why this queues
#' instead of returning directly to a browser caller.
align_addin_annotate <- function() {
  result <- align_annotate_selection()
  if (!is.null(result$error)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::showDialog("Align", result$error)
    } else {
      message("Align: ", result$error)
    }
    return(invisible(result))
  }
  if (is.null(.align_state$pending_annotations)) .align_state$pending_annotations <- list()
  .align_state$pending_annotations[[length(.align_state$pending_annotations) + 1]] <- result
  invisible(result)
}

#' Drains the addin's pending-annotation queue (GET /annotate/pending) —
#' read-once, so a slow or duplicate poll can't double-apply an entry.
align_drain_pending_annotations <- function() {
  pending <- .align_state$pending_annotations
  if (is.null(pending)) pending <- list()
  .align_state$pending_annotations <- list()
  list(annotations = pending)
}
