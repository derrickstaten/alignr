# Marker insertion — the zero-AI primitive that turns a block of the user's
# script into a tracked Align figure (ALI-198 T2, reshaped by DS-423).
#
# Three entry points share this file's logic:
#  - POST /annotate with {startLine, endLine} (server.R): the primary path
#    since DS-423. The plugin's "Add" list shows the figures
#    align_scan_unmarked_plots found; clicking one sends its line range here.
#    The user never selects code.
#  - POST /annotate with no body: the fallback for code the detector missed —
#    wraps whatever is selected in the editor.
#  - align_addin_annotate(): the RStudio Addins-menu binding for that same
#    selection path. It runs in the R console with no line to the browser,
#    so its result is queued (align_drain_pending_annotations) for the
#    browser's poll to pick up.
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

#' `# ---- end fig:<id>` closes the region it names (DS-449) — same syntax as
#' file-region-utils.ts's buildEndMarkerLine. Leading dashes on purpose:
#' RStudio folds comment lines that END in dashes, so this never shows up as
#' a section of its own.
.align_end_marker_re <- "^\\s*#\\s*-{4,}\\s*end\\s+fig:([A-Za-z0-9_-]+)\\s*$"
.align_end_marker_line <- function(marker_id) sprintf("# ---- end fig:%s", marker_id)

#' Where a region ends: `bodyEnd` is its last body line, `closeLine` its own
#' end marker (NA when it has none). Without an end marker the region runs to
#' the line before `next_marker_line` (NA = EOF), the pre-DS-449 rule markers
#' written by hand keep.
.align_region_bounds <- function(lines, marker_line, next_marker_line, marker_id) {
  limit <- if (is.na(next_marker_line)) length(lines) else next_marker_line - 1
  if (marker_line + 1 <= limit) {
    for (i in (marker_line + 1):limit) {
      if (grepl(.align_end_marker_re, lines[i]) && sub(.align_end_marker_re, "\\1", lines[i]) == marker_id) {
        return(list(bodyEnd = i - 1, closeLine = i))
      }
    }
  }
  list(bodyEnd = limit, closeLine = NA)
}

#' Code that reads as "this draws a figure". Deliberately broad and zero-LLM
#' (Round 4.B): a false positive is a card nobody clicks, a false negative is
#' a figure the user has to select by hand via the fallback link.
.align_plot_hint_pattern <- "ggplot\\(|geom_|\\bplot\\(|\\bhist\\(|\\bbarplot\\(|\\bboxplot\\(|\\bpie\\(|\\bimage\\(|\\bheatmap\\(|\\bpheatmap\\(|\\bp\\s*<-|\\bp\\s*=[^=]"

#' TRUE for every line that sits inside an existing marker region (a marker
#' line through the line before the next marker, or EOF).
.align_marked_line_mask <- function(lines) {
  marker_re <- "^\\s*#\\s*Figure:.*\\[fig:([A-Za-z0-9_-]+)\\]\\s*-{4,}\\s*$"
  marker_lines <- grep(marker_re, lines)
  mask <- logical(length(lines))
  if (length(marker_lines) > 0) {
    for (i in seq_along(marker_lines)) {
      start <- marker_lines[i]
      bounds <- .align_region_bounds(lines, start,
        if (i < length(marker_lines)) marker_lines[i + 1] else NA,
        sub(marker_re, "\\1", lines[start]))
      end <- if (is.na(bounds$closeLine)) bounds$bodyEnd else bounds$closeLine
      mask[start:max(start, end)] <- TRUE
    }
  }
  mask
}

#' The line a new figure goes after (0 = the top of an empty file), for a
#' cursor on `cursor_row` (DS-449). Pure, so the harness can test it.
#'
#' Never inside an existing figure: a cursor in a tracked region puts the new
#' one after that region (its end marker, or where it runs out), since a
#' marker dropped mid-region would cut the figure above it in two. Never
#' inside a statement either: a cursor in a multi-line top-level expression
#' puts it after the expression's last line (srcrefs, as
#' align_scan_unmarked_plots uses). Otherwise right below the cursor line.
#' An unparseable file (mid-edit typo) falls back to the end.
.align_insert_after_line <- function(lines, cursor_row) {
  n <- length(lines)
  if (n == 0) return(0L)
  cursor_row <- max(1L, min(as.integer(cursor_row), n))

  marker_re <- "^\\s*#\\s*Figure:.*\\[fig:([A-Za-z0-9_-]+)\\]\\s*-{4,}\\s*$"
  marker_lines <- grep(marker_re, lines)
  for (i in seq_along(marker_lines)) {
    start <- marker_lines[i]
    bounds <- .align_region_bounds(lines, start,
      if (i < length(marker_lines)) marker_lines[i + 1] else NA,
      sub(marker_re, "\\1", lines[start]))
    end <- if (is.na(bounds$closeLine)) bounds$bodyEnd else bounds$closeLine
    if (cursor_row >= start && cursor_row <= end) return(as.integer(max(start, end)))
  }

  exprs <- tryCatch(suppressWarnings(parse(text = lines, keep.source = TRUE)), error = function(e) NULL)
  if (is.null(exprs)) return(as.integer(n))
  for (ref in attr(exprs, "srcref")) {
    if (ref[[1L]] <= cursor_row && ref[[3L]] >= cursor_row) return(as.integer(ref[[3L]]))
  }
  as.integer(cursor_row)
}

#' Writes a new figure into the active R script and returns it as a tracked
#' region (DS-449): every plugin add path — starter, table chart, template,
#' AI-created figure — puts its code in the user's script, never only in the
#' document, because the script is the one copy of their code. Inserted as
#' an unsaved edit below the cursor (see .align_insert_after_line), wrapped
#' in a marker and an end marker so code typed later below it stays out.
#'
#' With no saved R script focused it refuses and says what to open, except
#' for `allow_new` (the built-in example): that opens the code in a new
#' untitled script, which the user saves and then adds from the Add tab —
#' Align never creates or saves a file itself.
#'
#' @return list(filePath, markerId, label, sourceCode) or list(error=).
align_insert_figure <- function(code, label = NULL, allow_new = FALSE) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(error = "RStudio API not available."))
  }
  code_lines <- strsplit(sub("\\s+$", "", as.character(code)), "\r\n|\r|\n")[[1]]
  # The source editor's active tab, not getActiveDocumentContext(): that one
  # answers with the console whenever the user last typed there, which made
  # an add refuse while the script sat open right beside it (live finding).
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  is_r_script <- !is.null(ctx) && nzchar(ctx$path) && grepl("\\.[Rr]$", ctx$path)

  if (!is_r_script) {
    if (isTRUE(allow_new)) {
      marker_id <- .align_generate_marker_id()
      block <- c(.align_marker_line(if (is.null(label)) "Figure 1" else label, marker_id), code_lines, .align_end_marker_line(marker_id))
      rstudioapi::documentNew(text = paste0(paste(block, collapse = "\n"), "\n"), type = "r")
      return(list(error = "Opened the example in a new script. Save it in your project, then add it from the Add tab."))
    }
    return(list(error = "Open the R script this figure should live in, then try again — Align writes the figure's code there."))
  }
  rel_path <- .align_rel_from_abs(ctx$path)
  if (is.null(rel_path)) {
    return(list(error = "This script is outside the R working directory. Set the working directory to its folder (Session > Set Working Directory) and try again."))
  }

  lines <- ctx$contents
  if (length(lines) == 1 && !nzchar(lines)) lines <- character(0)
  cursor_row <- tryCatch(ctx$selection[[1]]$range$start[[1]], error = function(e) length(lines))
  after <- .align_insert_after_line(lines, cursor_row)

  marker_id <- .align_generate_marker_id()
  if (is.null(label) || !nzchar(label)) label <- .align_default_marker_label(paste(lines, collapse = "\n"))
  block <- c(.align_marker_line(label, marker_id), code_lines, .align_end_marker_line(marker_id))

  if (after == 0) {
    rstudioapi::insertText(rstudioapi::document_position(1, 1), paste0(paste(block, collapse = "\n"), "\n"), id = ctx$id)
  } else {
    # Appended to the end of line `after`, with a blank line above and, when
    # code follows directly, below — so the figure reads as its own block.
    following_blank <- after >= length(lines) || !nzchar(trimws(lines[after + 1]))
    text <- paste0("\n\n", paste(block, collapse = "\n"), if (following_blank) "" else "\n")
    rstudioapi::insertText(rstudioapi::document_position(after, nchar(lines[after]) + 1), text, id = ctx$id)
  }
  # Deliberately no documentSave() — Align never saves the user's file.

  list(filePath = rel_path, markerId = marker_id, label = .align_marker_label_text(label), sourceCode = paste(code_lines, collapse = "\n"))
}

#' The label as it ends up in the marker line (brackets stripped, trimmed).
.align_marker_label_text <- function(label) {
  safe <- trimws(gsub("[][]", "", label))
  if (nzchar(safe)) safe else "Untitled"
}

#' Comment lines never contain runnable plotting code — prose like "select
#' the `p <- ...` block" in a file header used to trigger the nudge (ALI-273
#' live finding). A trailing comment is stripped too, so `x <- 1 # ggplot(`
#' doesn't hit on the comment half. A `#` inside a string is mis-stripped,
#' which only ever costs a missed hint.
.align_strip_comments <- function(lines) sub("#.*$", "", lines)

#' Finds the figures in a script that aren't already tracked — the whole
#' detection mechanism behind the plugin's "Add" list (DS-423).
#'
#' Why parse rather than grep lines: a figure is a top-level R expression
#' (`p <- ggplot(...) + geom_point() + theme(...)` spanning 20 lines), and
#' the user shouldn't have to know where it starts and ends. `parse()` with
#' srcrefs gives every top-level expression's exact line range for free, so
#' each hit is a click-ready {startLine, endLine} rather than a hint about
#' one line. Still zero LLM calls. When the file doesn't parse (mid-edit
#' syntax error) the per-line scan takes over so the list never goes blank
#' on a typo. `line` is kept on every hit for older bundles that read it.
align_scan_unmarked_plots <- function(content) {
  lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return(list())
  marked <- .align_marked_line_mask(lines)

  exprs <- tryCatch(
    suppressWarnings(parse(text = lines, keep.source = TRUE)),
    error = function(e) NULL
  )
  if (is.null(exprs)) return(.align_scan_unmarked_plots_by_line(lines, marked))

  refs <- attr(exprs, "srcref")
  hits <- list()
  for (ref in refs) {
    start <- ref[[1L]]
    end <- ref[[3L]]
    if (any(marked[start:end])) next
    code <- .align_strip_comments(lines[start:end])
    if (!any(grepl(.align_plot_hint_pattern, code, perl = TRUE))) next
    hits[[length(hits) + 1]] <- list(
      line = start,
      startLine = start,
      endLine = end,
      snippet = trimws(lines[start])
    )
  }
  hits
}

#' Pre-DS-423 per-line scan, kept as the fallback for unparseable files.
.align_scan_unmarked_plots_by_line <- function(lines, marked) {
  hits <- list()
  for (i in seq_along(lines)) {
    if (marked[i]) next
    if (grepl(.align_plot_hint_pattern, .align_strip_comments(lines[i]), perl = TRUE)) {
      hits[[length(hits) + 1]] <- list(line = i, startLine = i, endLine = i, snippet = trimws(lines[i]))
    }
  }
  hits
}

#' The editor context both annotate entries need, or list(error=) when the
#' active document can't be tracked. Errors travel as data, same contract as
#' the rest of this package, since every call site (addin dialog, HTTP
#' response) shows them without a thrown condition to catch.
.align_annotate_context <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(error = "RStudio API not available."))
  }
  ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
  if (is.null(ctx) || !nzchar(ctx$path)) {
    return(list(error = "Save the file first — Align tracks files on disk, not unsaved editors."))
  }
  rel_path <- .align_rel_from_abs(ctx$path)
  if (is.null(rel_path)) {
    return(list(error = "This file is outside the R working directory. Set the working directory to its folder (Session > Set Working Directory) and try again."))
  }
  list(ctx = ctx, relPath = rel_path)
}

#' Inserts a marker above `start_row` of the active document (or adopts the
#' marker already sitting there) so the lines `start_row..end_row` become a
#' tracked figure region. Shared by the range and selection entries.
.align_annotate_rows <- function(ctx, rel_path, start_row, end_row) {
  n <- length(ctx$contents)
  if (start_row < 1 || end_row < start_row || start_row > n) {
    return(list(error = "That code is no longer where it was — the file changed. Try again."))
  }
  end_row <- min(end_row, n)
  region_text <- paste(ctx$contents[start_row:end_row], collapse = "\n")

  # A block that already contains a tracked figure further down (the user
  # selected the whole file, DS-423 live finding) would get a marker above
  # it whose region ends at that inner marker — a "figure" made of the
  # preamble, which renders nothing. Refuse and say which figure is inside.
  inner_re <- "^\\s*#\\s*Figure:\\s*(.*?)\\s*\\[fig:[A-Za-z0-9_-]+\\]\\s*-{4,}\\s*$"
  if (end_row > start_row) {
    inner <- grep(inner_re, ctx$contents[(start_row + 1):end_row], value = TRUE)
    if (length(inner) > 0) {
      inner_label <- sub(inner_re, "\\1", inner[[1]])
      return(list(error = sprintf(
        "That selection already contains a tracked figure (%s). Select just the plot code.", inner_label)))
    }
  }

  # Reuse an existing marker instead of stacking a second one (ALI-270
  # follow-up): a reload leaves markers in the file with no viz tracking
  # them, and re-adding the same block must adopt the existing id, not
  # duplicate the line. Adopt when the first row IS a marker line, or the
  # nearest non-blank line above it is one.
  marker_re <- "^\\s*#\\s*Figure:\\s*(.*?)\\s*\\[fig:([A-Za-z0-9_-]+)\\]\\s*-{4,}\\s*$"
  existing_line <- NULL
  if (grepl(marker_re, ctx$contents[start_row])) {
    existing_line <- ctx$contents[start_row]
  } else {
    i <- start_row - 1
    while (i >= 1 && grepl("^\\s*$", ctx$contents[i])) i <- i - 1
    if (i >= 1 && grepl(marker_re, ctx$contents[i])) existing_line <- ctx$contents[i]
  }
  if (!is.null(existing_line)) {
    # The rows may include the marker line itself — the region body a
    # later poll resolves never contains it, so strip it here too.
    body_lines <- Filter(
      function(l) !grepl(marker_re, l),
      strsplit(region_text, "\r\n|\r|\n")[[1]]
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

  # Insert directly above the first row — the rows themselves are left
  # untouched, so their text (returned below) still matches what ends up as
  # the region body once the poll re-scans the file.
  insert_at <- rstudioapi::document_position(start_row, 1)
  rstudioapi::insertText(insert_at, paste0(marker_line, "\n"), id = ctx$id)
  # Deliberately no documentSave() here — Align never saves a file on the
  # user's behalf, full stop. The marker lives in the live editor buffer;
  # the tracked-file poll reads that buffer directly for whichever file is
  # currently active (align_active_document_state, tracked_files.R) rather
  # than requiring a save to become visible.

  list(filePath = rel_path, markerId = marker_id, label = label, sourceCode = region_text)
}

#' DS-423 primary entry: track the figure occupying lines start..end of the
#' active document — the range align_scan_unmarked_plots reported and the
#' user clicked. No selection involved.
align_annotate_range <- function(start_line, end_line) {
  start_line <- suppressWarnings(as.integer(start_line))
  end_line <- suppressWarnings(as.integer(end_line))
  if (length(start_line) != 1 || length(end_line) != 1 || is.na(start_line) || is.na(end_line)) {
    return(list(error = "A line range is required."))
  }
  a <- .align_annotate_context()
  if (!is.null(a$error)) return(a)
  .align_annotate_rows(a$ctx, a$relPath, start_line, end_line)
}

#' Fallback entry: wraps the current RStudio editor selection. Kept for code
#' the detector doesn't recognise, and for the Addins-menu binding.
align_annotate_selection <- function() {
  a <- .align_annotate_context()
  if (!is.null(a$error)) return(a)
  ctx <- a$ctx
  sel <- ctx$selection[[1]]
  selected_text <- if (!is.null(sel)) sel$text else ""
  if (!nzchar(trimws(selected_text))) {
    return(list(error = "Select the figure's code in the editor first."))
  }
  start_row <- sel$range$start[[1]]
  end_row <- sel$range$end[[1]]
  # A selection ending at column 1 of the next line doesn't include that line.
  if (end_row > start_row && sel$range$end[[2]] == 1) end_row <- end_row - 1
  .align_annotate_rows(ctx, a$relPath, start_row, end_row)
}

#' Active RStudio document's tracking-relevant state (Round 4.A/4.C) — path,
#' whether there's a selection ready to annotate, and unmarked-plot nudge
#' figure hits (with line ranges since DS-423). Powers the plugin's "Your
#' code" list; zero LLM calls anywhere in it.
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
