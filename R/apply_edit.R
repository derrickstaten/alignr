# Safe-write mechanism for AI-approved code changes to file-tracked
# visualizations (ALI-198 T4c, Round 4.D).
#
# Why this can't just update the canvas in memory: the file is the source of
# truth for a file-tracked viz (ALI-198 T1) — the background poll re-scans
# the file on every tick and overwrites the canvas's sourceCode with
# whatever the file says. An approved AI edit that only touched in-memory
# canvas state would get silently reverted on the next poll tick the moment
# the file's actual content didn't match. So "apply" here means "write the
# file," full stop — the existing poll (already built in T1) picks up the
# resulting change and updates the canvas the same way it would for a human
# edit in RStudio. No separate canvas-mutation path needed.
#
# Why through rstudioapi and not a raw filesystem write (Round 4.D): a raw
# write could silently diverge from the live editor buffer if the user has
# the file open with unsaved edits, clobbering them or fighting RStudio's own
# "file changed on disk" prompt. Going through navigateToFile + modifyRange
# keeps the editor buffer authoritative.
#
# Deliberately NOT followed by documentSave(): Align never saves a file on
# the user's behalf, full stop — an approved AI edit lands in the live
# buffer only. The tracked-file poll reads that buffer directly for
# whichever file is currently active (align_active_document_state,
# tracked_files.R), so the change is visible on the canvas immediately
# without ever touching disk; the user decides if/when to actually save.

#' Locates a marker's region as 1-indexed line numbers within `lines`.
#' Mirrors file-region-utils.ts's parseFileRegions/findRegionById — kept in
#' sync by hand across the R/TS boundary, same caveat noted in annotate.R's
#' .align_marker_line. Region = the lines strictly after the marker line
#' through the line before the next marker (any id), or EOF.
.align_find_marker_region <- function(lines, marker_id) {
  marker_re <- "^\\s*#\\s*Figure:.*\\[fig:([A-Za-z0-9_-]+)\\]\\s*-{4,}\\s*$"
  marker_lines <- grep(marker_re, lines)
  if (length(marker_lines) == 0) return(list(found = FALSE))

  ids <- sub(marker_re, "\\1", lines[marker_lines])
  matches <- which(ids == marker_id)
  if (length(matches) == 0) return(list(found = FALSE))

  idx <- matches[1]
  start_line <- marker_lines[idx] + 1
  later_markers <- marker_lines[marker_lines > marker_lines[idx]]
  end_line <- if (length(later_markers) > 0) later_markers[1] - 1 else length(lines)
  # A marker on the file's last line has an empty region (start_line > end_line)
  # — valid (an empty figure body), callers just get zero replaced lines.

  list(found = TRUE, duplicate = length(matches) > 1, startLine = start_line, endLine = max(end_line, start_line - 1))
}

#' Writes an approved AI code change back into a tracked file's marker
#' region via the live RStudio editor buffer. See file header for why this
#' is the entire "apply" action — no canvas-side mutation happens here.
#'
#' @param file_path Relative path (same form as Visualization.fileSource.filePath).
#' @param marker_id Opaque marker id identifying the region to replace.
#' @param code New region body (the model's full replacement code, no marker line).
#' @return list(ok = TRUE) on success, or list(error = <string>) — same
#'   errors-as-data contract as the rest of this package.
align_apply_file_edit <- function(file_path, marker_id, code) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(error = "RStudio API not available."))
  }
  full <- .align_safe_rel_path(file_path)
  if (is.null(full) || !file.exists(full)) {
    return(list(error = "File not found."))
  }

  doc_id <- tryCatch(rstudioapi::navigateToFile(full), error = function(e) NULL)
  if (is.null(doc_id)) {
    return(list(error = "Could not open the file in the editor."))
  }

  ctx <- tryCatch(rstudioapi::getSourceEditorContext(id = doc_id), error = function(e) NULL)
  if (is.null(ctx)) {
    return(list(error = "Could not read the editor buffer."))
  }
  lines <- ctx$contents

  region <- .align_find_marker_region(lines, marker_id)
  if (!isTRUE(region$found)) {
    return(list(error = "Marker not found in the file — it may have been renamed or deleted. Try relinking."))
  }

  # Strip marker lines from the incoming body (ALI-275 live finding): the
  # model included the region's own "# Figure: ... ----" header in its
  # proposed code, and writing it verbatim duplicated the marker — splitting
  # the region in two and breaking rendering. The region is anchored by the
  # marker already in the file; a body must never carry one. Deterministic
  # and zero-AI, so prompt drift can't reintroduce it.
  marker_line_re <- "^\\s*#\\s*Figure:.*\\[fig:[A-Za-z0-9_-]+\\]\\s*-{4,}\\s*$"
  code_lines <- strsplit(as.character(code), "\r\n|\r|\n")[[1]]
  code <- paste(code_lines[!grepl(marker_line_re, code_lines)], collapse = "\n")

  # Whole-line replacement: start of the region's first line through the
  # start of the line just past its last — covers the region's own trailing
  # newline in one range, so the replacement just supplies its own to avoid
  # merging with whatever follows (the next marker, or EOF). A position past
  # the last line resolves to end-of-document in RStudio's (Ace-based)
  # editor rather than erroring — the standard idiom real-world rstudioapi
  # addins rely on for whole-line-range replacement — so this needs no
  # special-casing for a region that runs to EOF, or an empty region
  # (startLine == endLine + 1, a zero-width insert-only range).
  #
  # NOTE: this end-of-document clamping behavior could not be exercised
  # against a real RStudio session while building this (rstudioapi::isAvailable()
  # is FALSE in this headless test harness) — flagged for a live sanity check.
  range <- rstudioapi::document_range(
    rstudioapi::document_position(region$startLine, 1),
    rstudioapi::document_position(region$endLine + 1, 1)
  )
  new_code <- sub("\\n+$", "", code)
  rstudioapi::modifyRange(range, paste0(new_code, "\n"), id = doc_id)

  list(ok = TRUE)
}
