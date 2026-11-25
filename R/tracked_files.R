# Tracked-file read/stat endpoints for the file-as-source-of-truth live-render
# poll (ALI-198 T1). Same safety shape as docs.R's .align-file guard, but for
# arbitrary source files (R/Python scripts) a Visualization's fileSource
# points at — unlike .align files these commonly live in subdirectories
# (project layouts nest scripts under R/, scripts/, etc.), so this guard
# allows relative paths with separators but still rejects traversal out of
# the working directory.
#
# Split into two endpoints (stat, then read) so the poll's steady-state cost
# is one cheap mtime check per tracked file; content is only fetched, and the
# file only re-scanned for marker regions, when mtime actually moved.

#' Is `full` inside the directory `root`? Both must already be normalized
#' (normalizePath); either separator is accepted. A bare string prefix is not
#' enough: "/x/proj-secrets" starts with "/x/proj" but is a sibling, so the
#' match must end at a path separator (DS-908 #27). The directory itself does
#' not count as inside it. Case-folded on Windows, whose paths are
#' case-insensitive.
.align_path_within <- function(full, root) {
  full <- gsub("\\", "/", full, fixed = TRUE)
  root <- gsub("\\", "/", root, fixed = TRUE)
  if (.Platform$OS.type == "windows") {
    full <- tolower(full)
    root <- tolower(root)
  }
  prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  startsWith(full, prefix) && nchar(full) > nchar(prefix)
}

#' Relative path from the pane -> safe absolute path inside getwd(), or NULL.
#'
#' The path is document state (a figure's fileSource) that any editor of a
#' shared document can set, so it is treated as hostile (DS-908 #27): no
#' absolute paths, no ".." segment anywhere (rejected as a segment, so a name
#' like "fig..v2.R" is still fine), and the resolved path — symlinks
#' followed, since normalizePath resolves those for files that exist — must
#' sit inside the working directory with a separator boundary. Before
#' DS-908 the ".." test used a regex with fixed = TRUE (never matched) and
#' the containment test was a bare prefix, so "../proj-secrets/key.txt"
#' escaped from proj/.
.align_safe_rel_path <- function(path) {
  path <- as.character(path)
  if (length(path) != 1 || is.na(path) || !nzchar(path)) return(NULL)
  if (grepl("^[/\\\\]", path) || grepl("^[A-Za-z]:", path)) return(NULL)  # absolute path (posix or windows)
  if (any(strsplit(path, "[/\\\\]")[[1]] == "..")) return(NULL)
  root <- normalizePath(getwd(), winslash = "/")
  full <- suppressWarnings(normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE))
  if (!.align_path_within(full, root)) return(NULL)
  full
}

#' Does the path name a script a figure can be linked to? The pane only ever
#' asks /file/stat and /file/read about this machine's own links to scripts
#' (packages/rstudio-host/src/link-trust.ts gates every caller on
#' isRScriptPath, and data files never travel this way), so the server
#' refuses everything else too: even a path that passes the containment
#' check cannot name `.Renviron` or a credentials file in the project
#' (DS-908 #27). Keep the pattern in step with isRScriptPath in
#' packages/core/src/plugin-chrome/native-trust.ts.
.align_is_script_path <- function(path) {
  grepl("\\.(r|rmd|qmd|py)$", path, ignore.case = TRUE)
}

#' Resolve a pane-supplied path for the tracked-file endpoints: inside the
#' working directory and a script, or NULL (DS-908 #27).
.align_tracked_script_path <- function(path) {
  full <- .align_safe_rel_path(path)
  if (is.null(full) || !.align_is_script_path(full)) return(NULL)
  full
}

#' mtime/size for a tracked file — the poll's cheap "did anything change" check.
align_stat_file <- function(path) {
  full <- .align_tracked_script_path(path)
  if (is.null(full) || !file.exists(full)) {
    return(list(path = path, exists = FALSE))
  }
  info <- file.info(full)
  list(
    path = path,
    exists = TRUE,
    mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S%z"),
    bytes = as.numeric(info$size)
  )
}

#' Raw text content of a tracked file — fetched only after align_stat_file
#' reports a changed mtime, so the poll never re-reads unchanged files. This
#' is a disk read of whatever the user has already chosen to save; it is
#' never used to justify writing anything back (see align_active_document_state
#' for the live, unsaved-included alternative used whenever it applies).
align_read_file <- function(path) {
  full <- .align_tracked_script_path(path)
  if (is.null(full) || !file.exists(full)) {
    return(list(error = "File not found."))
  }
  list(content = paste(readLines(full, warn = FALSE), collapse = "\n"))
}

#' Live content of the RStudio document currently focused in the editor, if
#' any — read straight from the in-memory buffer via rstudioapi, never disk.
#'
#' Why this exists: Align must never save a file on the user's behalf (not
#' even to make polling simpler). The tracked-file poll (T1) used to work
#' around that by requiring annotate/apply-edit to force a save so their
#' change would show up in a disk read — which was exactly the wrong
#' tradeoff. This is the correct one: whichever file is currently active in
#' the editor is read live, unsaved edits included, no save ever triggered.
#' A file the user has switched away from can't be read this way — rstudioapi
#' has no API to inspect a document that isn't the focused one — so the poll
#' falls back to align_stat_file/align_read_file (disk) for anything that
#' isn't the current result of this function. That fallback only ever reads
#' content the user already chose to persist themselves, which is not the
#' same thing as Align persisting it for them.
align_active_document_state <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(list(available = FALSE))
  }
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || !nzchar(ctx$path)) {
    return(list(available = TRUE, path = NULL, content = NULL))
  }
  rel_path <- .align_rel_from_abs(ctx$path)
  if (is.null(rel_path)) {
    return(list(available = TRUE, path = NULL, content = NULL))
  }
  list(available = TRUE, path = rel_path, content = paste(ctx$contents, collapse = "\n"))
}
