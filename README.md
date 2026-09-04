# alignr

Align for RStudio and Positron: the Align figure editor in your Viewer pane,
rendering R visualizations through your live session — your installed
packages and your in-memory data frames, no upload. Sign in to use Align AI
in the panel, and hand a document to [Align Web](https://alignfigures.com)
with one click.

## Install

```r
install.packages("remotes")
remotes::install_github("derrickstaten/alignr")
```

Dependencies install automatically. If R asks *"Do you want to install from
sources the packages which need compilation?"*, answer **no** — the prebuilt
binaries are what you want, and building from source needs Xcode's
command-line tools. To never be asked, run
`options(install.packages.compile.from.source = "never")` first.

Then, in RStudio or Positron:

```r
alignr::align_open()
```

`align_pop_out()` opens the same session in your system browser; `align_stop()`
shuts the local server down. Positron is supported through the same Viewer
pane mechanism.

## Releases

This repository is a **distribution mirror**: every push here is a release
cut automatically from the [align-web](https://github.com/derrickstaten/align-web)
production deploy, with the built plugin bundle committed under `inst/www`.
Versions are dated (`2026.9.2`); a second release on the same day appends a
counter. On startup the plugin compares the release it was built from with
the Align server it is talking to and prints a one-line notice when they
differ — update with the install line above.

Issues and pull requests belong on **align-web**; nothing here is edited by
hand.

## Developing (align-web checkout only)

`inst/www` is Vite's output for `packages/rstudio-host` and is gitignored in
align-web — build-on-demand, never committed there (ALI-269):

```bash
npm run build --workspace=@align/rstudio-host
```

Then load the package from source rather than installing it, so the release
build in your R library stays as users have it (DS-423):

```r
pkgload::load_all("r-package")   # from the align-web repo root
alignr::align_open()
```

Restarting R puts you back on the installed release. (`source("demo-plugin-code/r/01-setup-and-open-align.R")` does all of this plus demo data.)

Guards that enforce the policy:

- `align_start()` **refuses to start** when `inst/www/index.html` is missing.
- Each build writes `inst/www/build-stamp.json`; when your working directory
  is inside the repo, `align_start()` compares that stamp against the newest
  mtime under `packages/rstudio-host/src` and `packages/core/src` and prints
  a **STALE** message if source changed after the build.
- A rebuilt bundle is not picked up by a running server: restart with
  `align_stop(); align_open()`.

The from-scratch local procedure and the release mechanics live in
`docs/tech/rstudio-plugin.md` in align-web. Release runs are visible at
https://github.com/derrickstaten/align-web/actions/workflows/release-plugin.yml. Headless checks:
`Rscript tests/test-alignr.R` from this directory.

## License

MIT — see `LICENSE.md`.
