# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-12, at the request of Steven L. Shafer, so
# that the Simulation API's documentation is a web page with a URL that can be
# sent to an integrator (an EHR vendor, say): https://stanpumpr.io/api.
#
# STATUS: run and verified on R 4.3.3 by tests/testthat/test-api-docs.R.
# -----------------------------------------------------------------------------
#
# THE API DOCUMENTATION PAGE
# ==========================
# The documentation is one Markdown file, inst/apidocs/api.md, installed with
# the package.  apiDocsHtml() renders it into a self-contained HTML page (its
# styles inline, no scripts, nothing fetched from elsewhere) with an anchor on
# every section heading and a table of contents built from them.  The page is
# served in two places, both rendered from the same file when they start, so
# neither can drift from the Markdown:
#
#   run_app()  adds it as the resource path "api" (apiDocsDir()), so the
#              Shiny app serves it at <app>/api: https://stanpumpr.io/api.
#   run_api()  serves it at GET / of the HTTP service itself.
#
# shiny::markdown() (commonmark, with the GitHub extensions: tables) does the
# rendering, as it does for the in-app help, so no package is added.

API_DOCS_PATH <- "api"
API_DOCS_TITLE <- "stanpumpR Simulation API"

# A heading's text to the anchor GitHub would give it: lower case, tags and
# punctuation dropped, spaces to hyphens ("The time grid" -> "the-time-grid").
apiDocsSlug <- function(text) {
  text <- gsub("<[^>]+>", "", text)
  text <- gsub("&[a-z]+;|&#[0-9]+;", "", text)
  text <- tolower(trimws(text))
  text <- gsub("[^a-z0-9 -]", "", text)
  gsub(" +", "-", text)
}

# Give every <h2> and <h3> an id, and return the page with the <h2>s listed.
apiDocsAnchor <- function(html) {
  pattern <- "<(h[23])>(.*?)</h[23]>"
  hits <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
  contents <- character(0)
  for (h in hits) {
    level <- sub(pattern, "\\1", h, perl = TRUE)
    text <- sub(pattern, "\\2", h, perl = TRUE)
    id <- apiDocsSlug(text)
    html <- sub(h, sprintf('<%s id="%s">%s</%s>', level, id, text, level), html, fixed = TRUE)
    if (level == "h2") contents <- c(contents, sprintf('<li><a href="#%s">%s</a></li>', id, text))
  }
  list(html = html, contents = contents)
}

API_DOCS_CSS <- "
:root { --fg: #1f2328; --muted: #59636e; --bg: #ffffff; --panel: #f6f8fa;
        --border: #d1d9e0; --accent: #0b5cad; --note: #fff8e1; --note-border: #e0b400; }
@media (prefers-color-scheme: dark) {
  :root { --fg: #e6edf3; --muted: #9198a1; --bg: #0d1117; --panel: #161b22;
          --border: #30363d; --accent: #58a6ff; --note: #2a2410; --note-border: #9e7c00; }
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--bg); color: var(--fg);
       font: 16px/1.6 -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; }
header { border-bottom: 1px solid var(--border); padding: 12px 16px; }
header a { color: var(--fg); font-weight: 600; text-decoration: none; }
.layout { display: grid; grid-template-columns: 220px minmax(0, 1fr); gap: 32px;
          max-width: 1200px; margin: 0 auto; padding: 24px 16px 64px; }
nav { position: sticky; top: 16px; align-self: start; font-size: 14px; }
nav h2 { font-size: 12px; text-transform: uppercase; letter-spacing: .05em; color: var(--muted);
         margin: 0 0 8px; border: 0; padding: 0; }
nav ul { list-style: none; margin: 0; padding: 0; }
nav li { margin: 4px 0; }
nav a { color: var(--muted); text-decoration: none; }
nav a:hover { color: var(--accent); }
main { min-width: 0; overflow-wrap: break-word; }
h1 { font-size: 2em; margin: 0 0 16px; }
h2 { font-size: 1.5em; margin: 40px 0 16px; padding-bottom: 6px; border-bottom: 1px solid var(--border); }
h3 { font-size: 1.2em; margin: 28px 0 12px; }
a { color: var(--accent); }
hr { display: none; }
code { font: 0.875em/1.5 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
       background: var(--panel); padding: 0.15em 0.35em; border-radius: 4px;
       overflow-wrap: anywhere; }
pre { background: var(--panel); border: 1px solid var(--border); border-radius: 6px;
      padding: 12px 16px; overflow-x: auto; }
pre code { background: none; padding: 0; overflow-wrap: normal; }
table { border-collapse: collapse; width: 100%; margin: 16px 0; }
th, td { border: 1px solid var(--border); padding: 6px 12px; text-align: left; vertical-align: top; }
th { background: var(--panel); }
td code { white-space: nowrap; }
blockquote { margin: 16px 0; padding: 8px 16px; background: var(--note);
             border-left: 4px solid var(--note-border); border-radius: 4px; }
blockquote p { margin: 4px 0; }
footer { color: var(--muted); font-size: 13px; border-top: 1px solid var(--border);
         margin-top: 48px; padding-top: 12px; }
@media (max-width: 800px) {
  .layout { grid-template-columns: minmax(0, 1fr); gap: 0; }
  nav { position: static; border-bottom: 1px solid var(--border); padding-bottom: 12px; margin-bottom: 8px; }
  td, th { overflow-wrap: anywhere; }
  td code { white-space: normal; }
}
"

#' The Simulation API documentation as an HTML page
#'
#' Renders `inst/apidocs/api.md` into a self-contained HTML page with a table
#' of contents. The Shiny app serves it at `/api` (https://stanpumpr.io/api)
#' and [run_api()] serves it at `/`.
#'
#' @param path the Markdown file to render
#' @returns the page, as a single string
#' @export
apiDocsHtml <- function(path = system.file("apidocs", "api.md", package = "stanpumpR")) {
  if (!nzchar(path) || !file.exists(path)) stop("The API documentation (apidocs/api.md) was not found.")
  md <- readLines(path, encoding = "UTF-8", warn = FALSE)
  body <- apiDocsAnchor(as.character(shiny::markdown(md, extensions = TRUE)))
  # External links open in a new tab, so the reader keeps the documentation.
  html <- gsub('<a href="(https?://[^"]*)"', '<a href="\\1" target="_blank" rel="noopener"', body$html)
  version <- as.character(utils::packageVersion("stanpumpR"))
  paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '<title>', API_DOCS_TITLE, '</title>\n',
    '<meta name="description" content="HTTP API for stanpumpR pharmacokinetic simulation: ',
    'send a dose table, patient covariates and a duration; receive predicted plasma and ',
    'effect-site concentrations as JSON, CSV, Excel or XML.">\n',
    '<style>', API_DOCS_CSS, '</style>\n</head>\n<body>\n',
    '<header><a href="https://stanpumpr.io" target="_blank" rel="noopener">stanpumpR</a> &middot; Simulation API</header>\n',
    '<div class="layout">\n<nav aria-label="Contents"><h2>Contents</h2><ul>\n',
    paste(body$contents, collapse = "\n"),
    '\n</ul></nav>\n<main>\n', html,
    '\n<footer>API version ', API_VERSION, ' &middot; stanpumpR ', version,
    ' &middot; <a href="https://github.com/StevenLShafer/stanpumpR" target="_blank" rel="noopener">Source on GitHub</a></footer>\n',
    '</main>\n</div>\n</body>\n</html>\n'
  )
}

# The page written to a directory as index.html, for shiny::addResourcePath():
# a directory served that way answers <prefix>/ and <prefix> with its
# index.html.  Rendered once, when the app starts.
apiDocsDir <- function(dir = file.path(tempdir(), "stanpumpr-api-docs")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # Bytes, not text: a text connection on Windows would turn every \n into
  # \r\n.  The page is UTF-8 (enc2utf8) whatever the session's locale.
  writeBin(charToRaw(enc2utf8(apiDocsHtml())), file.path(dir, "index.html"))
  dir
}
