# The orientation documents (AGENTS.md and the docs/ guides) name functions
# and files by hand, and a rename or a deletion leaves them pointing at
# nothing: the 2026-10-31 review found several such names.  These tests hold
# every backticked `name()` to a definition somewhere in the package, its
# test helpers, its JavaScript or the packages it imports, and every
# backticked repository path to a file that exists.
#
# The documents are not part of the installed package, so the tests run only
# from a source checkout (devtools::test()), and skip under R CMD check.

agentDocsRoot <- function() {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  if (!file.exists(file.path(root, "AGENTS.md")) || !dir.exists(file.path(root, "R")))
    testthat::skip("not a source checkout")
  root
}

AGENT_DOCS <- c("AGENTS.md", "docs/adding-a-drug.md", "docs/architecture.md",
                "docs/weight-adjustment.md")

agentDocCode <- function(root, doc) {
  text <- paste(readLines(file.path(root, doc), warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  gsub("`", "", regmatches(text, gregexpr("`[^`\n]+`", text))[[1]], fixed = TRUE)
}

# Every name assigned (`name <-`, `name =`) or declared (`function name`) in
# the package's R code, its test helpers and its JavaScript (inst/www, and
# the visualizer's source in tools/viewer/src).  Local functions
# and reactives inside app_server() count: AGENTS.md names them on purpose.
agentDocDefinitions <- function(root, jsOnly = FALSE) {
  files <- c(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE),
             list.files(file.path(root, "tests", "testthat"), "^helper.*\\.R$",
                        full.names = TRUE),
             list.files(file.path(root, "inst", "www"), "\\.js$", full.names = TRUE),
             # the Visualizer's source, whose functions AGENTS.md names
             # (displayVolumes()); built into inst/www/stanpumpR-viewer.html
             list.files(file.path(root, "tools", "viewer", "src"), "\\.js$", full.names = TRUE),
             # the scripts that build and check the data (checkFdaLabels())
             list.files(file.path(root, "data-raw"), "\\.R$", full.names = TRUE))
  if (jsOnly) files <- files[grepl("\\.js$", files)]
  unique(unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    # a plain binding only: not a member (state$values$x <-) or the tail of
    # a longer name
    c(regmatches(lines, regexpr("(?<![$@A-Za-z0-9._])[A-Za-z.][A-Za-z0-9._]*(?=\\s*(<-|=[^=]))",
                                lines,
                                perl = TRUE)),
      sub("function\\s+", "",
          regmatches(lines, regexpr("function\\s+[A-Za-z_][A-Za-z0-9_]*", lines,
                                    perl = TRUE))),
      # a JavaScript method of an object literal (View's draw(st, ui) {),
      # how the visualizer defines most of its functions
      if (grepl("\\.js$", f)) {
        m <- sub("\\s*\\(.*$", "",
                 trimws(regmatches(lines, regexpr("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*\\([^()]*\\)\\s*\\{",
                                                  lines, perl = TRUE))))
        setdiff(m, c("if", "for", "while", "switch", "catch", "function"))
      })
  })))
}

# Whether the JavaScript defines `Object.method`: assigned as such
# (Object.method = , Object.prototype.method = ), or written as a method or
# property inside the braces of the object literal or class that defines
# Object (const Cards = { ... fit(view) { ... } ... }).
agentDocJsMember <- function(root, name) {
  parts <- strsplit(name, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(FALSE)
  object <- parts[1]
  method <- parts[2]
  files <- c(list.files(file.path(root, "inst", "www"), "\\.js$", full.names = TRUE),
             list.files(file.path(root, "tools", "viewer", "src"), "\\.js$", full.names = TRUE))
  for (f in files) {
    text <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (grepl(sprintf("\\b%s(\\.prototype)?\\.%s\\s*=[^=]", object, method), text, perl = TRUE))
      return(TRUE)
    start <- regexpr(sprintf("(\\b(const|let|var)\\s+%s\\s*=\\s*|\\bclass\\s+%s\\b[^{]*)\\{",
                             object, object), text, perl = TRUE)
    if (start < 0) next
    # the body: from that brace to the one that closes it
    chars <- strsplit(substring(text, start + attr(start, "match.length") - 1), "")[[1]]
    depth <- cumsum((chars == "{") - (chars == "}"))
    end <- which(depth == 0)[1]
    body <- paste(chars[seq_len(if (is.na(end)) length(chars) else end)], collapse = "")
    # a shorthand method (fit(view) {), or a property whose value is a
    # function (fit: function, fit: (v) =>, fit: v =>); never a property
    # holding a value (level: 0), which cannot be called
    if (grepl(sprintf(paste0("(^|\\n)\\s*(async\\s+|get\\s+|set\\s+|static\\s+)?%s\\s*",
                             "(\\([^()]*\\)\\s*\\{|:\\s*(async\\s+)?(function\\b|\\([^()]*\\)\\s*=>|[A-Za-z_$][A-Za-z0-9_$]*\\s*=>))"),
                      method), body, perl = TRUE))
      return(TRUE)
  }
  FALSE
}

test_that("a JavaScript method is found only on the object that defines it", {
  root <- agentDocsRoot()
  expect_true(agentDocJsMember(root, "Cards.fit"))
  # draw() is a method of View, not of Cards
  expect_false(agentDocJsMember(root, "Cards.draw"))
  expect_false(agentDocJsMember(root, "Cards.fitt"))
  expect_false(agentDocJsMember(root, "Cardz.fit"))
  # level: 0 is a property of Cards, not a method
  expect_false(agentDocJsMember(root, "Cards.level"))
})

test_that("every function the orientation documents name exists", {
  root <- agentDocsRoot()
  defined <- agentDocDefinitions(root)
  # A JavaScript method named through its object (Cards.fit() in the
  # Visualizer) is found only when that object defines it, so a misspelling of
  # either, or a method another object defines, still fails.
  jsMember <- function(name) agentDocJsMember(root, name)
  imports <- c("base", "stats", "utils", "shiny", "testthat", "mirai")
  inImports <- function(name)
    any(vapply(imports, function(pkg)
      requireNamespace(pkg, quietly = TRUE) && exists(name, envir = asNamespace(pkg)),
      logical(1)))
  # pkg::fn() must be an export of the package it names
  exportedBy <- function(qualified) {
    parts <- strsplit(qualified, "::", fixed = TRUE)[[1]]
    requireNamespace(parts[1], quietly = TRUE) &&
      parts[2] %in% getNamespaceExports(parts[1])
  }
  for (doc in AGENT_DOCS) {
    code <- agentDocCode(root, doc)
    calls <- unique(sub("\\(\\)$", "",
                        code[grepl("^[A-Za-z.][A-Za-z0-9._]*\\(\\)$", code)]))
    for (name in calls)
      expect_true(name %in% defined || inImports(name) || jsMember(name),
                  info = sprintf("%s names %s(), which nothing defines", doc, name))
    # any qualified call, with or without arguments (renv::install("pkg"))
    qualified <- unique(sub("\\(.*$", "",
                            code[grepl("^[A-Za-z][A-Za-z0-9.]*::[A-Za-z.][A-Za-z0-9._]*\\(", code)]))
    for (name in qualified) {
      # an optional package (rsconnect, plumber) may not be installed; its
      # exports cannot be checked then, which is no evidence of a stale name
      pkg <- strsplit(name, "::", fixed = TRUE)[[1]][1]
      if (!requireNamespace(pkg, quietly = TRUE)) next
      expect_true(exportedBy(name),
                  info = sprintf("%s names %s(), which its package does not export", doc, name))
    }
  }
})

test_that("every repository path the orientation documents name exists", {
  root <- agentDocsRoot()
  for (doc in AGENT_DOCS) {
    code <- agentDocCode(root, doc)
    paths <- code[grepl("^(R|inst|tests|docs|data-raw|man|vignettes|\\.github)/[^ ]+$", code)]
    paths <- sub("#.*$", "", paths)              # an anchor names a section
    paths <- unique(paths[!grepl("[<>*{}]", paths)])  # a pattern, not a file
    for (path in paths)
      expect_true(file.exists(file.path(root, path)),
                  info = sprintf("%s names %s, which does not exist", doc, path))
    # a bare file name (`simCpCe.R`, `app.R`, `helpers.R`) is one of the
    # package's files at the root, in R/ or among the test helpers
    bare <- unique(sub("#.*$", "", code[grepl("^[A-Za-z0-9_.-]+\\.R(#.*)?$", code)]))
    for (file in bare)
      expect_true(any(file.exists(file.path(root, c(".", "R", "tests/testthat"), file))),
                  info = sprintf("%s names %s, which is no file of the package", doc, file))
  }
})
