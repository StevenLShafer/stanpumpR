# The help system: registry, Markdown rendering, links and search.
# See R/help-content.R.

test_that("the page registry is well formed", {
  reg <- helpPageRegistry()
  expect_true(nrow(reg) > 50)
  expect_false(any(duplicated(reg$id)))
  expect_true(all(reg$section %in% names(HELP_SECTIONS)))
  expect_true(all(nzchar(reg$title)))
  expect_identical(reg$sectionTitle, unname(HELP_SECTIONS[reg$section]))
  # The sidebar order is by section
  expect_identical(unique(reg$section), names(HELP_SECTIONS)[names(HELP_SECTIONS) %in% reg$section])
  expect_equal(reg$id[1], "home")
})

test_that("every static page has a Markdown file and every file is reachable", {
  static <- helpStaticPages()
  generated <- c("drugs/index", "scenarios/index", "references")
  for (id in setdiff(static$id, generated)) {
    expect_true(helpHasMarkdown(id), info = paste("missing inst/help/", id, ".md"))
  }
  reg <- helpPageRegistry()
  for (id in helpMarkdownIds()) {
    reachable <- id %in% reg$id ||
      (startsWith(id, "drugs/") && paste0(id) %in% reg$id) ||
      (startsWith(id, "scenarios/") && id %in% reg$id)
    expect_true(reachable, info = paste("orphan help file:", id))
  }
})

test_that("Markdown files start at a second-level heading or body text, not an h1", {
  for (id in helpMarkdownIds()) {
    text <- helpReadMarkdown(id)
    # Comments inside fenced code blocks are not headings
    text <- gsub("```.*?```", "", text, perl = TRUE)
    text <- gsub("(?s)```.*?```", "", text, perl = TRUE)
    expect_false(grepl("(^|\n)# ", text), info = paste(id, "has an h1; the registry supplies the title"))
  }
})

test_that("every page renders", {
  reg <- helpPageRegistry()
  for (id in reg$id) {
    html <- helpPageHTML(id)
    expect_type(html, "character")
    expect_length(html, 1)
    expect_true(nchar(html) > 50, info = paste("page", id, "is nearly empty"))
    ui <- helpPageUI(id)
    expect_s3_class(ui, "shiny.tag")
    rendered <- as.character(ui)
    expect_true(grepl(reg$title[reg$id == id], rendered, fixed = TRUE),
                info = paste("title missing from", id))
  }
})

test_that("every help: and scenario: link in the Markdown resolves", {
  reg <- helpPageRegistry()
  scenarioIds <- helpScenarioIds()
  for (id in helpMarkdownIds()) {
    targets <- helpLinkTargets(helpReadMarkdown(id))
    for (t in targets$pages) {
      expect_true(t %in% reg$id, info = sprintf("%s links to unknown page '%s'", id, t))
    }
    for (t in targets$scenarios) {
      expect_true(t %in% scenarioIds, info = sprintf("%s links to unknown scenario '%s'", id, t))
    }
  }
})

test_that("help links become data attributes and external links open in a new tab", {
  html <- helpMarkdownToHTML("See [propofol](help:drugs/propofol), [load](scenario:propofol-bolus) and [PubMed](https://pubmed.ncbi.nlm.nih.gov/1/).")
  expect_match(html, 'href="#" data-help-page="drugs/propofol"', fixed = TRUE)
  expect_match(html, 'data-help-scenario="propofol-bolus"', fixed = TRUE)
  expect_match(html, 'href="https://pubmed.ncbi.nlm.nih.gov/1/" target="_blank" rel="noreferrer"', fixed = TRUE)
  expect_false(grepl("help:", html, fixed = TRUE))
})

test_that("GitHub-flavoured tables render", {
  html <- helpMarkdownToHTML("| a | b |\n|---|---|\n| 1 | 2 |")
  expect_match(html, "<table>")
  expect_match(html, "<td>2</td>")
})

test_that("headings get ids and a table of contents", {
  out <- helpHeadingIds("<h2>First thing</h2><p>x</p><h2>Second &amp; last</h2><h3>Sub</h3><h2>First thing</h2>")
  expect_equal(out$toc$id, c("first-thing", "second-last", "sub", "first-thing-2"))
  expect_equal(out$toc$level, c(2L, 2L, 3L, 2L))
  expect_match(out$html, '<h2 id="first-thing">First thing</h2>', fixed = TRUE)
  expect_match(out$html, '<h2 id="first-thing-2">First thing</h2>', fixed = TRUE)
  expect_match(out$html, '<h3 id="sub">Sub</h3>', fixed = TRUE)
})

test_that("pages with three or more sections show a table of contents", {
  ui <- as.character(helpPageUI("dose-table"))
  expect_match(ui, "help-toc", fixed = TRUE)
  expect_match(ui, 'href="#columns"', fixed = TRUE)
})

test_that("an unknown page gives the not-found page, not an error", {
  expect_false(helpPageExists("no/such/page"))
  expect_false(helpPageExists(c("home", "faq")))
  expect_false(helpPageExists(NA_character_))
  ui <- as.character(helpPageUI("../../etc/passwd"))
  expect_match(ui, "Page not found", fixed = TRUE)
  expect_match(ui, "etc/passwd", fixed = TRUE)
  expect_false(grepl("<script", ui, fixed = TRUE))
  ui <- as.character(helpPageUI("<script>alert(1)</script>"))
  expect_false(grepl("<script>", ui, fixed = TRUE))
})

test_that("plain text stripping decodes entities and collapses whitespace", {
  expect_equal(helpPlainText("<p>a &amp; b</p>\n\n<p>c&nbsp;d</p>"), "a & b c d")
  expect_equal(helpSlug("The effect site &amp; ke0!"), "the-effect-site-ke0")
})

test_that("search finds pages and ranks title matches first", {
  index <- helpSearchIndex()
  expect_true(all(nchar(index$text) > 0))

  hits <- helpSearch("propofol", index)
  expect_true(nrow(hits) > 3)
  expect_equal(hits$id[1], "drugs/propofol")
  expect_true("models/interaction" %in% hits$id)
  expect_true(all(nzchar(hits$snippet[hits$id != "drugs/propofol"]) | hits$hits[hits$id != "drugs/propofol"] >= 100))

  expect_equal(nrow(helpSearch("Eleveld", index)), nrow(helpSearch("eleveld", index)))
  expect_true("drugs/remifentanil" %in% helpSearch("Kim", index)$id)
})

test_that("search is safe with short, empty and regex-like terms", {
  index <- helpSearchIndex()
  expect_equal(nrow(helpSearch("", index)), 0)
  expect_equal(nrow(helpSearch("p", index)), 0)
  expect_equal(nrow(helpSearch(NULL, index)), 0)
  expect_equal(nrow(helpSearch("xyzzyqwv", index)), 0)
  expect_equal(nrow(helpSearch("(", index)), 0)
  expect_error(helpSearch(".*[", index), NA)
  expect_true(nrow(helpSearch("mcg/kg/min", index)) > 0)
  expect_true(nrow(helpSearch("1:30", index)) > 0)
})

test_that("search results render with highlighted snippets", {
  hits <- helpSearch("laryngoscopy", helpSearchIndex())
  ui <- as.character(helpSearchResultsUI("laryngoscopy", hits))
  expect_match(ui, "<mark>laryngoscopy</mark>", fixed = TRUE)
  expect_match(ui, 'data-help-page="models/interaction"', fixed = TRUE)
  none <- as.character(helpSearchResultsUI("qqqq", hits[0, ]))
  expect_match(ui, "match", fixed = TRUE)
  expect_match(none, "Nothing found", fixed = TRUE)
})

test_that("highlighting marks the term on the plain text, not inside HTML entities", {
  expect_equal(helpHighlight("a & amp b", "amp"), "a &amp; <mark>amp</mark> b")
  expect_equal(helpHighlight("Propofol and propofol", "propofol"),
               "<mark>Propofol</mark> and <mark>propofol</mark>")
  expect_equal(helpHighlight("x < y", "lt"), "x &lt; y")
  expect_equal(helpHighlight("1:30 or 1:30", "1:30"), "<mark>1:30</mark> or <mark>1:30</mark>")
  expect_equal(helpHighlight("none here", "zzz"), "none here")
})

test_that("the sidebar lists every page once and marks the current one", {
  reg <- helpPageRegistry()
  nav <- as.character(helpSidebarNav(reg, "models/effect-site"))
  for (id in reg$id) {
    expect_equal(lengths(regmatches(nav, gregexpr(sprintf('data-help-page="%s"', id), nav, fixed = TRUE))), 1,
                 info = id)
  }
  expect_match(nav, 'aria-current="page"', fixed = TRUE)
  expect_equal(lengths(regmatches(nav, gregexpr('aria-current="page"', nav, fixed = TRUE))), 1)
  # The current page's section and "Getting started" are open; the drug list is not
  expect_match(nav, "<details class=\"help-nav-section\" open>\\s*<summary>\\s*Models and methods")
  expect_false(grepl("<details class=\"help-nav-section\" open>\\s*<summary>\\s*Drug library", nav))
})

test_that("the Help panel UI builds", {
  ui <- as.character(helpNavPanel())
  expect_match(ui, "help_search", fixed = TRUE)
  expect_match(ui, "help_content", fixed = TRUE)
  expect_match(ui, "help_nav", fixed = TRUE)
})

test_that("help inputs are excluded from URL bookmarks", {
  expect_true(all(c("mainNav", "help_goto", "help_search", "help_scenario_load") %in% bookmarksToExclude))
})

test_that("formatting helpers are tidy", {
  expect_equal(helpFormatNumber(c(1234.5678, 0.000123456, NA, 0, Inf)),
               c("1,230", "0.000123", "—", "0", "—"))
  expect_equal(helpReferenceShort("Minto CF et al., Anesthesiology 1997;86:10-23. https://pubmed.ncbi.nlm.nih.gov/9009935/"),
               "Minto CF et al., Anesthesiology 1997;86:10-23.")
  expect_equal(helpDrugTitle(c("nitrousOxide", "propofol")), c("Nitrous Oxide", "Propofol"))
  expect_match(helpPageLink("faq"), '<a href="#" data-help-page="faq">Frequently asked questions</a>', fixed = TRUE)
})
