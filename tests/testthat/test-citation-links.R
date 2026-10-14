# The References panel's citation links (citationItemHTML() in
# R/utils-shiny-ui.R): which URLs become links, where, and with what text.
# Drafted by Claude Code, 2026-10-14, at the request of Steven L. Shafer,
# after Dean Attali's review of the panel (PR 335).

linkHTML <- function(url, label) {
  sprintf('<a href="%s" target="_blank" rel="noreferrer">%s</a>', url, label)
}
item <- function(reference) as.character(citationItemHTML("x", reference, NULL))

test_that("a PubMed or DOI URL at the end of a citation becomes a labelled link", {
  html <- item("Smith A. J Clin Pharm 2001;1:1-2. https://pubmed.ncbi.nlm.nih.gov/123/")
  expect_match(html, paste0("1-2. ", linkHTML("https://pubmed.ncbi.nlm.nih.gov/123/", "PubMed")), fixed = TRUE)
  html <- item("Smith A. J Clin Pharm 2001;1:1-2. https://doi.org/10.1000/abc")
  expect_match(html, paste0("1-2. ", linkHTML("https://doi.org/10.1000/abc", "DOI")), fixed = TRUE)
})

test_that("a URL in the middle of a citation is linked in place, and each of several is", {
  html <- item(paste0(
    "Smith A. J 2001. https://pubmed.ncbi.nlm.nih.gov/111/ (intravenous); ",
    "Jones B. J 2002. https://doi.org/10.1000/xyz (oral)"))
  expect_match(html, paste0(linkHTML("https://pubmed.ncbi.nlm.nih.gov/111/", "PubMed"), " (intravenous); "), fixed = TRUE)
  expect_match(html, paste0(linkHTML("https://doi.org/10.1000/xyz", "DOI"), " (oral)"), fixed = TRUE)
  expect_false(grepl("pubmed.ncbi.nlm.nih.gov/111/ (intravenous)", html, fixed = TRUE))
})

test_that("punctuation after a URL is not part of the link", {
  html <- item("Smith A (J 2001, https://doi.org/10.1016/j.clinthera.2010.02.011).")
  expect_match(html, paste0(linkHTML("https://doi.org/10.1016/j.clinthera.2010.02.011", "DOI"), ")."), fixed = TRUE)
  html <- item("Smith A. https://pubmed.ncbi.nlm.nih.gov/111/, and Jones B.")
  expect_match(html, paste0(linkHTML("https://pubmed.ncbi.nlm.nih.gov/111/", "PubMed"), ", and"), fixed = TRUE)
})

test_that("URLs that are not on a listed site, or not https, stay as text", {
  html <- item("Smith A. https://example.org/paper and http://pubmed.ncbi.nlm.nih.gov/111/")
  expect_false(grepl("<a ", html, fixed = TRUE))
  expect_match(html, "https://example.org/paper and http://pubmed.ncbi.nlm.nih.gov/111/", fixed = TRUE)
})

test_that("citationText() reads the citation with each listed URL as its site's name", {
  expect_equal(citationText("Smith A. J 2001. https://pubmed.ncbi.nlm.nih.gov/111/ (intravenous); Jones B. https://doi.org/10.1/x)."),
               "Smith A. J 2001. PubMed (intravenous); Jones B. DOI).")
  expect_equal(citationText("Smith A. https://example.org/paper"), "Smith A. https://example.org/paper")
})

test_that("a missing reference reads Not Available, and the text is HTML-escaped", {
  expect_match(item(NULL), "Not Available", fixed = TRUE)
  expect_match(item(""), "Not Available", fixed = TRUE)
  expect_match(item("Smith A & <Jones B>"), "Smith A &amp; &lt;Jones B&gt;", fixed = TRUE)
})

test_that("every drug's reference links each of its PubMed and DOI URLs", {
  # The drug models' own citations, as the panel shows them
  drugDefaults <- getDrugDefaultsGlobal()
  for (drug in drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]) {
    reference <- getDrugPK(drug, 70, 170, 40, "male", drugDefaults[drugDefaults$Drug == drug, ])$reference
    if (is.null(reference) || identical(reference, "Not Available")) next
    html <- item(reference)
    urls <- regmatches(reference, gregexpr("https://(pubmed\\.ncbi\\.nlm\\.nih\\.gov|doi\\.org)/[^ ]*", reference))[[1]]
    for (url in urls) {
      expect_match(html, paste0('href="', sub("[.,;:)]+$", "", url), '"'), fixed = TRUE,
                   label = paste(drug, url))
    }
  }
})
