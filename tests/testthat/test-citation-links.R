# The links in the drug citations (citationItemHTML(), R/utils-shiny-ui.R):
# every PubMed, DOI or PAGE URL in a reference is a link, wherever it stands,
# without the punctuation around it.  (Claude Code, 2026-10-14, at the request
# of Steven L. Shafer, after a production audit found a DOI link returning 404
# from a trailing parenthesis and fourteen references with no link because
# text followed their URL.)

item <- function(reference) as.character(citationItemHTML("x", reference, NULL))
# The site links; the investigators' pop-up links are href="#"
hrefs <- function(html) regmatches(html, gregexpr('href="https[^"]*"', html))[[1]]

test_that("a URL at the end of the citation is linked, as before", {
  html <- item("Eleveld DJ et al., Br J Anaesth 2018;120:942-959. https://doi.org/10.1016/j.bja.2018.01.018")
  expect_equal(hrefs(html), 'href="https://doi.org/10.1016/j.bja.2018.01.018"')
  expect_match(html, ">DOI</a>", fixed = TRUE)
  expect_match(html, 'target="_blank"', fixed = TRUE)
  expect_match(html, 'rel="noreferrer"', fixed = TRUE)
  expect_false(grepl("https://doi.org/10.1016/j.bja.2018.01.018<", html, fixed = TRUE))
})

test_that("a URL followed by text is linked too, and the text kept", {
  html <- item("Smith A. J Pain 2001;1:1-9. https://pubmed.ncbi.nlm.nih.gov/12345/ (the oral model; the IV model is Jones B, https://doi.org/10.1000/xyz)")
  expect_equal(hrefs(html), c('href="https://pubmed.ncbi.nlm.nih.gov/12345/"', 'href="https://doi.org/10.1000/xyz"'))
  expect_match(html, ">PubMed</a> (the oral model; the IV model is Jones B, <a", fixed = TRUE)
  expect_match(html, ">DOI</a>)\n", fixed = TRUE)
})

test_that("sentence punctuation and an unbalanced parenthesis are not part of the link", {
  expect_equal(citationTrimURL("https://doi.org/10.1016/j.clinthera.2010.02.011)"), "https://doi.org/10.1016/j.clinthera.2010.02.011")
  expect_equal(citationTrimURL("https://doi.org/10.1000/abc."), "https://doi.org/10.1000/abc")
  expect_equal(citationTrimURL("https://doi.org/10.1000/abc).;"), "https://doi.org/10.1000/abc")
  expect_equal(citationTrimURL("https://doi.org/10.1000/abc]."), "https://doi.org/10.1000/abc")
  # A DOI's own parentheses are balanced and are kept
  expect_equal(citationTrimURL("https://doi.org/10.1016/S0140-6736(99)01234-5"), "https://doi.org/10.1016/S0140-6736(99)01234-5")
  expect_equal(citationTrimURL("https://doi.org/10.1016/S0140-6736(99)01234-5)"), "https://doi.org/10.1016/S0140-6736(99)01234-5")
  html <- item("Boellner SW, Clin Ther 2010;32:252-264 (https://doi.org/10.1016/j.clinthera.2010.02.011).")
  expect_equal(hrefs(html), 'href="https://doi.org/10.1016/j.clinthera.2010.02.011"')
  expect_match(html, ">DOI</a>).\n", fixed = TRUE)
})

test_that("a URL on another site, or over http, is plain text", {
  html <- item("Author A. Journal 2000. https://example.org/paper http://doi.org/10.1/x")
  expect_length(hrefs(html), 0)
  expect_match(html, "https://example.org/paper http://doi.org/10.1/x", fixed = TRUE)
})

test_that("the text is escaped and the investigators still linked around a mid-text URL", {
  html <- item("Shafer SL & Varvel JR. https://pubmed.ncbi.nlm.nih.gov/2240675/ <reanalysis>")
  expect_match(html, "Shafer SL</a> &amp; <a", fixed = TRUE)
  expect_match(html, 'data-investigator="varvel-j">Varvel JR</a>', fixed = TRUE)
  expect_match(html, "&lt;reanalysis&gt;", fixed = TRUE)
  expect_equal(hrefs(html), 'href="https://pubmed.ncbi.nlm.nih.gov/2240675/"')
})

test_that("every recognised URL in every drug's citation is linked, and none ends in punctuation", {
  patients <- helpReferencePatients()
  dd <- getDrugDefaultsGlobal()
  unlinked <- character(0); dirty <- character(0)
  for (drug in dd$Drug[!isGasDrug(dd$Drug)]) {
    for (i in seq_len(nrow(patients))) {
      out <- helpDrugModelOutput(drug, patients[i, ])
      ref <- out$reference
      if (!is.character(ref) || length(ref) != 1 || !nzchar(ref)) next
      links <- citationLinks(ref)
      mentions <- lengths(regmatches(ref, gregexpr("https://(pubmed\\.ncbi\\.nlm\\.nih\\.gov|doi\\.org|www\\.page-meeting\\.org)/", ref)))
      html <- as.character(citationItemHTML(drug, ref, NULL))
      if (length(hrefs(html)) != mentions) unlinked <- c(unlinked, drug)
      if (!is.null(links) && any(grepl("[.,;:)\\]]$", links$link))) dirty <- c(dirty, paste(drug, links$link))
    }
  }
  expect_equal(unique(unlinked), character(0))
  expect_equal(unique(dirty), character(0))
})
