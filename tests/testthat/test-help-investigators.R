# The investigators: the registry read from inst/help/investigators.md, the
# links to their pop-ups in the help and the References panel, and the
# pop-ups themselves.  See R/help-investigators.R.

registry <- helpInvestigatorRegistry()
linkTo <- function(key) sprintf('data-investigator="%s"', key)
link <- function(x, context = "prose", exclude = NULL) {
  helpLinkInvestigators(x, context, exclude = exclude, registry = registry)
}

test_that("names split into given names and surname", {
  expect_equal(helpSplitName("Douglas J. Eleveld"),
               list(given = c("Douglas", "J."), surname = "Eleveld", initial = "D"))
  expect_equal(helpSplitName("Edmond I. Eger II")$surname, "Eger")
  expect_equal(helpSplitName("Alfred L. van Steveninck")$surname, "van Steveninck")
  expect_equal(helpSplitName("M. J. J. da Silva Neto")$surname, "da Silva Neto")
  expect_equal(helpSplitName("Tom De Smet")$surname, "De Smet")
  expect_equal(helpSplitName("Hesham S. Al-Sallami")$surname, "Al-Sallami")
  expect_equal(helpSplitName("Liu"), list(given = character(0), surname = "Liu", initial = NA_character_))
  expect_equal(helpInvestigatorKey("Eleveld", "D"), "eleveld-d")
  expect_equal(helpInvestigatorKey("Liu", NA), "liu")
  expect_equal(helpInvestigatorKey("Pöyhiä", "R"), "poyhia-r")
  expect_equal(helpInvestigatorKey("Schüttler", "J"), "schuttler-j")
  expect_equal(helpFoldAccents("Björkman Noël Müller Cortínez Ström Ørsted Åke"),
               "Bjorkman Noel Muller Cortinez Strom Orsted Ake")
})

test_that("the registry reads every bold name on the investigators page", {
  expect_gt(nrow(registry), 100)
  expect_false(any(duplicated(registry$key)))
  expect_true(all(nzchar(registry$key)))
  expect_true(all(grepl("^[a-z0-9-]+$", registry$key)))
  bold <- helpBoldNames(helpReadMarkdown("investigators"))
  for (b in bold) {
    expect_true(helpLooksLikeName(b), info = paste("bold text that is not a name:", b))
  }
  # Two spellings of one person are one entry, under the fuller name
  expect_equal(sum(registry$surname == "Bouillon"), 1)
  expect_equal(registry$name[registry$surname == "Bouillon"], "Thomas W. Bouillon")
  expect_equal(sum(registry$surname == "Anderson"), 1)
  # Shared surnames are told apart by the first initial
  kims <- registry[registry$surname == "Kim", ]
  expect_gte(nrow(kims), 2)
  expect_true(all(kims$ambiguous))
  expect_true(all(c("kim-t", "kim-h") %in% kims$key))
  expect_setequal(registry$key[registry$surname == "Holford"], c("holford-n", "holford-s"))
  expect_false(registry$ambiguous[registry$key == "eleveld-d"])
  # Shafer is linked only in citations
  expect_true(registry$citationOnly[registry$key == "shafer-s"])
  expect_equal(sum(registry$citationOnly), 1)
})

test_that("every investigator has at least one entry on the page", {
  blocks <- helpInvestigatorBlocks()
  expect_true(all(c("paragraph", "item", "row") %in% blocks$kind))
  expect_true("propofol" %in% blocks$drug)
  for (i in seq_len(nrow(registry))) {
    hit <- any(vapply(blocks$plain, function(p) helpMentions(p, registry[i, ]), logical(1)))
    expect_true(hit, info = paste("no entry for", registry$name[i]))
  }
})

test_that("a surname alone links when it belongs to one person", {
  html <- link("Eleveld and colleagues; Eleveld's model; Schnider's 1998 model.")
  expect_equal(lengths(regmatches(html, gregexpr(linkTo("eleveld-d"), html))), 2)
  expect_match(html, '<a href="#" class="investigator-link" data-investigator="eleveld-d">Eleveld</a>\'s model', fixed = TRUE)
  expect_match(html, linkTo("schnider-t"), fixed = TRUE)
})

test_that("given names and initials are part of the link", {
  expect_match(link("Douglas J. Eleveld's model"), '>Douglas J. Eleveld</a>', fixed = TRUE)
  expect_match(link("Eleveld DJ et al., Br J Anaesth 2018"), '>Eleveld DJ</a> et al.', fixed = TRUE)
  expect_match(link("James H. Philip at Brigham"), '>James H. Philip</a> at', fixed = TRUE)
  expect_match(link("by Jim Philip"), 'data-investigator="philip-j">Jim Philip</a>', fixed = TRUE)
  expect_match(link("Philip JH. Gas Man: an example"), 'data-investigator="philip-j">Philip JH</a>', fixed = TRUE)
  expect_match(link("Alfred L. van Steveninck"), '>Alfred L. van Steveninck</a>', fixed = TRUE)
  expect_match(link("Van Steveninck AL, Cohen AF"), '>Van Steveninck AL</a>', fixed = TRUE)
  expect_match(link("Tom De Smet and Michel M. R. F. Struys"), '>Tom De Smet</a> and <a[^>]*>Michel M. R. F. Struys</a>')
  expect_match(link("Edmond I. Eger II"), '>Edmond I. Eger II</a>', fixed = TRUE)
  expect_match(link("Jerry Reves saw"), '>Jerry Reves</a>', fixed = TRUE)
  expect_match(link("Pöyhiä R, Olkkola KT"), '>Pöyhiä R</a>', fixed = TRUE)
})

test_that("a shared surname links only with a given name or initials", {
  expect_match(link("Tae Kyun Kim"), 'data-investigator="kim-t">Tae Kyun Kim</a>', fixed = TRUE)
  expect_match(link("Kim TK et al., Anesthesiology 2017"), 'data-investigator="kim-t">Kim TK</a>', fixed = TRUE)
  expect_match(link("Hyun Chul Kim"), 'data-investigator="kim-h">Hyun Chul Kim</a>', fixed = TRUE)
  expect_match(link("Holford NHG"), 'data-investigator="holford-n">Holford NHG</a>', fixed = TRUE)
  expect_match(link("Sophie Holford"), 'data-investigator="holford-s">Sophie Holford</a>', fixed = TRUE)
  plain <- link("the Kim remifentanil model; Anderson and Holford (2008); Dr. Kim")
  expect_false(grepl('data-investigator="kim', plain))
  expect_false(grepl('data-investigator="holford', plain))
  expect_match(plain, linkTo("anderson-b"), fixed = TRUE)
})

test_that("a given name that is also a surname goes to the right person", {
  html <- link("James R. Jacobs and James Jacobs; the James equation; James WPT.")
  expect_equal(lengths(regmatches(html, gregexpr(linkTo("jacobs-j"), html))), 2)
  expect_equal(lengths(regmatches(html, gregexpr(linkTo("james-w"), html))), 2)
  expect_match(html, '>James R. Jacobs</a>', fixed = TRUE)
  expect_match(html, 'data-investigator="james-w">James</a> equation', fixed = TRUE)
  expect_match(html, '>James WPT</a>', fixed = TRUE)
})

test_that("a sentence's first word or a title is not taken for a given name", {
  expect_match(link("The Schnider model"), 'The <a href="#" class="investigator-link" data-investigator="schnider-t">Schnider</a> model', fixed = TRUE)
  expect_match(link("Dr Eleveld"), 'Dr <a[^>]*>Eleveld</a>')
})

test_that("Shafer is linked in citations and nowhere else", {
  prose <- link("Shafer and Varvel (1991); Steven L. Shafer; Dr Shafer.")
  expect_false(grepl("shafer", prose))
  expect_match(prose, linkTo("varvel-j"), fixed = TRUE)
  cite <- link("Shafer SL, Varvel JR. Pharmacokinetics", "citation")
  expect_match(cite, 'data-investigator="shafer-s">Shafer SL</a>', fixed = TRUE)
  expect_match(cite, 'data-investigator="varvel-j">Varvel JR</a>', fixed = TRUE)
})

test_that("Gas Man links to its website", {
  html <- link("follows Gas Man®, and Gas Man's output; GasMan too")
  expect_equal(lengths(regmatches(html, gregexpr("gasmanweb.com", html))), 3)
  expect_match(html, '<a href="https://www.gasmanweb.com/" class="help-term-link" target="_blank" rel="noreferrer">Gas Man®</a>,', fixed = TRUE)
  expect_match(html, '>Gas Man</a>\'s output', fixed = TRUE)
})

test_that("text already in a link, in code or in a tag is left alone", {
  html <- link('<a href="#">Eleveld DJ</a> <code>Eleveld</code> <pre>Kim TK</pre> <p title="Eleveld">Eleveld</p>')
  expect_equal(lengths(regmatches(html, gregexpr(linkTo("eleveld-d"), html))), 1)
  expect_match(html, '<a href="#">Eleveld DJ</a>', fixed = TRUE)
  expect_match(html, '<code>Eleveld</code>', fixed = TRUE)
  expect_match(html, '<pre>Kim TK</pre>', fixed = TRUE)
  expect_match(html, '<p title="Eleveld"><a href="#" class="investigator-link" data-investigator="eleveld-d">Eleveld</a></p>', fixed = TRUE)
  # So that linking twice is linking once
  once <- link("<p>Douglas J. Eleveld, Eleveld DJ, Kim TK, Gas Man, Shafer SL</p>", "citation")
  expect_identical(link(once, "citation"), once)
  expect_false(grepl("[\x01-\x06]", once))
  # An excluded person is not linked
  expect_false(grepl("eleveld", link("Eleveld DJ", exclude = "eleveld-d")))
  expect_equal(link(""), "")
  expect_null(link(NULL))
})

test_that("every help page links its investigators, and the drug pages their citations", {
  page <- as.character(helpPageUI("investigators"))
  expect_match(page, linkTo("eleveld-d"), fixed = TRUE)
  expect_match(page, linkTo("kim-t"), fixed = TRUE)
  expect_match(page, linkTo("philip-j"), fixed = TRUE)
  expect_false(grepl("shafer-s", page))
  expect_match(page, "gasmanweb.com", fixed = TRUE)

  refs <- as.character(helpPageUI("references"))
  expect_match(refs, 'data-investigator="shafer-s">Shafer SL</a>', fixed = TRUE)
  expect_match(refs, 'data-investigator="eleveld-d">Eleveld DJ</a>', fixed = TRUE)
  expect_match(refs, 'data-investigator="philip-j">Philip JH</a>', fixed = TRUE)
  expect_false(grepl("[\x01-\x06]", refs))

  propofol <- as.character(helpPageUI("drugs/propofol"))
  expect_match(propofol, 'data-investigator="eleveld-d">Eleveld DJ</a>', fixed = TRUE)
  # The TOC's heading text is plain
  expect_false(grepl("<a[^>]*><a", propofol))

  gas <- as.character(helpPageUI("models/gas-engine"))
  expect_match(gas, linkTo("philip-j"), fixed = TRUE)
  expect_match(gas, 'href="https://www.gasmanweb.com/"', fixed = TRUE)
  # "Gas Man" inside an existing help link is not linked again
  expect_false(grepl('<a[^>]*data-help-page[^>]*>[^<]*<a', gas))
})

test_that("the References panel's citations link their investigators, Shafer included", {
  item <- as.character(citationItemHTML(
    "fentanyl",
    "Shafer SL, Varvel JR, Aziz N, Scott JC. Anesthesiology 1990;73:1091-1102. https://pubmed.ncbi.nlm.nih.gov/2240675/",
    "#ff0000"))
  expect_match(item, 'data-investigator="shafer-s">Shafer SL</a>', fixed = TRUE)
  expect_match(item, 'data-investigator="varvel-j">Varvel JR</a>', fixed = TRUE)
  expect_match(item, 'data-investigator="scott-j">Scott JC</a>', fixed = TRUE)
  expect_match(item, 'href="https://pubmed.ncbi.nlm.nih.gov/2240675/"', fixed = TRUE)
  expect_match(as.character(citationItemHTML("propofol", "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959.", NULL)),
               'data-investigator="eleveld-d">Eleveld DJ</a>', fixed = TRUE)
  # A reference with characters that HTML escapes is still escaped
  item <- as.character(citationItemHTML("x", "Smith A & <Jones B>", NULL))
  expect_match(item, "Smith A &amp; &lt;Jones B&gt;", fixed = TRUE)
})

test_that("the pop-up collects what the page says about the person", {
  data <- helpInvestigatorsData()
  html <- helpInvestigatorHTML(helpInvestigatorByKey("eleveld-d", data$registry), data)
  expect_match(html, "<h6>The pharmacokinetic models</h6>", fixed = TRUE)
  expect_match(html, "<strong>propofol</strong>", fixed = TRUE)
  expect_match(html, "<strong>remifentanil</strong>", fixed = TRUE)
  # The person is not linked to their own pop-up; the others named are
  expect_false(grepl("eleveld-d", html))
  expect_match(html, linkTo("schnider-t"), fixed = TRUE)
  # Eleveld's citations come from the drug models, not references.md
  expect_false(grepl("In the bibliography", html, fixed = TRUE))

  minto <- helpInvestigatorHTML(helpInvestigatorByKey("minto-c", data$registry), data)
  expect_match(minto, "<h6>In the bibliography</h6>", fixed = TRUE)
  expect_match(minto, "Minto CF, <a[^>]*>Schnider TW</a>")
  expect_false(grepl("minto-c", minto))

  shafer <- helpInvestigatorHTML(helpInvestigatorByKey("shafer-s", data$registry), data)
  expect_match(shafer, "<h6>The program</h6>", fixed = TRUE)
  expect_match(shafer, "STANPUMP", fixed = TRUE)
  expect_match(shafer, "<h6>In the bibliography</h6>", fixed = TRUE)
  expect_match(shafer, "Shafer SL, <a[^>]*>Gregg KM</a>. Algorithms to rapidly achieve")
  expect_false(grepl("shafer-s", shafer))

  philip <- helpInvestigatorHTML(helpInvestigatorByKey("philip-j", data$registry), data)
  expect_match(philip, "<h6>The inhaled anesthetics</h6>", fixed = TRUE)
  expect_match(philip, "gasmanweb.com", fixed = TRUE)

  modal <- helpInvestigatorModal("eleveld-d", data)
  expect_s3_class(modal, "shiny.tag")
  rendered <- as.character(modal)
  expect_match(rendered, "Douglas J. Eleveld", fixed = TRUE)
  expect_match(rendered, 'data-help-page="investigators"', fixed = TRUE)
  expect_null(helpInvestigatorModal("nobody", data))
  expect_null(helpInvestigatorModal(NULL, data))
  expect_null(helpInvestigatorModal(c("eleveld-d", "kim-t"), data))
})
