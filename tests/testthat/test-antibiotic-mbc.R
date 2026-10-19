# The antibiotics' shaded band: free drug at the expected MBC.
#
# antibioticMbcTable() (R/antibioticMbc.R) holds, per antibiotic and pathogen,
# the MIC and MBC, and antibioticMbcBand() applies the band rule.  These tests
# keep the shipped band, the model functions and the help in step with it.
#
# (Claude Code, 2026-10-19, at the request of Steven L. Shafer; run on R 4.3.3.)

test_that("every antibiotic has an MBC band, and only the antibiotics", {
  x <- antibioticMbcTable()
  expect_setequal(unique(x$Drug), antibioticMicTable()$Drug)
  expect_true(all(x$Role %in% c("principal", "core", "excluded")))
  for (d in unique(x$Drug)) {
    expect_equal(sum(x$Drug == d & x$Role == "principal"), 1, label = paste(d, "principal pathogens"))
  }
  expect_null(antibioticMbcBand("propofol"))
})

test_that("each row is internally consistent and sourced", {
  x <- antibioticMbcTable()
  for (i in seq_len(nrow(x))) {
    r <- x[i, ]
    lab <- paste(r$Drug, r$Pathogen)
    expect_true(nzchar(r$Source), label = paste(lab, "source"))
    expect_false(grepl("PROVISIONAL", r$Source), label = lab)
    # An MBC is never below the MIC it belongs to
    if (!is.na(r$MBC50) && !is.na(r$MIC50)) expect_gte(r$MBC50, r$MIC50, label = paste(lab, "MBC50"))
    if (!is.na(r$MBC90) && !is.na(r$MIC90)) expect_gte(r$MBC90, r$MIC90, label = paste(lab, "MBC90"))
    if (!is.na(r$MBC50) && !is.na(r$MBC90)) expect_lte(r$MBC50, r$MBC90, label = lab)
    if (r$Role == "excluded") {
      # An excluded pathogen says why, and carries no MBC that could move the band
      expect_true(nzchar(r$Note), label = paste(lab, "reason"))
      expect_true(is.na(r$MBC50) && is.na(r$MBC90), label = lab)
    } else {
      expect_true(nzchar(r$Basis), label = paste(lab, "basis"))
    }
  }
  # The principal pathogen carries both edges' MBCs
  p <- x[x$Role == "principal", ]
  expect_false(anyNA(p$MBC50))
  expect_false(anyNA(p$MBC90))
})

test_that("the band follows the rule and the library ships it", {
  dd <- getDrugDefaultsGlobal()
  x <- antibioticMbcTable()
  for (d in antibioticMicTable()$Drug) {
    b <- antibioticMbcBand(d)
    rows <- x[x$Drug == d, ]
    p <- rows[rows$Role == "principal", ]
    killed <- rows[rows$Role != "excluded", ]
    expect_equal(unname(b$free), c(p$MBC50, p$MBC90, max(killed$MBC90, na.rm = TRUE)), label = d)
    expect_true(b$free[["lower"]] <= b$free[["typical"]] && b$free[["typical"]] <= b$free[["upper"]],
                label = paste(d, "order"))
    expect_equal(unname(b$plotted), signif(antibioticPlottedConcentration(d, unname(b$free)), 2), label = d)
    # The CSV carries the band on the plotted scale
    row <- dd[dd$Drug == d, ]
    expect_equal(c(row$Lower, row$Typical, row$Upper), unname(b$plotted), label = paste(d, "CSV band"))
    # ... and so does the model function, through getDrugPK() and on its own
    PK <- getDrugPK(d, 70, 170, 40, "male", row)
    expect_equal(c(PK$lowerTypical, PK$typical, PK$upperTypical), unname(b$plotted), label = d)
    X <- get(d)(70, 170, 40, "male")
    expect_equal(c(X$lowerTypical, X$typical, X$upperTypical), unname(b$plotted), label = paste(d, "model"))
  }
})

test_that("the shipped bands are the ones researched on 2026-10-19", {
  # A pin, so a change to the table is a deliberate change to the bands.
  # Free mg/L, then the plotted band.
  pins <- list(
    cefazolin     = list(free = c(0.5, 2, 2),     plotted = c(0.5, 2, 2)),
    cefalexin     = list(free = c(2, 4, 4),       plotted = c(2.4, 4.7, 4.7)),
    ceftriaxone   = list(free = c(0.5, 2, 2),     plotted = c(7.8, 30, 30)),
    clindamycin   = list(free = c(0.25, 0.25, 1.6), plotted = c(3.1, 3.1, 9.9)),
    gentamicin    = list(free = c(0.5, 2, 8),     plotted = c(0.5, 2, 8)),
    metronidazole = list(free = c(1, 4, 4),       plotted = c(1, 4.2, 4.2)),
    vancomycin    = list(free = c(2, 8, 8),       plotted = c(2.9, 11, 11))
  )
  for (d in names(pins)) {
    b <- antibioticMbcBand(d)
    expect_equal(unname(b$free), pins[[d]]$free, label = paste(d, "free"))
    expect_equal(unname(b$plotted), pins[[d]]$plotted, label = paste(d, "plotted"))
  }
})

test_that("free concentrations reach the curve through each drug's binding", {
  # Unbound curve: unchanged
  expect_equal(antibioticPlottedConcentration("cefazolin", c(0.5, 2)), c(0.5, 2))
  # Linear binding: divided by the free fraction
  expect_equal(antibioticPlottedConcentration("vancomycin", 0.7), 1)
  expect_equal(antibioticPlottedConcentration("gentamicin", 3), 3)
  # Saturable binding: the one-site fit, whose free fraction falls as the
  # concentration falls
  expect_equal(antibioticPlottedConcentration("ceftriaxone", 1), 1 + 354 / 24.7)
  lo <- antibioticPlottedConcentration("clindamycin", 0.25)
  hi <- antibioticPlottedConcentration("clindamycin", 1.6)
  expect_lt(0.25 / lo, 1.6 / hi)
  # The same map puts the MIC on the curve where the shipped threshold is
  tab <- antibioticMicTable()
  for (i in seq_len(nrow(tab))) {
    expect_equal(antibioticPlottedConcentration(tab$Drug[i], tab$MIC[i]), tab$Threshold[i],
                 tolerance = 0.03, label = paste(tab$Drug[i], "threshold"))
  }
  expect_error(antibioticPlottedConcentration("propofol", 1), "not an antibiotic")
})

test_that("clindamycin's band leaves out the staphylococci it does not kill", {
  x <- antibioticMbcTable()
  clinda <- x[x$Drug == "clindamycin", ]
  staph <- clinda[grepl("Staphylococcus aureus", clinda$Pathogen), ]
  expect_equal(staph$Role, "excluded")
  expect_match(staph$Source, "bacteriostatic", fixed = TRUE)
  # Elsewhere the principal pathogen is the MIC line's organism
  for (d in setdiff(antibioticMicTable()$Drug, "clindamycin")) {
    p <- x$Pathogen[x$Drug == d & x$Role == "principal"]
    organism <- antibioticMic(d)$Organism
    first <- function(s) sub("^([A-Za-z]+).*", "\\1", s)
    expect_equal(first(p), first(organism), label = d)
  }
})

test_that("each antibiotic's help page explains its band", {
  for (d in antibioticMicTable()$Drug) {
    html <- helpDrugPageHTML(d)
    expect_true(grepl("Shaded band: free drug at the MBC", html, fixed = TRUE), label = d)
    expect_true(grepl("free drug at the expected MBC", html, fixed = TRUE), label = d)
    expect_true(grepl(antibioticMbcBand(d)$principal, html, fixed = TRUE), label = d)
  }
  expect_false(grepl("Shaded band: free drug at the MBC", helpDrugPageHTML("propofol"), fixed = TRUE))
  page <- helpPageHTML("models/antibiotics")
  for (d in antibioticMicTable()$Drug) expect_true(grepl(sprintf("drugs/%s", d), page, fixed = TRUE), label = d)
})

test_that("clindamycin's band lies within the label's steady-state levels", {
  # Cleocin Phosphate label (revised 12/2021), Table 1: 600 mg every 8 hours
  # in a 70 kg adult gives Cmax,ss 12.0 and Cmin,ss 3.1 mcg/mL total.  The band
  # is free drug at the MBC of the organisms clindamycin kills, so it should
  # fall between the trough and the peak the label's usual dose produces.
  b <- antibioticMbcBand("clindamycin")$plotted
  expect_gte(b[["lower"]], 3.1)
  expect_lte(b[["upper"]], 12.0)
})
