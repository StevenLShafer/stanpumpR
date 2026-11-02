# Single Paper Review against the live Anthropic API: opt-in only.
#
# Skipped unless an API key is set (STANPUMPR_ANTHROPIC_API_KEY or
# ANTHROPIC_API_KEY) AND STANPUMPR_LIVE_API=1, so devtools::test() stays
# network-free.  It sends no real manuscript: it draws a one-page PDF of the
# fictional exampleol paper (the same model as
# inst/paper-review/example-extraction.json, with the same misprinted second
# half-life) and checks that Claude's extraction of it reaches the review
# intact.  A run costs real money (tokens on the key's account); the token
# counts are left in getOption("paperReviewLiveUsage") for whoever ran it.
#
#   Sys.setenv(STANPUMPR_LIVE_API = "1"); devtools::test(filter = "paper-review-live")

skipUnlessLive <- function() {
  testthat::skip_if_not(identical(Sys.getenv("STANPUMPR_LIVE_API"), "1"),
                        "live API tests are opt-in: set STANPUMPR_LIVE_API=1")
  testthat::skip_if(is.null(paperReviewApiKey(NULL, list())), "no Anthropic API key")
  testthat::skip_if_not_installed("httr2")
}

# A one-page PDF of the fictional paper, as raw bytes; the file is deleted.
exampleolPdf <- function() {
  lines <- c(
    "Pharmacokinetics of exampleol in adult volunteers",
    "A. Example. Fixture Journal 2026 (a fictional paper made for software testing)",
    "",
    "Methods (p. 1). Twenty-four healthy adults (12 women, 12 men; age 25-65 years, mean 42;",
    "weight 55-110 kg, mean 75; height 158-192 cm, mean 174) received exampleol",
    "hydrochloride 1 mg/kg as an intravenous bolus. Arterial plasma concentrations (ng/mL)",
    "were fitted with a three-compartment model with an effect site.",
    "",
    "Table 2. Population estimates (RSE %)",
    "   V1  8 L (12)     V2  20 L (15)     V3  120 L (20)",
    "   CL  1.2 L/min (6)   Q2  2.0 L/min (18)   Q3  0.6 L/min (22)",
    "   Covariates: CL = 1.2 x (WT/70)^0.75;  V1 = 8 x (1 - 0.005 (AGE - 40))",
    "   Time to peak effect after a bolus: 3.0 min (RSE 10%)",
    "",
    "Table 3. Derived quantities (typical values)",
    "   Half-lives: 1.30 min, 20.8 min, 3.56 h",
    "   Vss 148 L;  ke0 0.273 /min",
    "   AUC(0-inf) after 1 mg/kg in a 70-kg subject: 972 ng*h/mL",
    "",
    "Figure 2 (described). Mean concentrations after the bolus, ng/mL:",
    "   2 min 3810;  10 min 754;  30 min 335;  60 min 127;  120 min 52.5;  360 min 23.5")
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  grDevices::pdf(file, width = 8.5, height = 11)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  y <- seq(0.96, by = -0.035, length.out = length(lines))
  graphics::text(0.04, y, lines, adj = 0, cex = 0.8, family = "sans")
  grDevices::dev.off()
  readBin(file, "raw", file.info(file)$size)
}

test_that("a live extraction of the fictional paper reaches the review intact", {
  skipUnlessLive()
  pdf <- exampleolPdf()
  expect_true(paperReviewIsPdf(pdf))
  result <- paperReviewExtract(pdf, key = paperReviewApiKey(NULL, list()))
  options(paperReviewLiveUsage = result$usage)
  x <- result$extraction
  expect_setequal(names(x), unlist(paperReviewSchema()$required))
  expect_equal(x$structure$compartments, 3)
  ref <- paperReviewReferencePatient(x)
  expect_equal(paperReviewParameterValue(x, "CL", ref$weight)$value, 1.2)
  expect_equal(paperReviewParameterValue(x, "V1", ref$weight)$value, 8)
  expect_equal(paperReviewParameterValue(x, "V3", ref$weight)$value, 120)

  review <- paperReviewRun(x)
  expect_equal(review$identifiability$verdict, "printed")
  t <- review$consistency
  expect_false(is.null(t))
  # The misprinted second half-life is caught, and the true ones match.
  expect_true(any(t$quantity == "t_half_2" & t$status == "disagrees"))
  expect_true(any(t$quantity == "Vss" & t$status == "matches"))
  # Every finding still cites a place in the paper or is a summary line.
  located <- vapply(review$findings, function(f) length(f$locations) > 0, logical(1))
  expect_gt(mean(located), 0.5)
})
