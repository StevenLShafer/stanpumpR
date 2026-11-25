# Capecitabine (R/drugs_capecitabine.R): Gieschke 2003.  Drafted by Claude
# Code, 2026-11-06.

noEvents <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60

test_that("2250 mg by mouth reproduces Table 6 on the molar reading", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "capecitabine", Time = 0, Dose = 2250, Units = "mg PO"),
    noEvents, 70, 170, 61, "male", 48 * 60, FALSE, adjustToFFM = FALSE)
  d <- w$capecitabine$wide; f <- w$capecitabine5FU$wide
  expect_equal(auc(d), 20.34, tolerance = 0.01)            # Table 6 median 20.13
  expect_equal(max(d$Plasma), 7.10, tolerance = 0.01)
  expect_equal(d$Time[which.max(d$Plasma)] / 60, 1.04, tolerance = 0.03)
  expect_equal(auc(f), 0.684, tolerance = 0.01)            # Table 6 median 0.707
  expect_equal(max(f$Plasma), 0.239, tolerance = 0.02)
})

test_that("the molar factors and Table 4's rates", {
  set <- getDrugPK("capecitabine", 70, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$bioavailability_PO, 246.21 / 359.35)
  expect_equal_rounded(set$ka_PO * 60, 1.09)
  expect_equal_rounded(set$v1, 90.6)
  expect_equal_rounded(set$cl1 * 60, 75.8)
  expect_identical(set$metabolite$name, "capecitabine5FU")
  m <- getDrugPK("capecitabine5FU", 70, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) * m$v1 / (m$cl1 * 60) * 3600), 37)   # seconds
})

test_that("anchored at Gieschke's cohort: its typical patients get Table 4, FFM scales the rest", {
  # (a) Table 1 (PDF p3): three studies, heights solved from each study's BSA
  # and weight by Du Bois.  No parameter has a sex term, so the cohort's six
  # typical patients, pooled geometrically by their numbers, receive Table 4
  dubois <- function(bsa, w) round((bsa / (0.007184 * w^0.425))^(1 / 0.725), 1)
  expect_equal(c(dubois(1.8, 70), dubois(1.9, 77), dubois(1.8, 73)),
               c(168.7, 171.9, 164.6))
  kc <- cohortFactorsIndependent(CAPECITABINE_COHORT)
  expect_equal(round(c(kc$volume, kc$clearance), 4), c(0.9472, 0.9602))
  g <- list(c(70, 168.7, 61, 148, 100), c(77, 171.9, 62, 140, 93),
            c(73, 164.6, 63, 15, 9))
  pooled <- function(drug, field) {
    s <- 0
    for (x in g) for (sex in c("male", "female")) {
      n <- if (sex == "male") x[4] else x[5]
      s <- s + n * log(get(drug)(x[1], x[2], x[3], sex)$PK$default[[field]])
    }
    exp(s / 505)
  }
  expect_equal(pooled("capecitabine", "v1"), 90.6)
  expect_equal(pooled("capecitabine", "cl1") * 60, 75.8)
  expect_equal(pooled("capecitabine5FU", "v1"), 17.8)
  expect_equal(pooled("capecitabine5FU", "cl1") * 60, 1190)
  # (b) twice the reference fat-free mass: volumes x 2 and clearances x 2^0.75
  # over the cohort's factors; 5-FU is formed from all of 5'-DFUR's clearance
  w <- uniroot(function(w) ffmAlSallami(w, 200, 61, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  x <- capecitabine(w, 200, 61, "male")
  big <- x$PK$default
  expect_equal(big$v1, 90.6 * 2 / kc$volume)
  expect_equal(big$cl1 * 60, 75.8 * 2^0.75 / kc$clearance)
  expect_equal(x$metabolite$kFormation, big$cl1 / big$v1)
  fu <- capecitabine5FU(w, 200, 61, "male")$PK$default
  expect_equal(fu$v1, 17.8 * 2 / kc$volume)
  expect_equal(fu$cl1 * 60, 1190 * 2^0.75 / kc$clearance)
  # (c) the switch off: Table 4 at any size
  off <- capecitabine(w, 200, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(off$v1, off$cl1 * 60), c(90.6, 75.8))
  off5 <- capecitabine5FU(w, 200, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(off5$v1, off5$cl1 * 60), c(17.8, 1190))
})

test_that("capecitabine is a chemotherapy drug with the metabolite-only capecitabine5FU", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "capecitabine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("capecitabine", 70, 170, 60, "male")$reference, "Gieschke")
  expect_identical(therapeuticRange("capecitabine")$band_status, "not_applicable")
  expect_identical(therapeuticRange("capecitabine5FU")$band_status, "not_applicable")
})
