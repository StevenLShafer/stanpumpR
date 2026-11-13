# Cyclophosphamide (R/drugs_cyclophosphamideJoerger.R): Joerger 2007, and the
# group that chooses it for adults.  Drafted by Claude Code, 2026-11-06.

test_that("Table IV: one compartment, CL 4.23 L/h, V 34.6 L, half-life 5.7 h", {
  set <- getDrugPK("cyclophosphamide", 70, 165, 56, "female",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 4.23)
  expect_equal_rounded(set$v1, 34.6)
  expect_equal(set$cl2, 0)
  expect_equal(round(log(2) / set$lambda_1 / 60, 2), 5.67)
})

test_that("the switch scales by the library's factors and leaves the published values off", {
  size <- pkSizeFactors(100, 165, 56, "female", TRUE)
  on <- cyclophosphamideJoerger(100, 165, 56, "female")$PK$default
  expect_equal_rounded(on$v1, 34.6 * size$volume)
  expect_equal_rounded(on$cl1 * 60, 4.23 * size$clearance)
  off <- cyclophosphamideJoerger(100, 165, 56, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off$v1, 34.6)
})

test_that("cyclophosphamide is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "cyclophosphamide", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("cyclophosphamide", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Joerger")
  expect_identical(therapeuticRange("cyclophosphamide")$band_status, "not_applicable")
})

# The group (R/modelSelection.R): Joerger for adults, McCune for children.
# McCune 2009 (R/drugs_cyclophosphamideMcCune.R), drafted by Claude Code,
# 2026-11-10: autoinduction on the Michaelis-Menten engine.
noEvents <- data.frame(Time = double(), Event = character())

test_that("the program chooses Joerger for adults and McCune for children", {
  expect_match(getDrugPK("cyclophosphamide", 70, 170, 50, "female")$reference, "Joerger")
  child <- getDrugPK("cyclophosphamide", 14, 95, 3, "male")
  expect_match(child$reference, "McCune")
  expect_false(is.null(child$michaelisMenten$autoinduction))
  expect_match(getDrugPK("cyclophosphamide", 40, 150, 12, "male")$reference, "McCune")
  expect_match(getDrugPK("cyclophosphamide", 55, 165, 15, "male")$reference, "Joerger")
})

test_that("Table II per m2 of BSA (Mosteller), the enzyme block as printed", {
  X <- cyclophosphamideMcCune(14.2, 95, 3.16, "male", adjustToFFM = FALSE)
  bsa <- bsaMosteller(14.2, 95)
  d <- X$PK$default
  expect_equal_rounded(d$v1, 13.1 * bsa)
  ai <- X$michaelisMenten$autoinduction
  expect_equal_rounded(X$michaelisMenten$clLinear * 60, 1.83 * bsa)
  expect_equal_rounded(ai$cl * 60, 1.79 * bsa)
  expect_equal_rounded(ai$kEnz * 60, 6.57e-3)
  expect_equal(ai$emax, 5)
  expect_equal_rounded(ai$ec50, 0.6 * 261.1 / 1000)
  expect_equal(ai$imax, 0)
})

test_that("a 5-day course: clearance rises as the paper's medians do", {
  # The median child; 400 mg/m2 over 30 min daily.  The paper's median AUCs
  # are 431 uM h after dose 1 and 313 after dose 4 (112.5 and 81.7 mg h/L);
  # the typical-value model gives 105 and 70 (7% and 14% below), and the
  # clearance after dose 1, 400 / AUC, against the paper's median 3.58 L/h/m2.
  bsa <- bsaMosteller(14.2, 95)
  dose <- data.frame(Drug = "cyclophosphamide",
                     Time = rep((0:4) * 1440, each = 2) + c(0, 30),
                     Dose = rep(c(400 * bsa * 2, 0), 5), Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dose, noEvents, 14.2, 95, 3.16, "male", 6 * 1440,
                                   FALSE, adjustToFFM = FALSE)$cyclophosphamide$wide
  auc <- function(a, b) {
    k <- w$Time >= a & w$Time <= b
    sum(diff(w$Time[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2) / 60
  }
  a1 <- auc(0, 1440)
  a4 <- auc(3 * 1440, 4 * 1440)
  expect_equal(round(a1), 105)
  expect_equal(round(a4), 70)
  expect_lt(a4 / a1, 313 / 431)            # induction a little stronger than the medians
  expect_equal(round(400 / a1, 1), 3.8)    # L/h/m2 after dose 1; the paper's 3.58
})
