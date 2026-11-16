# Ifosfamide (R/drugs_ifosfamide.R): Kerbusch 2000, one compartment with the
# autoinduction of its metabolism.  Drafted by Claude Code, 2026-11-10.

noEvents <- data.frame(Time = double(), Event = character())
ifoRun <- function(grams, weight = 59, hours = 96) {
  dose <- data.frame(Drug = "ifosfamide", Time = c(0, 72 * 60),
                     Dose = c(grams * 1000 / 72, 0), Units = "mg/hr")
  simulateDrugsWithCovariates(dose, noEvents, weight, 170, 49, "male", hours * 60,
                              FALSE, adjustToFFM = FALSE)$ifosfamide$wide
}

test_that("Table 1 as printed: CL 2.94 L/h, V 43.5 L, Kenz,out 0.0546 /h, IC50 30.7 uM", {
  X <- ifosfamide(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal_rounded(X$PK$default$v1, 43.5)
  expect_equal_rounded(X$PK$default$cl1 * 60, 2.94)
  ai <- X$michaelisMenten$autoinduction
  expect_equal_rounded(ai$cl * 60, 2.94)
  expect_equal_rounded(ai$kEnz * 60, 0.0546)
  expect_equal(c(ai$emax, ai$imax), c(0, 1))
  expect_equal_rounded(ai$ic50, 30.7 * 261.09 / 1000)        # 8.016 mg/L
  expect_equal(round(log(2) / (ai$kEnz * 60), 1), 12.7)      # the enzyme half-life
})

test_that("Figure 6: peaks of about 115 and 200 uM after 10 and 20 g over 72 h", {
  peak <- function(g) max(ifoRun(g)$Plasma) / 261.09 * 1000
  expect_equal(round(peak(10)), 108)
  expect_equal(round(peak(20)), 204)
})

test_that("Table 2's AUCs are g h/L, not uM h: 1.65 for 10 g (the table's 1.7)", {
  w <- ifoRun(10, hours = 240)
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60 / 1000
  expect_equal(round(auc, 2), 1.65)
})

test_that("induction: the concentration falls during a constant infusion, as the enzyme rises", {
  w <- ifoRun(10)
  at <- function(h) approx(w$Time, w$Plasma, h * 60)$y
  expect_gt(at(20), at(70))
  # without induction the concentration would rise to R / CL throughout
  expect_lt(at(70), 10000 / 72 / 2.94)
})

test_that("both switch positions are pinned", {
  on <- ifosfamide(70, 170, 35, "male")$PK$default
  off <- ifosfamide(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$cl1, off$cl1)
  big <- ifosfamide(120, 170, 35, "male")
  size <- pkSizeFactors(120, 170, 35, "male", TRUE)
  expect_equal_rounded(big$PK$default$v1, 43.5 * size$volume)
  expect_equal_rounded(big$michaelisMenten$autoinduction$cl * 60, 2.94 * size$clearance)
})

test_that("ifosfamide is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "ifosfamide", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("ifosfamide", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Kerbusch")
  expect_identical(therapeuticRange("ifosfamide")$band_status, "not_applicable")
})
