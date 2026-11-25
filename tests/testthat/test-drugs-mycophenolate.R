# Mycophenolate (R/drugs_mycophenolate.R): van Hest 2005's two compartments,
# mycophenolic acid after oral mycophenolate mofetil.  Drafted by Claude Code,
# 2026-11-25.  Expectations from Table II and equations 6 and 7 of the source
# and Table 10 of the CellCept label, computed here, not read from the drug
# file.

events <- data.frame(Time = double(), Event = character())
auc <- function(w, a, b) {
  s <- w[w$Time >= a & w$Time <= b, ]
  sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
}
# the serum creatinine that gives a Cockcroft-Gault clearance of `crcl`
scrFor <- function(crcl, weight, age, sex)
  (140 - age) * weight / (72 * crcl) * (if (sex == "female") 0.85 else 1)

test_that("Table II and equations 6 and 7 at the reference (switch off)", {
  # a woman whose Cockcroft-Gault clearance is 48 mL/min, albumin 30 g/L
  f <- mycophenolate(69, 166, 50, "female", adjustToFFM = FALSE,
                     creatinine = scrFor(48, 69, 50, "female"),
                     albumin = 3.0)$PK$default
  expect_equal(f$v1, 91)
  expect_equal(f$cl1 * 60, 32.5)
  expect_equal(f$v2, 237)
  expect_equal(f$cl2 * 60, 35)
  expect_equal(f$ka_PO * 60, 4.1)
  expect_equal(f$tlag_PO / 60, 0.21)
  expect_equal(f$bioavailability_PO, 1)
  # a man clears 11% more
  m <- mycophenolate(69, 179, 50, "male", adjustToFFM = FALSE,
                     creatinine = scrFor(48, 69, 50, "male"),
                     albumin = 3.0)$PK$default
  expect_equal(m$cl1 / f$cl1, 1.11)
  expect_equal(m$v1, f$v1)
})

test_that("the text's worked examples (p. 1090)", {
  at <- function(crcl, alb) mycophenolate(69, 166, 50, "female", adjustToFFM = FALSE,
                                          creatinine = scrFor(crcl, 69, 50, "female"),
                                          albumin = alb / 10)$PK$default
  # V1 80 and 242 L at CLCR 60 and 10 mL/min (albumin 30 g/L)
  expect_equal(at(60, 30)$v1, 91 * (60 / 48)^-0.62)
  expect_equal(at(60, 30)$v1, 80, tolerance = 0.02)
  expect_equal(at(10, 30)$v1, 242, tolerance = 0.01)
  # V1 "decreases from 92 to 53 L" as albumin rises from 30 to 50 g/L: the
  # equation gives 91 to 51.0, the text's figures being rounded (4%)
  expect_equal(at(48, 50)$v1, 91 * (50 / 30)^-1.13)
  expect_equal(at(48, 50)$v1, 53, tolerance = 0.05)
  # CL 32 and 39 L/h at CLCR 60 and 10 mL/min; 33 to 18 L/h as albumin rises
  # from 30 to 50 g/L
  expect_equal(at(60, 30)$cl1 * 60, 32, tolerance = 0.02)
  expect_equal(at(10, 30)$cl1 * 60, 39, tolerance = 0.01)
  # (the equation gives 32.5 to 18.8, the text "33 to 18")
  expect_equal(at(48, 50)$cl1 * 60, 32.5 * (50 / 30)^-1.07)
  expect_equal(at(48, 50)$cl1 * 60, 18, tolerance = 0.05)
})

test_that("covariates are held within the studied ranges, and blanks take the reference", {
  at <- function(crcl, alb) mycophenolate(69, 166, 50, "female", adjustToFFM = FALSE,
                                          creatinine = scrFor(crcl, 69, 50, "female"),
                                          albumin = alb)$PK$default
  expect_equal(at(1, 3)$v1, at(4, 3)$v1)          # CLCR floor 4 mL/min
  expect_equal(at(300, 3)$cl1, at(142, 3)$cl1)    # CLCR ceiling 142 mL/min
  expect_equal(at(48, 0.5)$v1, at(48, 2.2)$v1)    # albumin floor 22 g/L
  expect_equal(at(48, 6)$cl1, at(48, 5.3)$cl1)    # albumin ceiling 53 g/L
  blank <- mycophenolate(69, 166, 50, "female", adjustToFFM = FALSE,
                         creatinine = scrFor(48, 69, 50, "female"))$PK$default
  expect_equal(blank$v1, at(48, 3)$v1)
  expect_equal(mycophenolate(69, 166, 50, "female", albumin = NA)$PK$default,
               mycophenolate(69, 166, 50, "female")$PK$default)
})

test_that("mofetil doses are converted to mycophenolic acid at 0.739", {
  expect_true(all(mycophenolateMofetilUnits %in% allUnits))
  expect_true(all(paste(mycophenolateMofetilUnits, c("bid", "tid")) %in% scheduledUnits))
  expect_equal(doseRoute(mycophenolateMofetilUnits), ROUTE_PO)
  pk <- mycophenolate(70, 170, 35, "male")
  expect_equal(doseProductFactor(c("mg PO mofetil", "mg PO"), pk$doseProducts),
               c(0.739, 1))
  # a single dose: the area is the MPA dose over the clearance
  p <- pk$PK$default
  d <- data.frame(Drug = "mycophenolate", Time = 0, Dose = 1000, Units = "mg PO mofetil")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 7 * 1440,
                                   FALSE)$mycophenolate$wide
  expect_equal(auc(w, 0, 7 * 1440), 739 / (p$cl1 * 60), tolerance = 0.01)
})

test_that("body size: anchored at van Hest's cohort, CL at each sex's", {
  # the cohort's typical man and woman (69 kg, 50 years, NCD-RisC heights) at
  # a fixed creatinine; with the switch on the Cockcroft-Gault clearance is
  # read at the pharmacokinetic weight, so CL's ratio carries that term
  for (who in list(list(h = 179.6, s = "male"), list(h = 166.1, s = "female"))) {
    on  <- mycophenolate(69, who$h, 50, who$s, TRUE, creatinine = 1, albumin = 3)$PK$default
    off <- mycophenolate(69, who$h, 50, who$s, FALSE, creatinine = 1, albumin = 3)$PK$default
    pkW <- pkSizeFactors(69, who$h, 50, who$s, TRUE)$pkWeight
    crclRatio <- creatinineClearanceCG(pkW, 50, who$s, 1) /
      creatinineClearanceCG(69, 50, who$s, 1)
    # CL: the patient's own sex within the cohort, so its factor is 1 here
    expect_equal(on$cl1 / off$cl1, crclRatio^-0.12, tolerance = 1e-6)
    expect_equal(on$v1 / off$v1, crclRatio^-0.62 *
                   pkSizeFactors(69, who$h, 50, who$s, TRUE,
                                 cohort = do.call(cohortSizeFactors, MYCOPHENOLATE_COHORT))$volume,
                 tolerance = 1e-6)
  }
  # V2 and Q are anchored at the pooled cohort, 63% men: its typical woman is
  # below it and its typical man above
  woman <- mycophenolate(69, 166.1, 50, "female", creatinine = 1)$PK$default
  man   <- mycophenolate(69, 179.6, 50, "male", creatinine = 1)$PK$default
  expect_lt(woman$v2, 237); expect_gt(man$v2, 237)
  expect_lt(woman$cl2 * 60, 35); expect_gt(man$cl2 * 60, 35)
  # a larger patient gets larger volumes with the switch on only
  big <- mycophenolate(100, 180, 50, "male", creatinine = 1)$PK$default
  ref <- mycophenolate(70, 180, 50, "male", creatinine = 1)$PK$default
  expect_gt(big$v2, ref$v2)
  bigOff <- mycophenolate(100, 180, 50, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(bigOff$v2, 237)
})

test_that("CellCept label Table 10: early after kidney transplantation", {
  # 1 g twice daily, less than 40 days after transplantation: AUC(0-12)
  # 27.3 +/- 10.9 mcg.h/mL, Cmax 8.16 +/- 4.50 mcg/mL.  A 70 kg man of 50 with
  # a creatinine clearance of 50 mL/min and albumin 3.7 g/dL (van Hest's day
  # 28 medians) lies within one standard deviation of both.
  d <- data.frame(Drug = "mycophenolate", Time = 0, Dose = 1000,
                  Units = "mg PO mofetil bid")
  w <- simulateDrugsWithCovariates(d, events, 70, 175, 50, "male", 7 * 1440,
                                   FALSE, creatinine = scrFor(50, 70, 50, "male"),
                                   albumin = 3.7)$mycophenolate$wide
  a <- auc(w, 6 * 1440, 6 * 1440 + 720)
  expect_gt(a, 27.3 - 10.9); expect_lt(a, 27.3 + 10.9)
  cmax <- max(w$Plasma[w$Time >= 6 * 1440 & w$Time <= 6 * 1440 + 720])
  expect_gt(cmax, 8.16 - 4.50); expect_lt(cmax, 8.16 + 4.50)
})
