# Flecainide (R/drugs_flecainide.R): Doki 2012's one compartment with
# CYP2D6-dependent decline in metabolic clearance with age.  Drafted by
# Claude Code, 2026-11-13.  Expectations from the source's equations and
# text, not from the drug file.

events <- data.frame(Time = double(), Event = character())
# the serum creatinine that gives a Japanese eGFR at an age
scrFor <- function(egfr, age, female = FALSE)
  (egfr / (194 * age^(-0.287) * (if (female) 0.739 else 1)))^(-1 / 1.094)

test_that("the paper's typical clearance: 18.6 L/h at 66 kg and an eGFR of 73", {
  p <- flecainide(66, 170, 50, "male", adjustToFFM = FALSE,
                  creatinine = scrFor(73, 50))$PK$default
  expect_equal(p$cl1 * 60, 8.83 + 9.79)
  expect_equal(p$v1, 6.0 * 66)
  expect_equal(p$ka_PO * 60, 0.62)
  # women: the metabolic part x 0.713
  w <- flecainide(66, 170, 50, "female", adjustToFFM = FALSE,
                  creatinine = scrFor(73, 50, TRUE))$PK$default
  expect_equal(w$cl1 * 60, 8.83 * 0.713 + 9.79)
  # the renal part follows eGFR and weight
  r <- flecainide(66, 170, 50, "male", adjustToFFM = FALSE,
                  creatinine = scrFor(36.5, 50))$PK$default
  expect_equal(r$cl1 * 60, 8.83 + 9.79 / 2)
})

test_that("CYP2D6 steepens the decline with age after 55 or 60, not before", {
  cl <- function(age, g) flecainide(66, 170, age, "male", adjustToFFM = FALSE,
                                    creatinine = scrFor(73, age), cyp2d6 = g)$PK$default$cl1 * 60
  for (g in CYP2D6_VALUES) expect_equal(cl(50, g), 8.83 + 9.79)
  expect_equal(cl(70, "normal"), 8.83 + 9.79)
  expect_equal(cl(70, "intermediate"), 8.83 * (60 / 70)^1.62 + 9.79)
  expect_equal(cl(70, "poor"), 8.83 * (55 / 70)^2.83 + 9.79)
  expect_equal(cl(70, "ultrarapid"), cl(70, "normal"))
  # the paper: from 52 to 70 years, with eGFR 102 - 0.486 age, total CL/F
  # falls 6.1% (hom-EM), 16.3% (het-EM) and 28.9% (IM/PM)
  fall <- function(g) {
    c52 <- flecainide(66, 170, 52, "male", adjustToFFM = FALSE,
                      creatinine = scrFor(102 - 0.486 * 52, 52), cyp2d6 = g)$PK$default$cl1
    c70 <- flecainide(66, 170, 70, "male", adjustToFFM = FALSE,
                      creatinine = scrFor(102 - 0.486 * 70, 70), cyp2d6 = g)$PK$default$cl1
    100 * (1 - c70 / c52)
  }
  expect_equal(c(fall("normal"), fall("intermediate"), fall("poor")),
               c(6.1, 16.3, 28.9), tolerance = 0.01)
  expect_error(flecainide(70, 170, 35, "male", cyp2d6 = "fast"), "cyp2d6")
})

test_that("100 mg twice daily lies in the label's 0.2-1.0 mcg/mL", {
  d <- data.frame(Drug = "flecainide", Time = 0, Dose = 100, Units = "mg PO bid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 7 * 1440,
                                   FALSE)$flecainide$wide
  s <- w[w$Time >= 6 * 1440 & w$Time <= 6 * 1440 + 720, ]
  expect_gt(min(s$Plasma), 200); expect_lt(max(s$Plasma), 1000)
  # peak about 3 h after the dose (label: about 3 h, range 1-6)
  expect_equal((s$Time[which.max(s$Plasma)] - 6 * 1440) / 60, 3, tolerance = 0.1)
})

test_that("the Japanese eGFR helper is Matsuo 2009, floored at 18 years", {
  expect_equal(egfrMatsuo(50, "male", 1), 194 * 50^(-0.287))
  expect_equal(egfrMatsuo(50, "female", 1), 194 * 50^(-0.287) * 0.739)
  expect_equal(egfrMatsuo(2, "male", 0.5), egfrMatsuo(18, "male", 0.5))
})
