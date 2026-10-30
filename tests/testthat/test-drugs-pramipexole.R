# pramipexole: see R/drugs_pramipexole.R (FDA Clinical Pharmacology Review of
# NDA 22-421, Table 3).

# The creatinine that gives a Cockcroft-Gault clearance at 75 kg, 60 y, male
scrForCrCl <- function(crcl) (140 - 60) * 75 / (72 * crcl)

test_that("Table 3, per mg of salt (x 1.431), at a CrCl above the knee", {
  p <- pramipexole(75, 175, 60, "male", FALSE, creatinine = scrForCrCl(130))$PK$default
  s <- 1.431
  expect_equal(p$cl1 * 60 / s, 29.2)
  expect_equal(p$v1 / s, 35.6)
  expect_equal(p$cl2 * 60 / s, 115)
  expect_equal(p$v2 / s, 313)
  expect_equal(p$ka_PO * 60, 0.517)
  expect_equal(p$tlag_PO / 60, 0.221)
})

test_that("the hockey stick: 0.74% per mL/min below 121, flat above", {
  cl <- function(crcl) pramipexole(75, 175, 60, "male", FALSE,
                                   creatinine = scrForCrCl(crcl))$PK$default$cl1
  expect_equal(cl(150), cl(121))
  expect_equal(cl(50) / cl(121), 1 + 0.0074 * (50 - 121))
  expect_equal(cl(20) / cl(121), 1 + 0.0074 * (20 - 121))
})

test_that("weight on V3: 2.26% per kg from 75 kg, held at 40 kg and above", {
  v3 <- function(w) pramipexole(w, 175, 60, "male", FALSE)$PK$default$v2 / 1.431
  expect_equal(v3(100), 313 * (1 + 0.0226 * 25))
  expect_equal(v3(30), v3(40))
  expect_gt(v3(30), 0)
})

test_that("IR 0.25 mg tid at steady state: Jenner 2009's exposure", {
  # Healthy volunteers: Cmax 1.09 ng/mL, AUC0-24 16.0; the model, at a CrCl
  # above the knee and 75 kg, 1.01 and 17.9
  res <- pdSimulate(pdRegimen("pramipexole", "mg PO", 0.25, 480, 21), maximum = 7 * 1440,
                    weight = 75, height = 175, creatinine = scrForCrCl(130))
  expect_equal_rounded(17.95, pdAUC(res, 6 * 1440, 7 * 1440), tolerance = 0.01)
  expect_equal(pdTerminalHalfLife("pramipexole", 75, 175, 60, "male", adjustToFFM = FALSE,
                                  creatinine = scrForCrCl(130)), 10.0, tolerance = 0.01)
})
