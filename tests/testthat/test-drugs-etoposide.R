# Etoposide in adults (R/drugs_etoposide.R): Krogh-Madsen 2012.  Drafted by
# Claude Code, 2026-11-06.

scr116 <- (140 - 52) * 79.4 / (72 * 116)    # Cockcroft-Gault 116 mL/min

test_that("Table 2 for a man at CLCR 116: CL 2.145 L/h, 54% renal, Vss 13.9 L, half-lives 86 min and 5.9 h", {
  set <- getDrugPK("etoposide", 79.4, 175, 52, "male", adjustToFFM = FALSE,
                   creatinine = scr116)$PK$default
  expect_equal(round(set$cl1 * 60, 3), 2.145)
  expect_equal(round(0.00996 * 116 / (set$cl1 * 60), 2), 0.54)
  expect_equal_rounded(set$v1, 9.13)
  expect_equal_rounded(set$cl2 * 60, 1.14)
  expect_equal(round(set$v1 + set$v2, 2), 13.88)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives * c(60, 1), c(0, 2)), c(86, 5.94))
})

test_that("women's central volume is 27.9% smaller", {
  v <- function(sex) etoposideKroghMadsen(79.4, 175, 52, sex, adjustToFFM = FALSE)$PK$default$v1
  expect_equal(v("female") / v("male"), 1 - 0.2793)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  m <- function(...) etoposideKroghMadsen(70, 165, 30, "female", adjustToFFM = FALSE, ...)$PK$default$cl1 * 60
  expect_equal(m(pregnant = TRUE) - 0.99, 1.5 * (m() - 0.99))
  expect_equal(m(creatinine = 0.6, pregnant = TRUE), m(creatinine = 0.6))
})

test_that("etoposide is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "etoposide", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("etoposide", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Krogh-Madsen")
  expect_identical(therapeuticRange("etoposide")$band_status, "not_applicable")
})
