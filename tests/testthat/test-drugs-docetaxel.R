# Docetaxel (R/drugs_docetaxel.R): Bruno 1996.  Drafted by Claude Code,
# 2026-11-06.

htMean <- 1.78^2 * 3600 / 70   # Mosteller BSA 1.78 m2 at 70 kg

test_that("Table II at the mean covariates: CL 36.7 L/h, Vss 149 L, half-lives 5.8 min, 29.7 min, 11.4 h", {
  set <- getDrugPK("docetaxel", 70, htMean, 56, "female",
                   adjustToFFM = FALSE)$PK$default
  expect_equal(round(set$cl1 * 60, 2), 36.70)
  expect_equal(round(set$v1 + set$v2 + set$v3, 1), 148.6)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives * c(60, 60, 1), 2), c(5.84, 29.75, 11.42))
  expect_equal(round(set$cl1 / set$v1, 3), round(4.417 / 60, 3))
})

test_that("albumin is read in g/dL; blank is the mean 40.1 g/L", {
  cl <- function(...) docetaxel(70, htMean, 56, "female", adjustToFFM = FALSE, ...)$PK$default$cl1 * 60
  expect_equal(cl(), cl(albumin = 4.01))
  expect_equal(cl(albumin = 3.1) - cl(), 1.78 * 0.225 * (31 - 40.1), tolerance = 1e-3)
  # the paper's quoted effects: albumin 31 and 48 g/L, -9.9% and +8.6%
  expect_equal(round(100 * (cl(albumin = 3.1) / cl() - 1), 1), -9.9)
  expect_equal(round(100 * (cl(albumin = 4.8) / cl() - 1), 1), 8.6)
})

test_that("age enters linearly, held at 18 below it", {
  cl <- function(age) docetaxel(70, htMean, age, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(round(100 * (cl(71) / cl(56) - 1), 1), -6.9)
  expect_equal(cl(5), cl(18))
})

test_that("docetaxel is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "docetaxel", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("docetaxel", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Bruno")
  expect_identical(therapeuticRange("docetaxel")$band_status, "not_applicable")
})

test_that("getDrugPK passes the Patient Profile's albumin to docetaxel", {
  a <- getDrugPK("docetaxel", 70, htMean, 56, "female", adjustToFFM = FALSE, albumin = 3.1)
  b <- docetaxel(70, htMean, 56, "female", adjustToFFM = FALSE, albumin = 3.1)
  expect_equal(a$PK$default$cl1, b$PK$default$cl1)
})

test_that("pregnancy: Janssen 2021's factors on CL, V1 and the shallow V2", {
  off <- docetaxel(70, 165, 30, "female", FALSE)$PK$default
  on <- docetaxel(70, 165, 30, "female", FALSE, pregnant = TRUE)$PK$default
  expect_equal(c(on$cl1 / off$cl1, on$v1 / off$v1, on$v2 / off$v2), c(1.08, 1.19, 1.12))
  expect_equal(c(on$v3, on$cl2, on$cl3), c(off$v3, off$cl2, off$cl3))
})
