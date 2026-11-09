# Doxorubicin (R/drugs_doxorubicinPerezBlanco.R): Perez-Blanco 2016, three
# compartments forming two-compartment doxorubicinol.  Drafted by Claude
# Code, 2026-11-09.

noEvents <- data.frame(Time = double(), Event = character())

test_that("the model is Table 2", {
  X <- doxorubicinPerezBlanco(70, 170, 35, "male", adjustToFFM = FALSE)
  d <- X$PK$default
  expect_equal_rounded(d$cl1 * 60, 62.4)
  expect_equal_rounded(d$v1, 17.7)
  expect_equal_rounded(d$v2, 1830)
  expect_equal_rounded(d$v3, 71)
  expect_equal_rounded(d$cl2 * 60, 50.7)
  expect_equal_rounded(d$cl3 * 60, 28.4)
  expect_equal_rounded(X$metabolite$kFormation, 0.22 * d$cl1 / d$v1)
  expect_identical(X$metabolite$name, "doxorubicinolPerezBlanco")
  M <- doxorubicinolPerezBlanco(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(M$cl1 * 60, 26.8)
  expect_equal_rounded(M$v1, 79.8)
  expect_equal_rounded(M$v2, 653)
  expect_equal_rounded(M$cl2 * 60, 424)
})

test_that("the half-lives", {
  PK <- getDrugPK("doxorubicinPerezBlanco", 70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) / c(PK$lambda_1, PK$lambda_2, PK$lambda_3) / 60, c(3, 2, 1)),
               c(0.086, 2.17, 45.8))
})

test_that("AUCs as the Methods compute them: dose/CL and Fm x dose/CLm", {
  dose <- data.frame(Drug = "doxorubicinPerezBlanco", Time = c(0, 30),
                     Dose = c(89 * 2, 0), Units = "mg/hr")
  sim <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 35, "male", 60 * 24 * 21,
                                     FALSE, adjustToFFM = FALSE)
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60 / 1000
  # three weeks catch about 96% of the 45.8-h terminal phase's area
  expect_equal(auc(sim$doxorubicinPerezBlanco$wide), 89 / 62.4, tolerance = 0.05)
  expect_equal(auc(sim$doxorubicinolPerezBlanco$wide), 0.22 * 89 / 26.8, tolerance = 0.05)
})

test_that("both switch positions are pinned", {
  on <- doxorubicinPerezBlanco(70, 170, 35, "male")$PK$default
  off <- doxorubicinPerezBlanco(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$cl1, off$cl1)
  expect_equal(doxorubicinPerezBlanco(120, 170, 35, "male", adjustToFFM = FALSE)$PK$default$cl1,
               off$cl1)
})

test_that("a chemotherapy drug and its metabolite-only doxorubicinol, plasma only, no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "doxorubicinPerezBlanco", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  m <- dd[dd$Drug == "doxorubicinolPerezBlanco", ]
  expect_true(is.na(m$Category) || m$Category == "")
  PK <- getDrugPK("doxorubicinPerezBlanco", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Perez-Blanco")
  expect_identical(therapeuticRange("doxorubicinPerezBlanco")$band_status, "not_applicable")
})
