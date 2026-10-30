# amantadineER: see R/drugs_amantadineER.R (FDA reviewers' model of NDA
# 209410, Table 6; the study's means are Table 8).  The exactness of the
# zero-order share is in test-transit.R.

# The creatinine giving an MDRD eGFR at 60 y, male
scrForEGFR <- function(egfr) uniroot(function(s) egfrMDRD(60, "male", s) - egfr, c(0.2, 20),
                                     tol = 1e-12)$root
singleDose <- function(egfr)
  pdSimulate(data.frame(Drug = "amantadineER", Time = 0, Dose = 160 * AMANTADINE_BASE_PER_HCL,
                        Units = "mg PO"), maximum = 648 * 60, weight = 83, height = 175,
             creatinine = scrForEGFR(egfr))

test_that("Table 6, per mg of hydrochloride, and the zero-order share", {
  m <- amantadineER(70, 170, 60, "male", FALSE, creatinine = scrForEGFR(42))
  p <- m$PK$default
  b <- 151.25 / 187.71
  expect_equal(p$v1 / b, 437)
  expect_equal(p$cl1 / p$v1 * 60, 0.02, tolerance = 1e-8)
  expect_equal(p$ka_PO * 60, 0.344)
  expect_equal(m$zeroOrderPO, list(duration = 8.79 * 60, into = "central", fraction = 1 - 0.799))
})

test_that("k is proportional to the MDRD eGFR", {
  k <- function(e) { p <- amantadineER(70, 170, 60, "male", FALSE,
                                       creatinine = scrForEGFR(e))$PK$default; p$cl1 / p$v1 }
  expect_equal(k(84) / k(42), 2, tolerance = 1e-8)
})

test_that("the study's normal and severe groups (Table 8) are reproduced", {
  # 160 mg HCl: normal (mean eGFR 89.25) Cmax 281, AUC 10109; severe (21.13)
  # Cmax 345, AUC 39031.  The moderate group is under-predicted (header).
  normal <- singleDose(89.25); severe <- singleDose(21.13)
  expect_equal(pdPeak(normal)[["C"]], 281, tolerance = 0.05)
  expect_equal(pdPeak(severe)[["C"]], 345, tolerance = 0.05)
  expect_equal(pdAUC(normal, 0, 648 * 60), 10109, tolerance = 0.16)
  expect_equal(pdAUC(severe, 0, 648 * 60), 39031, tolerance = 0.08)
})

test_that("the band is the AGNP update 2026 range (Hart 2026, Table 5)", {
  dd <- getDrugDefaultsGlobal()
  expect_equal(unlist(dd[dd$Drug == "amantadineER", c("Lower", "Upper", "Typical")]),
               c(Lower = 600, Upper = 1700, Typical = 1150))
})
