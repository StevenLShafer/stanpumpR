# antithrombinAlfa: see R/drugs_antithrombinAlfa.R (DeJongh 2014, Table 3).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
dejongh <- function(cl, vss) {
  vc <- vss / 2.51
  c(cl1 = cl / 60, v1 = vc, cl2 = 0.613 / 60, v2 = vss - vc)
}
pkOf <- function(...) {
  p <- antithrombinAlfa(...)$PK$default
  c(cl1 = p$cl1, v1 = p$v1, cl2 = p$cl2, v2 = p$v2)
}

test_that("Table 3, surgery and delivery, switch off: the published values", {
  for (w in c(50, 70, 120)) {
    expect_equal(pkOf(w, 165, 30, "female", FALSE), dejongh(0.665, 7.72))
    expect_equal(pkOf(w, 165, 30, "female", FALSE, pregnant = TRUE), dejongh(1.38, 14.3))
  }
})

test_that("switch on: fat-free mass at the reference man, who gets Table 3", {
  expect_equal(pkOf(70, 170, 35, "male", TRUE), dejongh(0.665, 7.72))
  size <- pkSizeFactors(90, 160, 40, "female", TRUE)
  ref <- dejongh(0.665, 7.72)
  expect_equal(pkOf(90, 160, 40, "female", TRUE),
               ref * c(size$clearance, size$volume, size$clearance, size$volume))
})

test_that("the checks in the header", {
  p <- pkOf(70, 170, 35, "male", FALSE)
  expect_equal(p[["cl1"]] * 60 * 1000 / 70, 9.5, tolerance = 1e-3)        # mL/h/kg
  expect_equal((p[["v1"]] + p[["v2"]]) * 1000 / 70, 110.3, tolerance = 1e-3)  # mL/kg
  expect_equal(70 / (p[["v1"]] * 10), 2.276, tolerance = 1e-3)              # IU/dL per IU/kg
  k10 <- p[["cl1"]] / p[["v1"]]; k12 <- p[["cl2"]] / p[["v1"]]; k21 <- p[["cl2"]] / p[["v2"]]
  b <- k10 + k12 + k21
  beta <- (b - sqrt(b^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / beta / 60, 11.88, tolerance = 1e-3)                  # hours
  expect_equal(ANTITHROMBIN_ALFA_PREGNANT_CL, 2.075, tolerance = 1e-3)
  expect_equal(ANTITHROMBIN_ALFA_PREGNANT_VSS, 1.852, tolerance = 1e-3)
})

test_that("the baseline is plotted, and only plotted", {
  PK <- getDrugPK("antithrombinAlfa", 70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 44.7)
  dose <- data.frame(Drug = "antithrombinAlfa", Time = 60, Dose = 50, Units = "IU/kg")
  w <- simCpCe(dose, noEvents, PK, 180, FALSE)$wide
  expect_equal(w$Plasma[w$Time < 60], rep(44.7, sum(w$Time < 60)))
  expect_equal(max(w$Plasma[w$Time == 60]), 44.7 + 3500 / (77.2 / 2.51), tolerance = 1e-6)
  PK2 <- getDrugPK("antithrombinAlfa", 70, 170, 35, "male", adjustToFFM = FALSE,
                   factorBaselines = list(antithrombinBaseline = 30))
  w2 <- simCpCe(dose, noEvents, PK2, 180, FALSE)$wide
  expect_equal(w2$Plasma - w$Plasma, rep(30 - 44.7, nrow(w)))
  expect_true(all(c("antithrombin", "antithrombinAlfa") %in%
                    patientFieldReaders("antithrombinBaseline")))
})
