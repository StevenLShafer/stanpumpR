# cetagliptin: see R/drugs_cetagliptin.R (Zhou 2024, Supplementary Table 1).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("switch off, the supplement's values", {
  X <- cetagliptin(70, 170, 46, "male", adjustToFFM = FALSE)
  p <- X$PK$default
  expect_equal(p$v1, 8.668)
  expect_equal(p$v2, 558)
  expect_equal(p$cl2 * 60, 9.671)
  expect_equal(p$ka_PO * 60, 0.06521)
  expect_equal(p$bioavailability_PO, 1)
  expect_equal(X$michaelisMenten$vmax * 60, 9373)
  expect_equal(X$michaelisMenten$km, 171.5)
  expect_equal(p$cl1 * 60, 9373 / 171.5)              # 54.7 L/h below Km
})

test_that("switch on: volumes by the volume factor, Vmax and Cl2 by the clearance factor", {
  size <- pkSizeFactors(95, 170, 46, "male", TRUE, legacyVolume = 1)
  X <- cetagliptin(95, 170, 46, "male")
  expect_equal(X$PK$default$v1, 8.668 * size$volume)
  expect_equal(X$PK$default$v2, 558 * size$volume)
  expect_equal(X$michaelisMenten$vmax, 9373 / 60 * size$clearance)
  expect_equal(X$michaelisMenten$km, 171.5)
  expect_equal_rounded(X$PK$default$v1, 10.1686484)
})

test_that("the engine matches an independent solution of the same equations", {
  PK <- getDrugPK("cetagliptin", 70, 170, 46, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "cetagliptin", Time = c(0, 1440), Dose = 100, Units = "mg PO"),
               noEvents, PK, 3 * 1440, FALSE)$wide
  ka <- 0.06521 / 60; V <- 8.668; V2 <- 558; Q <- 9.671 / 60
  vmax <- 9373 / 60; km <- 171.5
  f <- function(A) {
    C <- A[2] / V
    c(-ka * A[1], ka * A[1] - vmax * C / (km + C) - Q * C + Q * A[3] / V2, Q * C - Q * A[3] / V2)
  }
  h <- 0.25
  tt <- seq(0, 3 * 1440, by = h)
  A <- c(0, 0, 0); cp <- numeric(length(tt))
  for (j in seq_along(tt)) {
    if (tt[j] %in% c(0, 1440)) A[1] <- A[1] + 100000
    cp[j] <- A[2] / V
    k1 <- f(A); k2 <- f(A + h / 2 * k1); k3 <- f(A + h / 2 * k2); k4 <- f(A + h * k3)
    A <- A + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  ref <- approx(tt, cp, w$Time)$y
  expect_lt(max(abs(w$Plasma - ref)) / max(ref), 2e-3)
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("Table 2: the first day's AUC at 50 and 100 mg, more than dose proportional", {
  PK <- getDrugPK("cetagliptin", 70, 170, 46, "male", adjustToFFM = FALSE)
  auc <- function(dose) {
    w <- simCpCe(data.frame(Drug = "cetagliptin", Time = 0, Dose = dose, Units = "mg PO"),
                 noEvents, PK, 1440, FALSE)$wide
    sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  }
  a50 <- auc(50); a100 <- auc(100)
  expect_equal(a50, 717, tolerance = 0.05)     # paper 717 ng.h/mL
  expect_equal(a100, 1830, tolerance = 0.05)   # paper 1830
  expect_gt(a100 / a50, 2.3)                   # paper 2.55
  expect_equal_rounded(a100, 1822.2764)
})

test_that("units: oral, daily; no bilirubin argument", {
  expect_equal(getDrugDefaults("cetagliptin")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_setequal(names(formals(cetagliptin)),
                  c("weight", "height", "age", "sex", "adjustToFFM"))
})
