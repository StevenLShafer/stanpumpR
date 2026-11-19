# cetagliptin: see R/drugs_cetagliptin.R (Lu 2024, Table 2).
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("switch off, Table 2 values", {
  X <- cetagliptin(70, 170, 46, "male", adjustToFFM = FALSE)
  p <- X$PK$default
  expect_equal(p$v1, 204)
  expect_equal(p$v2, 160)
  expect_equal(p$cl1 * 60, 30.55)
  expect_equal(p$cl2 * 60, 133)
  expect_equal(p$ka_PO * 60, 1.56)
  expect_equal(p$bioavailability_PO, 1)
  mm <- X$michaelisMenten
  expect_equal(mm$vmax, 0)
  expect_equal(mm$clLinear * 60, 30.55)
  expect_equal(mm$saturableEfflux$vmax * 60, 41731)
  expect_equal(mm$saturableEfflux$km, 11.63)
})

test_that("switch on: the reference anchor (Lu 2024 prints no demographics)", {
  size <- pkSizeFactors(95, 170, 46, "male", TRUE, legacyVolume = 1)
  X <- cetagliptin(95, 170, 46, "male")
  expect_equal(X$PK$default$v1, 204 * size$volume)
  expect_equal(X$PK$default$v2, 160 * size$volume)
  expect_equal(X$michaelisMenten$clLinear, 30.55 / 60 * size$clearance)
  expect_equal(X$michaelisMenten$saturableEfflux$vmax, 41731 / 60 * size$clearance)
  expect_equal(X$michaelisMenten$saturableEfflux$km, 11.63)
})

sim <- function(dose, days) {
  PK <- getDrugPK("cetagliptin", 70, 170, 46, "male", adjustToFFM = FALSE)
  w <- withOutputTimes(seq(0, days * 1440, by = 5), simCpCe(
    data.frame(Drug = "cetagliptin", Time = (seq_len(days) - 1) * 1440, Dose = dose,
               Units = "mg PO"), noEvents, PK, days * 1440, FALSE))$wide
  w[!duplicated(w$Time), ]
}
aucDay <- function(w, day) {
  s <- w[w$Time >= (day - 1) * 1440 & w$Time <= day * 1440, ]
  sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
}

test_that("the engine matches an independent solution of the efflux equations", {
  w <- sim(100, 2)
  ka <- 1.56 / 60; V <- 204; V2 <- 160; Q <- 133 / 60; CL <- 30.55 / 60
  vE <- 41731 / 60; kt <- 11.63
  f <- function(A) {
    C <- A[2] / V; back <- vE * C / (kt + C)
    c(-ka * A[1] + back,
      ka * A[1] - back - CL * C - Q * C + Q * A[3] / V2,
      Q * C - Q * A[3] / V2)
  }
  h <- 0.05
  tt <- seq(0, 2 * 1440, by = h)
  A <- c(0, 0, 0); cp <- numeric(length(tt))
  for (j in seq_along(tt)) {
    if (abs(tt[j] %% 1440) < h / 2) A[1] <- A[1] + 100000
    cp[j] <- A[2] / V
    k1 <- f(A); k2 <- f(A + h / 2 * k1); k3 <- f(A + h / 2 * k2); k4 <- f(A + h * k3)
    A <- A + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  ref <- approx(tt, cp, w$Time)$y
  expect_lt(max(abs(w$Plasma - ref), na.rm = TRUE) / max(ref), 2e-3)
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("Lu 2022's healthy volunteers, the data Lu 2024 fitted: day 1 and day 7", {
  # measured means (Lu 2022, Table 2): AUC0-24 ng.h/mL
  w50 <- sim(50, 7); w100 <- sim(100, 7); w200 <- sim(200, 7)
  expect_equal(aucDay(w50, 1), 857, tolerance = 0.10)
  expect_equal(aucDay(w100, 1), 2280, tolerance = 0.10)
  expect_equal(aucDay(w200, 1), 5500, tolerance = 0.15)
  expect_equal(aucDay(w50, 7), 1440, tolerance = 0.15)
  expect_equal(aucDay(w100, 7), 3120, tolerance = 0.10)
  expect_equal(aucDay(w200, 7), 7790, tolerance = 0.20)
  # more than dose proportional, from the efflux saturating
  expect_gt(aucDay(w200, 1) / aucDay(w50, 1), 5)
  # the accumulation stays modest (measured 1.38-1.72), where Zhou 2024's
  # saturable elimination gave 2.8 at 200 mg
  expect_lt(aucDay(w200, 7) / aucDay(w200, 1), 1.6)
  expect_equal_rounded(aucDay(w100, 1), 2170.2298123)
})

test_that("Zhou 2024's patients, data Lu 2024 never saw", {
  w50 <- sim(50, 14); w100 <- sim(100, 14)
  expect_equal(aucDay(w50, 1), 717, tolerance = 0.25)     # paper 717
  expect_equal(aucDay(w100, 1), 1830, tolerance = 0.25)   # paper 1830
  expect_equal(aucDay(w50, 14), 1530, tolerance = 0.15)   # paper 1530
  expect_equal(aucDay(w100, 14), 3120, tolerance = 0.15)  # paper 3120
})

test_that("the efflux block is validated", {
  expect_error(validateMichaelisMenten(list(vmax = 0, km = 1, clLinear = 1,
                                            saturableEfflux = list(vmax = -1, km = 1)),
                                       "x"), "saturableEfflux")
})

test_that("units: oral, daily; no bilirubin argument", {
  expect_equal(getDrugDefaults("cetagliptin")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_setequal(names(formals(cetagliptin)),
                  c("weight", "height", "age", "sex", "adjustToFFM"))
})
