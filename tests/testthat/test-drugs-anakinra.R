# anakinra: see R/drugs_anakinra.R (Urien 2013, Table 1 and the Results).
urien <- function(bw) list(cl = 0.847 * bw^0.47 / 60, v = 2.581 * bw^0.76)

test_that("the Results' equations, switch off", {
  for (w in c(4.3, 21, 70, 120)) {
    pk <- anakinra(w, 150, 10, "male", FALSE)$PK$default
    ref <- urien(w)
    expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v))
    expect_equal(pk$ka_SC, 0.38 / 60)
    expect_equal(pk$bioavailability_SC, 1)
  }
})

test_that("Table 1's values at 70 kg and the derived checks", {
  pk <- anakinra(70, 175, 35, "male", FALSE)$PK$default
  expect_equal(pk$cl1 * 60, 6.24, tolerance = 0.002)
  expect_equal(pk$v1, 65.2, tolerance = 0.002)
  k <- pk$cl1 / pk$v1; ka <- pk$ka_SC
  expect_equal(log(2) / k / 60, 7.24, tolerance = 0.002)          # half-life, h
  expect_equal(log(ka / k) / (ka - k) / 60, 4.85, tolerance = 0.002) # tmax after one dose, h
  # Figure 4's dose for a mean Css of 0.4 mg/L: 2.4 mg/kg/day at 10 kg, 1 at 50 kg
  dose <- function(w) urien(w)$cl * 60 * 0.4 * 24 / w
  expect_equal(dose(10), 2.4, tolerance = 0.02)
  expect_equal(dose(50), 1.0, tolerance = 0.03)
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 170, 40, "female", TRUE)
  pk <- anakinra(110, 170, 40, "female", TRUE)$PK$default
  expect_equal(pk$v1, urien(size$pkWeight)$v)
  expect_equal(pk$cl1, urien(size$pkWeight)$cl)
})

test_that("3 mg/kg SC daily at 20 kg: the steady state against NOMID's", {
  PK <- getDrugPK("anakinra", 20, 115, 6, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "anakinra", Time = 0, Dose = 3,
                          Units = "mg/kg SC qd"),
               data.frame(Time = numeric(0), Event = character(0)), PK,
               14 * 1440, FALSE)$wide
  day <- w$Time >= 12 * 1440 & w$Time <= 13 * 1440
  peak <- max(w$Plasma[day]); trough <- min(w$Plasma[day])
  # NOMID (label 12.3): median C24h 203 (53-1979), Cmax 3628 (655-8511) ng/mL
  expect_equal(trough, 142, tolerance = 0.03)
  expect_equal(peak, 1421, tolerance = 0.03)
  expect_true(trough > 53 && trough < 1979 && peak > 655 && peak < 8511)
})

# The intravenous route: Ogungbenro 2016, Table 1 (updated model), a parallel
# system that takes only intravenous doses
ogungbenro <- c(v1 = 5.74, v2 = 3.56, cl1 = 7.86 / 60, cl2 = 0.65 / 60)
ivOf <- function(...) {
  p <- getDrugPK("anakinra", ...)$parallelSystems[[1]]
  expect_equal(p$routes, ROUTE_IV)
  unlist(p$PK$default[c("v1", "v2", "cl1", "cl2")])
}
noEvents <- data.frame(Time = numeric(0), Event = character(0))

cohortIV <- do.call(cohortSizeFactors, ANAKINRA_IV_COHORT)
test_that("intravenous: Table 1 anchored at Galea 2011's cohort, the published values with the switch off", {
  ref <- pkSizeFactors(70, 170, 35, "male", TRUE)
  expect_equal(ivOf(70, 170, 35, "male"),
               ogungbenro * c(1 / cohortIV$volume, 1 / cohortIV$volume,
                              1 / cohortIV$clearance, 1 / cohortIV$clearance))
  expect_equal(ivOf(70, 170, 35, "male")[["cl1"]] * 60, 8.723, tolerance = 1e-3)
  expect_equal(ivOf(50, 160, 60, "female", adjustToFFM = FALSE), ogungbenro)
  size <- pkSizeFactors(110, 165, 50, "female", TRUE)
  expect_equal(ivOf(110, 165, 50, "female"),
               ogungbenro * c(size$volume / cohortIV$volume, size$volume / cohortIV$volume,
                              size$clearance / cohortIV$clearance,
                              size$clearance / cohortIV$clearance))
  # the cohort: 7 men and 18 women of 73.4 kg and 53 years, NCD-RisC's UK heights
  expect_equal(unlist(cohortIV), c(volume = 0.8703, clearance = 0.9010), tolerance = 1e-3)
  expect_equal(getDrugPK("anakinra", 70, 170, 35, "male")$routes, ROUTE_SC)
})

test_that("intravenous: 100 mg then 2 mg/kg/h reaches its steady state; each route runs on its own model", {
  PK <- getDrugPK("anakinra", 70, 170, 35, "male")
  iv <- data.frame(Drug = "anakinra", Time = c(0, 0), Dose = c(100, 2),
                   Units = c("mg", "mg/kg/hr"))
  w <- simCpCe(iv, noEvents, PK, 2880, FALSE)$wide
  p <- PK$parallelSystems[[1]]$PK$default
  expect_equal(w$Plasma[w$Time == 2880], 140 / (p$cl1 * 60) * 1000, tolerance = 2e-3)  # ng/mL
  expect_equal(max(w$Plasma[w$Time <= 0.01]), 100 / p$v1 * 1000, tolerance = 1e-3)
  # an SC dose is untouched by the IV system, an IV dose by the SC one
  sc <- data.frame(Drug = "anakinra", Time = 0, Dose = 100, Units = "mg SC")
  wSC <- simCpCe(sc, noEvents, PK, 1440, FALSE)$wide
  wBoth <- simCpCe(rbind(sc, data.frame(Drug = "anakinra", Time = 0, Dose = 100, Units = "mg")),
                   noEvents, PK, 1440, FALSE)$wide
  wIV <- simCpCe(data.frame(Drug = "anakinra", Time = 0, Dose = 100, Units = "mg"),
                 noEvents, PK, 1440, FALSE)$wide
  t <- intersect(intersect(wSC$Time, wIV$Time), wBoth$Time)
  t <- t[t > 0][round(seq(1, sum(t > 0), length.out = 4))]
  pick <- function(x) sapply(t, function(s) x$Plasma[x$Time == s][1])
  expect_equal(pick(wBoth), pick(wSC) + pick(wIV), tolerance = 1e-8)
  ref <- anakinra(70, 170, 35, "male")$PK$default
  k <- ref$cl1 / ref$v1; ka <- ref$ka_SC
  expect_equal(pick(wSC), 100 / ref$v1 * ka / (ka - k) *
                 (exp(-k * t) - exp(-ka * t)) * 1000, tolerance = 1e-6)
})

test_that("intravenous: against Yang 2003's healthy men, which it was not fitted to", {
  p <- ivOf(70, 170, 35, "male")
  expect_equal(p[["cl1"]] * 60 / 8.22, 1.061, tolerance = 1e-3)   # CL 137 mL/min
  k10 <- p[["cl1"]] / p[["v1"]]; k12 <- p[["cl2"]] / p[["v1"]]; k21 <- p[["cl2"]] / p[["v2"]]
  b <- k10 + k12 + k21
  hl <- log(2) / ((b + c(1, -1) * sqrt(b^2 - 4 * k10 * k21)) / 2) / 60
  expect_equal(hl, c(0.4790, 4.301), tolerance = 1e-3)
})

test_that("intravenous: against Fisher 1994's patients with sepsis, about 0.55 of the model", {
  cl <- c(17, 67, 133) / c(3.54, 14.8, 25.2)            # L/h, rate / mean concentration
  expect_equal(cl, c(4.802, 4.527, 5.278), tolerance = 1e-3)
  p <- ivOf(70, 170, 35, "male")
  expect_true(all(cl / (p[["cl1"]] * 60) > 0.5 & cl / (p[["cl1"]] * 60) < 0.62))
})
