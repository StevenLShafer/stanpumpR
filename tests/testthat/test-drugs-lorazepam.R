# lorazepam: see the header of R/drugs_lorazepam.R for the model (Barr 2001
# disposition, anchored at its cohort; Greenblatt 1982 absorption; Greenblatt
# 2000 effect site) and why Nielsen-Kudsk 1983, Gonzalez 2017 and Swart 2004
# are not used.  Pins were worked out from the published numbers by hand (the
# fat-free-mass formula and the cohort's factors written out again), not from
# the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

lorazepamRun <- function(dose, units, maximum = 1440) {
  dt <- data.frame(Drug = "lorazepam", Time = 0, Dose = dose, Units = units)
  simulateDrugsWithCovariates(dt, noEvents, 70, 170, 35, "male", maximum,
                              FALSE)$lorazepam$wide
}

# Amount-state matrix solution: depot, central, peripheral.  ng/mL out.
twoComp <- function(t, D, p, route = c("IV", "PO", "IM")) {
  route <- match.arg(route)
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  ka <- if (route == "IV") 0 else p[[paste0("ka_", route)]]
  F  <- if (route == "IV") 1 else p[[paste0("bioavailability_", route)]]
  M <- rbind(c(-ka, 0, 0), c(ka, -(k10 + k12), k21), c(0, k12, -k21))
  x0 <- if (route == "IV") c(0, D, 0) else c(F * D, 0, 0)
  e <- eigen(M)
  coef <- solve(e$vectors, x0)
  vapply(t, function(tt) {
    Re(sum(e$vectors[2, ] * coef * exp(e$values * tt))) / p$v1 * 1000
  }, numeric(1))
}


# Barr 2001, Table 4 (absolute) and Table 2 (the lorazepam group, n = 14).
barr <- list(v1 = 40.8, v2 = 102, cl1 = 0.107, cl2 = 1.86)
barrCohort <- list(weight = 85, height = 176, age = 74, men = 13, women = 1)

test_that("the reference patient: Barr's Table 4 over his cohort's size factors", {
  # Switch on: Table 4 divided by the cohort's factors (13 men and 1 woman at
  # 85 kg, 176 cm, 74 y: volume 1.131261187, clearance 1.096912954, worked
  # out by hand from Al-Sallami's equation).  Switch off: Table 4 as printed.
  pk <- list(
    on = list(v1 = 36.0659416916, v2 = 90.1648542291,
              cl1 = 0.0975464823, cl2 = 1.6956678230),
    off = barr)
  for (adjust in c(TRUE, FALSE)) {
    actual <- lorazepam(70, 170, 35, "male", adjustToFFM = adjust)
    d <- pk[[if (adjust) "on" else "off"]]
    expected <- list(
      PK = list(default = list(
        v1 = d$v1, v2 = d$v2, v3 = 1,
        cl1 = d$cl1, cl2 = d$cl2, cl3 = 0,
        ka_PO = 0.0213276056, bioavailability_PO = 0.9, tlag_PO = 0,
        ka_IM = 0.0488131817, bioavailability_IM = 0.96, tlag_IM = 0
      )),
      tPeak = 18.1,
      MEAC = 0,
      typical = 104,
      upperTypical = 152,
      lowerTypical = 51,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
  k <- cohortFactorsIndependent(barrCohort)
  expect_equal_rounded(c(k$volume, k$clearance), c(1.131261187, 1.096912954))
})


test_that("the cohort's typical patient receives Barr's published values", {
  # The geometric mean of the cohort's man and woman, weighted 13 to 1.
  man   <- lorazepam(85, 176, 74, "male")$PK$default
  woman <- lorazepam(85, 176, 74, "female")$PK$default
  for (p in names(barr)) {
    expect_equal(exp((13 * log(man[[p]]) + log(woman[[p]])) / 14), barr[[p]],
                 tolerance = 1e-10, info = p)
  }
})


test_that("Barr's model gives his 952-minute elimination half-life", {
  # Discussion: "a much longer elimination half-life for lorazepam (952 min)".
  p <- lorazepam(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  a <- k10 + k12 + k21
  r <- c(a + sqrt(a^2 - 4 * k10 * k21), a - sqrt(a^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / r[2], 952, tolerance = 1e-3)
  expect_equal(p$v1 + p$v2, 142.8)                       # Vss, as derived
})


test_that("published values with the switch off, fat-free mass with it on", {
  off <- lorazepam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off[c("v1", "v2", "cl1", "cl2")], barr)
  # 120 kg, 170 cm, 50 y man: fat-free-mass ratio 1.30490670 (volumes) and
  # its 0.75 power (clearances), over the cohort's factors.
  on <- lorazepam(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on[c("v1", "v2", "cl1", "cl2")],
                       list(v1 = 47.0626870123, v2 = 117.6567175309,
                            cl1 = 0.1190957311, cl2 = 2.0702622418))
})


test_that("the time to peak effect gives back Greenblatt's 8.8 min equilibration", {
  pk <- getDrugPK("lorazepam", 70, 170, 35, "male")
  expect_equal(log(2) / pk$PK$default$ke0, 8.8, tolerance = 0.005)
})


test_that("each route is the two-compartment solution", {
  p <- lorazepam(70, 170, 35, "male")$PK$default
  for (r in list(c("mg", "IV"), c("mg PO", "PO"), c("mg IM", "IM"))) {
    w <- lorazepamRun(2, r[1])
    expect_equal(w$Plasma, twoComp(w$Time, 2, p, r[2]), tolerance = 1e-8,
                 info = r[1])
  }
})


test_that("the reference patient against the label at each route", {
  p <- lorazepam(70, 170, 35, "male")$PK$default
  # Barr's ICU patients sit below the label's volunteer figures (the header's
  # finding, not a fit).  4 mg IV: about 70 ng/mL initially (label); 58.9 at
  # 15 min
  expect_equal(twoComp(15, 4, p, "IV"), 58.86, tolerance = 1e-3)
  t <- seq(0, 300, by = 0.1)
  # 2 mg by mouth: about 20 ng/mL at about 2 h (label); 13.8 at 54 min
  po <- twoComp(t, 2, p, "PO")
  expect_equal(max(po), 13.84, tolerance = 1e-3)
  expect_equal(t[which.max(po)], 54.1, tolerance = 2e-3)
  # 4 mg IM: about 48 ng/mL (label); 42.3 at 25 min
  im <- twoComp(t, 4, p, "IM")
  expect_equal(max(im), 42.30, tolerance = 1e-3)
  expect_equal(t[which.max(im)], 25.1, tolerance = 2e-3)
})


test_that("lorazepam is offered IV, by mouth and IM, in Hypnotics and sedatives", {
  dd <- getDrugDefaultsGlobal(FALSE)
  units <- strsplit(dd$Units[dd$Drug == "lorazepam"], ",")[[1]]
  expect_true(all(c("mg", "mg/kg", "mg/hr", "mg PO", "mg IM") %in% units))
  expect_false(any(doseRoute(units) == ROUTE_IN))
  expect_equal(dd$Category[dd$Drug == "lorazepam"], "Hypnotics and sedatives")
})


test_that("the CSV row agrees with the drug function and Barr's C50s", {
  dd  <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "lorazepam", ]
  x   <- lorazepam(70, 170, 35, "male")
  expect_equal(row$Lower, x$lowerTypical)
  expect_equal(row$Upper, x$upperTypical)
  expect_equal(row$Typical, x$typical)
  # Barr 2001: C50 for Ramsay >= 3, 4, 5 = 51, 104, 152 ng/mL (moderate sedation)
  expect_equal(c(row$Lower, row$Typical, row$Upper), c(51, 104, 152))
  expect_equal(row$endCe, 51)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Concentration.Units, "ng")
})
