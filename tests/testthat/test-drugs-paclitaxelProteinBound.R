# Nab-paclitaxel (R/drugs_paclitaxelProteinBound.R): Chen 2014, saturable
# elimination and saturable distribution both ways.  Drafted by Claude Code,
# 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())

test_that("Table 2 in micrograms, and the three volumes are the label's 1741 L", {
  # the switch off gives the published values (on, they are anchored at the cohort)
  X <- paclitaxelProteinBound(70, 170, 50, "female", adjustToFFM = FALSE)
  d <- X$PK$default; mm <- X$michaelisMenten
  expect_equal(c(d$v1, d$v2, mm$saturableDistribution[["volume"]]), c(15.8, 75.4, 1650))
  expect_equal(d$cl2 * 60, 41.6)
  expect_equal(mm$vmax * 60 * 1000, 8070)                 # ug/h
  expect_equal(mm$km * 1000, 40.2)                        # ug/L
  expect_equal(mm$saturableDistribution$vmax * 60 * 1000, 325000)
  expect_equal(mm$saturableDistribution$km * 1000, 4260)
  expect_equal(round(d$v1 + d$v2 + mm$saturableDistribution[["volume"]]), 1741)
  expect_null(mm$saturableDistribution[["k21"]])
})

test_that("albumin on VMEL: (2/3.9)^0.554, about 30% lower at 2 g/dL; blank is 3.9", {
  ref <- paclitaxelProteinBound(70, 170, 50, "female")$michaelisMenten$vmax
  low <- paclitaxelProteinBound(70, 170, 50, "female", albumin = 2)$michaelisMenten$vmax
  expect_equal(low / ref, (2 / 3.9)^0.554)
  expect_equal(paclitaxelProteinBound(70, 170, 50, "female", albumin = 3.9)$michaelisMenten$vmax, ref)
  expect_true(is.finite(paclitaxelProteinBound(70, 170, 50, "female", albumin = 0)$michaelisMenten$vmax))
})

test_that("switch off fixed; switch on scaled to fat-free mass at Chen's cohort (Gemini audit F4)", {
  a <- paclitaxelProteinBound(50, 160, 50, "female", FALSE)
  b <- paclitaxelProteinBound(120, 180, 50, "male", FALSE)
  expect_equal(a$PK$default, b$PK$default)
  expect_equal(a$michaelisMenten, b$michaelisMenten)
  on <- function(w, h, s) paclitaxelProteinBound(w, h, 50, s)
  sz <- function(w, h, s) pkSizeFactors(w, h, 50, s, TRUE, legacyVolume = 1, legacyClearance = 1,
                                        cohort = do.call(cohortSizeFactors, PACLITAXEL_CHEN_COHORT))
  for (pt in list(list(50, 160, "female"), list(120, 180, "male"))) {
    X <- do.call(on, pt); f <- do.call(sz, pt)
    expect_equal(X$PK$default$v1, 15.8 * f$volume)
    expect_equal(X$michaelisMenten$saturableDistribution[["volume"]], 1650 * f$volume)
    expect_equal(X$michaelisMenten$vmax * 60 * 1000, 8070 * f$clearance)
    expect_equal(X$michaelisMenten$saturableDistribution$vmax * 60 * 1000, 325000 * f$clearance)
    expect_equal(X$michaelisMenten$km * 1000, 40.2)          # concentrations unscaled
  }
  expect_gt(on(120, 180, "male")$PK$default$v1, on(50, 160, "female")$PK$default$v1)
})

simulate <- function(mgPerM2, bsa = 1.8, hours = 21 * 24) {
  h <- uniroot(function(h) bsaMosteller(70, h) - bsa, c(100, 250))$root
  dose <- data.frame(Drug = "paclitaxelProteinBound", Time = c(0, 30),
                     Dose = c(mgPerM2 * bsa * 2, 0), Units = "mg/hr")
  simulateDrugsWithCovariates(dose, noEvents, 70, h, 50, "female", hours * 60,
                              FALSE, adjustToFFM = FALSE)$paclitaxelProteinBound$wide
}

test_that("the label: clearance 13-30 L/h/m2 over 80-300 mg/m2, half-life 13-27 h", {
  for (d in c(80, 300)) {
    w <- simulate(d)
    auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
    cl <- d / auc                                          # L/h/m2
    expect_true(cl > 13 && cl < 30)
    late <- w[w$Time > 72 * 60 & w$Time < 120 * 60, ]
    t12 <- log(2) / (-coef(lm(log(Plasma) ~ Time, late))[[2]] * 60)
    expect_true(t12 > 13 && t12 < 27)
  }
  # saturable elimination: clearance falls as the dose rises
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  expect_gt(auc(simulate(300)) / 300, 1.5 * auc(simulate(80)) / 80)
})

test_that("the saturable return matches an independent integration of Chen's equations", {
  p <- PACLITAXEL_CHEN
  dose <- 468                                                # mg over 0.5 h
  rhs <- function(t, a) {
    c1 <- a[1] / p$v1; c2 <- a[2] / p$v2; c3 <- a[3] / p$v3
    vmel <- p$vmel / 1000; vmtr <- p$vmtr / 1000; kmel <- p$kmel / 1000; kmtr <- p$kmtr / 1000
    inf <- if (t < 0.5) dose / 0.5 else 0
    out <- vmtr * c1 / (kmtr + c1); back <- vmtr * c2 / (kmtr + c1)   # Fig. 1: both by C1
    c(inf - vmel * c1 / (kmel + c1) - out + back - p$q2 * (c1 - c3),
      out - back, p$q2 * (c1 - c3))
  }
  # classical RK4, 0.05-min steps for the first 2 h and 0.5-min after (a
  # 0.5-min step throughout is 0.6% low at the end of the infusion), in hours
  a <- c(0, 0, 0); t <- 0; times <- c(0.5, 2, 24, 72)
  ref <- numeric(0)
  for (stop in times) {
    while (t < stop - 1e-12) {
      h <- if (t < 2 - 1e-12) 0.05 / 60 else 0.5 / 60
      k1 <- rhs(t, a); k2 <- rhs(t + h / 2, a + h / 2 * k1)
      k3 <- rhs(t + h / 2, a + h / 2 * k2); k4 <- rhs(t + h, a + h * k3)
      a <- a + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4); t <- t + h
    }
    ref <- c(ref, a[1] / p$v1)
  }
  w <- simulate(260, hours = 96)
  got <- approx(w$Time, w$Plasma, times * 60)$y
  expect_equal(got, ref, tolerance = 1e-3)
})

test_that("a chemotherapy drug of its own, plasma only, with no band, on the saturable engine", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "paclitaxelProteinBound", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_true(drugOnSaturableEngine("paclitaxelProteinBound"))
  expect_match(getDrugPK("paclitaxelProteinBound", 70, 170, 50, "male")$reference, "Chen")
  expect_identical(therapeuticRange("paclitaxelProteinBound")$band_status, "not_applicable")
})

test_that("saturableDistribution takes k21 or volume, not both and not neither", {
  X <- paclitaxelProteinBound(70, 170, 50, "female")
  bad <- X; bad$michaelisMenten$saturableDistribution$k21 <- 0.01
  expect_error(validateMichaelisMenten(bad$michaelisMenten, "x"), "either k21")
  bad$michaelisMenten$saturableDistribution[["volume"]] <- NULL; bad$michaelisMenten$saturableDistribution[["k21"]] <- NULL
  expect_error(validateMichaelisMenten(bad$michaelisMenten, "x"), "either k21")
})

test_that("Joerger's paclitaxel keeps its first-order return (no volume read from vmax)", {
  sd <- paclitaxelJoerger(70, 170, 50, "female")$michaelisMenten$saturableDistribution
  expect_null(sd[["volume"]])
  expect_true(sd$k21 > 0)
})
