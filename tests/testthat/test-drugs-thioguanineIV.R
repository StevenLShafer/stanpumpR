# Thioguanine, intravenous (R/drugs_thioguanineIV.R): Kitchen 1997, two
# compartments with Michaelis-Menten elimination.  Drafted by Claude Code,
# 2026-11-13.

noEvents <- data.frame(Time = double(), Event = character())

test_that("the Results' parameters, per m2, in mg", {
  bsa <- bsaMosteller(50, 160)
  X <- thioguanineIV(50, 160, 16, "male", FALSE)
  expect_equal(X$PK$default$v1, 56 * bsa)
  mm <- X$michaelisMenten
  expect_equal(mm$vmax * 60, 3.7 * 56 * bsa * 0.16719)          # mg/h
  expect_equal(mm$km, 2.5 * 0.16719)
  expect_equal(c(mm$k23, mm$k32) * 60, c(0.42, 0.29))
})

test_that("24-h infusions reproduce the paper's two clearances through its own formula", {
  bsa <- bsaMosteller(50, 160)
  X <- thioguanineIV(50, 160, 16, "male", FALSE)
  css <- sapply(c(10, 20), function(r) {
    dose <- data.frame(Drug = "thioguanineIV", Time = c(0, 1440), Dose = c(r * bsa, 0),
                       Units = "mg/hr")
    w <- simulateDrugsWithCovariates(dose, noEvents, 50, 160, 16, "male", 1500, FALSE,
                                     adjustToFFM = FALSE)$thioguanineIV$wide
    approx(w$Time, w$Plasma, 1440)$y / 0.16719                    # uM
  })
  expect_equal(round(css, 2), c(1.01, 3.18))
  cl <- 3.7 * 56 / (2.5 + css) * 1000 / 60                         # mL/min/m2
  expect_equal(round(cl), c(985, 609))
  expect_equal(cl, c(987, 608), tolerance = 0.003)                # the paper's, rounded
})

test_that("an independent RK4 of Fig. 1's equations agrees with the engine", {
  bsa <- bsaMosteller(50, 160)
  p <- THIOGUANINE_KITCHEN
  vc <- p$vc * bsa; vm <- p$vm; km <- p$km                         # L, uM/h, uM
  R <- 20 * bsa / 0.16719                                          # umol/h
  f <- function(x, t) {
    k0 <- if (t < 24) R else 0
    c(k0 / vc - vm * x[1] / (km + x[1]) - p$kcp * x[1] + p$kpc * x[2] / vc,
      p$kcp * vc * x[1] - p$kpc * x[2])
  }
  h <- 0.001; x <- c(0, 0); t <- 0; at <- c(6, 24, 26, 30); ref <- numeric(0)
  for (T in at) {
    while (t < T - 1e-9) {
      k1 <- f(x, t); k2 <- f(x + h / 2 * k1, t + h / 2); k3 <- f(x + h / 2 * k2, t + h / 2)
      k4 <- f(x + h * k3, t + h); x <- x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4); t <- t + h
    }
    ref <- c(ref, x[1])
  }
  dose <- data.frame(Drug = "thioguanineIV", Time = c(0, 1440), Dose = c(20 * bsa, 0),
                     Units = "mg/hr")
  w <- simulateDrugsWithCovariates(dose, noEvents, 50, 160, 16, "male", 31 * 60, FALSE,
                                   adjustToFFM = FALSE)$thioguanineIV$wide
  expect_equal(approx(w$Time, w$Plasma, at * 60)$y / 0.16719, ref, tolerance = 1e-3)
})

test_that("both switch positions are pinned", {
  on <- thioguanineIV(90, 170, 20, "male")$PK$default
  off <- thioguanineIV(90, 170, 20, "male", FALSE)$PK$default
  w <- pkSizeFactors(90, 170, 20, "male", TRUE)$pkWeight
  expect_equal(on$v1, 56 * bsaMosteller(w, 170))
  expect_equal(off$v1, 56 * bsaMosteller(90, 170))
})

test_that("thioguanineIV is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "thioguanineIV", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("thioguanineIV", 50, 160, 16, "male")$reference, "Kitchen")
  expect_identical(therapeuticRange("thioguanineIV")$band_status, "not_applicable")
})
