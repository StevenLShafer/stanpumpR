# The insulins' shared effect site (R/insulinEffectSite.R; Shafer, 2026-11-19).

noEvents <- data.frame(Time = numeric(0), Event = character(0))

effectPeakHours <- function(drug, units) {
  # one-minute output times: the native grid is too coarse to find a peak
  o <- withOutputTimes(0:(12 * 60), simulateDrugsWithCovariates(
    data.frame(Drug = drug, Time = 0, Dose = units, Units = "U SC"),
    noEvents, 70, 177, 35, "male", 24 * 60, FALSE))
  w <- o[[drug]]$wide
  w$Time[which.max(w$`Effect Site`)] / 60
}

test_that("ke0 reproduces the 23-minute intravenous peak on Howey's and Galloway's disposition", {
  # Two exponentials: Galloway 1992's 5.1-min fast phase, Howey 1994's
  # 46.3-min terminal phase, the shares set by Howey's 32-min mean residence time.
  l <- log(2) / c(5.1, 46.3)
  mrt <- function(f) { a <- c(f, 1 - f); sum(a / l^2) / sum(a / l) }
  f <- uniroot(function(f) mrt(f) - 32, c(0.01, 0.99), tol = 1e-10)$root
  a <- c(f, 1 - f)
  k <- INSULIN_KE0 / 60
  ce <- function(t) sum(a * k / (k - l) * (exp(-l * t) - exp(-k * t)))
  peak <- optimize(ce, c(1, 120), maximum = TRUE, tol = 1e-8)$maximum
  expect_equal(peak, 23, tolerance = 0.01)
})

test_that("every insulin carries the same ke0 and no tPeak", {
  insulins <- c("insulinRegular", "insulinRegularU500", "insulinNPH",
                "insulinHumanMix70", "insulinLispro", "insulinLisproMix75",
                "insulinLisproMix50", "insulinGlargine", "insulinAspart",
                "insulinAspartFiasp", "insulinDegludec")
  for (d in insulins) {
    m <- get(d)(70, 170, 35, "male")
    expect_null(m$tPeak); expect_equal(m$ke0, 0.876 / 60, info = d)
  }
})

test_that("subcutaneous effect peaks fall near the measured glucose-infusion peaks", {
  # Measured: regular 179 min (Howey), 3.0 h (Woodworth); lispro 99-116 min;
  # NPH 5.4-5.9 h.  The model (2026-11-19): 2.5, 2.2 and 5.0 h.
  expect_equal(effectPeakHours("insulinRegular", 10), 2.53, tolerance = 0.05)
  expect_equal(effectPeakHours("insulinLispro", 10), 2.20, tolerance = 0.05)
  expect_equal(effectPeakHours("insulinNPH", 25), 5.02, tolerance = 0.05)
  # the order the clamp studies measured: lispro, then regular, then NPH
  expect_lt(effectPeakHours("insulinLispro", 10), effectPeakHours("insulinRegular", 10))
  expect_lt(effectPeakHours("insulinRegular", 10), effectPeakHours("insulinNPH", 25))
})
