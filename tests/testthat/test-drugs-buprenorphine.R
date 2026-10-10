# Buprenorphine: Bjornsson 2023 disposition, one first-order sublingual input
# fitted to the published two-pathway one, intranasal from Eriksen 1989, ke0
# from Yassen 2006.  See the header of R/drugs_buprenorphine.R.  Pins were
# worked out from the published numbers by hand (Python), not from the code
# under test.  (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  # CL = 52.1 x (50/35)^-0.233 x (70/72.4)^0.413 L/h = 47.282 L/h
  actual <- buprenorphine(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 64.3,
        v2 = 130,
        v3 = 1580,
        cl1 = 0.78803912,
        cl2 = 3.1,
        cl3 = 1.005,
        ka_SL = 0.0129498493,
        bioavailability_SL = 0.422692,
        tlag_SL = 0,
        ka_IN = 0.0227053660,
        bioavailability_IN = 0.482,
        tlag_IN = 0
      )
    ),
    tPeak = 0,
    ke0 = 0.00447,
    MEAC = 0,
    typical = 2.2,
    upperTypical = 3,
    lowerTypical = 1.25,
    sublingualSaturation = list(Imax = 0.816527, ID50 = 3.42670),
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("reproduces the paper's worked clearance examples", {
  # Bjornsson 2023: 60.8 L/h at 18 y, 45.1 at 65 y (72.4 kg); 44.7 at 50 kg,
  # 59.5 at 100 kg (35 y).  Switch off, so clearance sees total weight.
  cl <- function(w, a) buprenorphine(w, 175, a, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(round(c(cl(72.4, 18), cl(72.4, 65), cl(50, 35), cl(100, 35)), 1),
               c(60.8, 45.1, 44.7, 59.5))
  expect_equal(cl(72.4, 35), 52.1)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126,
  # pharmacokinetic weight 70 x 1.3049067 = 91.34 kg for the CL covariate.
  actual <- buprenorphine(120, 170, 50, "male")
  expected <- list(
    v1 = 83.9055008,
    v2 = 169.637871,
    v3 = 2061.752586,
    cl1 = 0.87959367,
    cl2 = 3.7848291,
    cl3 = 1.2270172
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the sublingual fraction absorbed follows the paper's power law", {
  # 18.1%, 14.0% and 12.0% at 8, 16 and 24 mg (Bjornsson 2023, Results)
  expect_equal(round(buprenorphineSublingualF(c(8, 16, 24)), 3), c(0.181, 0.140, 0.120))
  # The Imax form fitted to it agrees within 2.5% over 2-32 mg ...
  fitted <- function(d) BUPRENORPHINE_F_SL *
    oralSaturationFraction(d, BUPRENORPHINE_SL_SATURATION)
  d <- exp(seq(log(2), log(32), length.out = 50))
  expect_lt(max(abs(fitted(d) / buprenorphineSublingualF(d) - 1)), 0.025)
  # ... with the values worked out by hand (Python)
  expect_equal(round(fitted(c(2, 8, 16, 32)), 4), c(0.2955, 0.1811, 0.1384, 0.1109))
  # and levels off below 2 mg rather than rising without limit
  # (0.4037 at 0.2 mg, against the power law's 0.711)
  expect_equal(round(fitted(0.2), 4), 0.4037)
  expect_lt(fitted(0.2), BUPRENORPHINE_F_SL)
  expect_null(validateOralSaturation(NULL, "buprenorphine", "sublingualSaturation"))
  expect_error(validateOralSaturation(list(Imax = 2, ID50 = 1), "x", "sublingualSaturation"),
               "sublingualSaturation")
})

test_that("the CSV row matches the model's band and offers no MEAC", {
  # The plot and the recovery threshold read the CSV, not the drug function.
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "buprenorphine", ]
  out <- buprenorphine(70, 170, 40, "male")
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, out$lowerTypical)
  expect_equal(row$Upper, out$upperTypical)
  expect_equal(row$Typical, out$typical)
  expect_equal(row$endCe, BUPRENORPHINE_WITHDRAWAL)
  expect_equal(row$Category, "Opioids")
  routes <- unique(doseRoute(row$Units[[1]]))
  expect_setequal(routes, c(ROUTE_IV, ROUTE_SL, ROUTE_IN))
  # 70% receptor occupancy by Nasser 2014's Emax model is the typical value
  expect_equal(round(91.4 * 2.2 / (0.67 + 2.2), 1), 70.1)
})

simBup <- function(units, dose, maximum = 1440) {
  d <- data.frame(Drug = "buprenorphine", Time = 0, Dose = dose, Units = units)
  r <- simulateDrugsWithCovariates(d, noEvents, 72.4, 170, 35, "male",
                                   maximum, FALSE, adjustToFFM = FALSE)
  r$buprenorphine$results
}

test_that("16 mg sublingual peaks where the fitted single input does", {
  # Hand calculation on the same disposition at F 0.14: Cmax 4.932 ng/mL at
  # 49.65 min (published two-pathway input: 6.09 ng/mL at 52 min).  The
  # dose-dependent fraction at 16 mg is 0.13843, so 4.932 x 0.13843 / 0.14.
  r <- simBup("mg SL", 16)
  cp <- r[r$Site == "Plasma", ]
  i <- which.max(cp$Y)
  expect_equal(cp$Y[i], 4.8768, tolerance = 0.005)
  expect_true(abs(cp$Time[i] - 49.65) < 2)
  # The concentration at 24 h, 0.2837 x 0.98880 (published 0.338)
  expect_equal(cp$Y[cp$Time == 1440], 0.2805, tolerance = 0.005)
})

test_that("0.3 mg intranasal peaks at Eriksen's 30.6 minutes", {
  r <- simBup("mg IN", 0.3, 240)
  cp <- r[r$Site == "Plasma", ]
  i <- which.max(cp$Y)
  expect_true(abs(cp$Time[i] - 30.6) < 2)
  expect_equal(cp$Y[i], 0.4471, tolerance = 0.005)
})

test_that("the effect site peaks about 134 minutes after an intravenous bolus", {
  # ke0 0.00447 /min against this disposition; Escher 2007 saw maximum
  # antinociception at 120 min after 0.15 mg.  Peak Ce per mg 1.1715 ng/mL.
  r <- simBup("mg", 1, 600)
  ce <- r[r$Site == "Effect Site", ]
  i <- which.max(ce$Y)
  expect_equal(ce$Y[i], 1.1715, tolerance = 0.002)
  expect_true(abs(ce$Time[i] - 134.2) < 15)
})

test_that("each sublingual dose is scaled by its own fraction, and scheduled doses repeat", {
  # 8 mg absorbs 0.18105, 16 mg 0.13843: doubling the dose raises the
  # concentrations by 2 x 0.13843 / 0.18105 = 1.5292, not 2
  one <- simBup("mg SL", 8)
  two <- simBup("mg SL", 16)
  expect_equal(two$Y[two$Site == "Plasma"], 1.5292 * one$Y[one$Site == "Plasma"],
               tolerance = 1e-4)
  # The same dose in mcg is scaled by its size in mg, not its number
  mcg <- simBup("mcg SL", 8000)
  expect_equal(mcg$Y[mcg$Site == "Plasma"], one$Y[one$Site == "Plasma"])
  daily <- simBup("mg SL qd", 16, 3 * 1440)
  cp <- daily[daily$Site == "Plasma", ]
  # The second day starts from a non-zero trough and peaks higher than the first
  expect_gt(max(cp$Y[cp$Time > 1440 & cp$Time < 2880]), max(cp$Y[cp$Time < 1440]))
})
