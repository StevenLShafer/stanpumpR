# Buprenorphine: Bjornsson 2023 disposition, one first-order sublingual input
# fitted to the published two-pathway one (intranasal withdrawn 2026-10-27), ke0
# from Yassen 2006 carried as a tPeak.  See the header of R/drugs_buprenorphine.R.  Pins were
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
        ka_SL = 0.0171905401,
        bioavailability_SL = 0.422692,
        tlag_SL = 17.587178,
        ka_IM = 0.140710,
        bioavailability_IM = 0.947096,
        tlag_IM = 0
      )
    ),
    tPeak = 61.2,
    MEAC = 0,
    typical = 0.4,
    upperTypical = 1,
    lowerTypical = 0.2,
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

test_that("the age term is evaluated at no younger than 18 years", {
  # (age/35)^-0.233 is infinite at 0; a newborn or child gets the clearance of
  # an 18-year-old of the same size: 52.1 x (18/35)^-0.233 = 60.84 L/h at
  # 72.4 kg with the switch off.
  cl <- function(a) buprenorphine(72.4, 175, a, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl(0), cl(18))
  expect_equal(cl(5), cl(18))
  expect_equal(round(cl(18), 1), 60.8)
  expect_true(is.finite(cl(0)))
  # Adults are unaffected
  expect_lt(cl(30), cl(18))
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
  # The library's analgesic band, not Hart 2026's range for opioid use
  # disorder (see the header)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0.2, 1, 0.4))
  expect_equal(row$endCe, BUPRENORPHINE_ANALGESIA_END)
  expect_equal(row$Category, "Opioids")
  routes <- unique(doseRoute(row$Units[[1]]))
  expect_setequal(routes, c(ROUTE_IV, ROUTE_SL, ROUTE_IM))
  # 70% receptor occupancy by Nasser 2014's Emax model is 2.2 ng/mL
  expect_equal(round(91.4 * 2.2 / (0.67 + 2.2), 1), 70.1)
})

simBup <- function(units, dose, maximum = 1440) {
  d <- data.frame(Drug = "buprenorphine", Time = 0, Dose = dose, Units = units)
  r <- simulateDrugsWithCovariates(d, noEvents, 72.4, 170, 35, "male",
                                   maximum, FALSE, adjustToFFM = FALSE)
  r$buprenorphine$results
}

test_that("16 mg sublingual peaks where the fitted single input does", {
  # Hand calculation on the same disposition at F 0.14, with the fitted lag:
  # Cmax 5.8589 ng/mL at 55.59 min (published two-pathway input: 6.09 ng/mL
  # at 52 min).  The dose-dependent fraction at 16 mg is 0.13843, so
  # 5.8589 x 0.13843 / 0.14 = 5.7933.
  r <- simBup("mg SL", 16)
  cp <- r[r$Site == "Plasma", ]
  i <- which.max(cp$Y)
  expect_equal(cp$Y[i], 5.7933, tolerance = 0.005)
  # The time grid is geometric from the dose, so the true peak time lies
  # between the grid points either side of the plotted maximum
  expect_true(cp$Time[i - 1] < 55.59 && 55.59 < cp$Time[i + 1])
  # Nothing reaches the plasma until the lag has passed
  expect_true(all(cp$Y[cp$Time < 17.5] == 0))
  # The concentration at 24 h, 0.2835 x 0.98880 (published 0.338)
  expect_equal(cp$Y[cp$Time == 1440], 0.2803, tolerance = 0.005)
})

test_that("the time until threshold is blank during the sublingual lag only", {
  # The cost of the lag (see the header): no state for the dose until it has
  # passed, so the readout is NA for 17.6 min after the dose, then a time.
  d <- data.frame(Drug = "buprenorphine", Time = 0, Dose = 16, Units = "mg SL")
  r <- simulateDrugsWithCovariates(d, noEvents, 72.4, 170, 35, "male",
                                   600, TRUE, adjustToFFM = FALSE)
  rec <- r$buprenorphine$results
  rec <- rec[rec$Site == "Recovery", ]
  expect_true(all(is.na(rec$Y[rec$Time > 0 & rec$Time < 17.5])))
  expect_false(anyNA(rec$Y[rec$Time > 18]))
})

test_that("the intranasal route is withdrawn", {
  # Eriksen 1989 measured a 1.77 ng/mL peak after 0.3 mg; the route this
  # disposition gave peaked at 0.447, and F would have had to be 1.91 to
  # close it.  Withdrawn 2026-10-27 (docs/opioid-review-2026-10-27.md).
  dd <- getDrugDefaultsGlobal()
  units <- dd$Units[[which(dd$Drug == "buprenorphine")]]
  expect_false(any(grepl(" IN\\b", units)))
  expect_null(buprenorphine(70, 170, 40, "male")$PK$default$ka_IN)
  expect_equal(round(0.482 * 1.77 / 0.4471, 2), 1.91)
})

test_that("the effect site peaks 61.2 minutes after an intravenous bolus", {
  # Yassen 2006's ke0 0.00447 /min peaks the effect site at 61.18 min on
  # Yassen's own disposition (V 8.39/23.3/127 L, CL 1.45, Q 1.55/0.953 L/min);
  # carried as that tPeak (Minto 2003), it solves to ke0 0.010678 /min on
  # Bjornsson's.  Peak Ce per mg 1.9611 ng/mL (worked out by hand from the
  # sums of exponentials).  The naive pairing peaked at 134 min.
  expect_equal(getDrugPK("buprenorphine", 72.4, 170, 35, "male",
                         adjustToFFM = FALSE)$PK$default$ke0,
               0.010678, tolerance = 0.001)
  r <- simBup("mg", 1, 600)
  ce <- r[r$Site == "Effect Site", ]
  i <- which.max(ce$Y)
  expect_equal(ce$Y[i], 1.9611, tolerance = 0.002)
  expect_true(abs(ce$Time[i] - 61.2) < 5)
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

test_that("sublingual saturation is unaffected by intravenous rows in the same table", {
  # The saturation step once multiplied the sublingual doses by a conversion
  # vector one entry per row of the whole table, which R recycled with a
  # warning whenever other routes were present.  The mixed table must run
  # clean and equal the sum of its parts.
  run <- function(rows) {
    d <- data.frame(Drug = "buprenorphine", Time = c(0, 60, 120)[rows],
                    Dose = c(0.3, 8, 2)[rows],
                    Units = c("mg", "mg SL", "mg SL")[rows])
    r <- simulateDrugsWithCovariates(d, noEvents, 72.4, 170, 35, "male",
                                     600, FALSE, adjustToFFM = FALSE)$buprenorphine$results
    r <- r[r$Site == "Plasma", ]
    approx(r$Time, r$Y, c(100, 200, 400))$y
  }
  expect_no_warning(mixed <- run(1:3))
  expect_equal(mixed, run(1) + run(2:3), tolerance = 1e-3)
})

test_that("pregnancy: elimination clearance x 1.83, absorption unchanged (Zhu 2025)", {
  expect_equal(BUPRENORPHINE_PREGNANCY_CL_FACTOR, round(218 / 119, 2))
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.
  on  <- getDrugPK("buprenorphine", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("buprenorphine", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_equal(off, getDrugPK("buprenorphine", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default
  b <- off$PK$default
  expect_equal(a$cl1 / b$cl1, BUPRENORPHINE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  for (p in c("v1", "v2", "v3", "cl2", "cl3", "ka_SL", "bioavailability_SL", "tlag_SL")) {
    expect_identical(a[[p]], b[[p]], info = p)
  }
  expect_identical(on$sublingualSaturation, off$sublingualSaturation)
  expect_equal_rounded(1.486028542, a$cl1)
  expect_equal_rounded(51.83191192, a$v1)
  # the 61.2 min time to peak effect is held; ke0 is solved again
  expect_equal(on$tPeak, off$tPeak)
  expect_equal_rounded(0.007938672353, a$ke0)
  expect_match(on$reference, "Zhu Y", fixed = TRUE)
  expect_match(on$reference, "pregnancy:", fixed = TRUE)
  expect_no_match(off$reference, "Zhu Y")
})

test_that("the band reproduces its derivation from McQuay 1980's demand analgesia", {
  # 0.3 mg IV at induction and 0.3 mg 3 h later, IV in group V and IM in group
  # M; analgesia ended a median 480 min after the second dose, the same in both
  # groups.  The study's average patient: 65 y, 66 kg.
  for (second in c("mg", "mg IM")) {
    d <- data.frame(Drug = "buprenorphine", Time = c(0, 180), Dose = 0.3,
                    Units = c("mg", second))
    w <- simulateDrugsWithCovariates(d, noEvents, 66, 170, 65, "male", 900,
                                     FALSE)$buprenorphine$wide
    ce <- w[["Effect Site"]]
    # The lower edge and threshold: the effect site when demands resumed
    expect_equal(round(approx(w$Time, ce, 180 + 480)$y, 2),
                 BUPRENORPHINE_ANALGESIA_END, info = second)
    # The upper edge: the peak after the second dose
    expect_equal(round(max(ce[w$Time > 180]), 1), 1, info = second)
  }
})

test_that("the intramuscular route reproduces Bullingham 1980's IM/IV plasma ratios", {
  # Phase II of Bullingham 1980, Table II: 0.3 mg IM or IV to the same
  # patients (62 y, 66.5 kg on average); the group means' ratio at each time.
  plasma <- function(units) {
    d <- data.frame(Drug = "buprenorphine", Time = 0, Dose = 0.3, Units = units)
    w <- simulateDrugsWithCovariates(d, noEvents, 66.5, 170, 62.1, "male", 200,
                                     FALSE)$buprenorphine$wide
    approx(w$Time, w[["Plasma"]], c(2, 10, 60, 180))$y
  }
  ratio <- plasma("mg IM") / plasma("mg")
  measured <- c(3.56 / 14.62, 3.31 / 3.15, 0.79 / 0.77, 0.36 / 0.38)
  expect_equal(round(ratio, 2), c(0.25, 0.94, 1.03, 0.99))
  # within 12% of every measured ratio; the largest miss is at 10 min, where
  # a single first-order input still rises too slowly (see the header)
  expect_true(all(abs(log(ratio / measured)) < 0.12))
  expect_equal(BUPRENORPHINE_F_IM, 0.947096)
  expect_equal(BUPRENORPHINE_KA_IM, 0.140710)
})
