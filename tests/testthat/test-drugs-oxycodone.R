# Oxycodone: intravenous disposition pooled from Poyhia 1991, Kirvela 1996
# and Liukas 2011; age and renal covariates on clearance; oral absorption and
# ke0 from Lalovic 2006.  Expected values are worked out from the header's
# arithmetic, not read back from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("returns the pooled per-kilogram parameters with weight scaling", {
  # Switch off: per-kilogram values linear in total weight.  70 kg, 30 y, so
  # the age factor is 1; a blank creatinine at 30 y gives CKD-EPI eGFR above
  # 100, so the renal factor is 1.
  actual <- oxycodone(70, 170, 30, "male", adjustToFFM = FALSE)
  vss <- 2.93 * 70
  expected <- list(
    PK = list(
      default = list(
        v1 = 0.131 * vss,
        v2 = (1 - 0.131) * vss,
        v3 = 1,
        cl1 = 0.0132 * 70,
        cl2 = 2.33 * 0.0132 * 70,
        cl3 = 0,
        ka_PO = 0.01,
        bioavailability_PO = 0.67,
        tlag_PO = 0
      )
    ),
    tPeak = 12.35,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 9.6,
    lowerTypical = 24,
    reference = actual$reference,
    metabolite = list(
      name = "oxymorphone",
      # 1% of oxymorphone's 2.0 L/min over the molecular weight ratio, out of v1
      kFormation = 0.01 * 2.0 / (301.34 / 315.36) / (0.131 * vss),
      firstPassFraction = 0.0357,
      mwRatio = 301.34 / 315.36
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm male.  FFM ratio 1.3049067 (Al-Sallami), so volumes
  # x 1.3049067 and the reference clearance x 1.3049067^0.75 = 1.2209126.
  actual <- oxycodone(120, 170, 30, "male")$PK$default
  vss <- 2.93 * 70 * 1.3049067
  expect_equal(actual$v1, 0.131 * vss, tolerance = 1e-5)
  expect_equal(actual$v2, (1 - 0.131) * vss, tolerance = 1e-5)
  expect_equal(actual$cl1, 0.0132 * 70 * 1.2209126, tolerance = 1e-5)
  expect_equal(actual$cl2, 2.33 * 0.0132 * 70 * 1.2209126, tolerance = 1e-5)
})

test_that("clearance falls with age above 30 years (Liukas 2011)", {
  # Creatinine fixed low enough that eGFR stays above 100 at every age, so
  # only the age factor moves.
  cl <- function(age) oxycodone(70, 170, age, "male", adjustToFFM = FALSE,
                                creatinine = 0.5)$PK$default$cl1
  expect_equal(cl(20), cl(30))
  expect_equal(cl(80) / cl(30), 1 - 0.00527 * 50, tolerance = 1e-9)
  # Volumes do not change with age
  v <- function(age) oxycodone(70, 170, age, "male", adjustToFFM = FALSE)$PK$default$v2
  expect_equal(v(85), v(30))
})

test_that("renal failure reduces clearance by a quarter (Kirvela 1996)", {
  # Kirvela's uraemic patients: creatinine 644 micromol/L = 7.29 mg/dL.  The
  # renal factor is 1 - 0.27 x (1 - eGFR/100).
  scr  <- 644 / 88.4
  egfr <- egfrCKDEPI2009(36, "male", scr)
  expect_lt(egfr, 10)
  ref <- oxycodone(67, 175, 36, "male", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  esrd <- oxycodone(67, 175, 36, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(esrd$cl1 / ref$cl1, 1 - 0.27 * (1 - egfr / 100), tolerance = 1e-9)
  expect_equal(esrd$cl1 / ref$cl1, 0.75, tolerance = 0.02)
  # Distribution and volumes are not adjusted
  expect_equal(esrd$cl2, ref$cl2)
  expect_equal(esrd$v1 + esrd$v2, ref$v1 + ref$v2)
})

test_that("reproduces Liukas's clearance by age group to within 15%", {
  # Table I and II of Liukas 2011, mean weight, height, age; three groups
  # mostly men, the oldest mostly women.  Observed mL/min/kg.
  groups <- list(
    list(w = 81, h = 177, a = 27.1, s = "male",   cl = 11.9),
    list(w = 83, h = 169, a = 66.3, s = "male",   cl = 8.6),
    list(w = 82, h = 169, a = 76.5, s = "male",   cl = 8.6),
    list(w = 68, h = 160, a = 83.5, s = "female", cl = 7.9)
  )
  for (g in groups) {
    p <- oxycodone(g$w, g$h, g$a, g$s)$PK$default
    expect_equal(1000 * p$cl1 / g$w / g$cl, 1, tolerance = 0.15)
  }
})

test_that("the terminal half-life is 3.4 h in the reference adult", {
  p <- getDrugPK("oxycodone", 70, 170, 30, "male",
                 getDrugDefaults("oxycodone"))$PK$default
  expect_equal(log(2) / p$lambda_2 / 60, 3.43, tolerance = 0.01)
  # And ke0 is Lalovic's t1/2 ke0 of 11 min
  expect_equal(p$ke0, log(2) / 11, tolerance = 0.001)
})

test_that("the oral curve peaks near 30 ng/mL after 15 mg", {
  # Lalovic 2006: the mean concentration curve peaks near 30 ng/mL; AUC
  # 10.8 ug.min/mL in 73 kg adults aged 21-30.  Without a lag the model peaks
  # at 35 min, earlier than Lalovic's mean tmax of 65 min.
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "oxycodone", Time = 0, Dose = 15, Units = "mg PO"),
    noEvents, 73, 175, 25, "male", 1440, FALSE)
  w <- o$oxycodone$wide
  expect_equal(w$Time[which.max(w$Plasma)], 35, tolerance = 0.1)
  # ka was set on the average of a man and a woman of Lalovic's size; the
  # man alone peaks a little lower.
  expect_equal(max(w$Plasma), 30, tolerance = 0.1)
  auc <- trapz(w$Time, w$Plasma) / 1000
  expect_equal(auc / 10.8, 1, tolerance = 0.12)
})

test_that("oxymorphone forms at 1% of oxycodone IV and 3.35% orally", {
  ratio <- function(units, cyp2d6 = CYP2D6_DEFAULT) {
    o <- simulateDrugsWithCovariates(
      data.frame(Drug = "oxycodone", Time = 0, Dose = 10, Units = units),
      noEvents, 70, 170, 30, "male", 2880, FALSE, cyp2d6 = cyp2d6)
    trapz(o$oxymorphone$wide$Time, o$oxymorphone$wide$Plasma) /
      trapz(o$oxycodone$wide$Time, o$oxycodone$wide$Plasma)
  }
  expect_equal(ratio("mg") / 0.01, 1, tolerance = 0.02)
  expect_equal(ratio("mg PO") / 0.0335, 1, tolerance = 0.02)
  # Balyan 2017: intermediate about 0.63 of normal
  expect_equal(ratio("mg PO", "intermediate") / ratio("mg PO"), 0.65, tolerance = 0.03)
})
