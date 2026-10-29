# Oxycodone: intravenous disposition pooled from Poyhia 1991, Kirvela 1996
# and Liukas 2011; age and renal covariates on clearance; oral absorption and
# ke0 from Lalovic 2006.  Expected values are worked out from the header's
# arithmetic, not read back from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("the pooled disposition is on the hydrochloride dose basis", {
  # Poyhia 1991 Table 1: weight (kg), the tabulated free-base dose (mg), CL
  # (L/min) and Vss (L/kg).  Each patient was given 0.07 mg/kg of the
  # hydrochloride, so each CL and Vss is scaled by 0.07 x weight / dose.
  w   <- c(75, 78, 90, 80, 76, 61, 62, 61, 57)
  d   <- c(4.1, 4.3, 4.7, 4.3, 4.1, 3.1, 3.1, 3.1, 3.1)
  cl  <- c(0.59, 1.33, 1.22, 0.66, 0.38, 0.82, 0.92, 0.64, 0.45)
  vss <- c(2.12, 2.44, 2.83, 2.40, 3.65, 2.74, 2.84, 2.56, 1.83)
  # The table's doses are the paper's 0.05 mg/kg free base, not the 0.07 given
  expect_equal(mean(d / w), 0.053, tolerance = 0.01)
  f <- 0.07 * w / d
  poyhiaCL  <- 1000 * mean(cl * f) / mean(w)      # mL/min/kg
  poyhiaVss <- mean(vss * f)
  expect_equal(poyhiaCL, 14.5, tolerance = 0.005)
  expect_equal(poyhiaVss, 3.46, tolerance = 0.005)
  # Kirvela 1996 control medians, x 0.07 / 0.05; Liukas 2011 unchanged
  pooledCL  <- (9 * poyhiaCL  + 10 * 16.7 * 1.4 + 11 * 11.9) / 30
  pooledVss <- (9 * poyhiaVss + 10 * 2.39 * 1.4 + 11 * 3.7) / 30
  p <- oxycodone(70, 170, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(1000 * p$cl1 / 70, pooledCL, tolerance = 0.005)
  expect_equal((p$v1 + p$v2) / 70, pooledVss, tolerance = 0.005)
  # Bioavailability is the pooled clearance over Lalovic's CL/F
  expect_equal(p$bioavailability_PO, pooledCL / 19.7, tolerance = 0.005)
})

test_that("returns the pooled per-kilogram parameters with weight scaling", {
  # Switch off: per-kilogram values linear in total weight.  70 kg, 30 y, so
  # the age factor is 1; a blank creatinine at 30 y gives CKD-EPI eGFR above
  # 100, so the renal factor is 1.
  actual <- oxycodone(70, 170, 30, "male", adjustToFFM = FALSE)
  vss <- 3.51 * 70
  expected <- list(
    PK = list(
      default = list(
        v1 = 0.131 * vss,
        v2 = (1 - 0.131) * vss,
        v3 = 1,
        cl1 = 0.0165 * 70,
        cl2 = 2.33 * 0.0165 * 70,
        cl3 = 0,
        ka_PO = 0.01,
        bioavailability_PO = 0.84,
        tlag_PO = 0
      )
    ),
    tPeak = 12.09,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 24,
    lowerTypical = 9.6,
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
  vss <- 3.51 * 70 * 1.3049067
  expect_equal(actual$v1, 0.131 * vss, tolerance = 1e-5)
  expect_equal(actual$v2, (1 - 0.131) * vss, tolerance = 1e-5)
  expect_equal(actual$cl1, 0.0165 * 70 * 1.2209126, tolerance = 1e-5)
  expect_equal(actual$cl2, 2.33 * 0.0165 * 70 * 1.2209126, tolerance = 1e-5)
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

test_that("follows Liukas's age effect, above Liukas's own level", {
  # Table I and II of Liukas 2011, mean weight, height, age; three groups
  # mostly men, the oldest mostly women.  Observed mL/min/kg.  Liukas's
  # clearances are on the hydrochloride as given; the pooled level is 39%
  # above its young group's once Poyhia and Kirvela are put on that basis
  # (see DOSE BASIS in the drug file), so the model runs 15-35% above every
  # group, and the age effect, relative to the youngest group, is within 15%.
  groups <- list(
    list(w = 81, h = 177, a = 27.1, s = "male",   cl = 11.9),
    list(w = 83, h = 169, a = 66.3, s = "male",   cl = 8.6),
    list(w = 82, h = 169, a = 76.5, s = "male",   cl = 8.6),
    list(w = 68, h = 160, a = 83.5, s = "female", cl = 7.9)
  )
  ratio <- vapply(groups, function(g) {
    p <- oxycodone(g$w, g$h, g$a, g$s)$PK$default
    1000 * p$cl1 / g$w / g$cl
  }, numeric(1))
  expect_equal(ratio, c(1.308, 1.353, 1.250, 1.152), tolerance = 0.002)
  expect_equal(ratio / ratio[1], rep(1, 4), tolerance = 0.15)
})

test_that("the terminal half-life is 3.3 h in the reference adult", {
  p <- getDrugPK("oxycodone", 70, 170, 30, "male",
                 getDrugDefaults("oxycodone"))$PK$default
  expect_equal(log(2) / p$lambda_2 / 60, 3.28, tolerance = 0.01)
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

test_that("oral liquid is the tablet's absorption, entered as prescribed", {
  # The ROXICODONE label and the FDA review of NDA 200535 find oxycodone
  # oral solution and immediate-release tablets bioequivalent, so the liquid
  # is not a separate formulation.  Run through the metabolite engine, which
  # oxycodone takes because it forms oxymorphone.
  PK <- getDrugPK("oxycodone", 20, 115, 6, "female")
  expect_null(PK$oralFormulations)
  events <- data.frame(Time = numeric(0), Event = character(0))
  run <- function(dose, units)
    simCpCe(data.frame(Drug = "oxycodone", Time = 0, Dose = dose, Units = units),
            events, PK, 12 * 60, plotRecovery = FALSE)
  tablet <- run(2, "mg PO")
  expect_equal(run(2, "mg PO liquid")$results, tablet$results)
  # 0.1 mg/kg in a 20 kg child is 2 mg, oxymorphone included.
  liquidKg <- run(0.1, "mg/kg PO liquid")
  expect_equal(liquidKg$results, tablet$results)
  expect_equal(liquidKg$metaboliteSeries, tablet$metaboliteSeries)
  units <- getDrugDefaults("oxycodone")$Units[[1]]
  expect_true(all(c("mg PO liquid", "mg/kg PO liquid", "mg PO liquid qid",
                    "mg/kg PO liquid qid") %in% units))
})

test_that("the Pregnant field changes nothing: Kokki 2012 against this model (R/pregnancy.R)", {
  # The factor 3.8/2.6 was dropped on 2026-10-28 (see the header): the model
  # does not name `pregnant`, so getDrugPK() never passes it.
  expect_false("pregnant" %in% names(formals(oxycodone)))
  # Kokki 2012's labouring women, median 82 kg, 161 cm, 29 y: CL 0.92 L/min,
  # Vss 95 + 100 L.  This model's non-pregnant values for that woman:
  pk <- oxycodone(82, 161, 29, "female")$PK$default
  expect_equal(round(pk$cl1, 2), 1.03)
  expect_equal(round(0.92 / pk$cl1, 2), 0.89)
  expect_equal(round(195 / (pk$v1 + pk$v2), 2), 0.92)
})

test_that("intravenous infusion and target-controlled infusion are offered (issue #131)", {
  defaults <- getDrugDefaults("oxycodone")
  expect_equal(defaults$Bolus.Units, "mg")
  expect_equal(defaults$Infusion.Units, "mg/hr")
  expect_true(all(c("mg", "mg/hr", TCI_UNIT_PLASMA, TCI_UNIT_EFFECT) %in% defaults$Units[[1]]))
  PK <- getDrugPK("oxycodone", 70, 170, 40, "male")
  expect_true(suggestSupported(PK))
  # A 6 mg/hr infusion for an hour is 6 mg: the plasma AUC is the dose over
  # clearance, run through the metabolite engine with oxymorphone attached
  infusion <- simCpCe(
    data.frame(Drug = "oxycodone", Time = c(0, 60), Dose = c(6, 0), Units = "mg/hr"),
    noEvents, PK, 48 * 60, plotRecovery = FALSE)
  plasma <- infusion$results[infusion$results$Site == "Plasma", ]
  expect_equal(trapz(plasma$Time, plasma$Y) / 1000,
               6 / PK$PK[[1]]$cl1, tolerance = 0.01)   # mg.min/L over L/min
  # An effect-site target of 20 ng/mL is reached and held
  tci <- simCpCe(
    data.frame(Drug = "oxycodone", Time = c(0, 120), Dose = c(20, 0), Units = TCI_UNIT_EFFECT),
    noEvents, PK, 180, plotRecovery = FALSE)
  ce <- tci$results[tci$results$Site == "Effect Site", ]
  expect_equal(stats::approx(ce$Time, ce$Y, c(30, 60, 110))$y, c(20, 20, 20), tolerance = 1e-3)
  expect_lte(max(ce$Y[ce$Time <= 120]), 20 * 1.001)
})
