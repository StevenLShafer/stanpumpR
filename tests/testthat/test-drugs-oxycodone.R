# Oxycodone: intravenous disposition pooled from Poyhia 1991, Kirvela 1996
# and Liukas 2011; age and renal covariates on clearance; oral absorption and
# ke0 from Lalovic 2006.  Expected values are worked out from the header's
# arithmetic, not read back from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))
trapz <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("the pooled disposition is on the hydrochloride dose basis", {
  # All three intravenous studies computed CL and Vss on oxycodone base (see
  # DOSE BASIS in the drug file), and each goes onto the hydrochloride by the
  # ratio of molar masses.
  base <- 315.37; hydrochloride <- 351.83; trihydrate <- 405.87
  toHCl <- hydrochloride / base
  # Poyhia 1991 Table 1: weight (kg), the tabulated dose (mg), CL (L/min) and
  # Vss (L/kg).  The tabulated dose is the base in the Oxanest (the
  # hydrochloride trihydrate) drawn up in round volumes.
  w   <- c(75, 78, 90, 80, 76, 61, 62, 61, 57)
  d   <- c(4.1, 4.3, 4.7, 4.3, 4.1, 3.1, 3.1, 3.1, 3.1)
  cl  <- c(0.59, 1.33, 1.22, 0.66, 0.38, 0.82, 0.92, 0.64, 0.45)
  vss <- c(2.12, 2.44, 2.83, 2.40, 3.65, 2.74, 2.84, 2.56, 1.83)
  drawnUp <- c(5.25, 5.5, 6.0, 5.5, 5.25, 4.0, 4.0, 4.0, 4.0)
  expect_equal(round(drawnUp * base / trihydrate, 1), d)
  # The papers' "0.05 mg/kg free base" is 0.07 mg/kg of the trihydrate, rounded
  expect_equal(round(0.07 * base / trihydrate, 2), 0.05)
  poyhiaCL  <- 1000 * mean(cl) / mean(w) * toHCl      # mL/min/kg
  poyhiaVss <- mean(vss) * toHCl
  expect_equal(poyhiaCL, 12.2, tolerance = 0.005)
  expect_equal(poyhiaVss, 2.90, tolerance = 0.005)
  # Kirvela 1996 control medians and Liukas 2011's youngest group, likewise
  pooledCL  <- (9 * poyhiaCL  + 10 * 1000 * 1.10 / 66 * toHCl + 11 * 11.9 * toHCl) / 30
  pooledVss <- (9 * poyhiaVss + 10 * 2.39 * toHCl + 11 * 3.7 * toHCl) / 30
  p <- oxycodone(70, 170, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(1000 * p$cl1 / 70, pooledCL, tolerance = 0.005)
  expect_equal((p$v1 + p$v2) / 70, pooledVss, tolerance = 0.005)
  # Bioavailability is the pooled clearance over Lalovic's CL/F
  expect_equal(p$bioavailability_PO, pooledCL / 19.7, tolerance = 0.005)
})

test_that("Liukas 2011 divided by the base, not the 5 mg of hydrochloride", {
  # Table II (CL, mL/min/kg; AUC, ug.min/mL, mean and SD) and Table I
  # (weight, mean and SD).  A mean CL/kg cannot be less than dose /
  # mean(AUC x weight), so the dose divided by is at most mean CL/kg x
  # (mean AUC x mean weight + SD(AUC) x SD(weight)).
  cl <- c(11.9, 8.6, 8.6, 7.9)
  auc <- c(5.0, 6.4, 6.9, 9.0); aucSD <- c(1.3, 1.2, 1.7, 2.2)
  w <- c(81, 83, 82, 68); wSD <- c(14, 9, 13, 8)
  maxDose <- cl * (auc * w + aucSD * wSD) / 1000      # mg
  expect_equal(round(maxDose[2], 2), 4.66)
  # To second order, the mean CL/kg expected on the 4.48 mg of base is the
  # reported one within 3%; on 5 mg it is more than 10% high in every group.
  expected <- function(dose) 1000 * dose / (auc * w) * (1 + (aucSD / auc)^2) * (1 + (wSD / w)^2)
  expect_true(all(abs(expected(5 * 315.37 / 351.83) / cl - 1) < 0.03))
  expect_true(all(expected(5) / cl > 1.10))
})

test_that("returns the pooled per-kilogram parameters with weight scaling", {
  # Switch off: per-kilogram values linear in total weight.  70 kg, 30 y, so
  # the age factor is 1; a blank creatinine at 30 y gives CKD-EPI eGFR above
  # 100, so the renal factor is 1.
  actual <- oxycodone(70, 170, 30, "male", adjustToFFM = FALSE)
  vss <- 3.27 * 70
  expected <- list(
    PK = list(
      default = list(
        v1 = 0.131 * vss,
        v2 = (1 - 0.131) * vss,
        v3 = 1,
        cl1 = 0.0147 * 70,
        cl2 = 2.33 * 0.0147 * 70,
        cl3 = 0,
        ka_PO = 0.01,
        bioavailability_PO = 0.75,
        tlag_PO = 0
      )
    ),
    tPeak = 12.36,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 24,
    lowerTypical = 9.6,
    reference = actual$reference,
    metabolite = list(
      name = "oxymorphone",
      # 1% of oxymorphone's 2.0 L/min over the molecular weight ratio, out of v1
      kFormation = 0.01 * 2.0 / (301.34 / 315.36) / (0.131 * vss),
      firstPassFraction = 0.0235 * 2.0 / ((301.34 / 315.36) * 1.379),  # 0.03567
      mwRatio = 301.34 / 315.36
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm male.  FFM ratio 1.3049067 (Al-Sallami), so volumes
  # x 1.3049067 and the reference clearance x 1.3049067^0.75 = 1.2209126.
  actual <- oxycodone(120, 170, 30, "male")$PK$default
  vss <- 3.27 * 70 * 1.3049067
  expect_equal(actual$v1, 0.131 * vss, tolerance = 1e-5)
  expect_equal(actual$v2, (1 - 0.131) * vss, tolerance = 1e-5)
  expect_equal(actual$cl1, 0.0147 * 70 * 1.2209126, tolerance = 1e-5)
  expect_equal(actual$cl2, 2.33 * 0.0147 * 70 * 1.2209126, tolerance = 1e-5)
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

test_that("follows Liukas's age effect, within 8% of Liukas's own level", {
  # Table I and II of Liukas 2011, mean weight, height, age; three groups
  # mostly men, the oldest mostly women.  Observed mL/min/kg, computed on the
  # base and put on the hydrochloride (see DOSE BASIS in the drug file).  The
  # pooled level is 11% above Liukas's young group's, so the model runs within
  # 8% of every group, and the age effect, relative to the youngest group, is
  # within 15%.
  toHCl <- 351.83 / 315.37
  groups <- list(
    list(w = 81, h = 177, a = 27.1, s = "male",   cl = 11.9),
    list(w = 83, h = 169, a = 66.3, s = "male",   cl = 8.6),
    list(w = 82, h = 169, a = 76.5, s = "male",   cl = 8.6),
    list(w = 68, h = 160, a = 83.5, s = "female", cl = 7.9)
  )
  ratio <- vapply(groups, function(g) {
    p <- oxycodone(g$w, g$h, g$a, g$s)$PK$default
    1000 * p$cl1 / g$w / (g$cl * toHCl)
  }, numeric(1))
  expect_equal(ratio, c(1.044, 1.080, 0.999, 0.920), tolerance = 0.002)
  expect_equal(ratio / ratio[1], rep(1, 4), tolerance = 0.15)
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
  # Vss 95 + 100 L, on the Oxanest (trihydrate) dose (Steven L. Shafer,
  # 2026-10-29), so on this model's hydrochloride basis:
  toHCl <- 351.83 / 405.87
  expect_equal(round(0.92 * toHCl, 2), 0.80)
  expect_equal(round(195 * toHCl), 169)
  # This model's non-pregnant values for that woman:
  pk <- oxycodone(82, 161, 29, "female")$PK$default
  expect_equal(round(pk$cl1, 2), 0.92)
  expect_equal(round(pk$v1 + pk$v2), 197)
  expect_equal(round(0.92 * toHCl / pk$cl1, 2), 0.87)
  expect_equal(round(195 * toHCl / (pk$v1 + pk$v2), 2), 0.86)
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
