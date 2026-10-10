# Lisdexamfetamine: d-amphetamine after Vyvanse in children (Tsuda 2020).
#
# What is guarded: the published parameters, the dose basis (capsule mass of
# lisdexamfetamine dimesylate converted once to d-amphetamine base), and the
# external check against Boellner 2010, including its known shortfall.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

oralLDX <- function(mg, weight = 36, height = 140, age = 10, sex = "male",
                    adjustToFFM = FALSE, maximum = 2880) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "lisdexamfetamine", Time = 0, Dose = mg, Units = "mg PO"),
    noEvents, weight, height, age, sex, maximum, FALSE, adjustToFFM = adjustToFFM
  )$lisdexamfetamine$wide
}


test_that("returns the correct calculations", {
  # Tsuda's 34.1 kg reference patient, switch off so the published weight
  # equations see 34.1 kg
  actual <- lisdexamfetamine(34.1, 140, 10, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = 133, v2 = 1, v3 = 1,
      cl1 = 8.96 * 1.26 / 60, cl2 = 0, cl3 = 0,
      ka_PO = 0.480 / 60,
      bioavailability_PO = 135.21 / 455.60,
      tlag_PO = 0.435 * 60
    )),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = actual$reference
  )

  expect_equal_rounded(actual, expected)
})


test_that("the weight equations are Tsuda's", {
  x <- lisdexamfetamine(55, 160, 15, "female", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(x$cl1 * 60, 8.96 * (55 / 34.1)^0.600 * 1.26)
  expect_equal_rounded(x$v1, 133 * (55 / 34.1)^0.776)
})


test_that("with the switch on the weight equations see the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y male: pharmacokinetic weight 70 x 1.3049067 kg
  pkW <- 70 * 1.3049067
  x <- lisdexamfetamine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(x$cl1 * 60, 8.96 * (pkW / 34.1)^0.600 * 1.26)
  expect_equal_rounded(x$v1, 133 * (pkW / 34.1)^0.776)
})


test_that("the capsule strength is converted to d-amphetamine base once", {
  # 30 / 50 / 70 mg capsules: 8.9 / 14.8 / 20.8 mg d-amphetamine base
  f <- lisdexamfetamine(36, 140, 10, "male")$PK$default$bioavailability_PO
  expect_equal(round(c(30, 50, 70) * f, 1), c(8.9, 14.8, 20.8))

  # AUC = base-equivalent dose / CL/F, with no second factor applied
  pk <- lisdexamfetamine(36, 140, 10, "male", adjustToFFM = FALSE)$PK$default
  w <- oralLDX(30, maximum = 20160)
  auc <- sum(diff(w$Time) * (utils::head(w$Plasma, -1) + utils::tail(w$Plasma, -1)) / 2)
  expect_equal(auc / (30 * f * 1000 / pk$cl1), 1, tolerance = 0.01)
})


test_that("the model is checked against Boellner 2010, and its shortfall is recorded", {
  # 18 US children, mean 36.0 kg: mean d-amphetamine Cmax 53.2 / 93.3 / 134.0
  # ng/mL after 30 / 50 / 70 mg, Tmax about 3.5 h.  Tsuda's typical curve on
  # the base-equivalent basis is 17-23% low and peaks at about 4.8 h; on the
  # capsule-mass basis it would be 2.6-2.8 times high.
  observed <- c(53.2, 93.3, 134.0)
  predicted <- vapply(c(30, 50, 70), function(mg) max(oralLDX(mg)$Plasma), numeric(1))
  ratio <- predicted / observed
  expect_true(all(ratio > 0.74 & ratio < 0.86))

  capsuleMassBasis <- predicted / (135.21 / 455.60)
  expect_true(all(capsuleMassBasis / observed > 2.5))

  # Linear: dose proportional, as Boellner found
  expect_equal(predicted / predicted[1], c(1, 50 / 30, 70 / 30), tolerance = 1e-6)

  w <- oralLDX(30, maximum = 1440)
  tmax_h <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax_h, 4.2)
  expect_lt(tmax_h, 5.4)
})


test_that("lisdexamfetamine is offered orally only, with no claimed effect or band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "lisdexamfetamine", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_equal(units, c("mg PO", "mg PO qd"))
  expect_true(all(doseRoute(units) == "PO"))
  expect_equal(row$Category, "Stimulants")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe), rep(0, 5))
})
