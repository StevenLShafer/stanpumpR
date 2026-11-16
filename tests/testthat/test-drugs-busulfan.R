# Busulfan (R/drugs_busulfan.R): McCune 2014, two compartments on normal fat
# mass with maturation, and a clearance that steps down during the course.
# Drafted by Claude Code, 2026-11-06.

p <- BUSULFAN_MCCUNE
noEvents <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)

test_that("the model is Table 2's equations at the patient's NFM and PMA", {
  w <- 60; h <- 165; age <- 30
  ffm <- busulfanFFM(w, h, "female")
  expect_equal(ffm, 37.99 * 1.65^2 * w / (35.98 * 1.65^2 + w))
  m <- busulfan(w, h, age, "female")$PK$default
  nfmCL <- ffm + 0.509 * (w - ffm); nfmV <- ffm + 0.203 * (w - ffm)
  pma <- 40 + 52 * age
  fmat <- pma^2.3 / (45.7^2.3 + pma^2.3)
  expect_equal_rounded(m$cl1, 11.4 * (nfmCL / 62)^0.75 * fmat / 60)
  expect_equal_rounded(m$cl2, 135.2 * (nfmCL / 62)^0.75 / 60)
  expect_equal_rounded(m$v1, 13.9 * nfmV / 59 * 1.07)
  expect_equal_rounded(m$v2, 29.9 * nfmV / 59 * 1.07)
})

test_that("maturation is half at 45.7 weeks PMA and reads the gestational age", {
  cl <- function(age, ga = NULL) busulfan(4, 52, age, "male", gestationalAge = ga)$PK$default$cl1
  # 45.7 weeks PMA at term is 5.7 weeks after birth
  term <- cl(5.7 / 52); adult <- busulfan(4, 52, 30, "male")$PK$default$cl1
  expect_equal(term / adult, 0.5 / (30 * 52 + 40)^2.3 * (45.7^2.3 + (30 * 52 + 40)^2.3),
               tolerance = 1e-9)
  expect_lt(cl(0.1, 30), cl(0.1))          # a preterm birth: less mature
  # the paper: 95% of adult clearance at about 2.5 years
  pma <- 40 + 52 * 2.5
  expect_gt(pma^2.3 / (45.7^2.3 + pma^2.3), 0.94)
})

test_that("Table S7's worked dose reproduces its target despite its FFM", {
  # S7 case 2: 40 y, 70 kg, 176 cm man, 204 mg/day for Css 770 ng/mL
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "busulfan", Time = c(0, 180), Dose = c(204 / 3, 0), Units = "mg/hr"),
    noEvents, 70, 176, 40, "male", 2 * 1440, FALSE)$busulfan$wide
  expect_equal(auc(wide) / 60 / 24, 770, tolerance = 0.05)
  # S7 case 1: 1 y, 10 kg, 76 cm girl, CL 2.2 L/h
  expect_equal(busulfan(10, 76, 1, "female")$PK$default$cl1 * 60, 2.2, tolerance = 0.03)
  # but S7's FFM is not the Methods' equation: 49.7 there, 56.1 here
  expect_equal(round(busulfanFFM(70, 176, "male"), 1), 56.1)
})

test_that("clearance falls to 0.932 at 6 h and 0.919 at 36 h after the first dose", {
  PK <- getDrugPK("busulfan", 70, 176, 40, "male")
  expect_identical(PK$courseSteps$after, c(360, 2160))
  base <- PK$PK$default$cl1
  expect_equal(PK$PK$busulfanCourse6h$cl1 / base, 0.932)
  expect_equal(PK$PK$busulfanCourse36h$cl1 / base, 0.919)
  expect_identical(PK$pkEvents, PK_EVENT_DEFAULT)   # internal, not user events
  # a continuous infusion settles at the plateau of each step in turn
  # (terminal half-life about 2.6 h; the steps at 6 h and 36 h after the start)
  rate <- 10
  sim <- function(start) simulateDrugsWithCovariates(
    data.frame(Drug = "busulfan", Time = start, Dose = rate, Units = "mg/hr"),
    noEvents, 70, 176, 40, "male", 3 * 1440, FALSE)$busulfan$wide
  at <- function(wide, t) wide$Plasma[which.min(abs(wide$Time - t))]
  css <- function(cl) rate / (cl * 60) * 1000          # ng/mL
  w0 <- sim(0)
  expect_lt(abs(at(w0, 35.9 * 60) / css(base * 0.932) - 1), 0.01)
  expect_lt(abs(at(w0, 70 * 60) / css(base * 0.919) - 1), 0.01)
  # without the steps the curve would not rise after 6 h: it does, by the step
  expect_gt(at(w0, 35.9 * 60), at(w0, 6 * 60))
  # the steps follow the first dose, not time zero: a start 2 h later shifts
  # the whole curve by 2 h
  w2 <- sim(120)
  for (t in c(5, 20, 40, 60) * 60)
    expect_equal(at(w2, t + 120), at(w0, t), tolerance = 1e-4)
})

test_that("both switch positions give the published model; extremes stay finite", {
  expect_equal(busulfan(90, 170, 50, "female", adjustToFFM = TRUE),
               busulfan(90, 170, 50, "female", adjustToFFM = FALSE))
  for (args in list(list(0.5, 30, 0, "male"), list(250, 200, 90, "female"))) {
    m <- do.call(busulfan, args)$PK
    for (set in m)
      expect_true(all(is.finite(unlist(set))) && set$cl1 > 0 && set$v1 > 0)
  }
})

test_that("busulfan is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "busulfan", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("busulfan", 70, 176, 40, "male")
  expect_equal(PK$tPeak, 0)
  expect_identical(PK$Concentration.Units, "ng")
  expect_match(PK$reference, "McCune")
  expect_identical(therapeuticRange("busulfan")$band_status, "not_applicable")
})
