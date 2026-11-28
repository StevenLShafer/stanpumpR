# Caspofungin (R/drugs_caspofunginXu.R, Xu 2026; R/drugs_caspofunginWurthwein.R,
# Wurthwein 2013; the group in R/modelSelection.R).  Drafted by Claude Code,
# 2026-11-28.  The expectations are Xu's Table 2 equations and an independent
# closed-form two-compartment infusion solution, computed here, not read from
# the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
bsa <- function(w, h) sqrt(w * h / 3600)

test_that("Xu 2026's Table 2: BSA^0.66 on CL and Q, BSA^1 on V1 and V2, both switch positions", {
  for (adj in c(TRUE, FALSE)) {
    set <- caspofunginXu(16, 105, 5, "female", adjustToFFM = adj)$PK$default
    b <- bsa(16, 105) / 0.79
    expect_equal(set$cl1 * 60, 0.196 * b^0.66)
    expect_equal(set$cl2 * 60, 1.01 * b^0.66)
    expect_equal(set$v1, 2.22 * b)
    expect_equal(set$v2, 1.63 * b)
    expect_equal(set$cl3, 0)
  }
  # the reference child, 0.79 m2: Table 2 exactly; CL/V1 is the paper's ke 0.088 /h
  w <- 0.79^2 * 3600 / 110
  set <- caspofunginXu(w, 110, 5, "male")$PK$default
  expect_equal(set$cl1 * 60, 0.196)
  expect_equal(round(set$cl1 / set$v1 * 60, 3), 0.088)
  expect_equal_rounded(0.3397605, caspofunginXu(70, 170, 35, "male")$PK$default$cl1 * 60)
})

# An independent steady state: 14 daily one-hour infusions of D mg, by the
# two-compartment closed form, summed
ssReference <- function(set, dose) {
  k10 <- set$cl1 / set$v1; k12 <- set$cl2 / set$v1; k21 <- set$cl2 / set$v2
  s <- k10 + k12 + k21
  l <- c((s + sqrt(s^2 - 4 * k10 * k21)) / 2, (s - sqrt(s^2 - 4 * k10 * k21)) / 2)
  coef <- c((k21 - l[1]) / (l[2] - l[1]), (k21 - l[2]) / (l[1] - l[2])) / set$v1
  rate <- dose / 60
  one <- function(t) {   # one infusion starting at 0, lasting 60 min
    on <- pmin(t, 60)
    sum(rate * coef / l * (1 - exp(-l * on)) * exp(-l * pmax(t - 60, 0)) * (t > 0))
  }
  conc <- function(t) sum(vapply(0:13 * 1440, function(s0) one(t - s0), 0))
  conc
}

test_that("the engine reproduces the closed form at steady state, and the AUC is dose / CL", {
  dt <- do.call(rbind, lapply(0:13, function(d) data.frame(
    Drug = "caspofungin", Time = d * 1440 + c(0, 60), Dose = c(50, 0), Units = "mg/m2/hr")))
  w <- simulateDrugsWithCovariates(dt, events, 21, 116, 6, "male", 14 * 1440, FALSE)$caspofungin$wide
  set <- caspofunginXu(21, 116, 6, "male")$PK$default
  dose <- 50 * bsa(21, 116)
  ref <- ssReference(set, dose)
  for (t in 13 * 1440 + c(30, 60, 120, 600, 1439)) {
    got <- approx(w$Time, w$Plasma, t)$y
    expect_equal(got, ref(t), tolerance = 2e-3)
  }
  # the last interval's AUC is dose / CL to within the approach to steady state
  tt <- seq(13 * 1440, 14 * 1440, by = 1)
  auc <- sum(vapply(tt, ref, 0)) / 60
  expect_equal(auc, dose / (set$cl1 * 60), tolerance = 0.01)
})

test_that("Xu: the CANCIDAS label's Table 8 is not matched, as the help page says", {
  # label: harmonic-mean CL 6.4 mL/min and AUC 120 mg h/L (2-11 y, 50 mg/m2);
  # adults 10.6 mL/min and 87.3 (50 mg).  The model: about half the clearance.
  child <- caspofunginXu(21, 116, 6, "male")$PK$default
  adult <- caspofunginXu(70, 170, 35, "male")$PK$default
  expect_equal(round(child$cl1 * 1000, 1), 3.4)
  expect_equal(round(adult$cl1 * 1000, 1), 5.7)
  expect_equal(round(50 * bsa(21, 116) / (child$cl1 * 60)), 204)
  expect_equal(round(50 / (adult$cl1 * 60)), 147)
})

test_that("caspofungin offers infusion units only, as a total and per m2, and no band", {
  # The label forbids an intravenous bolus, so no amount unit is offered: a
  # dose is a rate stopped after an hour (Devin's review of PR #1018).
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "caspofungin"])
  expect_setequal(units, c("mg/hr", "mg/m2/hr"))
  expect_equal(dd$Upper[dd$Drug == "caspofungin"], 0)
  expect_equal(dd$Category[dd$Drug == "caspofungin"], "Other")
})

test_that("Wurthwein 2013's Table 2, switch off: the linear weight term on CL and V1", {
  set <- caspofunginWurthwein(90, 175, 60, "male", adjustToFFM = FALSE)$PK$default
  f <- 1 + 0.0102 * (90 - 76)
  expect_equal(set$cl1 * 60, 0.411 * f)
  expect_equal(set$v1, 5.85 * f)
  expect_equal(set$cl2 * 60, 0.843)
  expect_equal(set$v2, 6.53)
  expect_equal(set$cl3, 0)
  # the full return at the reference man, switch off
  expected <- list(PK = list(default = list(v1 = 5.49198, v2 = 6.53, v3 = 1,
                                            cl1 = 0.00643078, cl2 = 0.01405, cl3 = 0)),
                   tPeak = 0, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0,
                   reference = paste0(
      "Wurthwein G et al., Antimicrob Agents Chemother 2013;57:1664-1671, Table 2 (46 adults ",
      "with invasive aspergillosis, 18-74 years, 43-104 kg): two compartments, CL 0.411 L/h ",
      "and V1 5.85 L x [1 + 0.0102 (weight - 76 kg)], Q 0.843 L/h and V2 6.53 L scaled by ",
      "fat-free mass; total plasma caspofungin. https://doi.org/10.1128/AAC.01912-12"))
  expect_equal_rounded(expected, caspofunginWurthwein(70, 170, 35, "male", adjustToFFM = FALSE),
                       tolerance = 1.5e-6)
})

test_that("Wurthwein, switch on: the weight term at the pharmacokinetic weight, Q and V2 by FFM", {
  size <- pkSizeFactors(110, 165, 50, "female", TRUE)
  set <- caspofunginWurthwein(110, 165, 50, "female")$PK$default
  f <- 1 + 0.0102 * (size$pkWeight - 76)
  expect_equal(set$cl1 * 60, 0.411 * f)
  expect_equal(set$v1, 5.85 * f)
  expect_equal(set$cl2 * 60, 0.843 * size$clearance)
  expect_equal(set$v2, 6.53 * size$volume)
  expect_equal_rounded(list(v1 = 5.5581788, v2 = 6.6334926, cl1 = 0.0065082949, cl2 = 0.014216678),
                       set[c("v1", "v2", "cl1", "cl2")], tolerance = 1.5e-6)
  # finite down to the smallest legal patient
  tiny <- caspofunginWurthwein(0.5, 40, 0, "female")$PK$default
  expect_true(all(is.finite(unlist(tiny))) && tiny$cl1 > 0 && tiny$v1 > 0)
})

test_that("Wurthwein: Table 3's steady-state AUCs are dose / 0.411 L/h", {
  set <- caspofunginWurthwein(76, 170, 61, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(set$cl1 * 60, 0.411)
  aucs <- c(170, 243, 365, 487)
  expect_equal(round(c(70, 100, 150, 200) / (set$cl1 * 60)), aucs)
  # and the label's adults clear faster: 6.4 mL/min at 70 kg against 10.6
  adult <- caspofunginWurthwein(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(adult$cl1 * 1000, 1), 6.4)
  expect_equal(round(50 / (adult$cl1 * 60)), 130)
})

test_that("Wurthwein: the engine's steady-state peak and trough after 2-hour infusions", {
  # Table 3: geometric-mean steady-state Cmax 13.8 and Cmin 4.2 mg/L at 70 mg;
  # the typical patient should lie near them (a population geometric mean is
  # not the typical patient's, so 15% is allowed)
  dt <- do.call(rbind, lapply(0:13, function(d) data.frame(
    Drug = "caspofungin", Time = d * 1440 + c(0, 120), Dose = c(35, 0), Units = "mg/hr")))
  w <- simulateDrugsWithCovariates(dt, events, 76, 170, 61, "male", 14 * 1440, FALSE)$caspofungin$wide
  cmax <- approx(w$Time, w$Plasma, 13 * 1440 + 120)$y
  cmin <- approx(w$Time, w$Plasma, 13 * 1440 - 0.01)$y
  expect_equal(cmax, 13.8, tolerance = 0.15)
  expect_equal(cmin, 4.2, tolerance = 0.15)
})

test_that("the program chooses Wurthwein for adults and Xu for children", {
  chosen <- function(age, weight) caspofungin(weight, 170, age, "male")$modelChosen
  expect_equal(chosen(35, 70), "caspofunginWurthwein")
  expect_equal(chosen(85, 70), "caspofunginWurthwein")   # older than 74: nearest
  expect_equal(chosen(40, 140), "caspofunginWurthwein")  # heavier than 104: still nearer by age
  expect_equal(chosen(6, 21), "caspofunginXu")
  expect_equal(chosen(0.5, 7), "caspofunginXu")
  expect_equal(chosen(16, 60), "caspofunginXu")
  # at 17 years, one year from each range, weight decides
  expect_equal(chosen(17, 40), "caspofunginXu")
  expect_equal(chosen(17, 60), "caspofunginWurthwein")
  expect_equal(caspofungin(70, 170, 35, "male")$PK,
               caspofunginWurthwein(70, 170, 35, "male")$PK)
  for (d in c("caspofunginWurthwein", "caspofunginXu")) {
    dd <- getDrugDefaultsGlobal()
    expect_true(is.na(dd$Category[dd$Drug == d]) || !nzchar(dd$Category[dd$Drug == d]))
  }
})
