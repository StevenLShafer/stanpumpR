# Caspofungin (R/drugs_caspofungin.R; Xu 2026).  Drafted by Claude Code,
# 2026-11-28.  The expectations are Xu's Table 2 equations and an independent
# closed-form two-compartment infusion solution, computed here, not read from
# the drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
bsa <- function(w, h) sqrt(w * h / 3600)

test_that("Xu 2026's Table 2: BSA^0.66 on CL and Q, BSA^1 on V1 and V2, both switch positions", {
  for (adj in c(TRUE, FALSE)) {
    set <- caspofungin(16, 105, 5, "female", adjustToFFM = adj)$PK$default
    b <- bsa(16, 105) / 0.79
    expect_equal(set$cl1 * 60, 0.196 * b^0.66)
    expect_equal(set$cl2 * 60, 1.01 * b^0.66)
    expect_equal(set$v1, 2.22 * b)
    expect_equal(set$v2, 1.63 * b)
    expect_equal(set$cl3, 0)
  }
  # the reference child, 0.79 m2: Table 2 exactly; CL/V1 is the paper's ke 0.088 /h
  w <- 0.79^2 * 3600 / 110
  set <- caspofungin(w, 110, 5, "male")$PK$default
  expect_equal(set$cl1 * 60, 0.196)
  expect_equal(round(set$cl1 / set$v1 * 60, 3), 0.088)
  expect_equal_rounded(0.3397605, caspofungin(70, 170, 35, "male")$PK$default$cl1 * 60)
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
  set <- caspofungin(21, 116, 6, "male")$PK$default
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

test_that("the CANCIDAS label's Table 8 is not matched, as the help page says", {
  # label: harmonic-mean CL 6.4 mL/min and AUC 120 mg h/L (2-11 y, 50 mg/m2);
  # adults 10.6 mL/min and 87.3 (50 mg).  The model: about half the clearance.
  child <- caspofungin(21, 116, 6, "male")$PK$default
  adult <- caspofungin(70, 170, 35, "male")$PK$default
  expect_equal(round(child$cl1 * 1000, 1), 3.4)
  expect_equal(round(adult$cl1 * 1000, 1), 5.7)
  expect_equal(round(50 * bsa(21, 116) / (child$cl1 * 60)), 204)
  expect_equal(round(50 / (adult$cl1 * 60)), 147)
})

test_that("caspofungin offers its label's and practice's units and no band", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "caspofungin"])
  expect_true(all(c("mg", "mg/hr", "mg/m2", "mg/m2/hr") %in% units))
  expect_equal(dd$Upper[dd$Drug == "caspofungin"], 0)
  expect_equal(dd$Category[dd$Drug == "caspofungin"], "Other")
})
