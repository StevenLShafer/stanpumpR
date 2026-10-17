# Azilsartan (R/drugs_azilsartan.R), from Tsai 2016 Table 1.  Drafted by
# Claude Code, 2026-10-17, at the request of Steven L. Shafer.
#
# The pins that matter here are not the arithmetic but the DOSE BASIS: the
# paper never says whether its doses are azilsartan or azilsartan medoxomil,
# and the whole curve is 20% out if that is read wrongly.  The derivation is
# in the drug header; these tests pin both the conversion and the exposures
# the paper simulated, which are the evidence for it.

events <- data.frame(Time = numeric(0), Event = character(0))

# The source's own reference subject: 58 years, 86 kg, the population medians
refPatient <- function(drug, dose, units = "mg PO", days = 28,
                       age = 58, weight = 86, sex = "female") {
  simulateDrugsWithCovariates(
    data.frame(Drug = drug, Time = 0, Dose = dose, Units = units),
    events, weight, 170, age, sex, days * 1440, FALSE,
    adjustToFFM = FALSE)[[drug]]$wide
}
aucOf <- function(w) {
  sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
}

test_that("the published parameters are on a per-medoxomil basis", {
  PK <- getDrugPK("azilsartan", 86, 170, 58, "female", adjustToFFM = FALSE)
  set <- PK$PK[[PK_EVENT_DEFAULT]]
  # Molecular weight ratio, azilsartan over azilsartan medoxomil
  expect_equal_rounded(456.45 / 568.52, AZILSARTAN_MOIETY)
  # Every volume and clearance is the published value divided by it
  expect_equal_rounded(3.98 / AZILSARTAN_MOIETY, set$v1)
  expect_equal_rounded(15.5 / AZILSARTAN_MOIETY, set$v2)
  expect_equal_rounded(1.47 / AZILSARTAN_MOIETY / 60, set$cl1)
  expect_equal_rounded(0.591 / AZILSARTAN_MOIETY / 60, set$cl2)
  # The rate constants are therefore EXACTLY as published: the conversion
  # moves the concentration scale and nothing else.
  expect_equal_rounded(1.47 / 3.98 / 60, set$cl1 / set$v1)
  expect_equal_rounded(0.195 / 60, set$ka_PO)
  # Apparent model: the dose is not multiplied by F a second time
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$tlag_PO, 0)
})

test_that("it is plasma only, with no band and no effect site", {
  PK <- getDrugPK("azilsartan", 86, 170, 58, "female", adjustToFFM = FALSE)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
  expect_equal(PK$upperTypical, 0)
  expect_equal(PK$lowerTypical, 0)
})

test_that("the dose basis reproduces the paper's simulated AUC", {
  # AUC is dose over clearance for a linear model, so this is the test that
  # discriminates azilsartan from azilsartan medoxomil as the entered mass.
  # Paper: 10919, 21675 and 42606 ng*h/mL at 20, 40 and 80 mg of AZL-M.
  for (i in seq_along(c(20, 40, 80))) {
    dose <- c(20, 40, 80)[i]
    paper <- c(10919, 21675, 42606)[i]
    got <- aucOf(refPatient("azilsartan", dose))
    expect_lt(abs(got - paper) / paper, 0.03)
    # And it is NOT what the medoxomil mass would give, which is 25% higher
    expect_gt(abs(got - dose * 1000 / 1.47) / (dose * 1000 / 1.47), 0.15)
  }
})

test_that("the steady-state peak matches the paper's simulation", {
  # Paper: 963, 1913 and 3738 ng/mL once daily at steady state.  These are
  # population means over the covariate distribution and this is the typical
  # subject, so a few percent apart is expected and agreement is the point.
  for (i in seq_along(c(20, 40, 80))) {
    w <- refPatient("azilsartan", c(20, 40, 80)[i], "mg PO qd")
    last <- w[w$Time >= 27 * 1440, ]
    paper <- c(963, 1913, 3738)[i]
    expect_lt(abs(max(last$Plasma) - paper) / paper, 0.05)
  }
})

test_that("the covariates move clearance the way the source says", {
  clOf <- function(age, weight, ffm = FALSE) {
    PK <- getDrugPK("azilsartan", weight, 170, age, "female",
                    adjustToFFM = ffm)
    PK$PK[[PK_EVENT_DEFAULT]]$cl1
  }
  base <- clOf(58, 86)
  # Age: -0.0149 L/h per year about 58, so a 75 year old clears less
  expect_equal_rounded((1.47 - 0.0149 * (75 - 58)) / 1.47, clOf(75, 86) / base)
  expect_lt(clOf(75, 86), base)
  expect_gt(clOf(40, 86), base)
  # Weight: a power of 0.267 on weight/86
  expect_equal_rounded((124 / 86)^0.267, clOf(58, 124) / base)
  expect_equal_rounded((58 / 86)^0.267, clOf(58, 58) / base)
  # The paper's own statement, Figure 2 and p996: exposure changes by up to
  # about 20% EITHER WAY ACROSS 40 TO 75 YEARS RELATIVE TO THE REFERENCE
  # subject, not by 20% between the two extremes.  Exposure goes as 1/CL.
  expect_lt(abs(base / clOf(40, 86) - 1), 0.25)   # young: lower exposure
  expect_lt(abs(base / clOf(75, 86) - 1), 0.25)   # old: higher exposure
  expect_gt(base / clOf(75, 86), 1.10)
  expect_lt(base / clOf(40, 86), 0.90)
})

test_that("the fat-free-mass switch leaves the reference subject's shape", {
  # With the switch on the package's own scaling replaces the published
  # weight term; the rate constants must stay positive and finite either way.
  for (ffm in c(FALSE, TRUE)) {
    PK <- getDrugPK("azilsartan", 86, 170, 58, "female", adjustToFFM = ffm)
    set <- PK$PK[[PK_EVENT_DEFAULT]]
    expect_true(all(vapply(set[c("v1", "v2", "cl1", "cl2", "ka_PO")],
                           function(x) is.finite(x) && x > 0, logical(1))),
                info = as.character(ffm))
  }
})

test_that("the peak is on firm ground and the tail is an extrapolation", {
  # Checked against the LABEL, not against the paper: reproducing a source's
  # own simulated exposures tests the transcription and says nothing about
  # how the model behaves outside the window it was fitted in.
  w <- refPatient("azilsartan", 40, days = 14)
  tmax <- w$Time[which.max(w$Plasma)] / 60
  expect_gt(tmax, 1.5)          # labelled time to peak is 1.5 to 3 h
  expect_lt(tmax, 3.2)
  # The terminal half-life is about 26 h against a labelled 11 to 12, because
  # nothing in the dataset observes the drug past 24 h and the two parameters
  # that set the terminal slope, Q and V2/F, were FIXED rather than fitted.
  # Pinned so that the caveat in the drug header stays true of the code.
  tail_ <- w[w$Time > 2880 & w$Plasma > 0, ]
  halfLife <- log(2) / -coef(lm(log(tail_$Plasma) ~ tail_$Time))[2] / 60
  expect_gt(halfLife, 20)
  expect_lt(halfLife, 32)
})
