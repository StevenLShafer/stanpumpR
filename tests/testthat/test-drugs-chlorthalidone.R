# Chlorthalidone (R/drugs_chlorthalidone.R), from Tsai 2016 Table 2.  Drafted
# by Claude Code, 2026-10-17, at the request of Steven L. Shafer.
#
# Two of the source's four covariates are deliberately NOT applied, for
# reasons in the drug header, and the tests pin that they are not: a later
# reader adding them must do it knowingly.

events <- data.frame(Time = numeric(0), Event = character(0))

refPatient <- function(dose, units = "mg PO", days = 28,
                       age = 58, weight = 86, sex = "female") {
  simulateDrugsWithCovariates(
    data.frame(Drug = "chlorthalidone", Time = 0, Dose = dose, Units = units),
    events, weight, 170, age, sex, days * 1440, FALSE,
    adjustToFFM = FALSE)$chlorthalidone$wide
}
aucOf <- function(w) {
  sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
}

test_that("the published parameters are carried unchanged", {
  PK <- getDrugPK("chlorthalidone", 86, 170, 58, "female",
                  adjustToFFM = FALSE)
  set <- PK$PK[[PK_EVENT_DEFAULT]]
  # The reference subject is the population median: 58 years, female.  There
  # is no prodrug and no mass conversion, unlike azilsartan in the same paper.
  expect_equal_rounded(62.1, set$v1)
  expect_equal_rounded(120, set$v2)
  expect_equal_rounded(4.13 / 60, set$cl1)
  expect_equal_rounded(36.9 / 60, set$cl2)
  expect_equal_rounded(1.13 / 60, set$ka_PO)
  expect_equal(set$bioavailability_PO, 1)
  expect_equal(set$tlag_PO, 0)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
})

test_that("the model reproduces the paper's simulated exposures", {
  # AUC: paper 2956 and 6035 ng*h/mL at 12.5 and 25 mg
  for (i in seq_along(c(12.5, 25))) {
    paper <- c(2956, 6035)[i]
    got <- aucOf(refPatient(c(12.5, 25)[i]))
    expect_lt(abs(got - paper) / paper, 0.03)
  }
  # Steady-state Cmax once daily: paper 176 and 372 ng/mL
  for (i in seq_along(c(12.5, 25))) {
    w <- refPatient(c(12.5, 25)[i], "mg PO qd")
    last <- w[w$Time >= 27 * 1440, ]
    paper <- c(176, 372)[i]
    expect_lt(abs(max(last$Plasma) - paper) / paper, 0.10)
  }
})

test_that("the two applied covariates move the right parameters", {
  setOf <- function(age, sex) {
    getDrugPK("chlorthalidone", 86, 170, age, sex,
              adjustToFFM = FALSE)$PK[[PK_EVENT_DEFAULT]]
  }
  base <- setOf(58, "female")
  # Age on clearance only, -0.0451 L/h per year about 58
  older <- setOf(75, "female")
  expect_equal_rounded((4.13 - 0.0451 * (75 - 58)) / 4.13, older$cl1 / base$cl1)
  expect_equal_rounded(base$v1, older$v1)     # age does not touch volume
  # Sex on BOTH, +25.3% on clearance and +119% on central volume
  male <- setOf(58, "male")
  expect_equal_rounded(1.253, male$cl1 / base$cl1)
  expect_equal_rounded(2.19, male$v1 / base$v1)
  expect_equal_rounded(base$v2, male$v2)      # peripheral volume is untouched
})

test_that("the cystatin C and BMI terms are deliberately absent", {
  # Neither is applied (see the drug header): cystatin C because the Patient
  # Profile has none and creatinine is not a substitute for it, BMI because
  # the printed equation cannot be read.  Both would show up as a dependence
  # on a covariate the model must currently ignore.
  #
  # Serum creatinine must not reach this model at all: it has no creatinine
  # argument, so getDrugPK cannot pass one.
  expect_false("creatinine" %in% names(formals(chlorthalidone)))
  # Height moves BMI at fixed weight.  With the BMI term absent, the central
  # volume must not depend on height except through the fat-free-mass switch,
  # which is off here.
  v <- function(h) getDrugPK("chlorthalidone", 86, h, 58, "female",
                             adjustToFFM = FALSE)$PK[[PK_EVENT_DEFAULT]]$v1
  expect_equal_rounded(v(160), v(190))
})

test_that("both switch positions give a usable parameter set", {
  for (ffm in c(FALSE, TRUE)) {
    set <- getDrugPK("chlorthalidone", 86, 170, 58, "female",
                     adjustToFFM = ffm)$PK[[PK_EVENT_DEFAULT]]
    expect_true(all(vapply(set[c("v1", "v2", "cl1", "cl2", "ka_PO")],
                           function(x) is.finite(x) && x > 0, logical(1))),
                info = as.character(ffm))
  }
})
