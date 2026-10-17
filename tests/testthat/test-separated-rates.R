# Every closed-form coefficient divides by a difference of two rate constants.
# Exact ties (ke0 == lambda_i, ka == lambda_i, ka == ke0) are nudged apart by
# separateEigenvalues() in getDrugPK() and the helpers, so that no engine ever
# divides by zero.  (Steven L. Shafer, 2026-10-12: "exponents cannot be
# identical or you get a divide by zero error"; the audit of that date found
# the one unguarded site, in calculateCe(), which test-calculateCe.R covers.)

test_that("separateEigenvalues nudges only an exact tie, and leaves zeros alone", {
  expect_equal(separateEigenvalues(0.5, c(0.1, 0.5)), 0.5 * (1 + 1e-8))
  expect_identical(separateEigenvalues(0.5, c(0.1, 0.4)), 0.5)
  expect_identical(separateEigenvalues(0, c(0.1, 0.5)), 0)          # no route
  expect_identical(separateEigenvalues(0.5, c(0, 0.1)), 0.5)        # no compartment
  expect_identical(separateEigenvalues(0.5, numeric(0)), 0.5)
  expect_identical(separateEigenvalues(0.5, NULL), 0.5)
  expect_equal(separateEigenvalues(c(0.1, 0.3), c(0.3, 0.7)), c(0.1, 0.3 * (1 + 1e-8)))
})

test_that("the oral, peak-time and tPeak helpers are finite at an exact tie", {
  pk <- getDrugPK("propofol", 70, 170, 40, "male")$PK$default
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)

  for (tie in c(lambda, pk$ke0)) {
    oral <- oralCoefficients(pk, ka = tie, bioavailability = 1)
    expect_true(all(is.finite(unlist(oral))), info = paste("ka =", tie))
  }
  expect_true(is.finite(effectSitePeakTime(
    c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3), lambda, pk$lambda_2)))
  expect_true(is.finite(ke0FromTPeakBolus(
    2, c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3), lambda)))
})

test_that("getDrugPK separates ke0 and every ka from the eigenvalues it returns", {
  # A one-compartment model whose ke0 is handed in equal to its only
  # eigenvalue, and whose ka ties with both: the coefficients must come back
  # finite, and the set's ke0 and ka must have been moved off lambda_1.  The
  # model stands in for propofol so that getDrugPK() accepts the name.
  k10 <- 0.05
  oneCompartment <- function(weight, height, age, sex, adjustToFFM = TRUE) {
    list(
      PK = list(default = list(v1 = 10, v2 = 1, v3 = 1, cl1 = 10 * k10, cl2 = 0, cl3 = 0,
                               ka_PO = k10, bioavailability_PO = 1, tlag_PO = 0)),
      tPeak = 0, ke0 = k10, MEAC = 1, typical = 1, upperTypical = 1, lowerTypical = 1,
      reference = "test"
    )
  }
  local_mocked_bindings(propofol = oneCompartment, .package = "stanpumpR")
  X <- getDrugPK("propofol", 70, 170, 40, "male")
  pk <- X$PK$default

  expect_equal(pk$lambda_1, k10)
  expect_false(pk$ke0 == pk$lambda_1)
  expect_equal(pk$ke0, k10, tolerance = 1e-6)
  expect_false(pk$ka_PO == pk$lambda_1)
  expect_false(pk$ka_PO == pk$ke0)
  expect_true(all(is.finite(unlist(pk[grepl("coef", names(pk))]))))

  # and a simulation on that set runs clean, with an effect site that peaks
  dt <- data.frame(Drug = "propofol", Time = c(0, 60), Dose = c(10, 10), Units = c("mg", "mg PO"),
                   stringsAsFactors = FALSE)
  out <- simCpCe(dt, eventTableInit, X, 240, FALSE)
  expect_false(anyNA(out$results$Y))
  ce <- out$results$Y[out$results$Site == "Effect Site"]
  expect_gt(max(ce), 0)
})

test_that("every library drug's PK set has its rates separate", {
  for (drug in getDrugDefaultsGlobal()$Drug) {
    fn <- get0(drug, mode = "function")
    if (is.null(fn)) next
    X <- tryCatch(getDrugPK(drug, 70, 170, 40, "male"), error = function(e) NULL)
    if (is.null(X) || is.null(X$PK)) next
    for (set in X$PK) {
      lambda <- c(set$lambda_1, set$lambda_2, set$lambda_3)
      lambda <- lambda[!is.na(lambda) & lambda > 0]
      rates <- unlist(set[intersect(c("ke0", "ka_PO", "ka_PO2", "ka_IM", "ka_IN", "ka_SL", "ka_RA", "ka_RAslow"), names(set))])
      rates <- rates[!is.na(rates) & rates > 0]
      for (r in rates) expect_false(any(r == lambda), info = paste(drug, r))
      if (!is.null(set$ke0) && set$ke0 > 0) {
        ka <- rates[names(rates) != "ke0"]
        expect_false(any(ka == set$ke0), info = drug)
      }
    }
  }
})
