# The physiology sliders (R/physiology.R): liver function, renal function,
# cardiac output and GI absorption change a drug's parameters through the
# well-stirred model (Wilkinson and Shand 1975; Shafer's chapter 2, Figures
# 10-12), for the drugs with an applied row in physiologyParameters.csv.

# A table with a test row, swapped in for one test and put back after.
withPhysiologyTable <- function(rows, code)
{
  env <- stanpumpR:::.physiologyCache
  old <- env$table
  env$table <- rows
  on.exit(env$table <- old, add = TRUE)
  force(code)
}

testRow <- function(drug, fr, fh, E, firstPass = FALSE)
  data.frame(drug = drug, fraction_renal = fr, fraction_hepatic = fh,
             extraction_ratio = E, oral_first_pass = firstPass,
             verification = "read_full_text", stringsAsFactors = FALSE)

test_that("checkPhysiology fills in normal and refuses what it cannot read", {
  expect_identical(checkPhysiology(NULL), PHYSIOLOGY_DEFAULT)
  expect_identical(checkPhysiology(c(liver = 0.5))[["liver"]], 0.5)
  expect_identical(checkPhysiology(c(liver = 0.5))[["renal"]], 1)
  expect_identical(checkPhysiology(list(cardiac = 2))[["cardiac"]], 2)
  expect_error(checkPhysiology(c(liver = 0.05)), "between")
  expect_error(checkPhysiology(c(liver = 11)), "between")
  expect_error(checkPhysiology(c(spleen = 1)), "named")
  expect_error(checkPhysiology(c(liver = NA_real_)), "between")
  expect_error(checkPhysiology(c(1, 2)), "named")
  expect_true(physiologyIsNormal(NULL))
  expect_false(physiologyIsNormal(c(gi = 2)))
})

test_that("the well-stirred factors follow the chapter's figures", {
  # Figure 10: a flow-limited drug (E near 1) follows hepatic blood flow, a
  # capacity-limited one (E near 0) is nearly indifferent to it.
  expect_equal(hepaticClearanceFactor(0.999, 0.5, 1), 0.5, tolerance = 2e-3)
  expect_equal(hepaticClearanceFactor(0.01, 0.5, 1), 1, tolerance = 1e-2)
  # Figure 11: the reverse for metabolic capacity.
  expect_equal(hepaticClearanceFactor(0.01, 1, 0.5), 0.5, tolerance = 1e-2)
  expect_equal(hepaticClearanceFactor(0.999, 1, 0.5), 1, tolerance = 2e-3)
  # The limits are exact
  expect_equal(hepaticClearanceFactor(1, 0.3, 7), 0.3)
  expect_equal(hepaticClearanceFactor(0, 0.3, 7), 7)
  # No change at normal, for any E
  for (E in c(0, 0.2, 0.5, 0.9, 1))
  {
    expect_equal(hepaticClearanceFactor(E, 1, 1), 1)
    expect_equal(hepaticExtractionAt(E, 1, 1), E)
  }
  # Against the model written out in full: CL = Q CLint / (Q + CLint) at
  # Q = 1.4 L/min (the chapter's), CLint from E.
  Q <- 1.4
  for (E in c(0.1, 0.5, 0.9)) for (q in c(0.2, 3)) for (l in c(0.3, 4))
  {
    clint <- Q * E / (1 - E)
    cl <- function(Q, clint) Q * clint / (Q + clint)
    expect_equal(hepaticClearanceFactor(E, q, l),
                 cl(q * Q, l * clint) / cl(Q, clint))
    expect_equal(hepaticExtractionAt(E, q, l), l * clint / (q * Q + l * clint))
  }
})

test_that("the clearance is split into its renal, hepatic and other shares", {
  row <- list(fraction_renal = 0.3, fraction_hepatic = 0.5, extraction_ratio = 0.4)
  p <- checkPhysiology(c(renal = 0.1))
  expect_equal(physiologyClearanceFactor(row, p), 0.3 * 0.1 + 0.5 + 0.2)
  p <- checkPhysiology(c(liver = 0.5))
  expect_equal(physiologyClearanceFactor(row, p),
               0.3 + 0.5 * hepaticClearanceFactor(0.4, 1, 0.5) + 0.2)
  # Kidneys shut down: only the renal share goes
  p <- checkPhysiology(c(renal = PHYSIOLOGY_MIN))
  expect_equal(physiologyClearanceFactor(row, p), 0.3 * 0.1 + 0.7)
  # The extrahepatic, non-renal share (esterases) is untouched by everything
  row <- list(fraction_renal = 0, fraction_hepatic = 0, extraction_ratio = NA)
  p <- checkPhysiology(c(liver = 0.1, renal = 0.1, cardiac = 0.1))
  expect_identical(physiologyClearanceFactor(row, p), 1)
})

test_that("first pass follows the extraction ratio and never passes F = 1", {
  row <- list(extraction_ratio = 0.7, oral_first_pass = TRUE)
  p <- checkPhysiology(c(liver = 0.5))
  E2 <- hepaticExtractionAt(0.7, 1, 0.5)
  expect_equal(physiologyFirstPassFactor(row, p), (1 - E2) / 0.3)
  expect_gt(physiologyFirstPassFactor(row, p), 1)
  row$oral_first_pass <- FALSE
  expect_identical(physiologyFirstPassFactor(row, p), 1)
  X <- list(PK = list(default = list(v1 = 1, cl1 = 1, ka_PO = 0.01,
                                     bioavailability_PO = 0.6)))
  out <- applyPhysiology(X, list(fraction_renal = 0, fraction_hepatic = 1,
                                 extraction_ratio = 0.9, oral_first_pass = TRUE),
                         checkPhysiology(c(liver = 0.1)))
  expect_identical(out$PK$default$bioavailability_PO, 1)
})

test_that("applyPhysiology scales what it should and nothing else", {
  X <- list(PK = list(default = list(v1 = 5, v2 = 20, v3 = 100, cl1 = 1, cl2 = 2,
                                     cl3 = 0.5, ka_PO = 0.02,
                                     bioavailability_PO = 0.5, tlag_PO = 3)))
  row <- list(fraction_renal = 0.1, fraction_hepatic = 0.8, extraction_ratio = 0.5,
              oral_first_pass = FALSE)
  p <- checkPhysiology(c(cardiac = 0.5, gi = 3))
  s <- applyPhysiology(X, row, p)$PK$default
  expect_equal(s$v1, 5 * 0.5 ^ PHYSIOLOGY_V1_CARDIAC_EXPONENT)
  expect_identical(s$v2, 20)
  expect_identical(s$v3, 100)
  expect_equal(s$cl1, physiologyClearanceFactor(row, p))
  expect_equal(s$cl2, 2 * 0.5)
  expect_equal(s$cl3, 0.5 * 0.5)
  expect_equal(s$ka_PO, 0.06)
  expect_identical(s$bioavailability_PO, 0.5)
  expect_identical(s$tlag_PO, 3)
})

test_that("the sliders' log10 values become factors", {
  f <- physiologyFromInputs(list(liver = -1, renal = 1, cardiac = log10(0.5), gi = 0.001))
  expect_equal(f[["liver"]], 0.1)
  expect_equal(f[["renal"]], 10)
  expect_equal(f[["cardiac"]], 0.5)
  expect_identical(f[["gi"]], 1)
  expect_identical(physiologyFromInputs(list()), PHYSIOLOGY_DEFAULT)
  expect_equal(physiologyFromInputs(list(liver = -5))[["liver"]], PHYSIOLOGY_MIN)
  expect_identical(physiologySummary(PHYSIOLOGY_DEFAULT), "")
  expect_match(physiologySummary(c(liver = 0.5)), "liver function")
})

test_that("getDrugPK applies a row, holds ke0 and moves tPeak", {
  withPhysiologyTable(testRow("fentanyl", 0.05, 0.95, 0.8), {
    base <- getDrugPK("fentanyl", 70, 170, 40, "male")
    expect_null(base$physiology)
    same <- getDrugPK("fentanyl", 70, 170, 40, "male", physiology = PHYSIOLOGY_DEFAULT)
    expect_identical(same$PK, base$PK)

    low <- getDrugPK("fentanyl", 70, 170, 40, "male",
                     physiology = c(cardiac = 0.5, liver = 0.5))
    b <- base$PK$default
    l <- low$PK$default
    fac <- 0.05 + 0.95 * hepaticClearanceFactor(0.8, 0.5, 0.5)
    expect_equal(l$cl1, b$cl1 * fac)
    expect_equal(l$v1, b$v1 * 0.5)
    expect_equal(l$cl2, b$cl2 * 0.5)
    expect_equal(l$ke0, b$ke0, tolerance = 1e-7)
    expect_false(isTRUE(all.equal(low$tPeak, base$tPeak)))
    expect_equal(low$physiology$clearance, fac)
    # Its parameters are those of an ordinary model with the changed values
    # (and the effect site held), so the simulation is unchanged code.
    expect_equal(l$k10, l$cl1 / l$v1)
  })
})

test_that("a drug without an applied row ignores the sliders", {
  withPhysiologyTable(testRow("fentanyl", 0.05, 0.95, 0.8), {
    base <- getDrugPK("propofol", 70, 170, 40, "male")
    moved <- getDrugPK("propofol", 70, 170, 40, "male",
                       physiology = c(liver = 0.1, cardiac = 0.1, renal = 0.1))
    expect_identical(moved$PK, base$PK)
    expect_null(moved$physiology)
    expect_identical(physiologyUnaffected(c("propofol", "fentanyl")), "propofol")
  })
  pending <- testRow("fentanyl", 0.05, 0.95, 0.8)
  pending$verification <- "pending_verification"
  withPhysiologyTable(pending, {
    expect_null(physiologyRow("fentanyl"))
  })
})

test_that("the simulation responds: a lower clearance leaves more drug", {
  withPhysiologyTable(testRow("fentanyl", 0.05, 0.95, 0.8), {
    dose <- data.frame(Drug = "fentanyl", Time = c(0, 0), Dose = c(100, 2),
                       Units = c("mcg", "mcg/kg/hr"), stringsAsFactors = FALSE)
    ev <- data.frame(Time = numeric(0), Event = character(0), stringsAsFactors = FALSE)
    run <- function(p)
      simulateDrugsWithCovariates(dose, ev, 70, 170, 40, "male", 240, FALSE,
                                  physiology = p)$fentanyl$results
    normal <- run(NULL)
    shock <- run(c(cardiac = 0.3))
    at <- function(r) {
      r <- r[r$Site == "Plasma", ]
      r$Y[which.max(r$Time)]
    }
    expect_gt(at(shock), at(normal))
  })
})

test_that("the physiology table is well formed", {
  tab <- physiologyTable()
  expect_named(tab, c("drug", "fraction_renal", "fraction_hepatic",
                      "extraction_ratio", "oral_first_pass", "basis",
                      "source_renal", "source_hepatic", "source_extraction",
                      "verification", "notes"))
  expect_false(anyDuplicated(tab$drug) > 0)
  expect_true(all(tab$verification %in% c(PHYSIOLOGY_APPLIED_VERIFICATION,
                                          "pending_verification")))
  dd <- getDrugDefaultsGlobal()
  expect_true(all(tab$drug %in% dd$Drug))
  applied <- tab[tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION, ]
  for (i in seq_len(nrow(applied)))
  {
    r <- applied[i, ]
    info <- r$drug
    expect_true(is_valid_number(r$fraction_renal, 0, 1), info = info)
    expect_true(is_valid_number(r$fraction_hepatic, 0, 1), info = info)
    expect_lte(r$fraction_renal + r$fraction_hepatic, 1 + 1e-9)
    if (r$fraction_hepatic > 0)
      expect_true(is_valid_number(r$extraction_ratio, 0, 1), info = info)
    expect_true(nzchar(r$source_renal) && nzchar(r$source_hepatic), info = info)
    if (r$fraction_hepatic > 0) expect_true(nzchar(r$source_extraction), info = info)
    # A model whose elimination lives outside the PK set cannot be scaled
    X <- get(r$drug, envir = asNamespace("stanpumpR"))(70, 170, 40, "male")
    expect_false(physiologyUnsupported(X), info = info)
    # And every drug with a row survives the extremes
    for (v in c(PHYSIOLOGY_MIN, PHYSIOLOGY_MAX))
      expect_no_error(getDrugPK(r$drug, 70, 170, 40, "male",
                                physiology = c(liver = v, renal = v,
                                               cardiac = v, gi = v)))
  }
})
