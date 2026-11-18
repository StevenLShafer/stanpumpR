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

test_that("renal metabolism with an extraction ratio is flow-limited too", {
  row <- list(fraction_renal = 0.27, fraction_hepatic = 0.6, extraction_ratio = 0.87,
              renal_extraction_ratio = 0.58)
  # Renal function alone: well-stirred in r, not linear
  p <- checkPhysiology(c(renal = 0.1))
  expect_equal(physiologyClearanceFactor(row, p),
               0.27 * hepaticClearanceFactor(0.58, 1, 0.1) + 0.6 + 0.13)
  expect_gt(physiologyClearanceFactor(row, p), 0.27 * 0.1 + 0.73)
  # Cardiac output reaches the renal share as well as the hepatic one
  p <- checkPhysiology(c(cardiac = 0.5))
  expect_equal(physiologyClearanceFactor(row, p),
               0.27 * hepaticClearanceFactor(0.58, 0.5, 1) +
                 0.6 * hepaticClearanceFactor(0.87, 0.5, 1) + 0.13)
  # Filtration (no renal extraction ratio) stays linear and indifferent to flow
  row$renal_extraction_ratio <- NA
  expect_equal(physiologyClearanceFactor(row, checkPhysiology(c(renal = 0.1))),
               0.27 * 0.1 + 0.73)
  expect_equal(physiologyClearanceFactor(row, checkPhysiology(c(cardiac = 0.5))),
               0.27 + 0.6 * hepaticClearanceFactor(0.87, 0.5, 1) + 0.13)
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

test_that("getDrugPK applies a row, scales ke0 with cardiac output and moves tPeak", {
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
    expect_equal(l$ke0, b$ke0 * 0.5 ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT, tolerance = 1e-7)
    # Liver function alone leaves the effect site where it was
    liver <- getDrugPK("fentanyl", 70, 170, 40, "male", physiology = c(liver = 0.5))
    expect_equal(liver$PK$default$ke0, b$ke0, tolerance = 1e-7)
    expect_false(isTRUE(all.equal(low$tPeak, base$tPeak)))
    expect_equal(low$physiology$clearance, fac)
    # Its parameters are those of an ordinary model with the changed values
    # (and the effect site held), so the simulation is unchanged code.
    expect_equal(l$k10, l$cl1 / l$v1)
  })
})

test_that("a model that supplies ke0 scales it with cardiac output", {
  withPhysiologyTable(testRow("etomidate", 0, 0.98, 0.5), {
    base <- getDrugPK("etomidate", 70, 170, 40, "male")
    low <- getDrugPK("etomidate", 70, 170, 40, "male", physiology = c(cardiac = 0.25))
    expect_equal(low$PK$default$ke0, base$PK$default$ke0 * 0.25 ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT,
                 tolerance = 1e-7)
  })
})

test_that("morphine's effect site is not slowed by cardiac output", {
  # Barrier-limited: ke0 = PS / (V lambda), so brain flow drops out
  base <- getDrugPK("morphine", 70, 170, 40, "male")
  low <- getDrugPK("morphine", 70, 170, 40, "male", physiology = c(cardiac = 0.3))
  expect_equal(low$PK$default$ke0, base$PK$default$ke0, tolerance = 1e-7)
  # ...while its clearance and V1 still follow cardiac output
  expect_lt(low$PK$default$cl1, base$PK$default$cl1)
  expect_lt(low$PK$default$v1, base$PK$default$v1)
  # A flow-limited drug's ke0 does fall
  f0 <- getDrugPK("propofol", 70, 170, 40, "male")
  f1 <- getDrugPK("propofol", 70, 170, 40, "male", physiology = c(cardiac = 0.3))
  expect_equal(f1$PK$default$ke0, f0$PK$default$ke0 * 0.3 ^ PHYSIOLOGY_KE0_CARDIAC_EXPONENT,
               tolerance = 1e-7)
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
                      "extraction_ratio", "blood_plasma_ratio",
                      "extraction_basis", "renal_extraction_ratio",
                      "oral_first_pass", "basis",
                      "source_renal", "source_hepatic", "source_extraction",
                      "ke0_permeability_limited", "verification", "notes"))
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
    if (!is.na(r$renal_extraction_ratio))
      expect_true(r$fraction_renal > 0 &&
                    is_valid_number(r$renal_extraction_ratio, 0, 1), info = info)
    expect_true(nzchar(r$source_renal) && nzchar(r$source_hepatic), info = info)
    if (r$fraction_hepatic > 0) expect_true(nzchar(r$source_extraction), info = info)
    if (r$fraction_hepatic > 0)
      expect_true(r$extraction_basis %in% c("measured", "computed"), info = info)
    # A computed extraction ratio is the model's own clearance at the
    # reference man over hepatic blood flow (computedExtractionRatio()), so it
    # cannot drift from the model it describes
    if (isTRUE(r$extraction_basis == "computed"))
    {
      expect_true(is_valid_number(r$blood_plasma_ratio, 0.05, 20), info = info)
      X <- get(r$drug, envir = asNamespace("stanpumpR"))(70, 170, 35, "male")
      expect_equal(r$extraction_ratio,
                   round(computedExtractionRatio(r$fraction_hepatic,
                                                 X$PK[[1]]$cl1,
                                                 r$blood_plasma_ratio), 3),
                   tolerance = 1e-9, info = info)
    }
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

test_that("the ke0 illustration's brain model is the ke0 equation", {
  # A step in arterial concentration: Ce = 1 - exp(-ke0 t), exactly
  times <- seq(0, 20, by = 0.05)
  ce <- effectSiteForArterial(times, rep(1, length(times)), 0.3)
  expect_equal(ce, 1 - exp(-0.3 * times), tolerance = 1e-12)
  # A ramp, exact for a piecewise-linear arterial curve
  ce <- effectSiteForArterial(times, times, 0.3)
  expect_equal(ce, times - (1 - exp(-0.3 * times)) / 0.3, tolerance = 1e-10)
  d <- physiologyKe0Drugs()
  # Calibrated: each drug has its stated ke0 at normal brain flow
  expect_equal(brainKe0(d$PS, d$lambda, 1), d$ke0)
  # Flow-limited: ke0 in proportion to brain flow, whatever the solubility
  for (i in 1:2) expect_equal(brainKe0(d$PS[i], d$lambda[i], 0.3) / d$ke0[i], 0.3, tolerance = 1e-6)
  # Permeability-limited: barely changes
  expect_gt(brainKe0(d$PS[3], d$lambda[3], 0.3) / d$ke0[3], 0.9)
})

test_that("the ke0 rationale renders on the help page", {
  fig <- helpPhysiologyKe0Figure()
  expect_match(fig, "<svg")
  expect_equal(lengths(regmatches(fig, gregexpr("<svg", fig))), 3L)
  expect_false(grepl("<script|on[a-z]+=", fig))
  tab <- helpPhysiologyKe0Table()
  expect_match(tab, "Brain flow")
  expect_match(tab, "CO<sup>0.35</sup>")
  prop <- helpPhysiologyPropofolTable()
  expect_equal(length(strsplit(prop, "\n")[[1]]), 5L)
  page <- helpExpandGenerated(paste(readLines(system.file("app-help", "models", "physiology.md",
                                                           package = "stanpumpR")), collapse = "\n"))
  expect_false(grepl("<!-- generated:", page))
})

# ---- The inhaled anesthetics (Shafer, 2026-11-15) ---------------------------
# The cardiac slider sets the gas engine's cardiac output and its split: the
# brain and heart keep cardiac^0.35 of their flow, in a compartment of their
# own (the effect site), and the rest of the output is shared in proportion.

SEVO_RUN <- data.frame(Drug = c("sevoflurane", "oxygen", "air", "ventilation"),
                       Time = 0, Dose = c(2, 3, 3, 6))

test_that("gasBodyPhysiology leaves a normal body alone and conserves flow and volume", {
  body <- getGasBody(70)
  expect_identical(gasBodyPhysiology(body, 1), body)
  expect_identical(gasBodyPhysiology(body, NULL), body)
  expect_error(gasBodyPhysiology(body, 0), "Invalid")
  expect_error(gasBodyPhysiology(body, NA_real_), "Invalid")
  share <- PHYSIOLOGY_GAS_PROTECTED_SHARE
  for (c in c(0.1, 0.3, 0.5, 2, 10)) {
    b <- gasBodyPhysiology(body, c)
    expect_equal(b$Q_cardiac, body$Q_cardiac * c)
    expect_equal(sum(gasTissueShares(b)), 1)
    expect_true(all(gasTissueShares(b) >= 0))
    expect_equal(gasStateCount(b), 6L)
    # The brain and heart keep share x c^0.35 of normal output.
    expect_equal(b$f_brain * b$Q_cardiac,
                 share * c^PHYSIOLOGY_KE0_CARDIAC_EXPONENT * body$Q_cardiac)
    # The vessel-rich group's volume is only divided, and the other tissues
    # share what is left in their normal proportions.
    expect_equal(b$V_brain + b$V_vrgRest, body$V_brain)
    expect_equal(b$V_brain, body$V_brain * share / body$f_brain)
    rest <- c(b$f_vrgRest, b$f_muscle, b$f_fat)
    expect_equal(rest / sum(rest),
                 c(body$f_brain - share, body$f_muscle, body$f_fat) /
                   (1 - share))
  }
  # At a tenth of normal most of the output is the heart-brain circuit
  # (0.16 x 0.1^0.35 = 0.0715 of normal output, of 0.1).
  expect_equal(gasBodyPhysiology(body, 0.1)$f_brain, 0.16 * 0.1^0.35 / 0.1)
  expect_gt(gasBodyPhysiology(body, 0.1)$f_brain, 0.7)
})

test_that("the split vessel-rich group fills exactly as the lumped one at normal output", {
  # Both parts have the group's flow per volume, so just off normal the
  # simulation is continuous with the five-state engine.
  sim1 <- simulateGases(SEVO_RUN, maximum = 60)
  simN <- simulateGases(SEVO_RUN, maximum = 60,
                        physiology = c(cardiac = 1 + 1e-9))
  expect_equal(ncol(sim1$state$sevoflurane), 5)
  expect_equal(ncol(simN$state$sevoflurane), 6)
  expect_equal(simN$results$Y, sim1$results$Y, tolerance = 1e-7)
  expect_equal(simN$state$sevoflurane[, 6], simN$state$sevoflurane[, 3],
               tolerance = 1e-7)
  # And at exactly normal nothing changes at all.
  expect_identical(simulateGases(SEVO_RUN, maximum = 60,
                                 physiology = PHYSIOLOGY_DEFAULT)$results,
                   sim1$results)
})

test_that("a low cardiac output makes end-tidal rise faster, most for a soluble agent", {
  at <- function(sim, drug, site, t) {
    r <- sim$results[sim$results$Drug == drug & sim$results$Site == site, ]
    stats::approx(r$Time, r$Y, t)$y
  }
  n2o <- data.frame(Drug = c("nitrousOxide", "oxygen", "ventilation"),
                    Time = 0, Dose = c(4, 2, 6))
  for (run in list(list(SEVO_RUN, "sevoflurane", 2), list(n2o, "nitrousOxide", 66.7))) {
    rise <- vapply(c(0.25, 1, 4), function(c)
      at(simulateGases(run[[1]], maximum = 30, physiology = c(cardiac = c)),
         run[[2]], "Alveolar", 2), numeric(1))
    expect_true(all(diff(rise) < 0))
  }
  # Proportionally, the change in FA/FI at 2 min is larger for sevoflurane
  # (blood:gas 0.65) than for nitrous oxide (0.47) -- the chapter's point.
  ratio <- function(dt, drug, c) {
    at(simulateGases(dt, maximum = 30, physiology = c(cardiac = c)), drug, "Alveolar", 2) /
      at(simulateGases(dt, maximum = 30), drug, "Alveolar", 2)
  }
  expect_gt(ratio(SEVO_RUN, "sevoflurane", 0.25), ratio(n2o, "nitrousOxide", 0.25))
})

test_that("in shock the brain keeps its flow, so it fills from the higher arterial tension", {
  brain <- function(c, t) {
    sim <- simulateGases(SEVO_RUN, maximum = 30, physiology = c(cardiac = c))
    r <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Brain", ]
    stats::approx(r$Time, r$Y, t)$y
  }
  # Without the split the 6 L group took the brain's preserved flow and the
  # brain at 2 min fell to a fifth of normal; with it, it stays near normal,
  # a little slower (less drug delivered a minute, as for the ke0 of the
  # intravenous drugs), and later rises above normal.
  expect_gt(brain(0.1, 2), 0.6 * brain(1, 2))
  expect_lt(brain(0.1, 2), brain(1, 2))
  expect_gt(brain(0.1, 30), brain(1, 30))
})

test_that("the washout and the time until threshold use the simulation's own body", {
  sim <- simulateGases(SEVO_RUN, maximum = 30, physiology = c(cardiac = 0.3))
  w <- gasWashout(sim, SEVO_RUN, weight = 70)
  expect_identical(w$body, sim$body)
  expect_equal(dim(w$amplitude$sevoflurane)[2], 6)
  rt <- gasRecoveryTime(w, "sevoflurane", 1)
  expect_true(all(is.finite(rt)))
  expect_true(any(rt > 0))
  # Just off normal, the split washout is the lumped one.
  w1 <- gasWashout(simulateGases(SEVO_RUN, maximum = 30), SEVO_RUN)
  wN <- gasWashout(simulateGases(SEVO_RUN, maximum = 30,
                                 physiology = c(cardiac = 1 + 1e-9)), SEVO_RUN)
  expect_equal(gasRecoveryTime(wN, "sevoflurane", 1),
               gasRecoveryTime(w1, "sevoflurane", 1), tolerance = 1e-5)
  expect_equal(macRecoveryTimeExact(wN, 50), macRecoveryTimeExact(w1, 50),
               tolerance = 1e-5)
})

test_that("the Visualizer draws the whole vessel-rich group, so its agent adds up", {
  DT <- cbind(SEVO_RUN, Units = c("%", "L/min", "L/min", "L/min"))
  sim <- simulateGases(SEVO_RUN, maximum = 30, physiology = c(cardiac = 0.3))
  E <- gasDrugEntries(sim, DT, maximum = 30, age = 50)
  r <- viewerGasRun(E$sevoflurane, 0, 30)
  b <- sim$body
  props <- getGasProperties()
  sevo <- props[props$gas == "sevoflurane", ]
  expect_equal(r$volume$vrg, b$V_brain + b$V_vrgRest)
  expect_equal(r$capacity$vrg, (b$V_brain + b$V_vrgRest) * sevo$tg_brain)
  s <- sim$state$sevoflurane
  whole <- (b$V_brain * s[, 3] + b$V_vrgRest * s[, 6]) / (b$V_brain + b$V_vrgRest)
  expect_equal(r$p$vrg, pmax(0, stats::approx(sim$timeLine, whole, r$t / 60,
                                             rule = 2, ties = "ordered")$y))
})

test_that("the panel note counts the inhaled agents only when cardiac output moves", {
  drugs <- c("sevoflurane", "oxygen", "ventilation", "propofol")
  txt <- function(p) as.character(physiologyNote(p, drugs))
  low <- txt(c(cardiac = 0.5))
  expect_match(low, "propofol, sevoflurane", fixed = TRUE)
  expect_false(grepl("no physiology data", low))
  expect_false(grepl("oxygen|ventilation", low))
  liver <- txt(c(liver = 0.5))
  expect_match(liver, "respond to cardiac output and body temperature only", fixed = TRUE)
  expect_false(grepl("no physiology data yet): </strong>sevoflurane", liver, fixed = TRUE))
})

# ---- CodeRabbit on #727 ------------------------------------------------------

test_that("simulateGases() keeps its positional arguments: physiology comes last", {
  args <- names(formals(simulateGases))
  expect_identical(args[1:9], c("doseTable", "weight", "age", "maximum", "cardiacOutput",
                                "uptakeEffect", "circuit", "deadSpace", "oxygenUptake"))
  expect_identical(utils::tail(args, 2), c("physiology", "temperature"))
  # The old positional call that turns the uptake coupling off still does.
  expect_equal(simulateGases(SEVO_RUN, 70, 50, 30, NULL, FALSE)$results,
               simulateGases(SEVO_RUN, 70, 50, 30, uptakeEffect = FALSE)$results)
})

test_that("the washout runs on the cardiac output the simulation ran on, an override included", {
  sim <- simulateGases(SEVO_RUN, maximum = 30, cardiacOutput = 5,
                       physiology = c(cardiac = 0.3))
  expect_equal(sim$Qco, 5)
  expect_equal(gasWashout(sim, SEVO_RUN)$Qco, 5)
  expect_equal(gasWashout(sim, SEVO_RUN, cardiacOutput = 3)$Qco, 3)
  expect_equal(gasWashout(simulateGases(SEVO_RUN, maximum = 30,
                                        physiology = c(cardiac = 0.3)), SEVO_RUN)$Qco,
               0.3 * getGasBody(70)$Q_cardiac)
})

test_that("an oral tPeak is re-solved from the oral curve and its lag", {
  # A three-compartment oral model that gives ke0, no tPeak, and an oral tPeak
  # route: the solved tPeak is the peak of the effect site after an oral
  # dose, found here by integrating the whole system on a fine grid.
  X <- list(PK = list(default = list(v1 = 10, v2 = 20, v3 = 50, cl1 = 1, cl2 = 2,
                                     cl3 = 0.5, ka_PO = 0.05,
                                     bioavailability_PO = 1, tlag_PO = 12)),
            ke0 = 0.1, tPeakRoute = "PO", reference = "test")
  pk <- getDrugPK("propofol", 70, 170, 35, "male", model = X)
  ke0 <- pk$PK$default$ke0
  h <- 0.002; n <- 300 / h
  gut <- 1; a1 <- 0; a2 <- 0; a3 <- 0; ce <- 0; best <- 0; tBest <- 0
  for (i in seq_len(n)) {
    c1 <- a1 / 10
    d <- c(-0.05 * gut,
           0.05 * gut - (1 + 2 + 0.5) * c1 + 2 * a2 / 20 + 0.5 * a3 / 50,
           2 * c1 - 2 * a2 / 20, 0.5 * c1 - 0.5 * a3 / 50, ke0 * (c1 - ce))
    gut <- gut + h * d[1]; a1 <- a1 + h * d[2]; a2 <- a2 + h * d[3]
    a3 <- a3 + h * d[4]; ce <- ce + h * d[5]
    if (ce > best) { best <- ce; tBest <- i * h }
  }
  expect_equal(pk$tPeak, 12 + tBest, tolerance = 1e-3)
})

test_that("an oral tPeak counts every depot carrying drug, each with its lag", {
  # Only the second depot carries drug: moving its lag by 120 min moves the
  # peak by 120 min, and the peak is the one-depot peak at that rate and lag.
  model <- function(lag2) list(
    PK = list(default = list(v1 = 10, v2 = 20, v3 = 50, cl1 = 1, cl2 = 2,
                             cl3 = 0.5, ka_PO = 0.05, bioavailability_PO = 1,
                             tlag_PO = 12, ka_PO2 = 0.02, fraction_PO2 = 1,
                             tlag_PO2 = lag2)),
    ke0 = 0.1, tPeakRoute = "PO", reference = "test")
  t30 <- getDrugPK("propofol", 70, 170, 35, "male", model = model(30))$tPeak
  t150 <- getDrugPK("propofol", 70, 170, 35, "male", model = model(150))$tPeak
  expect_equal(t150 - t30, 120, tolerance = 1e-6)
  one <- list(PK = list(default = list(v1 = 10, v2 = 20, v3 = 50, cl1 = 1, cl2 = 2,
                                       cl3 = 0.5, ka_PO = 0.02, bioavailability_PO = 1,
                                       tlag_PO = 30)),
              ke0 = 0.1, tPeakRoute = "PO", reference = "test")
  expect_equal(t30, getDrugPK("propofol", 70, 170, 35, "male", model = one)$tPeak,
               tolerance = 1e-6)
})

test_that("the email workbook records the physiology sliders", {
  rows <- physiologyCovariateRows(c(cardiac = 0.5), 32)
  expect_equal(nrow(rows), length(PHYSIOLOGY_FACTORS) + 1)
  expect_equal(rows$Value[rows$Covariate == "Body temperature (C)"], "32")
  expect_equal(physiologyCovariateRows(NULL)$Value[length(PHYSIOLOGY_FACTORS) + 1], "37")
  expect_equal(rows$Value[rows$Covariate == "Cardiac output (multiple of normal)"], "x0.5")
  expect_equal(rows$Value[rows$Covariate == "Liver function (multiple of normal)"], "normal")
  n <- length(PHYSIOLOGY_FACTORS)
  expect_true(all(physiologyCovariateRows(NULL)$Value[seq_len(n)] == "normal"))
  expect_true(all(physiologyCovariateRows("x", "y")$Value == "unreadable"))
})

# ---- Body temperature (Shafer, 2026-11-16) ----------------------------------
# Hypothermia raises every partition coefficient of a volatile agent by
# exp(-slope (T - 37)) (Zhou 2002 blood, Zhou 2001 tissues: tissue:blood
# unchanged) and lowers its MAC along Liu 2001's regression.

test_that("gasPropertiesAt() follows Zhou 2002 and Liu 2001, and is the table at 37 C", {
  p37 <- getGasProperties()
  expect_identical(gasPropertiesAt(NULL), p37)
  expect_identical(gasPropertiesAt(37), p37)
  expect_error(gasPropertiesAt(20), "Invalid body temperature")
  expect_error(gasPropertiesAt(NA_real_), "Invalid body temperature")
  p31 <- gasPropertiesAt(31)
  for (g in c("desflurane", "sevoflurane", "isoflurane")) {
    i <- p37$gas == g
    f <- exp(GAS_SOLUBILITY_TEMPERATURE_SLOPE[[g]] * 6)
    expect_equal(p31$lambda_blood[i], p37$lambda_blood[i] * f)
    # Tissue:blood unchanged (Zhou 2001).
    expect_equal(p31$tg_brain[i] / p31$lambda_blood[i], p37$tg_brain[i] / p37$lambda_blood[i])
    expect_equal(p31$tg_fat[i] / p31$lambda_blood[i], p37$tg_fat[i] / p37$lambda_blood[i])
  }
  # Zhou 2002's own regression for isoflurane at a haematocrit of 40%:
  # ln(lambda) = -0.0382 T + 0.0154 HCT + 1.120, so lambda(31) / lambda(37).
  zhou <- function(T) exp(-0.0382 * T + 0.0154 * 40 + 1.120)
  i <- p37$gas == "isoflurane"
  expect_equal(p31$lambda_blood[i] / p37$lambda_blood[i], zhou(31) / zhou(37))
  # Liu 2001: MAC = 0.0784 T - 1.203, so the ratio of the line at 31 and 37.
  expect_equal(gasMacTemperatureFactor(31), (0.0784 * 31 - 1.203) / (0.0784 * 37 - 1.203))
  expect_equal(p31$MAC40[i], p37$MAC40[i] * gasMacTemperatureFactor(31))
  # About 4.6% of normothermic MAC a degree.
  expect_equal(1 - gasMacTemperatureFactor(36), 0.0784 / (0.0784 * 37 - 1.203))
  # Nitrogen and oxygen are unchanged; nitrous oxide has its own test, below.
  other <- !p37$gas %in% c(names(GAS_SOLUBILITY_TEMPERATURE_SLOPE), "nitrousOxide")
  expect_identical(p31[other, ], p37[other, ])
})

test_that("hypothermia slows the rise of FA/FI, most for the more soluble agent, and lowers MAC", {
  run <- function(agent, dose, T) {
    dt <- data.frame(Drug = c(agent, "oxygen", "air", "ventilation"), Time = 0,
                     Dose = c(dose, 3, 3, 6))
    sim <- simulateGases(dt, maximum = 30, temperature = T)
    r <- sim$results
    a <- function(d, s, t) { x <- r[r$Drug == d & r$Site == s, ]; stats::approx(x$Time, x$Y, t)$y }
    list(fafi = a(agent, "Alveolar", 2) / dose, mac = a("MAC", "MAC", 30),
         sim = sim, et = a(agent, "Alveolar", 30))
  }
  iso37 <- run("isoflurane", 1.2, 37); iso31 <- run("isoflurane", 1.2, 31)
  des37 <- run("desflurane", 6, 37);   des31 <- run("desflurane", 6, 31)
  expect_lt(iso31$fafi, iso37$fafi)
  expect_lt(des31$fafi, des37$fafi)
  expect_lt(iso31$fafi / iso37$fafi, des31$fafi / des37$fafi)
  # The summed MAC is end-tidal over the hypothermic MAC.
  expect_equal(iso31$mac,
               iso31$et / macForAge(gasPropertiesAt(31)$MAC40[gasPropertiesAt(31)$gas == "isoflurane"], 50),
               tolerance = 1e-6)
  expect_gt(iso31$mac, iso37$mac)
  # The body carries the temperature, so the washout uses the same properties.
  expect_equal(iso31$sim$body$temperature, 31)
  expect_null(iso37$sim$body$temperature)
  dt <- data.frame(Drug = c("isoflurane", "oxygen", "air", "ventilation"), Time = 0,
                   Dose = c(1.2, 3, 3, 6))
  w <- gasWashout(iso31$sim, dt)
  expect_equal(w$body$temperature, 31)
  expect_true(all(is.finite(gasRecoveryTime(w, "isoflurane", 0.3))))
  # Nitrous oxide: more soluble in the cold (Allott 1973), so FA/FI rises
  # more slowly; its MAC falls (Antognini 1994), so the summed MAC rises.
  n2o <- data.frame(Drug = c("nitrousOxide", "oxygen", "ventilation"), Time = 0, Dose = c(4, 2, 6))
  cold <- simulateGases(n2o, maximum = 30, temperature = 31)$results
  warm <- simulateGases(n2o, maximum = 30)$results
  et <- function(r) { x <- r[r$Drug == "nitrousOxide" & r$Site == "Alveolar", ]; stats::approx(x$Time, x$Y, 2)$y }
  expect_lt(et(cold), et(warm))
  mac <- function(r) { x <- r[r$Drug == "MAC", ]; stats::approx(x$Time, x$Y, 30)$y }
  expect_gt(mac(cold), mac(warm))
})

test_that("nitrous oxide MAC follows Antognini 1994's regression in rats", {
  # MAC = 0.03827 T + 0.3836 atm (Figure 1): the ratio of its values at the
  # two groups' mean temperatures, 37.5 and 29.7 C
  mac <- function(T) 0.03827 * T + 0.3836
  ratio <- gasMacTemperatureFactor(29.7, "nitrousOxide") /
    gasMacTemperatureFactor(37, "nitrousOxide") / (mac(29.7) / mac(37))
  expect_equal(ratio, 1, tolerance = 1e-12)
  expect_equal(gasMacTemperatureFactor(37, "nitrousOxide"), 1)
  # About 2.1% per degree at 37 C, 19% lower at 28 C (the paper's Table 1:
  # 20% over 38 to 28 C), half the volatile agents' fall
  expect_equal(1 - gasMacTemperatureFactor(36, "nitrousOxide"), 0.0213, tolerance = 0.01)
  expect_equal(gasMacTemperatureFactor(28, "nitrousOxide"), 0.809, tolerance = 1e-3)
  expect_gt(gasMacTemperatureFactor(28, "nitrousOxide"), gasMacTemperatureFactor(28))
  p <- gasPropertiesAt(28)
  p37 <- getGasProperties()
  n <- p$gas == "nitrousOxide"
  expect_equal(p$MAC40[n], p37$MAC40[n] * gasMacTemperatureFactor(28, "nitrousOxide"))
  # Allott 1973: 2.33%/C in blood and the aqueous tissues (Table II), 1.13%/C
  # in fat (Table III)
  expect_equal(p$lambda_blood[n], p37$lambda_blood[n] * exp(0.0233 * 9))
  expect_equal(p$tg_brain[n], p37$tg_brain[n] * exp(0.0233 * 9))
  expect_equal(p$tg_muscle[n], p37$tg_muscle[n] * exp(0.0233 * 9))
  expect_equal(p$tg_fat[n], p37$tg_fat[n] * exp(0.0113 * 9))
  expect_equal(p[p$gas %in% c("nitrogen", "oxygen"), ], p37[p37$gas %in% c("nitrogen", "oxygen"), ])
})

test_that("the brain-and-heart share is Williams and Leggett's reference man", {
  # Table 17: brain 12% and heart 4% of resting cardiac output, 35-year-old male
  expect_equal(PHYSIOLOGY_GAS_PROTECTED_SHARE, 0.16)
})

test_that("the temperature slider's value is read safely, and the note says what it changes", {
  expect_equal(bodyTemperatureFromInput(NULL), 37)
  expect_equal(bodyTemperatureFromInput("x"), 37)
  expect_equal(bodyTemperatureFromInput(20), PHYSIOLOGY_TEMPERATURE_MIN)
  expect_equal(bodyTemperatureFromInput(33.5), 33.5)
  drugs <- c("sevoflurane", "nitrousOxide", "oxygen", "propofol")
  cold <- gsub("\\s+", " ", as.character(physiologyNote(PHYSIOLOGY_DEFAULT, drugs, temperature = 32)))
  # Body temperature now acts on the intravenous drugs with a physiology row
  # too (physiologyTemperatureFactor())
  expect_match(cold, "Changed: </strong> propofol, sevoflurane, nitrousOxide </p>", fixed = TRUE)
  expect_no_match(cold, "Not changed by body temperature", fixed = TRUE)
  expect_null(physiologyNote(PHYSIOLOGY_DEFAULT, drugs, temperature = 37))
})

test_that("body temperature lowers elimination capacity, not blood flow", {
  # Shafer, 2026-11-16: one factor exp(theta (T - 37)), theta = -log(1 - 0.0637)
  # (Michelsen 2001's 6.37% per degree), on hepatic intrinsic clearance inside
  # the well-stirred model, on renal clearance and on every other route
  theta <- -log(1 - 0.0637)
  expect_equal(physiologyTemperatureFactor(c(temperature = 28)), exp(-9 * theta))
  expect_equal(physiologyTemperatureFactor(NULL), 1)
  expect_equal(1 - physiologyTemperatureFactor(c(temperature = 36)), 0.0637)
  # The physiology vector carries it, checked as the slider is
  expect_equal(checkPhysiology(NULL)[["temperature"]], 37)
  expect_error(checkPhysiology(c(temperature = 20)), "temperature")
  expect_false(physiologyIsNormal(c(temperature = 33)))
  expect_true(physiologyIsNormal(c(temperature = 37)))
  # Every route of a drug cleared without blood flow limits falls by the factor
  row <- data.frame(fraction_renal = 0.2, fraction_hepatic = 0.5, extraction_ratio = 1e-9,
                    renal_extraction_ratio = NA)
  f <- exp(-9 * theta)
  expect_equal(physiologyClearanceFactor(row, checkPhysiology(c(temperature = 28))),
               f, tolerance = 1e-6)
  # A flow-limited drug changes less: the well-stirred model, liver x f
  row$extraction_ratio <- 0.9
  p <- checkPhysiology(c(temperature = 28))
  expect_equal(physiologyClearanceFactor(row, p),
               0.2 * f + 0.5 * hepaticClearanceFactor(0.9, 1, f) + 0.3 * f)
  expect_gt(physiologyClearanceFactor(row, p), f)
  # Propofol (hepatic E 0.87, renal E 0.58) keeps 82% of its clearance at
  # 28 C at normal blood flow, and Bartosova 2026's 42% only when the
  # cardiac slider also lowers flow (recorded in R/physiology.R)
  pk <- function(...) getDrugPK("propofol", 70, 170, 40, "male", physiology = c(...))$PK$default
  ratio <- pk(temperature = 28)$cl1 / pk()$cl1
  expect_equal(ratio, 0.816, tolerance = 2e-3)
  expect_lt(pk(temperature = 28, cardiac = 0.4)$cl1 / pk()$cl1, 0.5)
  # Volumes, intercompartmental clearances and ke0 do not move
  expect_equal(pk(temperature = 28)$v1, pk()$v1)
  expect_equal(pk(temperature = 28)$cl2, pk()$cl2)
  # A drug without a physiology row is unchanged
  expect_null(physiologyRow("notADrug"))
  # The summary says it
  expect_match(physiologySummary(c(temperature = 31)), "31 °C", fixed = TRUE)
})

test_that("the physiology panel opens on what the sliders are for (Shafer, 2026-11-17)", {
  html <- as.character(physiologyPanel())
  intro <- regexpr('id="physiology-intro"', html, fixed = TRUE)
  controls <- regexpr('id="physiology-controls"', html, fixed = TRUE)
  expect_gt(intro, 0)
  expect_gt(controls, intro)
  # The controls start hidden, and hold every slider and the reset
  expect_match(html, 'id="physiology-controls" style="display: none;"', fixed = TRUE)
  tail <- substring(html, controls)
  for (id in c(physiologyInputId(PHYSIOLOGY_FACTORS), "physiology_temperature",
               "physiologyReset", "physiologyNote"))
    expect_match(tail, paste0('id="', id, '"'), fixed = TRUE)
  # The statement: covariates belong in the Patient Profile, and a button
  # shows the sliders
  introHtml <- substring(html, intro, controls)
  expect_match(introHtml, "Patient Profile", fixed = TRUE)
  expect_match(introHtml, "covariate", fixed = TRUE)
  expect_match(introHtml, "physiology-continue", fixed = TRUE)
  expect_match(introHtml, 'data-help-page="models/physiology"', fixed = TRUE)
  # The temperature tip no longer says it spares the intravenous drugs
  expect_false(grepl("does not change the intravenous", html, fixed = TRUE))
  # The browser shows the controls on the button or the server's message
  js <- paste(readLines(system.file("www", "app.js", package = "stanpumpR")),
              collapse = "\n")
  expect_match(js, "'.physiology-continue'", fixed = TRUE)
  expect_match(js, "addCustomMessageHandler('physiology_show'", fixed = TRUE)
})

test_that("a session whose physiology is off normal is shown the sliders", {
  shiny::testServer(app_server, {
    messages <- character(0)
    session$sendCustomMessage <- function(type, message)
      messages <<- c(messages, type)
    shown <- function() sum(messages == "physiology_show")
    session$setInputs(physiology_cardiac = 0, physiology_liver = 0,
                      physiology_renal = 0, physiology_gi = 0,
                      physiology_temperature = PHYSIOLOGY_TEMPERATURE_DEFAULT)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    expect_equal(shown(), 0)
    # A link or a scenario setting cardiac output to half
    session$setInputs(physiology_cardiac = log10(0.5))
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    expect_gte(shown(), 1)
    # Body temperature alone is a change too
    before <- shown()
    session$setInputs(physiology_cardiac = 0, physiology_temperature = 32)
    session$elapse(PHYSIOLOGY_DEBOUNCE_MS + 100)
    session$flushReact()
    expect_gt(shown(), before)
  })
})

test_that("an extraction ratio computed from the hepatic share (Shafer, 2026-11-17)", {
  # Williams and Leggett 1989: 25% of 6.7 L/min
  expect_equal(PHYSIOLOGY_HEPATIC_BLOOD_FLOW, 1.675)
  expect_equal(computedExtractionRatio(1, 0.8375), 0.5)
  expect_equal(computedExtractionRatio(0.5, 0.8375, bloodPlasma = 0.5), 0.5)
  # A hepatic blood clearance above hepatic blood flow cannot be well stirred
  expect_true(is.na(computedExtractionRatio(1, 2)))
})

test_that("a metabolite-forming model is not one the sliders can scale", {
  # The formation rate constant would not move with the parent's clearance
  X <- doxorubicin(70, 170, 35, "male")
  expect_false(is.null(X$metabolite))
  expect_true(physiologyUnsupported(X))
  expect_true(physiologyUnsupported(list(biliaryStore = list())))
})

test_that("a child's chosen model the sliders cannot scale is skipped, and said", {
  low <- c(liver = 0.5, renal = 0.5, cardiac = 0.5, gi = 1)
  # vincristine chooses vincristinePediatric (target binding) for a child
  child <- getDrugPK("vincristine", 20, 110, 5, "male", physiology = low)
  expect_true(isTRUE(child$slidersSkipped))
  expect_null(child[["physiology", exact = TRUE]])
  normal <- getDrugPK("vincristine", 20, 110, 5, "male")
  expect_equal(child$PK[[1]]$cl1, normal$PK[[1]]$cl1)
  # The adult's model is changed
  adult <- getDrugPK("vincristine", 70, 170, 35, "male", physiology = low)
  expect_null(adult$slidersSkipped)
  expect_lt(adult$PK[[1]]$cl1,
            getDrugPK("vincristine", 70, 170, 35, "male")$PK[[1]]$cl1)
  note <- as.character(physiologyNote(low, "vincristine", skipped = "vincristine"))
  expect_match(note, "Not changed for this patient", fixed = TRUE)
  expect_false(grepl("Changed: ", note, fixed = TRUE))
})

test_that("the chemotherapy rows move clearance by their routes (2026-11-17)", {
  cl <- function(d, ...) getDrugPK(d, 70, 170, 35, "male", ...)$PK[[1]]$cl1
  # carboplatin: renal 0.71, nothing hepatic
  expect_equal(cl("carboplatin", physiology = c(renal = 0.1)) / cl("carboplatin"),
               0.71 * 0.1 + 0.29, tolerance = 1e-6)
  expect_equal(cl("carboplatin", physiology = c(liver = 0.1)), cl("carboplatin"))
  # docetaxel: almost all hepatic, capacity limited at E 0.4
  r <- physiologyRow("docetaxel")
  expect_equal(cl("docetaxel", physiology = c(liver = 0.5)) / cl("docetaxel"),
               0.03 + 0.97 * hepaticClearanceFactor(r$extraction_ratio, 1, 0.5),
               tolerance = 1e-6)
  # daunorubicin is held back
  expect_null(physiologyRow("daunorubicin"))
})

test_that("the antiseizure rows survive the interactions' rebuilds (2026-11-18)", {
  doses <- data.frame(Drug = c("valproate", "lamotrigine"), Time = c(0, 0),
                      Dose = c(500, 100), Units = c("mg PO bid", "mg PO bid"))
  events <- data.frame(Time = numeric(0), Event = character(0))
  level <- function(physiology) {
    s <- simulateDrugsWithCovariates(doses, events, 70, 170, 50, "male", 14 * 1440,
                                     FALSE, antiseizureInteractions = TRUE,
                                     physiology = physiology)
    stats::approx(s$lamotrigine$wide$Time, s$lamotrigine$wide$Plasma,
                  xout = 13 * 1440)$y
  }
  normal <- level(NULL)
  # Half the liver's capacity: lamotrigine is 0.906 hepatic at a low E, so
  # its clearance falls by nearly half and its level rises
  expect_gt(level(c(liver = 0.5)) / normal, 1.5)
  # Renal function moves it little (0.094 renal)
  expect_lt(abs(level(c(renal = 0.5)) / normal - 1), 0.1)
  # ethosuximide: Bauer 1982's renal share at steady state
  expect_equal(physiologyRow("ethosuximide")$fraction_renal, 0.25)
})

test_that("the remaining anaesthetic rows (2026-11-18)", {
  cl <- function(d, ...) getDrugPK(d, 70, 170, 35, "male", ...)$PK[[1]]$cl1
  for (d in c("fentanyl", "sufentanil", "hydromorphone", "ketamine",
              "rocuronium", "vecuronium"))
    expect_false(is.null(physiologyRow(d)), info = d)
  # sufentanil: Lange's measured 0.92, hepatic 0.62, renal 0.02, the rest
  # outside both
  r <- physiologyRow("sufentanil")
  expect_equal(cl("sufentanil", physiology = c(liver = 0.5)) / cl("sufentanil"),
               0.38 + 0.62 * hepaticClearanceFactor(0.92, 1, 0.5),
               tolerance = 1e-6)
  # rocuronium: a third renal, so the renal slider moves it by a third
  expect_equal(cl("rocuronium", physiology = c(renal = 0.1)) / cl("rocuronium"),
               0.33 * 0.1 + 0.67, tolerance = 1e-6)
  # hydromorphone's oral first pass follows its extraction ratio
  expect_true(physiologyRow("hydromorphone")$oral_first_pass)
  expect_lt(getDrugPK("hydromorphone", 70, 170, 35, "male",
                      physiology = c(liver = 2))$PK[[1]]$bioavailability_PO,
            getDrugPK("hydromorphone", 70, 170, 35, "male")$PK[[1]]$bioavailability_PO)
})
