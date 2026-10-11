# ---------------------------------------------------------------------------
# Library-wide covariate robustness and metadata consistency
#
# WHAT THIS FILE TESTS
#
# The 20 drug model functions in R/drugs_*.R, together with their metadata rows
# in inst/extdata/drugDefaults_global.csv.  The existing per-drug files
# (test-drugs-<name>.R) are *golden masters*: each pins the exact v1..v3 /
# cl1..cl3 a single model returns at one or two covariate points.  This file is
# deliberately the complementary shape.  It says nothing about any individual
# drug's numbers; instead it sweeps the whole library and asserts the
# properties that must hold for *every* drug, plus the consistency of the CSV
# metadata with the code that consumes it.  Adding a drug therefore needs no
# edit here beyond the three censuses -- two-compartment models, sex-sensitive
# models, and the covariate-bound pathologies -- each of which is labelled as a
# census in place and exists precisely so that such a change is visible.
#
# Reference / citation strings are deliberately OUT OF SCOPE here.  They are
# free text, they are edited independently of the numbers, and nothing in the
# simulation engine depends on them.  Nothing in this file asserts on them.
#
# KNOWN LIMITATIONS AND PINNED QUIRKS
#
#  1. remimazolam computes `Fcl1_sex <- exp(16.3/100)` for women (Eleveld 2025)
#     and then never multiplies it into cl1.  The sex effect on V3 *is*
#     applied.  Pinned below as a quirk: fixing drugs_remimazolam.R should
#     deliberately update this test.
#
#  2. The remifentanil Kim branch (used when BMI >= 30) carries clearances and
#     volumes that are *linear* in age with negative slopes, so cl2 crosses
#     zero at age 106.29 yr -- inside the app's allowed range (MAX_AGE = 110).
#     v2 does the same for very heavy women.  Pinned below as a quirk.
#
#  3. dexmedetomidine's neonatal (age <= 1 yr) CPBEnd clearance carries a
#     postnatal-age maturation factor that is exactly 0 when age is exactly 0
#     (MIN_AGE).  Pinned below as a quirk.
#
#  4. propofol still contains the original Schnider `default` list -- the only
#     caller of lbmJames() in the package -- but it is immediately overwritten
#     by the Eleveld block before it is returned.  The James lean-body-mass
#     pathology is therefore currently inert.  Pinned below (propofol's cl1 is
#     height-invariant) so that any attempt to re-enable Schnider trips here.
#
#  5. drugDefaults_global.csv violates Lower <= Typical <= Upper for two drugs
#     (dexmedetomidine, remimazolam).  simulationPlot() draws the ribbon from
#     Lower/Upper and the line from Typical, so for those two the "typical"
#     line falls outside its own band.  Pinned below as a quirk.
#
#  6. Bounds behaviour (group 3) asserts only that nothing errors or warns; it
#     does NOT claim the numbers produced at MIN/MAX covariates are clinically
#     meaningful.  Several models are extrapolated far outside their published
#     range there and the numbers are meaningless by construction.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (drug library coverage & metadata consistency).
# Expected values derived from first principles -- the published model
# equations (Eleveld 2025 remimazolam sex factors, Kim remifentanil linear age
# terms, James 1976 lean-body-mass parabola), closed-form calculus on those
# equations, and the algebraic invariants of a mammillary compartment system
# (Vieta's relations between the micro rate constants and the eigenvalues) --
# never by pasting back what the code printed.  Where pinning current behaviour
# IS the point, the comment says so explicitly.
# ---------------------------------------------------------------------------


# --- shared fixtures -------------------------------------------------------

# The drug functions are internal (not exported), so resolve them explicitly
# out of the namespace.  This is exactly how getDrugPK() reaches them, via
# eval(call(drug, ...)) guarded by the CSV allowlist, and it works whether the
# package was loaded with devtools::load_all() or properly installed.
stanpumpRns <- asNamespace("stanpumpR")

callDrug <- function(drug, weight, height, age, sex) {
  do.call(
    get(drug, envir = stanpumpRns, mode = "function"),
    list(weight, height, age, sex)
  )
}

# The six parameters every PK event must carry.  getDrugPK() reads exactly
# these out of X$PK[[event]] and converts them to micro rate constants.
PK_PARAMS <- c("v1", "v2", "v3", "cl1", "cl2", "cl3")

drugDefaults <- getDrugDefaultsGlobal()
drugList <- drugDefaults$Drug

# A *realistic* covariate grid.  Deliberately not the full cross product of
# weight x height x age: a 20 kg patient who is 50 cm tall is not a patient,
# and testing such a point only measures how a model extrapolates into
# nonsense.  These 14 profiles pair weight, height and age the way a real
# body does, and span neonate through very elderly, lean through extreme
# obesity, so that every internal covariate branch in the library is visited:
#   * dexmedetomidine switches to its neonatal model at age <= 1 yr
#   * oxytocin switches to a rat model at weight <= 1 kg (no realistic human
#     profile reaches it; MIN_WEIGHT = 0.1 in the bounds sweep below does)
#   * remifentanil switches from Eleveld to Kim at BMI >= 30
#   * propofol / remifentanil maturation functions are steep below ~5 yr
covariateProfiles <- data.frame(
  label  = c("neonate", "infant", "preschool", "school age", "adolescent",
             "young adult", "adult", "obese adult", "tall lean adult",
             "elderly", "obese elderly", "very elderly",
             "obese very elderly", "extreme obesity"),
  weight = c(3,   7,   20,  35,  50,  70,  70,  100, 70,  70,  100, 50,  100, 150),
  height = c(50,  68,  100, 140, 150, 170, 170, 170, 190, 170, 170, 150, 170, 190),
  age    = c(0.5, 0.5, 5,   11,  18,  25,  40,  40,  40,  70,  70,  95,  95,  95),
  stringsAsFactors = FALSE
)
covariateSexes <- c("male", "female")

# The covariate box the Shiny UI will actually let a user reach, straight from
# R/globalVariables.R.  Every corner of it must at least not crash.
covariateBounds <- expand.grid(
  weight = c(MIN_WEIGHT, MAX_WEIGHT),
  height = c(MIN_HEIGHT, MAX_HEIGHT),
  age    = c(MIN_AGE, MAX_AGE),
  sex    = covariateSexes,
  stringsAsFactors = FALSE
)

# Several models in the library are genuinely two-compartment.  Rather than
# carrying a separate code path, they signal that to cube() by setting
# v3 = 1 (an inert placeholder volume, never divided by zero) and cl3 = 0,
# which drives k31 = cl3 / v3 = 0 and makes cube() fall through to its
# quadratic branch.  So "cl3 == 0 AND v3 == 1" is a legal, meaningful state
# and is handled explicitly everywhere below -- the positivity assertion is
# never weakened for the three-compartment models to accommodate it.
isTwoCompartment <- function(pkEvent) {
  pkEvent$cl3 == 0 && pkEvent$v3 == 1
}

# Every (drug, event) pair that uses the two-compartment placeholder, as a
# census.  dexmedetomidine appears here only through its neonatal branch,
# which is why the census is keyed on drug:event and not on drug alone.
TWO_COMPARTMENT_EVENTS <- sort(c(
  "dexmedetomidine:default", "dexmedetomidine:CPBStart",
  "dexmedetomidine:CPB36", "dexmedetomidine:CPB35", "dexmedetomidine:CPB34",
  "dexmedetomidine:CPB33", "dexmedetomidine:CPB32", "dexmedetomidine:CPB31",
  "dexmedetomidine:CPBEnd",
  "lidocaine:default", "oliceridine:default", "oxycodone:default",
  "oxytocin:default", "rocuronium:default"
))


# --- 1. structural contract ------------------------------------------------

test_that("every drug in drugDefaults_global.csv is a callable model function", {
  # getDrugPK() rejects any drug not in this column and then calls a function
  # of that exact name, so the CSV column and the namespace must agree.
  expect_gt(length(drugList), 0)
  expect_identical(anyDuplicated(drugList), 0L)
  expect_false(any(is.na(drugList) | !nzchar(drugList)))

  missingFunctions <- drugList[
    !vapply(
      drugList,
      function(d) exists(d, envir = stanpumpRns, mode = "function", inherits = FALSE),
      logical(1)
    )
  ]
  expect_identical(missingFunctions, character(0))

  # The four positional arguments getDrugPK() supplies, in order.
  wrongSignature <- drugList[
    !vapply(
      drugList,
      function(d) identical(
        names(formals(get(d, envir = stanpumpRns, mode = "function"))),
        c("weight", "height", "age", "sex")
      ),
      logical(1)
    )
  ]
  expect_identical(wrongSignature, character(0))
})

test_that("every drug model returns the shape getDrugPK() consumes", {
  # getDrugPK() does, in order: X$tPeak, names(X$PK), then for each event
  # X$PK[[event]]$v1 .. $cl3.  Anything missing there produces a downstream
  # NULL arithmetic error rather than a useful message, so check it head on.
  shapeProblems <- character(0)

  for (drug in drugList) {
    X <- callDrug(drug, 70, 170, 40, "male")

    if (!is.list(X)) {
      shapeProblems <- c(shapeProblems, paste0(drug, ": not a list"))
      next
    }
    if (!"PK" %in% names(X) || !is.list(X$PK) || length(X$PK) == 0) {
      shapeProblems <- c(shapeProblems, paste0(drug, ": no usable PK element"))
      next
    }
    # getDrugPK() iterates names(X$PK); an unnamed element cannot be reached.
    if (is.null(names(X$PK)) || !all(nzchar(names(X$PK)))) {
      shapeProblems <- c(shapeProblems, paste0(drug, ": PK has unnamed events"))
    }
    # simCpCe() falls back to pkSets[[1]] when no events fire, and the event
    # table is matched against "default", so "default" must be present.
    if (!"default" %in% names(X$PK)) {
      shapeProblems <- c(shapeProblems, paste0(drug, ": no 'default' PK event"))
      next
    }
    if (!is.numeric(X$tPeak) || length(X$tPeak) != 1L || !is.finite(X$tPeak)) {
      shapeProblems <- c(shapeProblems, paste0(drug, ": tPeak not a finite scalar"))
    }
    # tPeak drives the optimize() search for ke0 over c(0, 200) in getDrugPK();
    # a value at or outside that interval could not be recovered.
    if (is.numeric(X$tPeak) && length(X$tPeak) == 1L &&
        is.finite(X$tPeak) && (X$tPeak <= 0 || X$tPeak >= 200)) {
      shapeProblems <- c(
        shapeProblems,
        paste0(drug, ": tPeak ", X$tPeak, " outside optimize() bracket (0, 200)")
      )
    }

    for (event in names(X$PK)) {
      present <- PK_PARAMS %in% names(X$PK[[event]])
      if (!all(present)) {
        shapeProblems <- c(shapeProblems, paste0(
          drug, ":", event, ": missing ",
          paste(PK_PARAMS[!present], collapse = ",")
        ))
        next
      }
      for (p in PK_PARAMS) {
        value <- X$PK[[event]][[p]]
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
          shapeProblems <- c(shapeProblems, paste0(
            drug, ":", event, ":", p, " is not a finite numeric scalar"
          ))
        }
      }
    }
  }

  expect_identical(shapeProblems, character(0))
})


# --- 2. covariate sweep ----------------------------------------------------

test_that("all six PK parameters stay finite and positive over realistic covariates", {
  # A compartmental model is only physically meaningful when every volume and
  # every clearance is strictly positive: getDrugPK() forms k10 = cl1/v1,
  # k21 = cl2/v2, k31 = cl3/v3 and hands them to cube(), whose closed-form
  # cubic solution assumes a nonnegative, stable mammillary system.  A zero or
  # negative volume divides by zero or flips the sign of a rate constant; a
  # negative clearance makes the system unstable (a concentration that grows
  # without bound).  So this is a first-principles requirement, not a
  # convention: it must hold for every drug at every realistic covariate.
  #
  # The single documented exception is the two-compartment placeholder
  # (cl3 == 0 with v3 == 1); those events are checked on the other five
  # parameters and the placeholder itself is verified exactly, below.
  violations <- character(0)

  for (drug in drugList) {
    for (i in seq_len(nrow(covariateProfiles))) {
      profile <- covariateProfiles[i, ]
      for (sex in covariateSexes) {
        X <- callDrug(drug, profile$weight, profile$height, profile$age, sex)
        for (event in names(X$PK)) {
          pkEvent <- X$PK[[event]]
          twoComp <- isTwoCompartment(pkEvent)
          # For a two-compartment model the third compartment is switched off
          # wholesale; a model that zeroed only one of the two would silently
          # produce k31 = 0 with a live v3, so flag that as well.
          if (!twoComp && (pkEvent$cl3 == 0 || pkEvent$v3 == 1)) {
            violations <- c(violations, paste0(
              drug, ":", event, "/", profile$label, "/", sex,
              " half-disabled third compartment (v3=", pkEvent$v3,
              ", cl3=", pkEvent$cl3, ")"
            ))
          }
          checkThese <- if (twoComp) setdiff(PK_PARAMS, "cl3") else PK_PARAMS
          for (p in checkThese) {
            value <- pkEvent[[p]]
            if (!is.finite(value) || value <= 0) {
              violations <- c(violations, paste0(
                drug, ":", event, "/", profile$label, "/", sex,
                " ", p, " = ", format(value)
              ))
            }
          }
        }
      }
    }
  }

  expect_identical(violations, character(0))
})

test_that("the two-compartment placeholder is exactly the documented set", {
  # A census, not a property: it pins which models are two-compartment so that
  # a drug silently losing (or gaining) its third compartment shows up here.
  observed <- character(0)

  for (drug in drugList) {
    for (i in seq_len(nrow(covariateProfiles))) {
      profile <- covariateProfiles[i, ]
      for (sex in covariateSexes) {
        X <- callDrug(drug, profile$weight, profile$height, profile$age, sex)
        for (event in names(X$PK)) {
          if (isTwoCompartment(X$PK[[event]])) {
            observed <- c(observed, paste0(drug, ":", event))
          }
        }
      }
    }
  }

  expect_identical(sort(unique(observed)), TWO_COMPARTMENT_EVENTS)

  # dexmedetomidine is the only drug whose compartment count depends on a
  # covariate: the adult Dyck model is three-compartment, the neonatal
  # (age <= 1 yr) model is two-compartment.
  expect_false(isTwoCompartment(callDrug("dexmedetomidine", 70, 170, 40, "male")$PK$default))
  expect_true(isTwoCompartment(callDrug("dexmedetomidine", 3.5, 50, 0.5, "male")$PK$default))
  expect_length(callDrug("dexmedetomidine", 70, 170, 40, "male")$PK, 1L)
  expect_length(callDrug("dexmedetomidine", 3.5, 50, 0.5, "male")$PK, 9L)
})

test_that("every drug yields a well-posed mammillary system over realistic covariates", {
  # The point of the positivity sweep above is that these parameters feed
  # cube().  Verify the consequence directly, from theory rather than from
  # recorded output.  For the three-compartment mammillary system
  #     x' = -(k10+k12+k13) x1 + k21 x2 + k31 x3, ...
  # the characteristic polynomial is
  #     L^3 + a2 L^2 + a1 L + a0,
  #     a2 = k10+k12+k13+k21+k31
  #     a1 = k10 k31 + k21 k31 + k21 k13 + k10 k21 + k31 k12
  #     a0 = k10 k21 k31
  # and compartmental theory guarantees three distinct, real, strictly
  # positive roots whenever the micro constants are positive.  Vieta's
  # relations then give sum(lambda) = a2, prod(lambda) = a0 and
  # sum of pairwise products = a1 -- three independent identities that must
  # hold for every drug in the library.  For the two-compartment models
  # k31 = 0 and the third root is exactly zero by construction.
  eigenProblems <- character(0)

  for (drug in drugList) {
    for (i in seq_len(nrow(covariateProfiles))) {
      profile <- covariateProfiles[i, ]
      for (sex in covariateSexes) {
        X <- callDrug(drug, profile$weight, profile$height, profile$age, sex)
        for (event in names(X$PK)) {
          pk <- X$PK[[event]]
          where <- paste0(drug, ":", event, "/", profile$label, "/", sex)

          k10 <- pk$cl1 / pk$v1
          k12 <- pk$cl2 / pk$v1
          k13 <- pk$cl3 / pk$v1
          k21 <- pk$cl2 / pk$v2
          k31 <- pk$cl3 / pk$v3
          lambda <- cube(k10, k12, k13, k21, k31)

          if (!all(is.finite(lambda))) {
            eigenProblems <- c(eigenProblems, paste0(where, " non-finite eigenvalues"))
            next
          }
          if (k31 > 0) {
            if (!(lambda[1] > lambda[2] && lambda[2] > lambda[3] && lambda[3] > 0)) {
              eigenProblems <- c(eigenProblems, paste0(
                where, " eigenvalues not distinct and positive: ",
                paste(format(lambda), collapse = ", ")
              ))
            }
            a2 <- k10 + k12 + k13 + k21 + k31
            a1 <- k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12
            a0 <- k10 * k21 * k31
            pairwise <- lambda[1] * lambda[2] + lambda[1] * lambda[3] +
              lambda[2] * lambda[3]
            # Relative tolerance: cube() solves the depressed cubic with
            # trigonometric (acos/cos) formulae, so the roots carry a few ulp
            # of trigonometric round-off; 1e-8 relative is far tighter than
            # anything that could hide a wrong root.
            if (abs(sum(lambda) - a2) > 1e-8 * a2 ||
                abs(pairwise - a1) > 1e-8 * a1 ||
                abs(prod(lambda) - a0) > 1e-8 * a0) {
              eigenProblems <- c(eigenProblems, paste0(where, " violates Vieta's relations"))
            }
          } else {
            # Two-compartment: cube() takes its quadratic branch, so the
            # third root is identically zero and the first two are the roots
            # of L^2 - (k10+k12+k21) L + k10 k21.
            if (!(lambda[1] > lambda[2] && lambda[2] > 0 && lambda[3] == 0)) {
              eigenProblems <- c(eigenProblems, paste0(
                where, " two-compartment eigenvalues wrong: ",
                paste(format(lambda), collapse = ", ")
              ))
            }
            if (abs(lambda[1] + lambda[2] - (k10 + k12 + k21)) >
                  1e-8 * (k10 + k12 + k21) ||
                abs(lambda[1] * lambda[2] - k10 * k21) > 1e-8 * k10 * k21) {
              eigenProblems <- c(eigenProblems, paste0(where, " violates Vieta's relations"))
            }
          }
        }
      }
    }
  }

  expect_identical(eigenProblems, character(0))
})


# --- 3. behaviour at the covariate bounds ----------------------------------

test_that("no drug model errors or warns anywhere in the covariate box", {
  # MIN/MAX_WEIGHT, MIN/MAX_HEIGHT and MIN/MAX_AGE are what R/globalVariables.R
  # lets the UI produce, so every corner is reachable by a user.  Deliberately
  # NOT asserted here: that the numbers are sensible.  Several models are
  # extrapolated far outside their published covariate range at these corners
  # and produce absurd (including non-positive) parameters -- see the pinned
  # census in the next test.  The contract this test enforces is only that the
  # app gets a value back rather than an exception, because getDrugPK() has no
  # tryCatch around eval(call(drug, ...)).
  #
  # Errors and warnings are accumulated rather than asserted one call at a
  # time, so a single broken model cannot mask the rest of the library, and
  # the failure message names every offending covariate corner at once.  Only
  # the fact of a condition is recorded, never its text: R translates
  # condition messages, and this suite has to pass in any locale.
  problems <- character(0)

  for (drug in drugList) {
    for (i in seq_len(nrow(covariateBounds))) {
      b <- covariateBounds[i, ]
      where <- paste0(drug, " @ weight=", b$weight, " height=", b$height,
                      " age=", b$age, " ", b$sex)
      X <- withCallingHandlers(
        tryCatch(
          callDrug(drug, b$weight, b$height, b$age, b$sex),
          error = function(e) {
            problems <<- c(problems, paste0(where, ": ERROR"))
            NULL
          }
        ),
        warning = function(w) {
          problems <<- c(problems, paste0(where, ": WARNING"))
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(X)) next
      # The shape has to survive the extremes even when the numbers do not.
      if (!is.list(X$PK) || !"default" %in% names(X$PK) ||
          !is.numeric(X$tPeak) || length(X$tPeak) != 1L) {
        problems <- c(problems, paste0(where, ": degraded return shape"))
      }
    }
  }

  expect_identical(problems, character(0))
})

test_that("pinned quirk: which models go non-positive at the covariate bounds", {
  # PINNED QUIRK -- this records the current unguarded surface so that adding a
  # covariate guard (clamping age, refusing BMI extremes, or bounding the Kim
  # branch) deliberately updates this test.  It is not a claim that the values
  # are correct; it is a claim about which models the app must protect the user
  # from.  cl3 == 0 on the two-compartment models is excluded because it is the
  # documented placeholder, not a pathology.
  observed <- character(0)

  for (drug in drugList) {
    for (i in seq_len(nrow(covariateBounds))) {
      b <- covariateBounds[i, ]
      X <- callDrug(drug, b$weight, b$height, b$age, b$sex)
      for (event in names(X$PK)) {
        pkEvent <- X$PK[[event]]
        checkThese <- if (isTwoCompartment(pkEvent)) {
          setdiff(PK_PARAMS, "cl3")
        } else {
          PK_PARAMS
        }
        for (p in checkThese) {
          value <- pkEvent[[p]]
          if (!is.finite(value) || value <= 0) {
            observed <- c(observed, paste0(drug, ":", event, ":", p))
          }
        }
      }
    }
  }

  expect_identical(
    sort(unique(observed)),
    c("dexmedetomidine:CPBEnd:cl1",
      "remifentanil:default:cl2",
      "remifentanil:default:v2")
  )
})

test_that("pinned quirk: remifentanil's Kim branch is linear in age and crosses zero", {
  # PINNED QUIRK.  Kim et al. report, for the obese remifentanil model,
  #     CL2 (L/min) = 1.94 - 0.028 * (age - 37)
  #     V3  (L)     = 4.00 - 0.0477 * (age - 37)
  #     V2  (L)     = 8.4 * (FFM/52.3)^0.573 - 0.0936 * (age - 37)
  # Linear-in-age terms with negative slopes have to cross zero somewhere; for
  # CL2 that is at age = 37 + 1.94/0.028 = 106.2857 yr, which is *inside* the
  # range the app allows (MAX_AGE = 110).  Expected values below come from the
  # published equation, not from the code's output.  Fixing this (a maturation
  # or a clamp) should deliberately update this test.
  #
  # BMI 41.5 puts this patient on the Kim branch (the Eleveld branch is used
  # below BMI 30).
  kimCl2 <- function(age) 1.94 - 0.028 * (age - 37)

  expect_equal_rounded(
    kimCl2(100),
    callDrug("remifentanil", 120, 170, 100, "male")$PK$default$cl2
  )
  # 0.008 L/min: still positive, but two orders of magnitude below the adult
  # value of 1.72 L/min, six years short of MAX_AGE.
  expect_equal_rounded(
    kimCl2(106),
    callDrug("remifentanil", 120, 170, 106, "male")$PK$default$cl2
  )
  # And frankly negative at MAX_AGE.
  atMaxAge <- callDrug("remifentanil", 120, 170, MAX_AGE, "male")$PK$default$cl2
  expect_equal_rounded(kimCl2(MAX_AGE), atMaxAge)
  expect_lt(atMaxAge, 0)

  # The other side of the branch point: below BMI 30 the model is Eleveld,
  # whose clearances are products of exponentials and therefore strictly
  # positive at every age.  So this is a branch-specific pathology, not an
  # age-range problem in remifentanil generally.  86 kg at 170 cm is BMI 29.76.
  expect_lt(86 / (170 / 100)^2, 30)
  belowBranch <- callDrug("remifentanil", 86, 170, MAX_AGE, "male")
  expect_gt(belowBranch$PK$default$cl2, 0)
  expect_gt(belowBranch$PK$default$v2, 0)
})

test_that("pinned quirk: dexmedetomidine CPBEnd clearance is exactly zero at age 0", {
  # PINNED QUIRK.  The neonatal (age <= 1 yr) dexmedetomidine model scales the
  # post-bypass clearance by a postnatal-age maturation factor of the form
  # PNA / (PNA + TM50) with PNA in days.  At age exactly 0 -- MIN_AGE, and a
  # value the UI will accept for a delivery-room patient -- that factor is
  # identically 0, so cl1 is 0 and k10 collapses to zero: a drug that is never
  # eliminated.  Just above zero it recovers smoothly, which shows this is the
  # singular point of the maturation function rather than a scaling error.
  # Fixing this (a minimum postnatal age, or gestational-age scaling) should
  # deliberately update this test.
  atZero <- callDrug("dexmedetomidine", 3.5, 50, MIN_AGE, "male")
  expect_identical(atZero$PK$CPBEnd$cl1, 0)

  justAbove <- callDrug("dexmedetomidine", 3.5, 50, 1e-6, "male")
  expect_gt(justAbove$PK$CPBEnd$cl1, 0)

  # Only CPBEnd carries the maturation term; the other eight events do not, so
  # they stay positive even at age 0.
  otherEvents <- setdiff(names(atZero$PK), "CPBEnd")
  expect_true(all(vapply(otherEvents, function(e) atZero$PK[[e]]$cl1 > 0, logical(1))))
})


# --- 4. the sex branch -----------------------------------------------------

test_that("both sexes are supported by every drug model", {
  # sex reaches the models as the bare strings "male"/"female" (see
  # R/globalVariables.R defaultSex and getDrugPK()'s signature).  Models branch
  # on one or the other and fall through for anything else, so both spellings
  # have to produce a usable parameter set for every drug.
  sexProblems <- character(0)

  for (drug in drugList) {
    for (sex in covariateSexes) {
      X <- callDrug(drug, 70, 170, 40, sex)
      for (event in names(X$PK)) {
        pkEvent <- X$PK[[event]]
        checkThese <- if (isTwoCompartment(pkEvent)) {
          setdiff(PK_PARAMS, "cl3")
        } else {
          PK_PARAMS
        }
        for (p in checkThese) {
          value <- pkEvent[[p]]
          if (!is.finite(value) || value <= 0) {
            sexProblems <- c(sexProblems, paste0(drug, ":", event, ":", p, "/", sex))
          }
        }
      }
    }
  }

  expect_identical(sexProblems, character(0))
})

test_that("the census of sex-sensitive models is exactly as documented", {
  # A census, not a property.  Most published models in this library carry no
  # sex covariate at all, which is a modelling decision and not a defect -- but
  # a model that *acquires* or *loses* a sex term should be a deliberate,
  # visible change.  Checked at two ages because sex terms in these models are
  # frequently gated on puberty (remifentanil's PPUB/ELDY window) or on
  # maturation (propofol's Al-Sallami FFM).
  census <- function(age) {
    flat <- function(X) unlist(lapply(X$PK, function(e) unlist(e[PK_PARAMS])))
    Filter(Negate(is.null), lapply(drugList, function(d) {
      m <- flat(callDrug(d, 70, 170, age, "male"))
      f <- flat(callDrug(d, 70, 170, age, "female"))
      if (isTRUE(all.equal(m, f, tolerance = 0))) NULL else d
    }))
  }

  expect_identical(sort(unlist(census(40))),
                   c("propofol", "remifentanil", "remimazolam"))
  expect_identical(sort(unlist(census(8))),
                   c("propofol", "remifentanil", "remimazolam"))
})

test_that("pinned quirk: remimazolam computes a female clearance factor it never applies", {
  # PINNED QUIRK.  Eleveld et al. (Br J Anaesth 2025;135:206-217) report two
  # sex effects for remimazolam, expressed as exponential fractional changes:
  #     Kcl1_sex = 16.3 %  ->  female CL1 = male CL1 * exp(0.163) = 1.177 x
  #     Kv3_sex  = 28.7 %  ->  female V3  = male V3  * exp(0.287) = 1.332 x
  # drugs_remimazolam.R computes both (Fcl1_sex and Fv3_sex) but multiplies
  # only Fv3_sex into v3; cl1 is left as 1.12 * (weight/70)^0.75 with no sex
  # term, so women are given a 15 % lower clearance than the paper specifies.
  # Expected ratios below come from the published percentages, not from the
  # code.  Fixing drugs_remimazolam.R should deliberately update this test.
  male   <- callDrug("remimazolam", 70, 170, 40, "male")$PK$default
  female <- callDrug("remimazolam", 70, 170, 40, "female")$PK$default

  # V3 is applied correctly ...
  expect_equal_rounded(exp(28.7 / 100), female$v3 / male$v3)
  # ... and cl3 inherits it through the (V3/18.6)^0.75 allometric term.
  expect_equal_rounded(exp(28.7 / 100)^0.75, female$cl3 / male$cl3)

  # ... but CL1 does not move at all.  The published value is exp(0.163).
  expect_identical(female$cl1, male$cl1)
  expect_false(isTRUE(all.equal(female$cl1 / male$cl1, exp(16.3 / 100))))

  # Independently confirm the male CL1 is the plain allometric term, i.e. that
  # nothing else is quietly absorbing the sex factor: doubling weight must
  # multiply cl1 by exactly 2^0.75 for both sexes.
  for (sex in covariateSexes) {
    at70  <- callDrug("remimazolam", 70, 170, 40, sex)$PK$default$cl1
    at140 <- callDrug("remimazolam", 140, 170, 40, sex)$PK$default$cl1
    expect_equal_rounded(2^0.75, at140 / at70)
  }
})


# --- 5. metadata consistency ----------------------------------------------

test_that("the CSV drug list, the Collate field and the namespace all agree", {
  # DESCRIPTION carries an explicit Collate:, so a drug file that is not listed
  # there is silently dropped from the build (R CMD build fails outright).  The
  # Collate list is also the only enumeration of the drug sources that survives
  # into an installed package, which is why it -- and not a directory listing
  # of R/ -- is the reference here.
  collate <- utils::packageDescription("stanpumpR", fields = "Collate")
  expect_false(is.na(collate))

  collateFiles <- trimws(unlist(strsplit(collate, "[[:space:],]+")))
  collateFiles <- collateFiles[nzchar(collateFiles)]
  collateDrugs <- sort(sub("^drugs_(.*)\\.R$", "\\1",
                           grep("^drugs_.*\\.R$", collateFiles, value = TRUE)))

  # Both directions at once: every CSV row has a source file and every drug
  # source file has a CSV row.
  expect_identical(collateDrugs, sort(drugList))

  # And every one of those names resolves to a function in the namespace.
  expect_true(all(vapply(
    collateDrugs,
    function(d) exists(d, envir = stanpumpRns, mode = "function", inherits = FALSE),
    logical(1)
  )))

  # getDrugDefaults() subsets by name; a duplicated or missing row would give
  # the caller two rows or none, and getDrugPK() would build a malformed list.
  for (drug in drugList) {
    expect_identical(nrow(getDrugDefaults(drug)), 1L)
  }
})

test_that("drugDefaults_global.csv columns are well formed", {
  expect_true(all(
    c("Drug", "Concentration.Units", "Bolus.Units", "Infusion.Units",
      "Default.Units", "Units", "Color", "Lower", "Upper", "Typical",
      "MEAC", "endCe") %in% names(drugDefaults)
  ))

  # simCpCe() switches on Concentration.Units with no default arm.  Any value
  # other than "mcg" or "ng" leaves mg_Conv/mcg_Conv/ng_Conv undefined and the
  # very next line fails with an obscure "object not found".
  expect_true(all(drugDefaults$Concentration.Units %in% c("mcg", "ng")))

  # Colors go straight into ggplot scales as literal strings.
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", drugDefaults$Color)))

  # MEAC divides the effect-site concentration in simCpCe() (guarded against 0)
  # and endCe is a plotted threshold; both must be non-negative numbers.
  expect_true(is.numeric(drugDefaults$MEAC))
  expect_true(is.numeric(drugDefaults$endCe))
  expect_false(anyNA(drugDefaults$MEAC))
  expect_false(anyNA(drugDefaults$endCe))
  expect_true(all(drugDefaults$MEAC >= 0))
  expect_true(all(drugDefaults$endCe >= 0))

  # Lower/Upper/Typical are numeric and the band is never inverted.
  expect_true(is.numeric(drugDefaults$Lower))
  expect_true(is.numeric(drugDefaults$Upper))
  expect_true(is.numeric(drugDefaults$Typical))
  expect_false(anyNA(drugDefaults$Lower))
  expect_false(anyNA(drugDefaults$Upper))
  expect_false(anyNA(drugDefaults$Typical))
  expect_true(all(drugDefaults$Lower <= drugDefaults$Upper))
})

test_that("pinned quirk: two drugs have a Typical outside their own Lower/Upper band", {
  # PINNED QUIRK.  simulationPlot() draws the shaded therapeutic band from
  # lowerTypical/upperTypical (which getDrugPK() fills from the CSV's Lower and
  # Upper) and the "typical" line from Typical.  For the ribbon to contain its
  # own line the CSV must satisfy Lower <= Typical <= Upper.  Two rows do not:
  #   dexmedetomidine  Lower 0.4  Typical 10   Upper 0.8   (Typical 12x Upper)
  #   remimazolam      Lower 0.3  Typical 0.2  Upper 0.6   (Typical < Lower)
  # Correcting either CSV row should deliberately update this test.  Rows where
  # all three are zero (naloxone, which has no published therapeutic range) are
  # excluded: they carry no band at all.
  banded <- drugDefaults[
    !(drugDefaults$Lower == 0 & drugDefaults$Upper == 0 & drugDefaults$Typical == 0),
  ]
  outOfBand <- banded$Drug[
    banded$Typical < banded$Lower | banded$Typical > banded$Upper
  ]
  expect_identical(sort(outOfBand), c("dexmedetomidine", "remimazolam"))
})

test_that("every unit string in the CSV is classifiable by the simCpCe grep logic", {
  # simCpCe() classifies a dose row purely by substring search on its Units
  # string:
  #   mass    grep("mg") / grep("mcg") / grep("ng")   -- applied in that order
  #   per kg  grep("kg")
  #   per hr  grep("hr")
  #   route   grepl("PO") / grepl("IM") / grepl("IN")
  #   bolus   NOT (min | hr | PO | IM | IN)
  # The mass conversions are applied one after another to the same column, so a
  # unit matching two of them would be converted twice.  Exactly one mass token
  # must therefore match, and at most one route token.  Everything the drug
  # table offers must also appear in allUnits (R/globalVariables.R), which is
  # what the dose-table dropdown is built from -- a unit in the CSV but not in
  # allUnits could never be selected.
  unitProblems <- character(0)

  for (i in seq_len(nrow(drugDefaults))) {
    drug <- drugDefaults$Drug[i]
    units <- drugDefaults$Units[[i]]

    if (!is.character(units) || length(units) == 0) {
      unitProblems <- c(unitProblems, paste0(drug, ": empty Units"))
      next
    }
    for (u in units) {
      masses <- c(mg = grepl("mg", u), mcg = grepl("mcg", u), ng = grepl("ng", u))
      if (sum(masses) != 1L) {
        unitProblems <- c(unitProblems, paste0(
          drug, " '", u, "': ", sum(masses), " mass tokens match (",
          paste(names(masses)[masses], collapse = "+"), ")"
        ))
      }
      routes <- c(PO = grepl("PO", u), IM = grepl("IM", u), IN = grepl("IN", u))
      if (sum(routes) > 1L) {
        unitProblems <- c(unitProblems, paste0(
          drug, " '", u, "': ambiguous route (",
          paste(names(routes)[routes], collapse = "+"), ")"
        ))
      }
      # A rate and an extravascular route are mutually exclusive: simCpCe()
      # would mark such a row neither bolus nor infusion and hand it to the
      # PO/IM/IN path with a per-time dose.
      isRate <- grepl("min", u) || grepl("hr", u)
      if (isRate && any(routes)) {
        unitProblems <- c(unitProblems, paste0(
          drug, " '", u, "': both a rate and an extravascular route"
        ))
      }
      # Per-kg units must be per kg of something, never a stray "kg".
      if (grepl("kg", u) && !grepl("/kg", u)) {
        unitProblems <- c(unitProblems, paste0(drug, " '", u, "': 'kg' not used as /kg"))
      }
      if (!u %in% allUnits) {
        unitProblems <- c(unitProblems, paste0(drug, " '", u, "': not offered in allUnits"))
      }
    }

    # The unit pre-selected for this drug has to be one the drug offers.
    defaultUnit <- drugDefaults$Default.Units[i]
    if (is.na(defaultUnit) || !defaultUnit %in% units) {
      unitProblems <- c(unitProblems, paste0(
        drug, ": Default.Units '", defaultUnit, "' not among its Units"
      ))
    }
    # Bolus./Infusion.Units drive the Suggest Dosing panel.  They may be NA for
    # a drug with no such route (oxycodone is oral only), but when present they
    # must come from the matching vocabulary in R/globalVariables.R.
    bolusUnit <- drugDefaults$Bolus.Units[i]
    if (!is.na(bolusUnit) && !bolusUnit %in% bolusUnits) {
      unitProblems <- c(unitProblems, paste0(drug, ": Bolus.Units '", bolusUnit, "' unknown"))
    }
    infusionUnit <- drugDefaults$Infusion.Units[i]
    if (!is.na(infusionUnit) && !infusionUnit %in% infusionUnits) {
      unitProblems <- c(unitProblems, paste0(
        drug, ": Infusion.Units '", infusionUnit, "' unknown"
      ))
    }
  }

  expect_identical(unitProblems, character(0))

  # oxycodone is the only oral-only drug and so the only row allowed to omit
  # both parenteral unit columns; pinned so a new NA elsewhere is noticed.
  expect_identical(
    drugDefaults$Drug[is.na(drugDefaults$Bolus.Units)], "oxycodone"
  )
  expect_identical(
    drugDefaults$Drug[is.na(drugDefaults$Infusion.Units)], "oxycodone"
  )
})

test_that("the Units column survives the expand / simplify round trip", {
  # getDrugDefaultsGlobal(expand = TRUE) splits the comma-separated Units cell
  # into a character vector; the app writes it back with drugUnitsSimplify()
  # when the user edits the drug table.  A unit containing a comma, or stray
  # whitespace around a comma, would not survive.
  raw <- getDrugDefaultsGlobal(expand = FALSE)
  expect_identical(raw$Drug, drugList)
  expect_type(raw$Units, "character")
  expect_identical(drugUnitsSimplify(drugUnitsExpand(raw$Units)), raw$Units)
  expect_identical(drugUnitsSimplify(drugDefaults$Units), raw$Units)
})


# --- 6. lbmJames pathology -------------------------------------------------

test_that("lbmJames matches the closed form of the James parabola", {
  # James (1976) lean body mass is, for weight w (kg) and height h (cm),
  #     lbm(w) = a * w - b * (w / h)^2,   a = 1.10, b = 128 for men
  #                                       a = 1.07, b = 148 for women
  # which is a downward parabola in w.  Two exact consequences follow by
  # calculus, and both are derived here rather than read off the code:
  #     vertex     w* = a h^2 / (2 b)     lbm(w*) = a^2 h^2 / (4 b)
  #     zero       w0 = a h^2 / b         lbm(w0) = 0
  # Above w0 the equation returns a NEGATIVE lean body mass.
  for (sex in covariateSexes) {
    a <- if (sex == "female") 1.07 else 1.10
    b <- if (sex == "female") 148 else 128
    for (h in c(150, 160, 170, 180, 190)) {
      wStar <- a * h^2 / (2 * b)
      wZero <- a * h^2 / b

      # Peak value equals a^2 h^2 / 4b exactly.
      expect_equal_rounded(a^2 * h^2 / (4 * b), lbmJames(wStar, h, sex))

      # Nothing on a dense weight grid beats the analytic vertex, and the grid
      # maximiser lands within one grid step of it.  The 1e-9 slack absorbs the
      # case where the grid happens to contain the vertex exactly (male, 160 cm
      # puts w* at exactly 110 kg) and the two evaluations differ by one ulp.
      grid <- seq(1, 400, by = 0.5)
      gridValues <- vapply(grid, function(w) lbmJames(w, h, sex), numeric(1))
      expect_lte(max(gridValues), lbmJames(wStar, h, sex) + 1e-9)
      expect_lte(abs(grid[which.max(gridValues)] - wStar), 0.5)

      # Zero crossing: the parabola's second root.  Compared against an
      # absolute floor rather than a relative tolerance because the expected
      # value is exactly 0 (the observed residual is ~3e-14, pure round-off in
      # the squared term).
      expect_lt(abs(lbmJames(wZero, h, sex)), 1e-9)

      # And genuinely negative past it -- a "lean body mass" of less than
      # nothing, for a patient the app will happily accept.
      expect_lt(lbmJames(wZero * 1.10, h, sex), 0)
    }
  }
})

test_that("pinned quirk: lbmJames is non-monotonic and negative at plausible sizes", {
  # PINNED QUIRK.  The pathology is not confined to absurd inputs.  For a
  # 170 cm man the vertex sits at 124.2 kg, so a 130 kg patient is assigned a
  # SMALLER lean body mass than a 120 kg patient; for a 160 cm woman the
  # equation reaches zero at 163 kg, well inside the range of patients seen in
  # bariatric practice.  Expected weights below come from the closed form
  # above, not from probing the code.
  expect_lt(lbmJames(130, 170, "male"), lbmJames(120, 170, "male"))
  expect_lt(lbmJames(200, 160, "female"), 0)

  # Vertex weights, straight from a h^2 / (2 b).
  expect_equal_rounded(1.10 * 170^2 / (2 * 128), 124.1796875)
  expect_equal_rounded(1.07 * 160^2 / (2 * 148),  92.5405405405405)

  # At the corner of the covariate box it is catastrophic rather than merely
  # wrong: MAX_WEIGHT at MIN_HEIGHT gives roughly -3.2e5 kg.
  expect_lt(lbmJames(MAX_WEIGHT, MIN_HEIGHT, "male"), -1e5)
  expect_lt(lbmJames(MAX_WEIGHT, MIN_HEIGHT, "female"), -1e5)
})

test_that("pinned quirk: no drug the app returns is affected by lbmJames today", {
  # PINNED QUIRK, and the reason the pathology above is currently inert.
  # propofol is the package's only caller of lbmJames(): it builds a Schnider
  # `default` list whose cl1 term contains lbmJames(weight, height, sex), and
  # then -- 190 lines later -- rebinds `default` to the Eleveld list before
  # returning.  The Schnider list, and with it the James equation, is dead
  # code.  The observable signature is that Eleveld's CL has no height term at
  # all (CL = exp(theta) * (weight/70)^0.75 * KCL * DCL), whereas Schnider's
  # does, through both the explicit height term and lbmJames.  So propofol's
  # returned cl1 must be exactly height-invariant.  Re-enabling Schnider --
  # which would expose users to the negative-LBM surface above -- should
  # deliberately update this test.
  cl1 <- function(h) callDrug("propofol", 70, h, 40, "male")$PK$default$cl1
  expect_identical(cl1(150), cl1(190))
  expect_identical(cl1(MIN_HEIGHT), cl1(MAX_HEIGHT))

  # v3 does scale with height (Eleveld's V3 uses Al-Sallami fat-free mass), so
  # the invariance above is specific to cl1 and not an artefact of the drug
  # ignoring height wholesale.
  v3 <- function(h) callDrug("propofol", 70, h, 40, "male")$PK$default$v3
  expect_false(isTRUE(all.equal(v3(150), v3(190))))
})
