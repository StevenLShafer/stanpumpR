#' Get the pharmacokinetic and pharmacodynamic values for a drug based on
#' patient covariates
#'
#' See \code{vignette("stanpumpR-single-PK", package = "stanpumpR")} for an example
#'
#' @param drug name of drug (string)
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as string: "female" or "male"
#' @param drugDefaults output from \code{getDrugDefaults(drug)}
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#'   Passed only to drug models that declare it; the rest ignore it.
#' @param cyp2c19 CYP2C19 metaboliser phenotype, one of \code{CYP2C19_VALUES}.
#'   Passed only to drug models that declare it (escitalopram, citalopram).
#' @param resolveMetabolite should a drug that names an active metabolite have
#'   that metabolite's coefficients built?  Set FALSE when resolving the
#'   metabolite itself, which stops a cascade from recursing.
#' @param adjustToFFM scale the model's volumes and clearances to the patient's
#'   fat-free mass (the default) rather than total body weight; see
#'   `docs/weight-adjustment.md`.  Ignored by models that carry their own
#'   fat-free-mass covariate (propofol, remifentanil).
#' @param osmolality baseline serum osmolality in mOsm/kg, before any osmotic
#'   agent.  Passed only to drug models that declare it (mannitol); the rest
#'   ignore it.
#' @param creatinine serum creatinine in mg/dL, or NULL (the default) for the
#'   assumed normal value for the patient's age and sex.  Passed only to the renal
#'   models that declare it (mannitol, vancomycin, gentamicin, cefazolin,
#'   sugammadex, gabapentin, pregabalin, oxycodone, meloxicam intravenous); see
#'   `R/renalFunction.R`.
#' @param model internal: a model's output to use in place of calling the
#'   drug's own function.  \code{getDrugPK()} sets it to build the PK sets of a
#'   drug's parallel systems (see \code{parallelSystemSets()}); leave NULL.
#'
#' @returns a list: the drug's PK sets (\code{PK}, one per PK event), its
#'   \code{tPeak} and \code{reference}, the covariates, and the library's
#'   values for the drug -- among them \code{MEAC} and \code{endCe}, the
#'   effect-site threshold of the recovery time that \code{simCpCe()}
#'   computes when \code{plotRecovery = TRUE}.
#'
#' @examples
#' PK <- stanpumpR::getDrugPK(
#'   drug = "remifentanil",
#'   weight = 70,
#'   height = 170,
#'   age = 50,
#'   sex = "male",
#'   stanpumpR::getDrugDefaults('remifentanil')
#' )
#'
#' @export
getDrugPK <- function(
  drug,
  weight,
  height,
  age,
  sex,
  drugDefaults = getDrugDefaults(drug),
  cyp2d6 = CYP2D6_DEFAULT,
  resolveMetabolite = TRUE,
  adjustToFFM = TRUE,
  osmolality = OSMOLALITY_DEFAULT,
  creatinine = NULL,
  cyp2c19 = CYP2C19_DEFAULT,
  model = NULL
)
{
  drugList <- getDrugDefaultsGlobal()$Drug
  if (!drug %in% drugList) stop("Unknown drug: ", drug)
  if (length(sex) != 1 || !sex %in% SEX_VALUES) {
    stop("Invalid sex: ", paste(sex, collapse = ", "),
         ". Must be one of: ", paste(SEX_VALUES, collapse = ", "))
  }
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  if (length(cyp2c19) != 1 || !cyp2c19 %in% CYP2C19_VALUES) {
    stop("Invalid cyp2c19: ", paste(cyp2c19, collapse = ", "),
         ". Must be one of: ", paste(CYP2C19_VALUES, collapse = ", "))
  }
  if (!is_valid_number(osmolality, MIN_OSMOLALITY, MAX_OSMOLALITY)) {
    stop("Invalid osmolality: ", paste(osmolality, collapse = ", "),
         ". Must be a number between ", MIN_OSMOLALITY, " and ",
         MAX_OSMOLALITY, " mOsm/kg")
  }
  # An empty creatinine field reports NA: that means "not entered", the same
  # as NULL, and the models fall back to the assumed normal value.  NaN is
  # not blank: it falls through to the check below and is rejected.
  if (!is.null(creatinine) && length(creatinine) == 1 && is.na(creatinine) &&
      !(is.numeric(creatinine) && is.nan(creatinine)))
    creatinine <- NULL
  if (!is.null(creatinine) &&
      !is_valid_number(creatinine, MIN_CREATININE, MAX_CREATININE)) {
    stop("Invalid creatinine: ", paste(creatinine, collapse = ", "),
         ". Must be a number between ", MIN_CREATININE, " and ",
         MAX_CREATININE, " mg/dL, or NULL for the assumed normal value")
  }

  # Every model takes the four patient covariates.  A pharmacogenetic
  # phenotype goes only to models that name it, so that a drug whose kinetics
  # depend on one can add it to its signature without every other drug model
  # having to change, and so that a model taking only ... is not handed an
  # argument it cannot forward.
  #
  # Both lookups below must search the SAME place, and get() is used rather
  # than match.fun() for exactly that reason.  exists() and get() default to
  # the calling frame, whose enclosure is this package's namespace, so they
  # find the drug functions whether the package is loaded or installed.
  # match.fun() does not: it searches parent.frame(2), the environment of
  # getDrugPK's own CALLER.  Under devtools::load_all() the drug functions
  # happen to be visible there and it resolved; in an installed package they
  # are internal and the caller is the user's workspace, so it failed with
  # "object 'remifentanil' of mode 'function' was not found".
  #
  # That combination passed all 1504 tests and broke R CMD check on four
  # platforms, because nothing in the suite runs against an installed
  # package.  Two lookups of the same name, one line apart, resolving in
  # different environments.
  covariates <- list(weight = weight, height = height, age = age, sex = sex)
  if (exists(drug, mode = "function") &&
      "cyp2d6" %in% names(formals(get(drug, mode = "function"))))
    covariates$cyp2d6 <- cyp2d6
  if (exists(drug, mode = "function") &&
      "cyp2c19" %in% names(formals(get(drug, mode = "function"))))
    covariates$cyp2c19 <- cyp2c19
  # Likewise the fat-free-mass switch: every drug model in the library
  # declares it, but a mocked model taking only ... need not.
  if (exists(drug, mode = "function") &&
      "adjustToFFM" %in% names(formals(get(drug, mode = "function"))))
    covariates$adjustToFFM <- adjustToFFM
  # And the baseline serum osmolality, which only an osmotic agent reads.
  if (exists(drug, mode = "function") &&
      "osmolality" %in% names(formals(get(drug, mode = "function"))))
    covariates$osmolality <- osmolality
  # And the serum creatinine, which only the renally cleared models read.
  if (exists(drug, mode = "function") &&
      "creatinine" %in% names(formals(get(drug, mode = "function"))))
    covariates["creatinine"] <- list(creatinine)
  # Dispatch on the name, not the resolved function, so that a drug with no
  # covariate function at all -- an inhaled gas, which belongs on the gas path
  # and never reaches here -- still fails with R's own "could not find
  # function", which is what test-gas-routing.R pins.
  X <- if (is.null(model)) do.call(drug, covariates) else model
  tPeak <- X$tPeak
  # Which curve tPeak was measured against.  Defaults to the intravenous
  # bolus, which is what every drug in the library assumed before oral-only
  # drugs arrived, so no existing model changes.
  tPeakRoute <- if (is.null(X$tPeakRoute)) ROUTE_IV else X$tPeakRoute
  if (length(tPeakRoute) != 1 || !tPeakRoute %in% TPEAK_ROUTES) {
    stop("Invalid tPeakRoute for ", drug, ": ", paste(tPeakRoute, collapse = ", "),
         ". Must be one of: ", paste(TPEAK_ROUTES, collapse = ", "))
  }

  events <- names(X$PK)
  for (event in events)
  {
    v1  <- X$PK[[event]]$v1
    v2  <- X$PK[[event]]$v2
    v3  <- X$PK[[event]]$v3
    cl1 <- X$PK[[event]]$cl1
    cl2 <- X$PK[[event]]$cl2
    cl3 <- X$PK[[event]]$cl3

    # Note on Oral, IM, and IN route PK #
    # Oral is (for now) state 4 for plasma and state 5 for effect site #
    # IM is state 5 for plasma and state 6 for effect site #
    # IN is state 6 for plasma and state 7 for effect site #
    # Plan to change these to make the code clearer:       #
    # For IV, states 1-3 for plasma, states 1-4 for effect site #
    # PO will add state_PO, associated with ka_PO #
    # IM will add state_IM, associated with ka_IM #
    # IN will add state_IN, associated with ka_IN #
    # RA will add state_RA, associated with ka_RA #

    # Set up PK for oral delivery
    if (is.null(X$PK[[event]]$ka_PO))
    {
      ka_PO <- 0
      bioavailability_PO <- 0
      tlag_PO <- 0
    } else {
      ka_PO <- X$PK[[event]]$ka_PO
      if (is.null(X$PK[[event]]$bioavailability_PO))
      {
        bioavailability_PO <- 1
      } else {
        bioavailability_PO <- X$PK[[event]]$bioavailability_PO
      }
      if (is.null(X$PK[[event]]$tlag_PO))
      {
        tlag_PO <- 0
      } else {
        tlag_PO <- X$PK[[event]]$tlag_PO
      }
    }

    # Set up PK for IM delivery
    if (is.null(X$PK[[event]]$ka_IM))
    {
      ka_IM <- 0
      bioavailability_IM <- 0
      tlag_IM <- 0
    } else {
      ka_IM <- X$PK[[event]]$ka_IM
      if (is.null(X$PK[[event]]$bioavailability_IM))
      {
        bioavailability_IM <- 1
      } else {
        bioavailability_IM <- X$PK[[event]]$bioavailability_IM
      }
      if (is.null(X$PK[[event]]$tlag_IM))
      {
        tlag_IM <- 0
      } else {
        tlag_IM <- X$PK[[event]]$tlag_IM
      }
    }

    # Set up PK for IN delivery
    if (is.null(X$PK[[event]]$ka_IN))
    {
      ka_IN <- 0
      bioavailability_IN <- 0
      tlag_IN <- 0
    } else {
      ka_IN <- X$PK[[event]]$ka_IN
      if (is.null(X$PK[[event]]$bioavailability_IN))
      {
        bioavailability_IN <- 1
      } else {
        bioavailability_IN <- X$PK[[event]]$bioavailability_IN
      }
      if (is.null(X$PK[[event]]$tlag_IN))
      {
        tlag_IN <- 0
      } else {
        tlag_IN <- X$PK[[event]]$tlag_IN
      }
    }

    # Set up PK for sublingual delivery, exactly as the routes above
    if (is.null(X$PK[[event]]$ka_SL))
    {
      ka_SL <- 0
      bioavailability_SL <- 0
      tlag_SL <- 0
    } else {
      ka_SL <- X$PK[[event]]$ka_SL
      bioavailability_SL <- if (is.null(X$PK[[event]]$bioavailability_SL)) 1 else X$PK[[event]]$bioavailability_SL
      tlag_SL <- if (is.null(X$PK[[event]]$tlag_SL)) 0 else X$PK[[event]]$tlag_SL
    }

    # Set up PK for regional anesthesia (RA): a local anesthetic injected into
    # tissue and absorbed first-order into the systemic circulation.
    if (is.null(X$PK[[event]]$ka_RA))
    {
      ka_RA <- 0
      bioavailability_RA <- 0
      tlag_RA <- 0
    } else {
      ka_RA <- X$PK[[event]]$ka_RA
      if (is.null(X$PK[[event]]$bioavailability_RA))
      {
        bioavailability_RA <- 1
      } else {
        bioavailability_RA <- X$PK[[event]]$bioavailability_RA
      }
      if (is.null(X$PK[[event]]$tlag_RA))
      {
        tlag_RA <- 0
      } else {
        tlag_RA <- X$PK[[event]]$tlag_RA
      }
    }

    # An optional slow second tissue depot for RA (parallel first-order
    # absorption).  The drug declares ka_RA_slow (1/min) and
    # fraction_RA_slow, the share of the ABSORBED dose that goes through the
    # slow depot.  Internally that is a second route, "RAslow", fed by the
    # same RA dose rows (simCpCe() duplicates them), with the bioavailability
    # split between the two depots and the same lag:
    #     bioavailability_RA     = F x (1 - fraction_RA_slow)
    #     bioavailability_RAslow = F x fraction_RA_slow
    ka_RAslow <- 0
    bioavailability_RAslow <- 0
    tlag_RAslow <- 0
    if (ka_RA > 0 && !is.null(X$PK[[event]]$ka_RA_slow))
    {
      slow <- X$PK[[event]]$fraction_RA_slow
      if (!is_valid_number(X$PK[[event]]$ka_RA_slow) || X$PK[[event]]$ka_RA_slow <= 0 ||
          !is_valid_number(slow, 0, 1))
        stop("Invalid slow RA depot for ", drug, ": needs ka_RA_slow > 0 and ",
             "fraction_RA_slow between 0 and 1.")
      ka_RAslow <- X$PK[[event]]$ka_RA_slow
      bioavailability_RAslow <- bioavailability_RA * slow
      bioavailability_RA <- bioavailability_RA * (1 - slow)
      tlag_RAslow <- tlag_RA
    }

    # An optional second oral depot (parallel first-order absorption with a
    # lag of its own).  The drug declares ka_PO2 (1/min), fraction_PO2, the
    # share of the ABSORBED oral dose that goes through it, and optionally
    # tlag_PO2 (min; the oral lag if absent).  Internally that is a second
    # route, "PO2", fed by the same oral dose rows (simCpCe() duplicates
    # them), with the bioavailability split between the two depots:
    #     bioavailability_PO  = F x (1 - fraction_PO2)
    #     bioavailability_PO2 = F x fraction_PO2
    # Diclofenac (two lagged absorption paths, Standing 2011) is the example.
    ka_PO2 <- 0
    bioavailability_PO2 <- 0
    tlag_PO2 <- 0
    if (ka_PO > 0 && !is.null(X$PK[[event]]$ka_PO2))
    {
      share <- X$PK[[event]]$fraction_PO2
      lag2 <- X$PK[[event]]$tlag_PO2
      if (is.null(lag2)) lag2 <- tlag_PO
      if (!is_valid_number(X$PK[[event]]$ka_PO2) || X$PK[[event]]$ka_PO2 <= 0 ||
          !is_valid_number(share, 0, 1) || !is_valid_number(lag2, 0, Inf))
        stop("Invalid second oral depot for ", drug, ": needs ka_PO2 > 0, ",
             "fraction_PO2 between 0 and 1 and tlag_PO2 >= 0.")
      ka_PO2 <- X$PK[[event]]$ka_PO2
      bioavailability_PO2 <- bioavailability_PO * share
      bioavailability_PO <- bioavailability_PO * (1 - share)
      tlag_PO2 <- lag2
    }

    if (is.null(X$PK[[event]]$customFunction))
    {
      customFunction <- ""
    } else {
      customFunction <- X$PK[[event]]$customFunction
    }


    k10 <- cl1 / v1
    k12 <- cl2 / v1
    k13 <- cl3 / v1
    k21 <- cl2 / v2
    k31 <- cl3 / v3

    roots <- cube(k10, k12, k13, k21, k31)
    lambda_1 <- roots[1]
    lambda_2 <- roots[2]
    lambda_3 <- roots[3]

    # Bolus Delivery
    p_coef_bolus_l1  <- 0
    p_coef_bolus_l2  <- 0
    p_coef_bolus_l3  <- 0

    e_coef_bolus_l1  <- 0
    e_coef_bolus_l2  <- 0
    e_coef_bolus_l3  <- 0
    e_coef_bolus_ke0 <- 0

    # Infusion delivery
    p_coef_infusion_l1  <- 0
    p_coef_infusion_l2  <- 0
    p_coef_infusion_l3  <- 0

    e_coef_infusion_l1  <- 0
    e_coef_infusion_l2  <- 0
    e_coef_infusion_l3  <- 0
    e_coef_infusion_ke0 <- 0

    # PO Delivery
    p_coef_PO_l1  <- 0
    p_coef_PO_l2  <- 0
    p_coef_PO_l3  <- 0
    p_coef_PO_ka <- 0

    e_coef_PO_l1  <- 0
    e_coef_PO_l2  <- 0
    e_coef_PO_l3  <- 0
    e_coef_PO_ke0 <- 0
    e_coef_PO_ka  <- 0

    # IM Delivery
    p_coef_IM_l1  <- 0
    p_coef_IM_l2  <- 0
    p_coef_IM_l3  <- 0
    p_coef_IM_ka  <- 0

    e_coef_IM_l1  <- 0
    e_coef_IM_l2  <- 0
    e_coef_IM_l3  <- 0
    e_coef_IM_ke0 <- 0
    e_coef_IM_ka  <- 0

    # IN Delivery
    p_coef_IN_l1  <- 0
    p_coef_IN_l2  <- 0
    p_coef_IN_l3  <- 0
    p_coef_IN_ka  <- 0

    e_coef_IN_l1  <- 0
    e_coef_IN_l2  <- 0
    e_coef_IN_l3  <- 0
    e_coef_IN_ke0 <- 0
    e_coef_IN_ka  <- 0

    # SL Delivery
    p_coef_SL_l1  <- 0
    p_coef_SL_l2  <- 0
    p_coef_SL_l3  <- 0
    p_coef_SL_ka  <- 0

    e_coef_SL_l1  <- 0
    e_coef_SL_l2  <- 0
    e_coef_SL_l3  <- 0
    e_coef_SL_ke0 <- 0
    e_coef_SL_ka  <- 0

    # RA Delivery
    p_coef_RA_l1  <- 0
    p_coef_RA_l2  <- 0
    p_coef_RA_l3  <- 0
    p_coef_RA_ka  <- 0

    e_coef_RA_l1  <- 0
    e_coef_RA_l2  <- 0
    e_coef_RA_l3  <- 0
    e_coef_RA_ke0 <- 0
    e_coef_RA_ka  <- 0

    # RA slow depot Delivery
    p_coef_RAslow_l1  <- 0
    p_coef_RAslow_l2  <- 0
    p_coef_RAslow_l3  <- 0
    p_coef_RAslow_ka  <- 0

    e_coef_RAslow_l1  <- 0
    e_coef_RAslow_l2  <- 0
    e_coef_RAslow_l3  <- 0
    e_coef_RAslow_ke0 <- 0
    e_coef_RAslow_ka  <- 0

    # Second oral depot Delivery
    p_coef_PO2_l1  <- 0
    p_coef_PO2_l2  <- 0
    p_coef_PO2_l3  <- 0
    p_coef_PO2_ka  <- 0

    e_coef_PO2_l1  <- 0
    e_coef_PO2_l2  <- 0
    e_coef_PO2_l3  <- 0
    e_coef_PO2_ke0 <- 0
    e_coef_PO2_ka  <- 0

    if (k31 > 0)
    {
      p_coef_bolus_l1 <- (k21 - lambda_1) * (k31 - lambda_1) /
        (lambda_1 - lambda_2) /
        (lambda_1 - lambda_3) / v1
      p_coef_bolus_l2 <- (k21 - lambda_2) * (k31 - lambda_2) /
        (lambda_2 - lambda_1) /
        (lambda_2 - lambda_3) /
        v1
      p_coef_bolus_l3 <- (k21 - lambda_3) * (k31 - lambda_3) /
        (lambda_3 - lambda_2) /
        (lambda_3 - lambda_1) /
        v1
    } else {
      if (lambda_2 > 0)
      {
        p_coef_bolus_l1 <- (k21 - lambda_1) / (lambda_2 - lambda_1) / v1
        p_coef_bolus_l2 <- (k21 - lambda_2) / (lambda_1 - lambda_2) / v1
      } else {
        # One compartment.  Cp(0) = dose / v1, so the coefficient is 1/v1.
        # This branch previously divided by lambda_1 as well, which inflated
        # every concentration by 1/k10.  No drug in the library reached it
        # until codeine, whose disposition is identified only as CL and Vss.
        p_coef_bolus_l1 <- 1 / v1
      }
    }

    p_coef_infusion_l1 <- p_coef_bolus_l1 / lambda_1
    if (lambda_2 > 0) p_coef_infusion_l2 <- p_coef_bolus_l2 / lambda_2
    if (lambda_3 > 0) p_coef_infusion_l3 <- p_coef_bolus_l3 / lambda_3

    # find ke0 from tPeak
    #
    # ke0 is whatever makes the peak EFFECT SITE concentration fall at tPeak.
    # Which plasma curve that peak is measured against matters: a tPeak
    # observed after an intravenous bolus has to be solved against the bolus
    # response, and one observed after an oral dose against the ORAL response.
    #
    # Solving an oral tPeak against the bolus curve counts the absorption
    # delay twice, because the oral curve already peaks late.  For hydrocodone
    # that error is about 24 minutes.  An oral tPeak is counted from the
    # dose, so the solve carries the drug's absorption lag (pregabalin's is
    # 19 min); without it the effect site would peak one lag late.
    if (!is.null(X$ke0) && X$ke0 > 0)
    {
      # A drug may supply ke0 directly, which is the escape hatch for a time
      # to peak effect defined against a curve this function cannot build.
      # Desmetramadol is the case: it is never dosed, and its peak effect is
      # observed after an oral dose of its PARENT, so the driving curve is
      # the metabolite profile formed from tramadol.  That profile depends on
      # tramadol's absorption and formation, neither of which is in scope
      # here, because the metabolite's own PK is resolved before the parent's
      # coefficients are built.  The drug file records the tPeak it was
      # solved for and how.
      ke0 <- X$ke0
    } else if (tPeak > 0)
    {
      if (identical(tPeakRoute, ROUTE_PO))
      {
        ke0 <- ke0FromTPeak(
          tPeak = tPeak,
          coef = c(p_coef_bolus_l1, p_coef_bolus_l2, p_coef_bolus_l3) *
                 ka_PO / (ka_PO - c(lambda_1, lambda_2, lambda_3)) *
                 bioavailability_PO,
          lambda = c(lambda_1, lambda_2, lambda_3),
          ka = ka_PO,
          drug = drug,
          lag = tlag_PO
        )
      } else {
        ke0 <- stats::optimize(
          tPeakError, c(0,200),tPeak,
          p_coef_bolus_l1,
          p_coef_bolus_l2,
          p_coef_bolus_l3,
          lambda_1,
          lambda_2,
          lambda_3
        )$minimum
      }
    } else {
      ke0 <- 0
    }

    if (ke0 > 0)
    {
      e_coef_bolus_l1 <- p_coef_bolus_l1 / (ke0 - lambda_1) * ke0
      e_coef_infusion_l1 <- e_coef_bolus_l1 / lambda_1

      if (lambda_2 > 0)
      {
        e_coef_bolus_l2 <-  p_coef_bolus_l2 / (ke0 - lambda_2) * ke0
        e_coef_infusion_l2 <- e_coef_bolus_l2 / lambda_2
      }
      if (lambda_3 > 0)
      {
        e_coef_bolus_l3 <- p_coef_bolus_l3 / (ke0 - lambda_3) * ke0
        e_coef_infusion_l3 <- if (lambda_3 > 0) e_coef_bolus_l3 / lambda_3
      }
      e_coef_bolus_ke0 <- - e_coef_bolus_l1 - e_coef_bolus_l2 - e_coef_bolus_l3
      e_coef_infusion_ke0 <- e_coef_bolus_ke0 / ke0
    }

    if (ka_PO > 0)
    {
      p_coef_PO_l1   <- p_coef_bolus_l1 / (ka_PO - lambda_1) * ka_PO * bioavailability_PO
      p_coef_PO_l2   <- p_coef_bolus_l2 / (ka_PO - lambda_2) * ka_PO * bioavailability_PO
      p_coef_PO_l3   <- p_coef_bolus_l3 / (ka_PO - lambda_3) * ka_PO * bioavailability_PO
      p_coef_PO_ka   <- - p_coef_PO_l1 - p_coef_PO_l2 - p_coef_PO_l3

      e_coef_PO_l1   <- e_coef_bolus_l1 / (ka_PO - lambda_1) * ka_PO * bioavailability_PO
      e_coef_PO_l2   <- e_coef_bolus_l2 / (ka_PO - lambda_2) * ka_PO * bioavailability_PO
      e_coef_PO_l3   <- e_coef_bolus_l3 / (ka_PO - lambda_3) * ka_PO * bioavailability_PO
      e_coef_PO_ke0  <- e_coef_bolus_ke0 / (ka_PO - ke0)     * ka_PO * bioavailability_PO
      e_coef_PO_ka   <- - e_coef_PO_l1 - e_coef_PO_l2 - e_coef_PO_l3 - e_coef_PO_ke0
    }

    if (ka_IM > 0)
    {
      p_coef_IM_l1  <- p_coef_bolus_l1 / (ka_IM - lambda_1) * ka_IM * bioavailability_IM
      p_coef_IM_l2  <- p_coef_bolus_l2 / (ka_IM - lambda_2) * ka_IM * bioavailability_IM
      p_coef_IM_l3  <- p_coef_bolus_l3 / (ka_IM - lambda_3) * ka_IM * bioavailability_IM
      p_coef_IM_ka  <- - p_coef_IM_l1 - p_coef_IM_l2 - p_coef_IM_l3

      e_coef_IM_l1  <- e_coef_bolus_l1 / (ka_IM - lambda_1) * ka_IM * bioavailability_IM
      e_coef_IM_l2  <- e_coef_bolus_l2 / (ka_IM - lambda_2) * ka_IM * bioavailability_IM
      e_coef_IM_l3  <- e_coef_bolus_l3 / (ka_IM - lambda_3) * ka_IM * bioavailability_IM
      e_coef_IM_ke0 <- e_coef_bolus_ke0 / (ka_IM - ke0)    * ka_IM * bioavailability_IM
      e_coef_IM_ka  <- - e_coef_IM_l1 - e_coef_IM_l2 - e_coef_IM_l3 - e_coef_IM_ke0
    }

    if (ka_IN > 0)
    {
      p_coef_IN_l1  <- p_coef_bolus_l1 / (ka_IN - lambda_1) * ka_IN * bioavailability_IN
      p_coef_IN_l2  <- p_coef_bolus_l2 / (ka_IN - lambda_2) * ka_IN * bioavailability_IN
      p_coef_IN_l3  <- p_coef_bolus_l3 / (ka_IN - lambda_3) * ka_IN * bioavailability_IN
      p_coef_IN_ka  <- - p_coef_IN_l1 - p_coef_IN_l2 - p_coef_IN_l3

      e_coef_IN_l1  <- e_coef_bolus_l1 / (ka_IN - lambda_1) * ka_IN * bioavailability_IN
      e_coef_IN_l2  <- e_coef_bolus_l2 / (ka_IN - lambda_2) * ka_IN * bioavailability_IN
      e_coef_IN_l3  <- e_coef_bolus_l3 / (ka_IN - lambda_3) * ka_IN * bioavailability_IN
      e_coef_IN_ke0 <- e_coef_bolus_ke0 / (ka_IN - ke0) *     ka_IN * bioavailability_IN
      e_coef_IN_ka  <- - e_coef_IN_l1 - e_coef_IN_l2 - e_coef_IN_l3 - e_coef_IN_ke0
    }

    if (ka_SL > 0)
    {
      p_coef_SL_l1  <- p_coef_bolus_l1 / (ka_SL - lambda_1) * ka_SL * bioavailability_SL
      p_coef_SL_l2  <- p_coef_bolus_l2 / (ka_SL - lambda_2) * ka_SL * bioavailability_SL
      p_coef_SL_l3  <- p_coef_bolus_l3 / (ka_SL - lambda_3) * ka_SL * bioavailability_SL
      p_coef_SL_ka  <- - p_coef_SL_l1 - p_coef_SL_l2 - p_coef_SL_l3

      e_coef_SL_l1  <- e_coef_bolus_l1 / (ka_SL - lambda_1) * ka_SL * bioavailability_SL
      e_coef_SL_l2  <- e_coef_bolus_l2 / (ka_SL - lambda_2) * ka_SL * bioavailability_SL
      e_coef_SL_l3  <- e_coef_bolus_l3 / (ka_SL - lambda_3) * ka_SL * bioavailability_SL
      e_coef_SL_ke0 <- e_coef_bolus_ke0 / (ka_SL - ke0) *     ka_SL * bioavailability_SL
      e_coef_SL_ka  <- - e_coef_SL_l1 - e_coef_SL_l2 - e_coef_SL_l3 - e_coef_SL_ke0
    }

    if (ka_RA > 0)
    {
      p_coef_RA_l1  <- p_coef_bolus_l1 / (ka_RA - lambda_1) * ka_RA * bioavailability_RA
      p_coef_RA_l2  <- p_coef_bolus_l2 / (ka_RA - lambda_2) * ka_RA * bioavailability_RA
      p_coef_RA_l3  <- p_coef_bolus_l3 / (ka_RA - lambda_3) * ka_RA * bioavailability_RA
      p_coef_RA_ka  <- - p_coef_RA_l1 - p_coef_RA_l2 - p_coef_RA_l3

      e_coef_RA_l1  <- e_coef_bolus_l1 / (ka_RA - lambda_1) * ka_RA * bioavailability_RA
      e_coef_RA_l2  <- e_coef_bolus_l2 / (ka_RA - lambda_2) * ka_RA * bioavailability_RA
      e_coef_RA_l3  <- e_coef_bolus_l3 / (ka_RA - lambda_3) * ka_RA * bioavailability_RA
      e_coef_RA_ke0 <- e_coef_bolus_ke0 / (ka_RA - ke0) *     ka_RA * bioavailability_RA
      e_coef_RA_ka  <- - e_coef_RA_l1 - e_coef_RA_l2 - e_coef_RA_l3 - e_coef_RA_ke0
    }

    if (ka_RAslow > 0)
    {
      p_coef_RAslow_l1  <- p_coef_bolus_l1 / (ka_RAslow - lambda_1) * ka_RAslow * bioavailability_RAslow
      p_coef_RAslow_l2  <- p_coef_bolus_l2 / (ka_RAslow - lambda_2) * ka_RAslow * bioavailability_RAslow
      p_coef_RAslow_l3  <- p_coef_bolus_l3 / (ka_RAslow - lambda_3) * ka_RAslow * bioavailability_RAslow
      p_coef_RAslow_ka  <- - p_coef_RAslow_l1 - p_coef_RAslow_l2 - p_coef_RAslow_l3

      e_coef_RAslow_l1  <- e_coef_bolus_l1 / (ka_RAslow - lambda_1) * ka_RAslow * bioavailability_RAslow
      e_coef_RAslow_l2  <- e_coef_bolus_l2 / (ka_RAslow - lambda_2) * ka_RAslow * bioavailability_RAslow
      e_coef_RAslow_l3  <- e_coef_bolus_l3 / (ka_RAslow - lambda_3) * ka_RAslow * bioavailability_RAslow
      e_coef_RAslow_ke0 <- e_coef_bolus_ke0 / (ka_RAslow - ke0) *     ka_RAslow * bioavailability_RAslow
      e_coef_RAslow_ka  <- - e_coef_RAslow_l1 - e_coef_RAslow_l2 - e_coef_RAslow_l3 - e_coef_RAslow_ke0
    }

    if (ka_PO2 > 0)
    {
      p_coef_PO2_l1  <- p_coef_bolus_l1 / (ka_PO2 - lambda_1) * ka_PO2 * bioavailability_PO2
      p_coef_PO2_l2  <- p_coef_bolus_l2 / (ka_PO2 - lambda_2) * ka_PO2 * bioavailability_PO2
      p_coef_PO2_l3  <- p_coef_bolus_l3 / (ka_PO2 - lambda_3) * ka_PO2 * bioavailability_PO2
      p_coef_PO2_ka  <- - p_coef_PO2_l1 - p_coef_PO2_l2 - p_coef_PO2_l3

      e_coef_PO2_l1  <- e_coef_bolus_l1 / (ka_PO2 - lambda_1) * ka_PO2 * bioavailability_PO2
      e_coef_PO2_l2  <- e_coef_bolus_l2 / (ka_PO2 - lambda_2) * ka_PO2 * bioavailability_PO2
      e_coef_PO2_l3  <- e_coef_bolus_l3 / (ka_PO2 - lambda_3) * ka_PO2 * bioavailability_PO2
      e_coef_PO2_ke0 <- e_coef_bolus_ke0 / (ka_PO2 - ke0) *     ka_PO2 * bioavailability_PO2
      e_coef_PO2_ka  <- - e_coef_PO2_l1 - e_coef_PO2_l2 - e_coef_PO2_l3 - e_coef_PO2_ke0
    }

    # Vd Peak Effect
    if (tPeak == 0)
    {
      vdPeakEffect <- 0
    } else {
      vdPeakEffect <-
        1 /
        (
          e_coef_bolus_l1 * exp(-lambda_1 * tPeak) +
            e_coef_bolus_l2 * exp(-lambda_2 * tPeak) +
            e_coef_bolus_l3 * exp(-lambda_3 * tPeak) +
            e_coef_bolus_ke0 * exp(-ke0 * tPeak)
        )
    }
    assign(
      event,
      list(
        v1 = v1,
        v2 = v2,
        v3 = v3,
        cl1 = cl1,
        cl2 = cl2,
        cl3 = cl3,
        k10 = k10,
        k12 = k12,
        k13 = k13,
        k21 = k21,
        k31 = k31,

        ka_PO = ka_PO,
        bioavailability_PO = bioavailability_PO,
        tlag_PO = tlag_PO,

        ka_IM = ka_IM,
        bioavailability_IM = bioavailability_IM,
        tlag_IM = tlag_IM,

        ka_IN = ka_IN,
        bioavailability_IN = bioavailability_IN,
        tlag_IN = tlag_IN,

        ka_SL = ka_SL,
        bioavailability_SL = bioavailability_SL,
        tlag_SL = tlag_SL,

        ka_RA = ka_RA,
        bioavailability_RA = bioavailability_RA,
        tlag_RA = tlag_RA,

        ka_RAslow = ka_RAslow,
        bioavailability_RAslow = bioavailability_RAslow,
        tlag_RAslow = tlag_RAslow,

        ka_PO2 = ka_PO2,
        bioavailability_PO2 = bioavailability_PO2,
        tlag_PO2 = tlag_PO2,

        customFunction = customFunction,

        lambda_1 = lambda_1,
        lambda_2 = lambda_2,
        lambda_3 = lambda_3,
        ke0 = ke0,

        # Bolus Coefficients
        p_coef_bolus_l1 = p_coef_bolus_l1,
        p_coef_bolus_l2 = p_coef_bolus_l2,
        p_coef_bolus_l3 = p_coef_bolus_l3,

        e_coef_bolus_l1 = e_coef_bolus_l1,
        e_coef_bolus_l2 = e_coef_bolus_l2,
        e_coef_bolus_l3 = e_coef_bolus_l3,
        e_coef_bolus_ke0 = e_coef_bolus_ke0,


        # Infusion Coefficients
        p_coef_infusion_l1 = p_coef_infusion_l1,
        p_coef_infusion_l2 = p_coef_infusion_l2,
        p_coef_infusion_l3 = p_coef_infusion_l3,

        e_coef_infusion_l1 = e_coef_infusion_l1,
        e_coef_infusion_l2 = e_coef_infusion_l2,
        e_coef_infusion_l3 = e_coef_infusion_l3,
        e_coef_infusion_ke0 = e_coef_infusion_ke0,

        # PO Coefficients
        p_coef_PO_l1 = p_coef_PO_l1,
        p_coef_PO_l2 = p_coef_PO_l2,
        p_coef_PO_l3 = p_coef_PO_l3,
        p_coef_PO_ka = p_coef_PO_ka,

        e_coef_PO_l1 = e_coef_PO_l1,
        e_coef_PO_l2 = e_coef_PO_l2,
        e_coef_PO_l3 = e_coef_PO_l3,
        e_coef_PO_ke0 = e_coef_PO_ke0,
        e_coef_PO_ka = e_coef_PO_ka,

        # IM Coefficients
        p_coef_IM_l1 = p_coef_IM_l1,
        p_coef_IM_l2 = p_coef_IM_l2,
        p_coef_IM_l3 = p_coef_IM_l3,
        p_coef_IM_ka = p_coef_IM_ka,

        e_coef_IM_l1 = e_coef_IM_l1,
        e_coef_IM_l2 = e_coef_IM_l2,
        e_coef_IM_l3 = e_coef_IM_l3,
        e_coef_IM_ke0 = e_coef_IM_ke0,
        e_coef_IM_ka = e_coef_IM_ka,

        # IN Coefficients
        p_coef_IN_l1 = p_coef_IN_l1,
        p_coef_IN_l2 = p_coef_IN_l2,
        p_coef_IN_l3 = p_coef_IN_l3,
        p_coef_IN_ka = p_coef_IN_ka,

        e_coef_IN_l1 = e_coef_IN_l1,
        e_coef_IN_l2 = e_coef_IN_l2,
        e_coef_IN_l3 = e_coef_IN_l3,
        e_coef_IN_ke0 = e_coef_IN_ke0,
        e_coef_IN_ka = e_coef_IN_ka,

        # SL Coefficients
        p_coef_SL_l1 = p_coef_SL_l1,
        p_coef_SL_l2 = p_coef_SL_l2,
        p_coef_SL_l3 = p_coef_SL_l3,
        p_coef_SL_ka = p_coef_SL_ka,

        e_coef_SL_l1 = e_coef_SL_l1,
        e_coef_SL_l2 = e_coef_SL_l2,
        e_coef_SL_l3 = e_coef_SL_l3,
        e_coef_SL_ke0 = e_coef_SL_ke0,
        e_coef_SL_ka = e_coef_SL_ka,

        # RA Coefficients
        p_coef_RA_l1 = p_coef_RA_l1,
        p_coef_RA_l2 = p_coef_RA_l2,
        p_coef_RA_l3 = p_coef_RA_l3,
        p_coef_RA_ka = p_coef_RA_ka,

        e_coef_RA_l1 = e_coef_RA_l1,
        e_coef_RA_l2 = e_coef_RA_l2,
        e_coef_RA_l3 = e_coef_RA_l3,
        e_coef_RA_ke0 = e_coef_RA_ke0,
        e_coef_RA_ka = e_coef_RA_ka,

        # RA slow depot Coefficients
        p_coef_RAslow_l1 = p_coef_RAslow_l1,
        p_coef_RAslow_l2 = p_coef_RAslow_l2,
        p_coef_RAslow_l3 = p_coef_RAslow_l3,
        p_coef_RAslow_ka = p_coef_RAslow_ka,

        e_coef_RAslow_l1 = e_coef_RAslow_l1,
        e_coef_RAslow_l2 = e_coef_RAslow_l2,
        e_coef_RAslow_l3 = e_coef_RAslow_l3,
        e_coef_RAslow_ke0 = e_coef_RAslow_ke0,
        e_coef_RAslow_ka = e_coef_RAslow_ka,

        # Second oral depot Coefficients
        p_coef_PO2_l1 = p_coef_PO2_l1,
        p_coef_PO2_l2 = p_coef_PO2_l2,
        p_coef_PO2_l3 = p_coef_PO2_l3,
        p_coef_PO2_ka = p_coef_PO2_ka,

        e_coef_PO2_l1 = e_coef_PO2_l1,
        e_coef_PO2_l2 = e_coef_PO2_l2,
        e_coef_PO2_l3 = e_coef_PO2_l3,
        e_coef_PO2_ke0 = e_coef_PO2_ke0,
        e_coef_PO2_ka = e_coef_PO2_ka
      )
    )
  }

  PK <- sapply(events, function(x) list(get0(x)))

  # The second oral depot is carried by advanceClosedFormPO_IM_IN() and
  # advanceClosedForm1(), not by the metabolite engine or the per-formulation
  # runs, and an oral tPeak is solved against the first depot alone.
  if (any(vapply(PK, function(s) isTRUE(s$ka_PO2 > 0), logical(1))))
  {
    if (resolveMetabolite && !is.null(X$metabolite))
      stop("Invalid second oral depot for ", drug, ": a drug that forms an ",
           "active metabolite cannot have one.")
    if (!is.null(X$oralFormulations))
      stop("Invalid second oral depot for ", drug, ": a drug with more than ",
           "one oral formulation cannot have one.")
    if (identical(tPeakRoute, ROUTE_PO) && tPeak > 0 && is.null(X$ke0))
      stop("Invalid second oral depot for ", drug, ": an oral tPeak is solved ",
           "against a single depot; supply ke0 directly.")
  }

  # Further oral formulations.  The model's ka_PO, bioavailability_PO and
  # tlag_PO describe its default oral form (morphine: the immediate-release
  # tablet); each entry of X$oralFormulations, named by a word of
  # ORAL_FORMULATIONS, is another form with absorption of its own.  Each gets
  # a full copy of every PK set with only the oral terms replaced, so that
  # simCpCe() can run that formulation's doses through the same engines and
  # add the result: the disposition is linear and shared.
  oralFormulations <- NULL
  if (!is.null(X$oralFormulations))
  {
    if (resolveMetabolite && !is.null(X$metabolite))
      stop("Invalid oralFormulations for ", drug, ": a drug that forms an ",
           "active metabolite cannot yet have more than one oral formulation, ",
           "because the metabolite's coefficients are built from the default ",
           "oral absorption.")
    oralFormulations <- lapply(
      stats::setNames(names(X$oralFormulations), names(X$oralFormulations)),
      function(f) lapply(PK, oralFormulationSet, X$oralFormulations[[f]], drug, f)
    )
  }

  # An active metabolite is resolved by simulating the metabolite's own
  # disposition and convolving the parent's plasma profile through it.  The
  # coefficients ride along inside each PK set, so simCpCe() can hand them
  # straight to advanceClosedFormMetabolite().
  metaboliteName <- NULL
  if (resolveMetabolite && !is.null(X$metabolite))
  {
    metaboliteName <- X$metabolite$name
    metaboliteDefaults <- getDrugDefaults(metaboliteName)

    # resolveMetabolite = FALSE: one level only.  A cascade such as codeine to
    # morphine to morphine-6-glucuronide would need a two-stage convolution,
    # which this does not attempt, and the guard keeps a metabolite that names
    # a metabolite of its own from recursing.
    metabolitePK <- getDrugPK(
      drug = metaboliteName,
      weight = weight, height = height, age = age, sex = sex,
      drugDefaults = metaboliteDefaults,
      cyp2d6 = cyp2d6,
      cyp2c19 = cyp2c19,
      osmolality = osmolality,
      creatinine = creatinine,
      adjustToFFM = adjustToFFM,
      resolveMetabolite = FALSE
    )
    metaboliteSet <- metabolitePK$PK[[PK_EVENT_DEFAULT]]

    # A parent and its metabolite need not report in the same units, and the
    # internal dose unit follows the reported one.  Without this the curve
    # would be wrong by a thousandfold.
    unitScale <- metaboliteUnitScale(
      drugDefaults$Concentration.Units,
      metaboliteDefaults$Concentration.Units
    )

    firstPass <- X$metabolite$firstPassFraction
    if (is.null(firstPass)) firstPass <- 0
    mwRatio <- X$metabolite$mwRatio
    if (is.null(mwRatio)) mwRatio <- 1

    for (event in events)
    {
      PK[[event]]$metabolite <- list(
        name  = metaboliteName,
        ke0   = metaboliteSet$ke0,
        coefs = metaboliteCoefficients(
          parent            = PK[[event]],
          metabolite        = metaboliteSet,
          kFormation        = X$metabolite$kFormation,
          mwRatio           = mwRatio,
          unitScale         = unitScale,
          firstPassFraction = firstPass
        )
      )
    }
  }

  #  thisDrug <- which(drugDefaults$Drug == drug)
  out <-
    list(
      drug = drug,
      PK = PK,
      tPeakRoute = tPeakRoute,
      tPeak = tPeak,
      pkEvents = events,
      reference = if (is.null(X$reference)) "Not Available" else X$reference,
      weight = weight,
      height = height,
      age = age,
      sex = sex,
      upperTypical        = drugDefaults$Upper,
      lowerTypical        = drugDefaults$Lower,
      typical             = drugDefaults$Typical,
      MEAC                = drugDefaults$MEAC,
      Concentration.Units = drugDefaults$Concentration.Units,
      Bolus.Units         = drugDefaults$Bolus.Units,
      Infusion.Units      = drugDefaults$Infusion.Units,
      Units               = drugDefaults$Units,
      Default.Units       = drugDefaults$Default.Units,
      # The time-until-threshold concentration, which simCpCe() reads when
      # asked for recovery.  Until October 2026 this was `emerge`, read from an
      # Emerge column the library does not have, so a direct getDrugPK() +
      # simCpCe() call computed every time until threshold as zero; the app
      # and simulateDrugsWithCovariates() set endCe by hand (audit F03).
      endCe               = drugDefaults$endCe
    )

  # Appended rather than declared, because assigning NULL to a list element
  # does not create it: a drug with no metabolite returns exactly the shape it
  # always has.
  out$metaboliteName <- metaboliteName
  # An osmotic agent reports serum osmolality rather than its own
  # concentration: simCpCe() reads the baseline, the fraction and the molecular
  # weight from here.  See R/drugs_mannitol.R.
  out$osmotic <- X$osmotic
  # A drug whose oral absorption saturates scales each oral dose by its own
  # fraction absorbed: simCpCe() applies it.  See oralSaturationFraction().
  out$oralSaturation <- validateOralSaturation(X$oralSaturation, drug)
  # The same for sublingual doses (buprenorphine).
  out$sublingualSaturation <- validateOralSaturation(
    X$sublingualSaturation, drug, "sublingualSaturation")
  # A drug with more than one oral formulation: the PK sets of each further
  # one, which simCpCe() uses for that formulation's doses.
  out$oralFormulations <- oralFormulations
  # A drug plotted as the sum of independent linear systems that share its
  # doses: ketorolac, whose S and R enantiomers have dispositions of their own
  # (R/drugs_ketorolac.R).  The model's own PK is the first system; each entry
  # of X$parallelSystems is another, with PK sets of the same shape.  Each
  # system receives doseFraction of every dose (the drug's own share is
  # X$doseFraction), and simCpCe() runs them on the same time line and adds
  # the results, which is exact because each is linear.
  out$doseFraction <- parallelDoseFraction(X$doseFraction, drug)
  # A system may also be limited to some routes (meloxicam: the oral fit takes
  # the oral doses, the intravenous fit the intravenous ones); the drug's own
  # system declares X$routes, each parallel system its own routes.
  out$routes <- parallelRoutes(X$routes, drug)
  if (!is.null(X$parallelSystems))
    out$parallelSystems <- parallelSystemSets(X, PK, events, tPeakRoute, drug,
                                              covariates)
  # A pulsed extended-release formulation (Adderall XR): the fractions and
  # delays simCpCe() splits each of its doses into; see R/oral-pulses.R.
  out$oralPulses <- validateOralPulses(X$oralPulses, drug)
  return(out)
}

#' Check the share of each dose a parallel system receives
#'
#' @param x the declared fraction, or NULL for the whole dose
#' @param drug the drug's name, for the error message
#' @returns the fraction, or NULL when none was declared
#' @keywords internal
parallelDoseFraction <- function(x, drug)
{
  if (is.null(x)) return(NULL)
  if (!is_valid_number(x, 0, Inf) || x <= 0)
    stop("Invalid doseFraction for ", drug, ": must be a positive number.")
  x
}

#' Check the routes a parallel system receives
#'
#' @param x the declared routes (values of \code{DOSE_ROUTES}), or NULL for
#'   every route
#' @param drug the drug's name, for the error message
#' @returns the routes, or NULL when none were declared
#' @keywords internal
parallelRoutes <- function(x, drug)
{
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) == 0 || anyNA(x) || !all(x %in% DOSE_ROUTES))
    stop("Invalid routes for ", drug, ": must be some of ",
         paste(DOSE_ROUTES, collapse = ", "), ".")
  unique(x)
}

#' PK sets of a drug's parallel systems
#'
#' Builds each system's PK sets through \code{getDrugPK()} itself, on the
#' drug's own effect site (the systems add in plasma, so they must share ke0).
#'
#' @param X the drug model's output
#' @param PK the drug's own finished PK sets
#' @param events their names
#' @param tPeakRoute,drug as in \code{getDrugPK()}
#' @param covariates the patient covariates passed to the drug model
#' @returns a list, one entry per system: \code{name}, \code{doseFraction},
#'   \code{routes} and \code{PK}
#' @keywords internal
parallelSystemSets <- function(X, PK, events, tPeakRoute, drug, covariates)
{
  if (!is.null(X$metabolite) || !is.null(X$oralFormulations) ||
      !is.null(X$oralSaturation) || !is.null(X$sublingualSaturation) ||
      !is.null(X$osmotic))
    stop("Invalid parallelSystems for ", drug, ": not available with an ",
         "active metabolite, several oral formulations, saturable absorption ",
         "or an osmotic agent.")
  ke0 <- PK[[1]]$ke0
  lapply(X$parallelSystems, function(sys) {
    if (!identical(names(sys$PK), events))
      stop("Invalid parallelSystems for ", drug, ": each system needs the ",
           "same PK events as the drug (", paste(events, collapse = ", "), ").")
    sets <- getDrugPK(
      drug = drug,
      weight = covariates$weight, height = covariates$height,
      age = covariates$age, sex = covariates$sex,
      model = list(PK = sys$PK, tPeak = X$tPeak, tPeakRoute = tPeakRoute,
                   ke0 = if (ke0 > 0) ke0)
    )$PK
    list(
      name = sys$name,
      doseFraction = parallelDoseFraction(
        if (is.null(sys$doseFraction)) 1 else sys$doseFraction, drug),
      routes = parallelRoutes(sys$routes, drug),
      PK = sets
    )
  })
}

#' Oral coefficients for a given absorption
#'
#' The same arithmetic as the "PO Coefficients" block of \code{getDrugPK()},
#' from a finished PK set, so that a further oral formulation can be given
#' its own absorption on the drug's own disposition and effect site.
#'
#' @param pkSet a PK set from \code{getDrugPK()}
#' @param ka absorption rate constant, per minute
#' @param bioavailability fraction of the dose absorbed
#'
#' @returns a list of the nine \code{p_coef_PO_*} and \code{e_coef_PO_*}
#'   coefficients
#' @keywords internal
oralCoefficients <- function(pkSet, ka, bioavailability)
{
  lambda <- c(pkSet$lambda_1, pkSet$lambda_2, pkSet$lambda_3)
  scale  <- ka / (ka - lambda) * bioavailability
  p <- c(pkSet$p_coef_bolus_l1, pkSet$p_coef_bolus_l2, pkSet$p_coef_bolus_l3) * scale
  e <- c(pkSet$e_coef_bolus_l1, pkSet$e_coef_bolus_l2, pkSet$e_coef_bolus_l3) * scale
  eKe0 <- pkSet$e_coef_bolus_ke0 / (ka - pkSet$ke0) * ka * bioavailability
  list(
    p_coef_PO_l1  = p[1],
    p_coef_PO_l2  = p[2],
    p_coef_PO_l3  = p[3],
    p_coef_PO_ka  = -sum(p),
    e_coef_PO_l1  = e[1],
    e_coef_PO_l2  = e[2],
    e_coef_PO_l3  = e[3],
    e_coef_PO_ke0 = eKe0,
    e_coef_PO_ka  = -sum(e) - eKe0
  )
}

#' A PK set with the oral absorption of another formulation
#'
#' @param pkSet a PK set from \code{getDrugPK()}, for the default formulation
#' @param absorption the formulation's \code{ka_PO}, \code{bioavailability_PO}
#'   and \code{tlag_PO}
#' @param drug,formulation names, for the error messages
#'
#' @returns \code{pkSet} with its oral terms replaced
#' @keywords internal
oralFormulationSet <- function(pkSet, absorption, drug, formulation)
{
  if (!formulation %in% ORAL_FORMULATIONS)
    stop("Invalid oralFormulations for ", drug, ": '", formulation, "' is not ",
         "one of ", paste(ORAL_FORMULATIONS, collapse = ", "))
  ka <- absorption$ka_PO
  bioavailability <- absorption$bioavailability_PO
  if (is.null(bioavailability)) bioavailability <- 1
  tlag <- absorption$tlag_PO
  if (is.null(tlag)) tlag <- 0
  if (!is_valid_number(ka, 0, Inf) || ka <= 0 ||
      !is_valid_number(bioavailability, 0, 1) || bioavailability <= 0)
    stop("Invalid oralFormulations for ", drug, ": the ", formulation,
         " needs ka_PO > 0 and bioavailability_PO in (0, 1]")
  if (pkSet$ka_PO <= 0)
    stop("Invalid oralFormulations for ", drug, ": the drug has no default ",
         "oral absorption for the ", formulation, " to be an alternative to")
  # simCpCe() adds the formulations' series point by point, which needs every
  # run on the same time line, and the engines put a point where each oral
  # dose starts to be absorbed.  A different lag would put those points in
  # different places.
  if (!isTRUE(all.equal(tlag, pkSet$tlag_PO)))
    stop("Invalid oralFormulations for ", drug, ": the ", formulation,
         " must share the default oral lag (", pkSet$tlag_PO, " min)")
  pkSet$ka_PO <- ka
  pkSet$bioavailability_PO <- bioavailability
  utils::modifyList(pkSet, oralCoefficients(pkSet, ka, bioavailability))
}

#' Time of the peak effect site concentration for a given plasma curve
#'
#' The plasma curve is supplied as a sum of exponentials.  Convolving it with
#' the effect site's own response, ke0 exp(-ke0 t), is again a sum of
#' exponentials over the same eigenvalues plus ke0, so the peak is found by
#' maximising that sum directly.
#'
#' @param coef coefficients of the plasma curve
#' @param lambda its eigenvalues, same length
#' @param ke0 effect site rate constant, per minute
#' @param upper longest time to search, in minutes
#' @returns the time of the maximum, in minutes
#' @keywords internal
effectSitePeakTime <- function(coef, lambda, ke0, upper = 4000)
{
  use <- lambda > 0
  coef <- coef[use]
  lambda <- lambda[use]
  e <- coef * ke0 / (ke0 - lambda)
  stats::optimize(
    function(t) sum(e * exp(-lambda * t)) - sum(e) * exp(-ke0 * t),
    c(0, upper), maximum = TRUE
  )$maximum
}


#' Back-solve ke0 from a time to peak effect observed after an oral dose
#'
#' The effect site always peaks LATER than the plasma curve driving it, because
#' dCe/dt = ke0 (Cp - Ce) is still positive at the moment Cp turns over.  The
#' peak time falls monotonically towards the plasma peak as ke0 rises, so there
#' is exactly one ke0 for any target later than the plasma peak, and none at
#' all for a target at or before it.
#'
#' That second case is a real failure rather than a numerical one: a reported
#' peak effect earlier than the reported peak concentration cannot be produced
#' by any effect site model, and usually means the two numbers came from
#' different sources or that the earlier one describes onset rather than
#' maximum.  It raises instead of returning a huge ke0 that would quietly make
#' the effect site a copy of the plasma curve.
#'
#' The time is counted from the dose.  An absorption lag delays the plasma and
#' the effect site alike, so the curves are built from the start of absorption
#' and the lag is added to both peaks.
#'
#' @param tPeak observed time to peak effect after an oral dose, in minutes
#' @param coef coefficients of the oral plasma curve on the drug's own
#'   eigenvalues, already carrying absorption and bioavailability
#' @param lambda those eigenvalues
#' @param ka absorption rate constant, per minute
#' @param drug the drug's name, used only in the error message
#' @param lag oral absorption lag, in minutes
#' @returns ke0, per minute
#' @keywords internal
ke0FromTPeak <- function(tPeak, coef, lambda, ka, drug = "this drug", lag = 0)
{
  if (is.null(ka) || ka <= 0)
    stop("An oral tPeak needs an oral absorption constant, but ", drug,
         " has none.")

  use <- lambda > 0
  # The oral plasma curve: the drug's own exponentials plus the absorption one,
  # whose coefficient is minus the sum of the others so the curve starts at zero.
  oralCoef   <- c(coef[use], -sum(coef[use]))
  oralLambda <- c(lambda[use], ka)

  plasmaPeak <- lag + stats::optimize(
    function(t) sum(oralCoef * exp(-oralLambda * t)),
    c(0, 4000), maximum = TRUE
  )$maximum

  if (tPeak <= plasmaPeak)
    stop("Cannot solve ke0 for ", drug, ": a peak effect at ", round(tPeak, 1),
         " min is not reachable, because the oral plasma concentration itself ",
         "peaks at ", round(plasmaPeak, 1), " min and the effect site always ",
         "peaks later. Either the time to peak effect belongs after that, or ",
         "the absorption constant is too slow.")

  peakAt <- function(ke0) lag + effectSitePeakTime(oralCoef, oralLambda, ke0)

  # Bracket: peakAt() falls towards plasmaPeak as ke0 grows.  Widen downwards
  # until the peak is later than the target, which must happen as ke0 -> 0.
  lo <- 1e-6
  hi <- 10
  while (peakAt(lo) < tPeak && lo > 1e-12) lo <- lo / 10
  stats::uniroot(function(ke0) peakAt(ke0) - tPeak, c(lo, hi),
                 tol = .Machine$double.eps^0.5)$root
}


# Calculate the error between the predicted and actual time of peak effect
# site concentration
tPeakError <-   function(lambda_4, tPeak, p_coef_bolus_1,p_coef_bolus_2,p_coef_bolus_3, lambda_1, lambda_2, lambda_3)
{
  e_coef_bolus_1 <- p_coef_bolus_1 / (lambda_4 - lambda_1) * lambda_4

  if (lambda_2 > 0)
  {
    e_coef_bolus_2 <-  p_coef_bolus_2 / (lambda_4 - lambda_2) * lambda_4
  } else {
    e_coef_bolus_2 <- 0
  }
  if (lambda_3 > 0)
  {
    e_coef_bolus_3 <- p_coef_bolus_3 / (lambda_4 - lambda_3) * lambda_4
  } else {
    e_coef_bolus_3 <- 0
  }
  e_coef_bolus_4 <- - e_coef_bolus_1 - e_coef_bolus_2 - e_coef_bolus_3

  predPeak <- stats::optimize(CE,c(0,100), e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4, maximum=TRUE)$maximum
  return((tPeak-predPeak)^2)
}

# calculate c3 based on 4 coefficients, 4 exponents, and time
CE <- function(t, e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4)
{
  e_coef_bolus_1 * exp(-lambda_1 * t) +
    e_coef_bolus_2 * exp(-lambda_2 * t) +
    e_coef_bolus_3 * exp(-lambda_3 * t) +
    e_coef_bolus_4 * exp(-lambda_4 * t)
}

