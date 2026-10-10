# -----------------------------------------------------------------------------
# Help pages generated from the drug library
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer.  Verified by tests/testthat/test-help-drugs.R.
#
# Every drug in inst/extdata/drugDefaults_global.csv gets a help page that is
# computed, not written: the model function in R/drugs_<drug>.R is run at a
# handful of reference patients and its volumes, clearances, rate constants,
# half-lives and effect-site equilibration are tabulated, together with the
# units, typical range and thresholds from the defaults table and the citation
# the model function returns.  The user's guide noted that a hand-maintained
# model table "drifted once already"; this one cannot.
#
# The narrative about each model -- the population it was fitted to, where it
# is extrapolated, who did the work -- is Markdown in inst/help/drugs/<drug>.md
# and is appended to the generated page.
# -----------------------------------------------------------------------------

#' The patients at which the drug models are tabulated
#'
#' Chosen to show which covariates a model responds to: the reference adult
#' most models are centred on; a woman, for the models with a sex term; an
#' older adult; an obese adult (BMI 41, which switches remifentanil to the Kim
#' model); a child; and an infant (dexmedetomidine switches to its infant model
#' at age <= 1).
#' @noRd
helpReferencePatients <- function() {
  data.frame(
    label  = c("Reference adult", "Adult woman", "Older adult", "Obese adult", "Child", "Infant"),
    age    = c(40, 40, 80, 40, 5, 0.5),
    weight = c(70, 60, 70, 120, 20, 7),
    height = c(170, 165, 170, 170, 110, 65),
    sex    = c(SEX_MALE, SEX_FEMALE, SEX_MALE, SEX_MALE, SEX_MALE, SEX_MALE),
    stringsAsFactors = FALSE
  )
}

#' getDrugPK() for one reference patient, or NULL if the model errors
#' @noRd
helpDrugPK <- function(drug, patient, drugDefaults = getDrugDefaultsGlobal(), ...) {
  row <- drugDefaults[drugDefaults$Drug == drug, ]
  if (nrow(row) == 0) return(NULL)
  tryCatch(
    suppressWarnings(getDrugPK(
      drug = drug,
      weight = patient$weight,
      height = patient$height,
      age = patient$age,
      sex = patient$sex,
      drugDefaults = row,
      ...
    )),
    error = function(e) NULL
  )
}

#' The drug's model function, or NULL for a drug without one (the gases)
#' @noRd
helpDrugFunction <- function(drug) {
  if (!exists(drug, mode = "function")) return(NULL)
  get(drug, mode = "function")
}

#' The raw list a drug's model function returns at one patient, or NULL
#'
#' This is the function's own output, before getDrugPK() turns it into rate
#' constants: it is where the metabolite link and its formation parameters
#' live.  `...` is for `cyp2d6` and `adjustToFFM`, passed only when the
#' function declares them.
#' @noRd
helpDrugModelOutput <- function(drug, patient, ...) {
  fn <- helpDrugFunction(drug)
  if (is.null(fn)) return(NULL)
  args <- list(weight = patient$weight, height = patient$height, age = patient$age, sex = patient$sex)
  extra <- list(...)
  extra <- extra[names(extra) %in% names(formals(fn))]
  tryCatch(suppressWarnings(do.call(fn, c(args, extra))), error = function(e) NULL)
}

#' Does the drug's model declare this argument (cyp2d6, adjustToFFM)?
#' @noRd
helpDrugDeclares <- function(drug, arg) {
  fn <- helpDrugFunction(drug)
  !is.null(fn) && arg %in% names(formals(fn))
}

#' The drugs whose model names `drug` as its active metabolite
#' @noRd
helpParentDrugs <- function(drug, drugDefaults = getDrugDefaultsGlobal()) {
  iv <- drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]
  adult <- helpReferencePatients()[1, ]
  Filter(function(d) {
    X <- helpDrugModelOutput(d, adult)
    !is.null(X$metabolite) && identical(X$metabolite$name, drug)
  }, iv)
}

#' Whether the fat-free-mass switch changes this model's parameters
#'
#' Evaluated at the obese reference patient, where the fat-free-mass and
#' total-weight factors differ most.  NA when the model takes no such switch.
#' @noRd
helpDrugRespondsToFFM <- function(drug, drugDefaults = getDrugDefaultsGlobal()) {
  if (!helpDrugDeclares(drug, "adjustToFFM")) return(NA)
  obese <- helpReferencePatients()[4, ]
  on <- helpDrugPK(drug, obese, drugDefaults, adjustToFFM = TRUE)
  off <- helpDrugPK(drug, obese, drugDefaults, adjustToFFM = FALSE)
  if (is.null(on) || is.null(off)) return(NA)
  a <- on$PK[[PK_EVENT_DEFAULT]]
  b <- off$PK[[PK_EVENT_DEFAULT]]
  !isTRUE(all.equal(c(a$v1, a$v2, a$v3, a$cl1, a$cl2, a$cl3),
                    c(b$v1, b$v2, b$v3, b$cl1, b$cl2, b$cl3)))
}

helpHalfLife <- function(lambda) {
  if (is.null(lambda) || length(lambda) == 0 || is.na(lambda) || lambda <= 0) return(NA_real_)
  log(2) / lambda
}

#' A half-life for a table whose column is in minutes
#'
#' Minutes, as every other time on the page, but a half-life of a day or
#' more also gives its length in days, so that amiodarone's 55-day terminal
#' half-life does not read only as "79,700".  (Claude Code, 2026-10-07, at
#' the request of Steven L. Shafer.)
#' @noRd
helpFormatHalfLife <- function(minutes) {
  shown <- helpFormatNumber(minutes)
  long <- !is.na(minutes) & is.finite(minutes) & minutes >= MINS_PER_DAY
  shown[long] <- sprintf("%s (%s d)", shown[long], helpFormatNumber(minutes[long] / MINS_PER_DAY))
  shown
}

#' Does the drug's defaults row carry no typical-range band?
#'
#' A drug with no established range (desethylamiodarone), or none that
#' applies to its model (amiodaroneIV, whose chronic trough window does not
#' describe loading), carries zeros in the band columns, which the plot draws
#' as no band at all; the help says so rather than printing "0 to 0", and the
#' drug's narrative says why.  (Claude Code, 2026-10-07; worded for both
#' cases 2026-10-08.)
#' @noRd
helpNoBand <- function(row) {
  isTRUE(row$Lower == 0) && isTRUE(row$Upper == 0) && isTRUE(row$Typical == 0)
}

#' Volumes, clearances and derived constants of a drug at the reference patients
#'
#' @returns a data frame with one row per patient for which the model ran:
#'   covariates, V1-V3 (L), CL1-CL3 (L/min), k10-k31 (1/min), the three
#'   disposition half-lives (min), ke0 (1/min), its half-time, tPeak (min) and
#'   the citation in force for that patient
#' @noRd
helpDrugParameterTable <- function(drug, drugDefaults = getDrugDefaultsGlobal(),
                                   patients = helpReferencePatients(), ...) {
  rows <- lapply(seq_len(nrow(patients)), function(i) {
    p <- patients[i, ]
    pk <- helpDrugPK(drug, p, drugDefaults, ...)
    if (is.null(pk)) return(NULL)
    d <- pk$PK[[PK_EVENT_DEFAULT]]
    if (is.null(d)) d <- pk$PK[[1]]
    lambdas <- sort(c(d$lambda_1, d$lambda_2, d$lambda_3), decreasing = TRUE)
    # A one- or two-compartment model carries placeholder volumes of 1 L for
    # the compartments it lacks, with zero clearance into them.  Report those
    # as absent rather than as a litre.
    v2 <- if (isTRUE(d$cl2 > 0)) d$v2 else NA_real_
    v3 <- if (isTRUE(d$cl3 > 0)) d$v3 else NA_real_
    data.frame(
      Patient = p$label, Age = p$age, Weight = p$weight, Height = p$height, Sex = p$sex,
      V1 = d$v1, V2 = v2, V3 = v3,
      CL1 = d$cl1, CL2 = d$cl2, CL3 = d$cl3,
      k10 = d$k10, k12 = d$k12, k13 = d$k13, k21 = d$k21, k31 = d$k31,
      halfLife1 = helpHalfLife(lambdas[1]),
      halfLife2 = helpHalfLife(lambdas[2]),
      halfLife3 = helpHalfLife(lambdas[3]),
      ke0 = d$ke0,
      ke0HalfTime = helpHalfLife(d$ke0),
      tPeak = pk$tPeak,
      # Which curve tPeak was observed against ("IV" or "PO"), and whether the
      # model supplied ke0 directly instead of a tPeak (desmetramadol)
      tPeakRoute = if (is.null(pk$tPeakRoute)) ROUTE_IV else pk$tPeakRoute,
      ke0Supplied = isTRUE(pk$tPeak == 0) && isTRUE(d$ke0 > 0),
      metabolite = if (is.null(pk$metaboliteName)) "" else pk$metaboliteName,
      events = paste(pk$pkEvents, collapse = ", "),
      reference = pk$reference,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

helpConcentrationUnits <- function(units) {
  switch(as.character(units),
         "mcg" = "mcg/mL", "ng" = "ng/mL", "mg" = "mg/mL",
         "%" = "% of one atmosphere",
         # An osmotic agent is plotted as the serum osmolality it produces.
         "mOsm" = "mOsm/kg (serum osmolality)",
         as.character(units))
}

helpReferenceShort <- function(reference) {
  if (is.null(reference) || is.na(reference) || !nzchar(reference)) return("Not available")
  trimws(sub("\\s+https?://\\S+$", "", reference))
}

helpDrugUnits <- function(row) {
  u <- row$Units
  if (is.list(u)) u <- u[[1]]
  u <- u[!is.na(u) & nzchar(u)]
  u
}

#' Section heading as an HTML string
#' @noRd
helpH2 <- function(text) sprintf("<h2>%s</h2>", htmltools::htmlEscape(text))

#' The teaching scenarios that give a drug, as a Markdown-free HTML list
#' @noRd
helpScenariosUsingDrugHTML <- function(drug) {
  scen <- Filter(function(s) drug %in% s$doses$Drug, helpScenarios())
  if (length(scen) == 0) return("")
  items <- vapply(scen, function(s) {
    sprintf('<li><a href="#" data-help-page="scenarios/%s">%s</a> \u2014 %s</li>',
            s$id, htmltools::htmlEscape(s$title), htmltools::htmlEscape(s$summary))
  }, character(1))
  paste0(helpH2("Teaching scenarios that use this drug"),
         "<ul>", paste(items, collapse = ""), "</ul>")
}

#' The generated page for one drug or gas, as an HTML string
#' @noRd
helpDrugPageHTML <- function(drug, drugDefaults = getDrugDefaultsGlobal()) {
  row <- drugDefaults[drugDefaults$Drug == drug, ]
  if (nrow(row) == 0) {
    return(sprintf("<p>There is no drug called <code>%s</code> in the library.</p>",
                   htmltools::htmlEscape(drug)))
  }
  row <- row[1, ]
  if (isGasDrug(drug)) helpGasPageHTML(drug, row) else helpIvDrugPageHTML(drug, row, drugDefaults)
}

helpIvDrugPageHTML <- function(drug, row, drugDefaults) {
  params <- helpDrugParameterTable(drug, drugDefaults)
  units <- helpDrugUnits(row)
  concUnits <- helpConcentrationUnits(row$Concentration.Units)
  adult <- helpReferencePatients()[1, ]
  pkRef <- helpDrugPK(drug, adult, drugDefaults)
  modelOut <- helpDrugModelOutput(drug, adult)
  metabolite <- if (is.null(pkRef$metaboliteName)) NULL else pkRef$metaboliteName
  parents <- helpParentDrugs(drug, drugDefaults)
  # A drug with no effect site in the model (tPeak and ke0 both zero).  When it
  # also names a metabolite it is a prodrug, such as codeine, whose effect
  # appears on the metabolite's row; without one (an antibiotic, say) it is
  # simply plotted as plasma only.  A model can say it is NOT a prodrug
  # (prodrug = FALSE): amiodarone is active itself, and merely has no
  # effect-site model, so its effect is not "the metabolite's".
  noEffectSite <- !is.null(pkRef) && isTRUE(pkRef$tPeak == 0) &&
    isTRUE(pkRef$PK[[PK_EVENT_DEFAULT]]$ke0 == 0)
  prodrug <- noEffectSite && !is.null(metabolite) && !isFALSE(modelOut$prodrug)
  # An antibiotic's threshold is free drug at the MIC; see R/antibioticThresholds.R
  mic <- antibioticMic(drug)

  # --- At a glance -----------------------------------------------------------
  esc <- htmltools::htmlEscape
  route <- doseRoute(units)
  routes <- c(if (ROUTE_PO %in% route) "oral", if (ROUTE_SL %in% route) "sublingual",
              if (ROUTE_IM %in% route) "intramuscular", if (ROUTE_IN %in% route) "intranasal",
              if (ROUTE_RA %in% route) "by tissue injection (regional anesthesia)")
  intravenous <- any(units %in% c(bolusUnits, infusionUnits))
  tci <- any(units %in% tciUnits)
  given <- if (length(units) == 0) {
    paste0("Not dosed directly: appears only as the active metabolite of ",
           paste(helpDrugTitle(parents), collapse = " and "))
  } else if (intravenous && length(routes)) {
    paste0("Intravenous, also ", paste(routes, collapse = " and "))
  } else if (intravenous) {
    "Intravenous"
  } else {
    paste0(tools::toTitleCase(paste(routes, collapse = " and ")), " only")
  }
  unitsShown <- if (length(units)) paste(units, collapse = ", ") else
    "None: this drug cannot be entered in the dose table"
  # A drug with no dosing unit reads the blank CSV cell as NA, which
  # nzchar() alone would print as "NA"
  defaultShown <- if (!is.na(row$Default.Units) && nzchar(as.character(row$Default.Units)))
    as.character(row$Default.Units) else "—"
  meacShown <- if (!is.na(row$MEAC) && row$MEAC > 0) {
    sprintf("%s %s", helpFormatNumber(row$MEAC), concUnits)
  } else if (prodrug) {
    "None: the effect is the metabolite's, which carries its own MEAC"
  } else if (identical(as.character(row$Category), "Opioids")) {
    # An opioid with no established MEAC (buprenorphine, a partial agonist)
    "None established: not on the MEAC panel (see the model notes)"
  } else {
    "Not an opioid: not on the MEAC panel"
  }
  glance <- data.frame(
    Property = esc(c("Given as", "Concentration units", "Units offered in the dose table",
                     "Default unit", "Target-controlled infusion", "Active metabolite", "Formed from",
                     "Typical concentration (shaded band)",
                     "Typical value", "MEAC", "Recovery threshold (endCe)", "Plot colour")),
    Value = c(
      esc(given),
      esc(concUnits),
      esc(unitsShown),
      esc(defaultShown),
      esc(if (tci) "Yes: Plasma target and Effect site target units" else "No"),
      if (!is.null(metabolite)) helpPageLink(paste0("drugs/", metabolite)) else esc("None modelled"),
      if (length(parents)) paste(vapply(parents, function(p) helpPageLink(paste0("drugs/", p)), character(1)),
                                 collapse = ", ") else esc("Not a modelled metabolite of any drug in the library"),
      esc(if (helpNoBand(row)) "None: no range applies to this model, so no band is drawn"
          else sprintf("%s to %s %s", helpFormatNumber(row$Lower), helpFormatNumber(row$Upper), concUnits)),
      esc(if (helpNoBand(row)) "None" else sprintf("%s %s", helpFormatNumber(row$Typical), concUnits)),
      esc(meacShown),
      esc(if (!is.null(mic)) {
            sprintf("%s %s %s, the level at which free drug equals the MIC (%s mg/L); timed on the plasma",
                    helpFormatNumber(row$endCe), concUnits, mic$Plotted, helpFormatNumber(mic$MIC))
          } else if (noEffectSite && !is.na(row$endCe) && row$endCe > 0) {
            sprintf("%s %s, timed on the plasma (no effect site in the model)",
                    helpFormatNumber(row$endCe), concUnits)
          } else if (noEffectSite) {
            "None by default; a threshold set under Drug Thresholds is timed on the plasma"
          } else if (is.na(row$endCe) || row$endCe <= 0) {
            "None by default; a threshold set under Drug Thresholds is timed on the effect site"
          } else
            sprintf("%s %s", helpFormatNumber(row$endCe), concUnits)),
      paste0(as.character(helpColorSwatch(row$Color)), " ", esc(as.character(row$Color)))
    ),
    stringsAsFactors = FALSE
  )
  glanceHTML <- helpRawTableHTML(glance)

  # --- Model source ----------------------------------------------------------
  sourceHTML <- "<p>The model function returned no citation.</p>"
  if (!is.null(params)) {
    refs <- unique(params$reference)
    items <- vapply(refs, function(r) {
      who <- params$Patient[params$reference == r]
      note <- if (length(refs) > 1) sprintf(" <span class='text-muted small'>(applies to: %s)</span>",
                                             paste(who, collapse = ", ")) else ""
      paste0("<li>", as.character(citationItemHTML(drug, r, row$Color)), note, "</li>")
    }, character(1))
    sourceHTML <- paste0(
      "<p>The pharmacokinetic parameters are those of:</p><ul class='help-citations'>",
      paste(items, collapse = ""), "</ul>",
      "<p class='small text-muted'>This is the citation the model function returns, and what the ",
      "References panel under the plot shows when the drug is simulated. ",
      "The notes further down this page say more about where the parameters come from.</p>"
    )
  }

  # --- Parameters at the reference patients ----------------------------------
  paramsHTML <- "<p>The model could not be evaluated at any reference patient.</p>"
  if (!is.null(params)) {
    shown <- data.frame(
      Patient = sprintf("%s (%s y, %s kg, %s cm, %s)", params$Patient,
                        helpFormatNumber(params$Age), params$Weight, params$Height, params$Sex),
      `V1 (L)` = helpFormatNumber(params$V1),
      `V2 (L)` = helpFormatNumber(params$V2),
      `V3 (L)` = helpFormatNumber(params$V3),
      `CL1 (L/min)` = helpFormatNumber(params$CL1),
      `CL2 (L/min)` = helpFormatNumber(params$CL2),
      `CL3 (L/min)` = helpFormatNumber(params$CL3),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    tPeakShown <- vapply(seq_len(nrow(params)), function(i) {
      if (isTRUE(params$ke0Supplied[i])) return("ke0 supplied by the model")
      if (isTRUE(params$tPeak[i] == 0)) return("none: no effect site")
      paste0(helpFormatNumber(params$tPeak[i]),
             if (identical(params$tPeakRoute[i], ROUTE_PO)) " after an oral dose" else "")
    }, character(1))
    derived <- data.frame(
      params$Patient,
      helpFormatHalfLife(params$halfLife1),
      helpFormatHalfLife(params$halfLife2),
      helpFormatHalfLife(params$halfLife3),
      ifelse(params$ke0 > 0, helpFormatNumber(params$ke0), "none"),
      helpFormatHalfLife(params$ke0HalfTime),
      tPeakShown,
      stringsAsFactors = FALSE
    )
    names(derived) <- c("Patient", "t\u00bd \u03b1 (min)", "t\u00bd \u03b2 (min)",
                        "t\u00bd \u03b3 (min)", "ke0 (1/min)", "t\u00bd ke0 (min)", "tPeak (min)")
    varies <- function(x) length(unique(round(x, 6))) > 1
    # The fat-free-mass switch (docs/weight-adjustment.md): most models are
    # scaled to it by default, a few carry their own size covariate, and one
    # (hydrocodone) is deliberately unscaled.
    ffm <- helpDrugRespondsToFFM(drug, drugDefaults)
    # Whether the model has covariates of its OWN is judged with the switch
    # off, so that a drug that varies between patients only because of the
    # default fat-free-mass scaling is not described as having covariates.
    paramsOff <- if (isTRUE(ffm)) helpDrugParameterTable(drug, drugDefaults, adjustToFFM = FALSE) else params
    ownCovariates <- varies(paramsOff$V1) || varies(paramsOff$CL1)
    covariateNote <- if (ownCovariates) {
      "The parameters change between these patients, so this model has covariates: see the notes below for which ones and how."
    } else if (isTRUE(ffm)) {
      "The published model has no covariates of its own. The values here differ between patients only because of the fat-free-mass scaling, on by default; untick <em>Adjust weight to fat-free mass</em> and they are identical at every size. Dose in per-kilogram units if you want the dose scaled too."
    } else {
      "The parameters are the same for every patient: this model has no covariates, and a dose is the same whether the patient weighs 20 kg or 120 kg. Dose in per-kilogram units if you want size taken into account."
    }
    ffmNote <- if (isTRUE(ffm)) {
      paste0("These values are with <em>Adjust weight to fat-free mass</em> ticked, as it is by default: ",
             "the model's volumes are scaled by the patient's fat-free mass relative to a 70 kg, 170 cm man, ",
             "and its clearances by that ratio to the 0.75 power. The 70 kg reference man is unchanged. ",
             "Unticking the box restores the scaling the published model used. See ",
             helpPageLink("models/fat-free-mass"), ".")
    } else if (isFALSE(ffm)) {
      paste0("The <em>Adjust weight to fat-free mass</em> switch does not change this model: it either carries ",
             "its own body-size covariate or, as its notes explain, is deliberately not scaled. See ",
             helpPageLink("models/fat-free-mass"), ".")
    } else ""
    effectNote <- if (prodrug) {
      paste0("This drug has <strong>no effect site of its own</strong>: ke0 is zero, only the plasma ",
             "concentration is plotted, and the effect appears on the ",
             helpPageLink(paste0("drugs/", metabolite)), " row, which receives the metabolite formed from it. See ",
             helpPageLink("models/metabolites"), ".")
    } else if (noEffectSite) {
      "This model has <strong>no effect site</strong>: ke0 is zero and only the plasma concentration is plotted."
    } else if (any(params$ke0Supplied) && length(units) == 0) {
      paste0("ke0 is supplied by the model rather than solved from a tPeak, because this drug is never dosed ",
             "directly and its time to peak effect is observed after a dose of its parent; see ",
             helpPageLink("models/effect-site", "The effect site and ke0"), ".")
    } else if (any(params$ke0Supplied)) {
      # Dosed directly, with a published equilibration rate (acetaminophen,
      # ibuprofen, alprazolam): the reason is the source, not a parent
      paste0("ke0 is supplied by the model rather than solved from a tPeak, because its source reports the ",
             "equilibration rate itself rather than a time to peak effect; the notes below say where it ",
             "comes from. See ", helpPageLink("models/effect-site", "The effect site and ke0"), ".")
    } else if (any(params$tPeakRoute == ROUTE_PO)) {
      paste0("ke0 is solved so that the effect-site concentration after an <strong>oral</strong> dose peaks at tPeak, ",
             "because that is how this drug's time to peak effect was observed; see ",
             helpPageLink("models/effect-site", "The effect site and ke0"), ".")
    } else {
      paste0("ke0 is solved so that the effect-site concentration after a bolus peaks at tPeak; see ",
             helpPageLink("models/effect-site", "The effect site and ke0"), ".")
    }
    paramsHTML <- paste0(
      "<p>", covariateNote, "</p>",
      if (nzchar(ffmNote)) paste0("<p>", ffmNote, "</p>") else "",
      helpTableHTML(shown, "Volumes and clearances"),
      helpTableHTML(derived, "Half-lives, effect-site equilibration and time to peak effect"),
      "<p class='small text-muted'>The disposition half-lives are ln(2) divided by the ",
      "eigenvalues of the compartment model (\u03b1 fastest, \u03b3 slowest); a dash means the ",
      "compartment is absent, and a half-life of a day or more also gives its length in days (d). ",
      effectNote, "</p>"
    )
  }

  # --- Active metabolite ---------------------------------------------------
  metaboliteHTML <- ""
  if (!is.null(metabolite) && !is.null(modelOut$metabolite)) {
    m <- modelOut$metabolite
    firstPass <- if (is.null(m$firstPassFraction)) 0 else m$firstPassFraction
    mwRatio <- if (is.null(m$mwRatio)) 1 else m$mwRatio
    formation <- data.frame(
      Parameter = c("Metabolite", "Formation rate constant, kFormation (1/min)",
                    "Formation half-time (min)",
                    "Fraction of an oral dose converted during first pass",
                    "Molecular weight ratio (metabolite / parent)"),
      Value = c(helpPageLink(paste0("drugs/", metabolite)),
                helpFormatNumber(m$kFormation),
                helpFormatHalfLife(log(2) / m$kFormation),
                helpFormatNumber(firstPass),
                helpFormatNumber(mwRatio)),
      stringsAsFactors = FALSE
    )
    phenotypeHTML <- ""
    if (helpDrugDeclares(drug, "cyp2d6")) {
      byPhenotype <- lapply(CYP2D6_VALUES, function(ph) {
        X <- helpDrugModelOutput(drug, adult, cyp2d6 = ph)
        if (is.null(X)) return(NULL)
        fp <- if (is.null(X$metabolite$firstPassFraction)) 0 else X$metabolite$firstPassFraction
        data.frame(phenotype = ph, kFormation = X$metabolite$kFormation, firstPass = fp,
                   cl1 = X$PK[[PK_EVENT_DEFAULT]]$cl1, stringsAsFactors = FALSE)
      })
      byPhenotype <- do.call(rbind, byPhenotype[!vapply(byPhenotype, is.null, logical(1))])
      normal <- byPhenotype[byPhenotype$phenotype == CYP2D6_NORMAL, ]
      if (nrow(byPhenotype) > 0 && nrow(normal) == 1) {
        tab <- data.frame(
          `CYP2D6 phenotype` = tools::toTitleCase(byPhenotype$phenotype),
          `Formation rate, relative to normal` = helpFormatNumber(byPhenotype$kFormation / normal$kFormation),
          `First-pass fraction` = helpFormatNumber(byPhenotype$firstPass),
          `Parent clearance CL1 (L/min)` = helpFormatNumber(byPhenotype$cl1),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        phenotypeHTML <- paste0(
          "<p>Formation is by CYP2D6, so the <strong>CYP 2D6</strong> field in the Patient Profile changes it. ",
          "At the reference adult:</p>",
          helpTableHTML(tab, "Effect of CYP2D6 phenotype"),
          "<p class='small text-muted'>Where formation is a branch of the parent's clearance, the parent's ",
          "own clearance moves with phenotype too, as the last column shows.</p>"
        )
      }
    }
    metaboliteHTML <- paste0(
      helpH2("Active metabolite"),
      "<p>A dose of this drug also produces a curve for <strong>", esc(helpDrugTitle(metabolite)),
      "</strong>, formed from it and added to that drug's own row (a row is created if ",
      esc(helpDrugTitle(metabolite)), " was not given). Formation is a first-order transfer out of ",
      "this drug's central compartment at the rate below; it does not change this drug's own curve, ",
      "whose fitted clearance already includes it. See ", helpPageLink("models/metabolites"), ".</p>",
      helpRawTableHTML(formation, "Formation at the reference adult"),
      phenotypeHTML
    )
  }

  # --- Formed from ------------------------------------------------------------
  formedHTML <- ""
  if (length(parents)) {
    items <- vapply(parents, function(p) {
      paste0("<li>", helpPageLink(paste0("drugs/", p)), "</li>")
    }, character(1))
    formedHTML <- paste0(
      helpH2("Formed as a metabolite"),
      "<p>This drug is the modelled active metabolite of ", if (length(parents) > 1) "these drugs" else "this drug",
      ". Giving ", if (length(parents) > 1) "any of them" else "it",
      " adds the metabolite formed to this drug's row, whether or not this drug was given itself, and the ",
      "time until threshold is solved from the combined curve. The formation parameters are on the parent's page.</p>",
      "<ul>", paste(items, collapse = ""), "</ul>"
    )
  }

  # --- CYP2C19 phenotype -----------------------------------------------------
  # A model that declares cyp2c19 (escitalopram, citalopram) has its clearance
  # move with the Patient Profile's CYP 2C19 field: tabulate it at the
  # reference adult.
  cyp2c19HTML <- ""
  if (helpDrugDeclares(drug, "cyp2c19")) {
    byPhenotype <- lapply(CYP2C19_VALUES, function(ph) {
      X <- helpDrugModelOutput(drug, adult, cyp2c19 = ph)
      if (is.null(X)) return(NULL)
      data.frame(phenotype = ph, cl1 = X$PK[[PK_EVENT_DEFAULT]]$cl1, stringsAsFactors = FALSE)
    })
    byPhenotype <- do.call(rbind, byPhenotype[!vapply(byPhenotype, is.null, logical(1))])
    normal <- byPhenotype[byPhenotype$phenotype == CYP2C19_NORMAL, ]
    if (!is.null(byPhenotype) && nrow(normal) == 1) {
      tab <- data.frame(
        `CYP2C19 phenotype` = tools::toTitleCase(rev(byPhenotype$phenotype)),
        `Clearance CL1 (L/min)` = helpFormatNumber(rev(byPhenotype$cl1)),
        `Relative to normal` = helpFormatNumber(rev(byPhenotype$cl1) / normal$cl1),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      cyp2c19HTML <- paste0(
        helpH2("CYP2C19 phenotype"),
        "<p>This model's clearance depends on CYP2C19, so the <strong>CYP 2C19</strong> field in the ",
        "Patient Profile changes it. At the reference adult:</p>",
        helpTableHTML(tab, "Effect of CYP2C19 phenotype"),
        "<p class='small text-muted'>Where the source did not estimate a phenotype separately, the ",
        "drug's own description below says which group it is given.</p>"
      )
    }
  }

  # --- Absorption routes -----------------------------------------------------
  absorptionHTML <- ""
  if (!is.null(pkRef)) {
    d <- pkRef$PK[[PK_EVENT_DEFAULT]]
    slowRA <- isTRUE(d$ka_RAslow > 0)
    routes <- list(PO = "Oral (PO)", SL = "Sublingual (SL)", IM = "Intramuscular (IM)",
                   IN = "Intranasal (IN)",
                   RA = if (slowRA) "Regional anesthesia (RA), fast depot" else "Regional anesthesia (RA)",
                   RAslow = "Regional anesthesia (RA), slow depot")
    rows <- lapply(names(routes), function(r) {
      ka <- d[[paste0("ka_", r)]]
      if (is.null(ka) || is.na(ka) || ka <= 0) return(NULL)
      data.frame(
        Route = routes[[r]],
        `ka (1/min)` = helpFormatNumber(ka),
        `Absorption half-time (min)` = helpFormatNumber(log(2) / ka),
        `Bioavailability` = helpFormatNumber(d[[paste0("bioavailability_", r)]]),
        `Lag time (min)` = helpFormatNumber(d[[paste0("tlag_", r)]]),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) > 0) {
      absorptionHTML <- paste0(
        helpH2("Extravascular routes"),
        "<p>Doses with PO, SL, IM, IN or RA units are absorbed by first-order kinetics into the central ",
        "compartment after a lag, with the fraction shown reaching the circulation. RA is a ",
        "local anesthetic injected into tissue (a nerve block or an infiltration). ",
        if (slowRA) paste0("Each RA dose is absorbed through a fast and a slow depot in parallel; ",
                           "the bioavailability of each is its share of the dose. ") else "",
        "See ",
        helpPageLink("models/absorption"), ".</p>",
        helpTableHTML(do.call(rbind, rows))
      )
      # Dose-dependent absorption: each oral or sublingual dose is scaled by
      # its own fraction (oralSaturationFraction()), in one of three forms.
      saturable <- list(
        list(sat = pkRef$oralSaturation, F = d$bioavailability_PO, route = "Oral",
             doses = c(300, 600, 900, 1200)),
        list(sat = pkRef$sublingualSaturation, F = d$bioavailability_SL, route = "Sublingual",
             doses = c(0.4, 2, 8, 16, 24, 32))
      )
      for (s in saturable) {
        if (is.null(s$sat)) next
        sat <- s$sat
        doses <- if (is.null(sat$exampleDoses)) s$doses else sat$exampleDoses
        f <- s$F * oralSaturationFraction(doses, sat)
        absorbed <- data.frame(helpFormatNumber(doses), helpFormatNumber(f),
                               helpFormatNumber(doses * f), stringsAsFactors = FALSE)
        names(absorbed) <- c(sprintf("%s dose (mg)", s$route), "Fraction absorbed",
                             "Amount absorbed (mg)")
        explanation <- switch(oralSaturationForm(sat),
          saturable = paste0(
            "<p>", s$route, " absorption <strong>saturates</strong>: the fraction absorbed falls as the dose ",
            "rises, as 1 &minus; ", helpFormatNumber(sat$Imax), " &times; D / (",
            helpFormatNumber(sat$ID50), " + D) with D the dose in mg, so the bioavailability above is ",
            "the limit for a very small dose. "),
          rising = paste0(
            "<p>", s$route, " bioavailability <strong>rises with the dose</strong>, as D / (",
            helpFormatNumber(sat$D50), " + D) with D the dose in mg, times the bioavailability above, ",
            "which is its maximum. "),
          power = paste0(
            "<p>Exposure is <strong>more than proportional to the dose</strong>: each ", tolower(s$route),
            " dose is scaled by (D / ", helpFormatNumber(sat$Dref), ")<sup>", helpFormatNumber(sat$exponent),
            "</sup> with D the dose in mg. This carries the source's empirical power of the daily dose ",
            "on apparent clearance, and reproduces its steady-state exposure for once-daily dosing; the ",
            "\"fraction\" can exceed 1 above the reference dose, because it is an exposure scale on ",
            "apparent parameters, not a physical bioavailability. ")
        )
        absorptionHTML <- paste0(
          absorptionHTML,
          explanation,
          "Each ", tolower(s$route), " dose is scaled by its own fraction. Doses entered as separate ",
          "rows at the same time are scaled separately, not by their sum.</p>",
          helpTableHTML(absorbed)
        )
      }
    }
  }

  # --- Time until threshold: free drug at the MIC -----------------------------
  micHTML <- ""
  if (!is.null(mic)) {
    unbound <- mic$Plotted == "unbound"
    tab <- data.frame(
      Quantity = c("Target organism", "MIC (free drug)", "What the curve shows",
                   "Unbound (free) fraction at that level", "Threshold on the curve"),
      Value = c(mic$Organism,
                sprintf("%s mg/L", helpFormatNumber(mic$MIC)),
                if (unbound) "Unbound (free) drug" else "Total (bound plus free) drug",
                if (unbound) "Not needed: the curve is already free drug"
                else helpFormatNumber(mic$FreeFraction),
                sprintf("%s %s %s", helpFormatNumber(row$endCe), concUnits, mic$Plotted)),
      stringsAsFactors = FALSE
    )
    micHTML <- paste0(
      helpH2("Time until threshold: free drug at the MIC"),
      "<p>The antibiotics have no effect site, so <em>Time until threshold</em> times the ",
      "<strong>plasma</strong> curve. It shows how long, if no more is given, until <strong>free</strong> ",
      esc(tolower(helpDrugTitle(drug))), " falls below the MIC: the time left above the MIC, which ",
      "is when to redose. It is free drug that acts on the organism, and susceptibility ",
      "breakpoints are set against it.</p>",
      if (unbound) {
        "<p>This model plots <strong>unbound</strong> drug, so the threshold is the MIC itself.</p>"
      } else {
        paste0("<p>This model plots <strong>total</strong> drug, bound plus free, which is what a ",
               "laboratory reports. Binding is not simulated. Instead the threshold is set to the total ",
               "concentration at which the free concentration equals the MIC, using the unbound fraction ",
               "measured at that low level", if (isTRUE(mic$Saturable)) {
                 " (binding is saturable, so the free fraction rises as the concentration rises; the value at the threshold is the one that matters)"
               } else "", ".</p>")
      },
      helpTableHTML(tab),
      "<p class='small text-muted'>Sources: ", esc(mic$MicSource), " ", esc(mic$BindingSource),
      " The threshold can be changed under Settings → Drug Thresholds; see ",
      helpPageLink("models/recovery"), ".</p>"
    )
  }

  # --- Events ----------------------------------------------------------------
  eventsHTML <- ""
  if (!is.null(params) && any(nzchar(params$events) & params$events != PK_EVENT_DEFAULT)) {
    withEvents <- params[params$events != PK_EVENT_DEFAULT, ]
    ev <- unique(unlist(strsplit(withEvents$events, ", ")))
    ev <- setdiff(ev, PK_EVENT_DEFAULT)
    eventsHTML <- paste0(
      helpH2("Events that change the kinetics"),
      sprintf("<p>For %s, the parameters change when these clinical events are entered (Additional Plots \u2192 Events): <strong>%s</strong>. See %s.</p>",
              paste(unique(withEvents$Patient), collapse = " and "),
              htmltools::htmlEscape(paste(ev, collapse = ", ")),
              helpPageLink("models/pk-events"))
    )
  }

  narrative <- helpMarkdownToHTML(helpReadMarkdown(paste0("drugs/", drug)))

  paste0(
    helpH2("At a glance"), glanceHTML,
    helpH2("Model source"), sourceHTML,
    helpH2("Parameters at reference patients"), paramsHTML,
    cyp2c19HTML,
    absorptionHTML,
    metaboliteHTML,
    formedHTML,
    micHTML,
    eventsHTML,
    if (nzchar(narrative)) paste0(helpH2("About this model"), narrative) else "",
    helpScenariosUsingDrugHTML(drug),
    helpH2("See also"),
    "<ul>",
    "<li>", helpPageLink("drugs/index", "All drugs"), "</li>",
    "<li>", helpPageLink("models/three-compartment"), "</li>",
    "<li>", helpPageLink("models/covariates"), "</li>",
    "<li>", helpPageLink("drug-library"), "</li>",
    "</ul>"
  )
}

#' Role of a gas entry in the dose table
#' @noRd
helpGasRole <- function(gas) {
  if (gas %in% potentAgents()) return("Anesthetic agent")
  switch(gas,
         air = "Carrier gas (fresh gas flow)",
         oxygen = "Carrier gas (fresh gas flow)",
         ventilation = "Ventilator setting (minute ventilation)",
         "Inhaled-gas entry")
}

helpGasPageHTML <- function(gas, row) {
  props <- getGasProperties()
  p <- props[props$gas == gas, ]
  units <- helpDrugUnits(row)

  esc <- htmltools::htmlEscape
  glance <- data.frame(
    Property = esc(c("Role", "Units in the dose table", "What the number means", "Plotted as",
                     "Recovery threshold (endCe)", "Plot colour")),
    Value = c(
      esc(helpGasRole(gas)),
      esc(paste(units, collapse = ", ")),
      esc(if (identical(units, "%")) "Vaporizer setting, % of one atmosphere in the fresh gas"
          else if (gas == "ventilation") "Minute ventilation, L/min (30% is dead space)"
          else "Flowmeter setting, L/min of fresh gas"),
      esc(if (gas %in% c("air", "ventilation")) "Not plotted: an input, not a result"
          else "Alveolar (end-tidal) tension as the \u201cplasma\u201d line; vessel-rich group (brain) tension as the \u201ceffect site\u201d line"),
      esc(if (!is.na(row$endCe) && row$endCe > 0) {
        if (gas %in% ageAdjustedThresholdGases())
          sprintf("%s%% at age 40 (0.1 MAC); follows MAC with age", helpFormatNumber(row$endCe))
        else sprintf("%s%%", helpFormatNumber(row$endCe))
      } else "Not timed"),
      paste0(as.character(helpColorSwatch(row$Color)), " ", esc(as.character(row$Color)))
    ),
    stringsAsFactors = FALSE
  )
  glanceHTML <- helpRawTableHTML(glance)

  propsHTML <- ""
  if (nrow(p) == 1) {
    tab <- data.frame(
      Property = c("Blood:gas partition coefficient", "Brain (vessel-rich group):gas",
                   "Muscle:gas", "Fat:gas", "MAC at age 40 (% of 1 atm)",
                   "Contributes to MAC equivalents"),
      Value = c(helpFormatNumber(p$lambda_blood), helpFormatNumber(p$tg_brain),
                helpFormatNumber(p$tg_muscle), helpFormatNumber(p$tg_fat),
                helpFormatNumber(p$MAC40), if (isTRUE(p$potent)) "Yes" else "No"),
      stringsAsFactors = FALSE
    )
    propsHTML <- paste0(
      helpH2("Physical properties"),
      "<p>These are Gas Man\u00ae's values, carried unchanged so that the engine can be checked against Gas Man; see ",
      helpPageLink("models/gas-engine"), ".</p>",
      helpTableHTML(tab)
    )
    if (isTRUE(p$flagged)) {
      propsHTML <- paste0(propsHTML,
        "<div class='help-callout help-callout-warn'><strong>Flagged value.</strong> ",
        htmltools::htmlEscape(p$flagNote), "</div>")
    }
    if (isTRUE(p$potent)) {
      ages <- c(1, 10, 20, 40, 60, 80)
      mac <- data.frame(
        `Age (years)` = ages,
        `MAC (%)` = helpFormatNumber(macForAge(p$MAC40, ages)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      propsHTML <- paste0(propsHTML,
        helpH2("MAC and age"),
        "<p>MAC falls about 6% per decade (Mapleson): MAC(age) = MAC\u2084\u2080 \u00d7 10<sup>\u22120.00269 (age \u2212 40)</sup>. ",
        "The MAC-equivalents panel divides the alveolar concentration by the MAC at the patient's age.</p>",
        helpTableHTML(mac))
    }
  }

  narrative <- helpMarkdownToHTML(helpReadMarkdown(paste0("drugs/", gas)))

  paste0(
    helpH2("At a glance"), glanceHTML,
    propsHTML,
    if (nzchar(narrative)) paste0(helpH2("Notes"), narrative) else "",
    helpScenariosUsingDrugHTML(gas),
    helpH2("See also"),
    "<ul>",
    "<li>", helpPageLink("inhaled-agents"), "</li>",
    "<li>", helpPageLink("models/gas-engine"), "</li>",
    "<li>", helpPageLink("models/gas-differences"), "</li>",
    "<li>", helpPageLink("models/recovery"), "</li>",
    "</ul>"
  )
}

#' The drug index page: every drug, its model source and its units
#' @noRd
helpDrugIndexHTML <- function(drugDefaults = getDrugDefaultsGlobal()) {
  adult <- helpReferencePatients()[1, ]
  iv <- drugDefaults[!isGasDrug(drugDefaults$Drug), ]
  gas <- drugDefaults[isGasDrug(drugDefaults$Drug), ]

  ivRows <- lapply(seq_len(nrow(iv)), function(i) {
    row <- iv[i, ]
    pk <- helpDrugPK(row$Drug, adult, drugDefaults)
    conc <- helpConcentrationUnits(row$Concentration.Units)
    units <- helpDrugUnits(row)
    given <- if (length(units) == 0) "metabolite only"
      else if (any(units %in% c(bolusUnits, infusionUnits))) {
        route <- doseRoute(units)
        paste(c("IV", if (ROUTE_PO %in% route) "oral", if (ROUTE_SL %in% route) "SL",
                if (ROUTE_IM %in% route) "IM",
                if (ROUTE_IN %in% route) "IN", if (ROUTE_RA %in% route) "RA",
                if (any(units %in% tciUnits)) "TCI"), collapse = ", ")
      } else {
        # No intravenous unit: name the routes it does have
        route <- unique(doseRoute(units))
        paste(c(if (ROUTE_PO %in% route) "oral", if (ROUTE_SL %in% route) "SL",
                if (ROUTE_IM %in% route) "IM", if (ROUTE_IN %in% route) "IN",
                if (ROUTE_RA %in% route) "RA"), collapse = ", ")
      }
    metabolite <- if (is.null(pk$metaboliteName)) "" else
      sprintf('<a href="#" data-help-page="drugs/%s">%s</a>', pk$metaboliteName, helpDrugTitle(pk$metaboliteName))
    data.frame(
      Drug = sprintf('<a href="#" data-help-page="drugs/%s">%s%s</a>', row$Drug,
                     as.character(helpColorSwatch(row$Color)), helpDrugTitle(row$Drug)),
      `Model source` = htmltools::htmlEscape(if (is.null(pk)) "Not available" else helpReferenceShort(pk$reference)),
      `Given as` = given,
      `Active metabolite` = metabolite,
      `Default unit` = htmltools::htmlEscape(if (!is.na(row$Default.Units) && nzchar(as.character(row$Default.Units)))
                                               as.character(row$Default.Units) else "—"),
      `Typical range` = if (helpNoBand(row)) "none" else
        sprintf("%s\u2013%s %s", helpFormatNumber(row$Lower), helpFormatNumber(row$Upper), conc),
      MEAC = if (!is.na(row$MEAC) && row$MEAC > 0) sprintf("%s %s", helpFormatNumber(row$MEAC), conc) else "",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  ivTable <- helpRawTableHTML(do.call(rbind, ivRows), "Intravenous drugs")

  gasRows <- lapply(seq_len(nrow(gas)), function(i) {
    row <- gas[i, ]
    data.frame(
      Entry = sprintf('<a href="#" data-help-page="drugs/%s">%s%s</a>', row$Drug,
                      as.character(helpColorSwatch(row$Color)), helpDrugTitle(row$Drug)),
      Units = htmltools::htmlEscape(paste(helpDrugUnits(row), collapse = ", ")),
      Role = htmltools::htmlEscape(helpGasRole(row$Drug)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  gasTable <- if (length(gasRows)) helpRawTableHTML(do.call(rbind, gasRows), "Inhaled agents and gas settings") else ""

  paste0(
    "<p>Every drug in stanpumpR is a published pharmacokinetic model. Which model was chosen ",
    "matters as much as the dose you type, so each drug has a page recording what the code ",
    "actually computes: its parameters at a set of reference patients, the citation, the units ",
    "it accepts and the typical concentrations that draw the shaded band. The model source shown ",
    "here is the one in force for a 40-year-old, 70 kg, 170 cm man; a few drugs switch models ",
    "with age or body size, and their pages say where. A drug with an active metabolite adds a ",
    "curve for it when given (", helpPageLink("models/metabolites"), "); TCI marks the drugs that ",
    "accept target-controlled infusion units (", helpPageLink("tci"), ").</p>",
    "<p>The library can be edited for the current session under ",
    helpPageLink("drug-library", "Settings \u2192 Drug Library"), ".</p>",
    helpH2("Intravenous drugs"), ivTable,
    helpH2("Inhaled anesthetics"), gasTable,
    "<p>How to add a drug is described under ", helpPageLink("contributing"), ".</p>"
  )
}

#' A table whose cells are already HTML (links, swatches)
#' @noRd
helpRawTableHTML <- function(df, caption = NULL) {
  if (is.null(df) || nrow(df) == 0) return("")
  header <- paste0("<tr>", paste0("<th>", htmltools::htmlEscape(names(df)), "</th>", collapse = ""), "</tr>")
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0("<tr>", paste0("<td>", unlist(df[i, ]), "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0(
    "<table class='table table-sm table-striped help-table'>",
    if (!is.null(caption)) paste0("<caption>", htmltools::htmlEscape(caption), "</caption>"),
    "<thead>", header, "</thead><tbody>", paste(body, collapse = ""), "</tbody></table>"
  )
}

#' The bibliography page: every citation the drug models return, then the
#' hand-written list of methods references in inst/help/references.md
#' @noRd
helpReferencesHTML <- function(drugDefaults = getDrugDefaultsGlobal()) {
  patients <- helpReferencePatients()
  iv <- drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]
  cites <- list()
  for (drug in iv) {
    for (i in seq_len(nrow(patients))) {
      pk <- helpDrugPK(drug, patients[i, ], drugDefaults)
      if (is.null(pk)) next
      ref <- pk$reference
      cites[[ref]] <- unique(c(cites[[ref]], drug))
    }
  }
  refs <- names(cites)
  refs <- refs[order(tolower(refs))]
  items <- vapply(refs, function(r) {
    drugs <- vapply(cites[[r]], function(d) {
      sprintf('<a href="#" data-help-page="drugs/%s">%s</a>', d, helpDrugTitle(d))
    }, character(1))
    paste0("<li>", as.character(citationItemHTML(paste(helpDrugTitle(cites[[r]]), collapse = ", "), r, NULL)),
           " <span class='small'>[", paste(drugs, collapse = ", "), "]</span></li>")
  }, character(1))

  paste0(
    helpH2("Pharmacokinetic models in the drug library"),
    "<p>These are the citations the drug model functions return, collected across the reference ",
    "patients, so a drug that switches models with age or size appears under each.</p>",
    "<ul class='help-citations'>", paste(items, collapse = ""), "</ul>",
    helpMarkdownToHTML(helpReadMarkdown("references"))
  )
}
