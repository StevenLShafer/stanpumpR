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
helpDrugPK <- function(drug, patient, drugDefaults = getDrugDefaultsGlobal()) {
  row <- drugDefaults[drugDefaults$Drug == drug, ]
  if (nrow(row) == 0) return(NULL)
  tryCatch(
    suppressWarnings(getDrugPK(
      drug = drug,
      weight = patient$weight,
      height = patient$height,
      age = patient$age,
      sex = patient$sex,
      drugDefaults = row
    )),
    error = function(e) NULL
  )
}

helpHalfLife <- function(lambda) {
  if (is.null(lambda) || length(lambda) == 0 || is.na(lambda) || lambda <= 0) return(NA_real_)
  log(2) / lambda
}

#' Volumes, clearances and derived constants of a drug at the reference patients
#'
#' @returns a data frame with one row per patient for which the model ran:
#'   covariates, V1-V3 (L), CL1-CL3 (L/min), k10-k31 (1/min), the three
#'   disposition half-lives (min), ke0 (1/min), its half-time, tPeak (min) and
#'   the citation in force for that patient
#' @noRd
helpDrugParameterTable <- function(drug, drugDefaults = getDrugDefaultsGlobal(),
                                   patients = helpReferencePatients()) {
  rows <- lapply(seq_len(nrow(patients)), function(i) {
    p <- patients[i, ]
    pk <- helpDrugPK(drug, p, drugDefaults)
    if (is.null(pk)) return(NULL)
    d <- pk$PK[[PK_EVENT_DEFAULT]]
    if (is.null(d)) d <- pk$PK[[1]]
    lambdas <- sort(c(d$lambda_1, d$lambda_2, d$lambda_3), decreasing = TRUE)
    data.frame(
      Patient = p$label, Age = p$age, Weight = p$weight, Height = p$height, Sex = p$sex,
      V1 = d$v1, V2 = d$v2, V3 = d$v3,
      CL1 = d$cl1, CL2 = d$cl2, CL3 = d$cl3,
      k10 = d$k10, k12 = d$k12, k13 = d$k13, k21 = d$k21, k31 = d$k31,
      halfLife1 = helpHalfLife(lambdas[1]),
      halfLife2 = helpHalfLife(lambdas[2]),
      halfLife3 = helpHalfLife(lambdas[3]),
      ke0 = d$ke0,
      ke0HalfTime = helpHalfLife(d$ke0),
      tPeak = pk$tPeak,
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
         "%" = "% of one atmosphere", as.character(units))
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

  # --- At a glance -----------------------------------------------------------
  esc <- htmltools::htmlEscape
  routes <- c(if (any(grepl(" PO$", units))) "oral", if (any(grepl(" IM$", units))) "intramuscular",
              if (any(grepl(" IN$", units))) "intranasal")
  glance <- data.frame(
    Property = esc(c("Class", "Concentration units", "Units offered in the dose table",
                     "Default unit", "Typical concentration (shaded band)",
                     "Typical value", "MEAC", "Recovery threshold (endCe)", "Plot colour")),
    Value = c(
      esc(if (length(routes)) paste0("Intravenous, also ", paste(routes, collapse = " and ")) else "Intravenous"),
      esc(concUnits),
      esc(paste(units, collapse = ", ")),
      esc(as.character(row$Default.Units)),
      esc(sprintf("%s to %s %s", helpFormatNumber(row$Lower), helpFormatNumber(row$Upper), concUnits)),
      esc(sprintf("%s %s", helpFormatNumber(row$Typical), concUnits)),
      esc(if (!is.na(row$MEAC) && row$MEAC > 0) sprintf("%s %s", helpFormatNumber(row$MEAC), concUnits) else "Not an opioid: not on the MEAC panel"),
      esc(sprintf("%s %s", helpFormatNumber(row$endCe), concUnits)),
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
    derived <- data.frame(
      params$Patient,
      helpFormatNumber(params$halfLife1),
      helpFormatNumber(params$halfLife2),
      helpFormatNumber(params$halfLife3),
      helpFormatNumber(params$ke0),
      helpFormatNumber(params$ke0HalfTime),
      helpFormatNumber(params$tPeak),
      stringsAsFactors = FALSE
    )
    names(derived) <- c("Patient", "t\u00bd \u03b1 (min)", "t\u00bd \u03b2 (min)",
                        "t\u00bd \u03b3 (min)", "ke0 (1/min)", "t\u00bd ke0 (min)", "tPeak (min)")
    varies <- function(x) length(unique(round(x, 6))) > 1
    covariateNote <- if (varies(params$V1) || varies(params$CL1)) {
      "The parameters change between these patients, so this model has covariates: see the notes below for which ones and how."
    } else {
      "The parameters are the same for every patient: this model has no covariates, and a dose is the same whether the patient weighs 20 kg or 120 kg. Dose in per-kilogram units if you want size taken into account."
    }
    paramsHTML <- paste0(
      "<p>", covariateNote, "</p>",
      helpTableHTML(shown, "Volumes and clearances"),
      helpTableHTML(derived, "Half-lives, effect-site equilibration and time to peak effect"),
      "<p class='small text-muted'>The disposition half-lives are ln(2) divided by the three ",
      "eigenvalues of the compartment model (\u03b1 fastest, \u03b3 slowest); a dash means the ",
      "compartment is absent. ke0 is solved so that the effect-site concentration after a bolus ",
      "peaks at tPeak; see ", helpPageLink("models/effect-site", "The effect site and ke0"), ".</p>"
    )
  }

  # --- Absorption routes -----------------------------------------------------
  absorptionHTML <- ""
  pkRef <- helpDrugPK(drug, helpReferencePatients()[1, ], drugDefaults)
  if (!is.null(pkRef)) {
    d <- pkRef$PK[[PK_EVENT_DEFAULT]]
    routes <- list(PO = "Oral (PO)", IM = "Intramuscular (IM)", IN = "Intranasal (IN)")
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
        "<p>Doses with PO, IM or IN units are absorbed by first-order kinetics into the central ",
        "compartment after a lag, with the fraction shown reaching the circulation. See ",
        helpPageLink("models/absorption"), ".</p>",
        helpTableHTML(do.call(rbind, rows))
      )
    }
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
    absorptionHTML,
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
    data.frame(
      Drug = sprintf('<a href="#" data-help-page="drugs/%s">%s%s</a>', row$Drug,
                     as.character(helpColorSwatch(row$Color)), helpDrugTitle(row$Drug)),
      `Model source` = htmltools::htmlEscape(if (is.null(pk)) "Not available" else helpReferenceShort(pk$reference)),
      `Default unit` = htmltools::htmlEscape(as.character(row$Default.Units)),
      `Typical range` = sprintf("%s\u2013%s %s", helpFormatNumber(row$Lower), helpFormatNumber(row$Upper), conc),
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
    "with age or body size, and their pages say where.</p>",
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
