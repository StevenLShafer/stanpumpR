# -----------------------------------------------------------------------------
# The antihypertensive model registry, and the few endpoint relations that can
# actually be evaluated
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer.
# See docs/antihypertensives.md for the audit behind it and
# data-raw/antihypertensiveRegistry.R for the two CSVs this file reads.
#
# WHY A REGISTRY AND NOT A SET OF DRUGS
# =====================================
# The commonly used antihypertensives do not share a defensible
# concentration-to-blood-pressure model, and most of them have no complete
# published pharmacokinetic model either.  Listing a drug must therefore not
# imply that anything can be simulated for it.  The registry carries one row
# per model record -- a molecule, in one formulation, by one route, and where
# a paper fitted something, in one population for one endpoint -- with
# independent evidence fields, so the difference between "there is a product"
# and "there is a model" is visible rather than implied.
#
# Twelve records are implemented as drugs in the library, and every one plots
# a CONCENTRATION: clevidipine, fenoldopamPediatric, carvedilol, azilsartan,
# chlorthalidone, hydrochlorothiazide, lisinopril, olmesartan, valsartan,
# benazepril, enalapril (with the enalaprilat it forms) and fosinopril, each
# in R/drugs_<name>.R.  No drug in the library plots a blood
# pressure, and bp_simulation_enabled is FALSE for every record in the
# registry.
#
# WHAT antihypertensiveEndpoint() IS FOR
# ======================================
# A few papers fitted a usable concentration-to-endpoint relation without
# publishing the pharmacokinetics that would reach it from a dose.  Those
# relations can still be evaluated against a concentration the caller
# supplies -- a measured one, or one from a simulation the caller trusts --
# and that is all this function does.  It is not a blood-pressure simulator:
# it returns the CHANGE the drug term predicts, never an absolute pressure,
# because none of these records publishes the baseline model that an absolute
# pressure would need.
# -----------------------------------------------------------------------------

#' The antihypertensive model registry
#'
#' One row per model record, as described in the file header.  The columns are
#' documented in \code{data-raw/antihypertensiveRegistry.R}.
#'
#' @returns a data frame of model records
#' @export
antihypertensiveRegistry <- function()
{
  utils::read.csv(
    system.file("extdata", "antihypertensiveRegistry.csv",
                package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")
}

#' The antihypertensive parameter audit
#'
#' Every number transcribed for the registry, with its unit, matrix,
#' population, endpoint, fitted/fixed/derived status, uncertainty and the TYPE
#' of uncertainty the source itself names, the source URL, and whether the
#' number was verified against the primary source or comes unverified from the
#' handoff specification.
#'
#' @returns a data frame of parameters
#' @export
antihypertensiveParameters <- function()
{
  utils::read.csv(
    system.file("extdata", "antihypertensiveParameters.csv",
                package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA")
}

# The endpoint relations that are complete enough to evaluate.  A relation is
# listed here only when every coefficient it needs was verified against the
# primary source (see the verification column of the parameter audit).
#
#   structure   "emax"   change = -max_drop * C / (EC50 + C)
#               "linear" change = slope * C            (slope already signed)
#   max_drop    the NORMALISED magnitude, always positive, with direction
#               "decrease": the canonical sign convention, so that at
#               C = EC50 the change is exactly -max_drop/2.  A paper that
#               prints Emax = -27 mmHg is stored as max_drop 27, never
#               subtracted again.
#   valid_max   the top of the concentration range the source supports, or NA
#               when the source's range could not be established.
ANTIHYPERTENSIVE_ENDPOINTS <- list(
  list(
    model_id = "fenoldopam__injection__iv__anaesthetised_children__map__hammer2008",
    structure = "emax", max_drop = 27, ec50 = 9.1, slope = NA,
    direction = "decrease", concentration_unit = "ng/mL",
    endpoint_unit = "mmHg", valid_max = 62.7,
    note = paste(
      "Anaesthetised children during deliberate hypotension. The drug term",
      "only: baseline mean arterial pressure was a significant covariate on",
      "the maximum effect, but the paper publishes no coefficient for it, so",
      "the fitted model cannot be reproduced in full. The range limit is the",
      "steady-state concentration of the highest rate studied,",
      "3.2 mcg/kg/min divided by the fitted clearance.")
  ),
  list(
    model_id = "terazosin__ir_tablet__oral__healthy_volunteers__sbp__campanero2008",
    structure = "emax", max_drop = 29.9, ec50 = 29.9, slope = NA,
    direction = "decrease", concentration_unit = "mcg/L",
    endpoint_unit = "mmHg", valid_max = NA,
    note = paste(
      "Healthy volunteers after a single 2 mg oral dose, fitted subject by",
      "subject. The source publishes no oral pharmacokinetics, so there is",
      "no dose-to-concentration chain, and no concentration range could be",
      "established.")
  ),
  list(
    model_id = "terazosin__ir_tablet__oral__healthy_volunteers__dbp__campanero2008",
    structure = "emax", max_drop = 39.7, ec50 = 28.7, slope = NA,
    direction = "decrease", concentration_unit = "mcg/L",
    endpoint_unit = "mmHg", valid_max = NA,
    note = paste(
      "As the systolic record. The large diastolic maximum is a fitted",
      "asymptote from a single 2 mg dose in healthy volunteers, not an",
      "achievable clinical target.")
  ),
  list(
    model_id = "amlodipine__tablet__oral__hypertensive_adults__erect_sbp__donnelly1993",
    structure = "linear", max_drop = NA, ec50 = NA, slope = -3.1,
    direction = "decrease", concentration_unit = "ng/mL",
    endpoint_unit = "mmHg", valid_max = NA,
    note = paste(
      "A placebo-corrected LOCAL slope for ERECT systolic pressure in 12",
      "patients on 5 mg daily. It is linear and unbounded, with no ceiling",
      "and no published concentration range, so every evaluation is an",
      "extrapolation beyond the exposure that one dose produced and large",
      "concentrations give physiologically impossible falls.")
  )
)

#' The antihypertensive endpoint relations that can be evaluated
#'
#' @returns a data frame of the available relations, one row each, with the
#'   registry's description of the record
#' @export
antihypertensiveEndpointsAvailable <- function()
{
  registry <- antihypertensiveRegistry()
  ids <- vapply(ANTIHYPERTENSIVE_ENDPOINTS, function(e) e$model_id, "")
  rows <- registry[match(ids, registry$model_id), ]
  data.frame(
    model_id = ids,
    drug = rows$drug,
    formulation = rows$formulation,
    route = rows$route,
    population = rows$population,
    endpoint = rows$endpoint,
    analyte = rows$analyte,
    matrix = rows$matrix,
    structure = vapply(ANTIHYPERTENSIVE_ENDPOINTS, function(e) e$structure, ""),
    concentration_unit = vapply(ANTIHYPERTENSIVE_ENDPOINTS,
                                function(e) e$concentration_unit, ""),
    endpoint_unit = vapply(ANTIHYPERTENSIVE_ENDPOINTS,
                           function(e) e$endpoint_unit, ""),
    pd_evidence = rows$pd_evidence,
    bp_simulation_enabled = rows$bp_simulation_enabled,
    stringsAsFactors = FALSE
  )
}

#' Evaluate a published antihypertensive endpoint relation
#'
#' Evaluates one registry record's concentration-to-endpoint relation at
#' concentrations the caller supplies.  This is NOT a blood-pressure
#' simulation: no dose is given, no baseline is modelled, and the result is
#' the CHANGE the drug term predicts, in the source's own endpoint and units.
#' Nothing in this package calls it to draw a curve.
#'
#' The concentration must be in the record's own unit and matrix: a plasma
#' relation evaluated on a whole-blood concentration, or a relation evaluated
#' on a different analyte, is a different quantity and the function cannot
#' detect it.
#'
#' @param model_id a \code{model_id} from
#'   \code{antihypertensiveEndpointsAvailable()}
#' @param concentration numeric, non-negative, in the record's concentration
#'   unit
#'
#' @returns a data frame with one row per concentration: the record's
#'   identity, population, endpoint, analyte and matrix, the concentration,
#'   the predicted change and its unit, the evidence grade, whether blood
#'   pressure simulation is enabled for the record (it never is), and any
#'   warnings that apply
#' @export
#' @examples
#' # Half the maximum effect at the EC50, as a FALL, never a rise
#' antihypertensiveEndpoint(
#'   "fenoldopam__injection__iv__anaesthetised_children__map__hammer2008",
#'   9.1)$predicted_change
antihypertensiveEndpoint <- function(model_id, concentration)
{
  if (!is.character(model_id) || length(model_id) != 1 || is.na(model_id))
    stop("model_id must be a single model_id string.")
  which_one <- which(vapply(ANTIHYPERTENSIVE_ENDPOINTS,
                            function(e) identical(e$model_id, model_id), TRUE))
  if (length(which_one) != 1) {
    registry <- antihypertensiveRegistry()
    if (model_id %in% registry$model_id)
      stop("No evaluable endpoint relation for ", model_id,
           ": that record is in the registry with execution_status '",
           registry$execution_status[registry$model_id == model_id],
           "'. See its limitations column, and docs/antihypertensives.md.")
    stop("Unknown model_id: ", model_id,
         ". See antihypertensiveEndpointsAvailable().")
  }
  e <- ANTIHYPERTENSIVE_ENDPOINTS[[which_one]]

  if (!is.numeric(concentration) || length(concentration) == 0 ||
      anyNA(concentration) || any(concentration < 0))
    stop("concentration must be non-negative and not missing, in ",
         e$concentration_unit, ".")

  change <- switch(
    e$structure,
    emax   = -e$max_drop * concentration / (e$ec50 + concentration),
    linear = e$slope * concentration,
    stop("Unknown endpoint structure: ", e$structure)
  )

  warnings <- rep(e$note, length(concentration))
  if (is.na(e$valid_max)) {
    warnings <- paste(
      warnings,
      "The source's supported concentration range could not be established,",
      "so every value is outside a verified range.")
  } else {
    beyond <- concentration > e$valid_max
    if (any(beyond))
      warnings[beyond] <- paste(
        warnings[beyond], "This concentration is above", e$valid_max,
        paste0(e$concentration_unit, ","), "the top of the range the source",
        "supports, so it is an extrapolation.")
  }
  if (identical(e$structure, "linear"))
    warnings <- paste(
      warnings,
      "The relation is linear and has no maximum, so it cannot be used as an",
      "Emax model and must not be evaluated at high concentrations.")

  registry <- antihypertensiveRegistry()
  row <- registry[registry$model_id == model_id, ]
  data.frame(
    model_id = model_id,
    drug = row$drug,
    formulation = row$formulation,
    route = row$route,
    population = row$population,
    endpoint = row$endpoint,
    analyte = row$analyte,
    matrix = row$matrix,
    concentration = concentration,
    concentration_unit = e$concentration_unit,
    predicted_change = change,
    predicted_change_unit = e$endpoint_unit,
    direction = e$direction,
    structure = e$structure,
    pd_evidence = row$pd_evidence,
    bp_simulation_enabled = row$bp_simulation_enabled,
    external_validation_status = row$external_validation_status,
    source_url = row$source_url,
    warnings = warnings,
    stringsAsFactors = FALSE
  )
}
