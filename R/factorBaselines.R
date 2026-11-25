# The patient's own clotting-factor activity, below the infused activity.
#
# A factor concentrate is plotted as the activity it leaves in the patient,
# not as the activity infused: the patient's own baseline (a severe
# haemophiliac's below 1 IU/dL, a normal adult's about 100) plus the infused
# activity the model predicts (Steven L. Shafer, 2026-11-25, for every factor
# concentrate: factor VIII, factor IX, von Willebrand factor, fibrinogen,
# antithrombin).  So a perioperative target can be read on the curve, which is
# where a haematologist sets it.  Three decisions of that day: activity is
# plotted in IU/dL (= % of normal), the unit of the labels, of the dosing rule
# (1 IU/kg of factor VIII raises its activity about 2 IU/dL) and of the
# perioperative targets; a blank field takes the baseline of the population
# the model was fitted in, which the drug's help page states; and each factor
# has a Patient Profile field of its own, shown only while a drug that reads
# it is in the dose table.
#
# A model reads a baseline by naming its field in its signature
# (`factorVIIIBaseline = NULL`), as a model reads the serum albumin, and
# returns it as `endogenousActivity`, in its plotted units; simCpCe() adds it to
# the plasma activity after the engines have run (the engines simulate only
# the infused activity, which is linear in the dose), and takes it back off
# the time-until-threshold target and the normalised curve.  The fields are
# carried through getDrugPK(), recalculatePK(), simulateDrugsWithCovariates()
# and the Simulation API as one named list, `factorBaselines`, so that a new
# factor needs a row here and nothing else.

# One row per field: its id (the model argument and the Patient Profile input),
# its label, its unit and the bounds a typed value is held to.  The bounds are
# chosen, not sourced: zero to three times a normal adult's activity, which
# takes in a severe deficiency and the highest level a concentrate is given to
# reach.
FACTOR_BASELINE_FIELDS <- list(
  factorVIIIBaseline = list(
    label = "Factor VIII baseline",
    units = "IU/dL",
    min = 0,
    max = 300,
    step = 0.5
  ),
  factorIXBaseline = list(
    label = "Factor IX baseline",
    units = "IU/dL",
    min = 0,
    max = 300,
    step = 0.5
  )
)

#' The baselines of the factor-concentrate fields, checked
#'
#' @param x a named list (or named numeric vector) of baselines, the names
#'   among `names(FACTOR_BASELINE_FIELDS)`; a missing, NULL or NA entry is a
#'   blank field
#' @returns a named list of the entries that hold a number, each within its
#'   field's bounds; an empty list when every field is blank
#' @keywords internal
checkFactorBaselines <- function(x)
{
  if (is.null(x) || length(x) == 0) return(list())
  if (is.null(names(x)) || any(!nzchar(names(x))))
    stop("factorBaselines must be named by field: ",
         paste(names(FACTOR_BASELINE_FIELDS), collapse = ", "))
  unknown <- setdiff(names(x), names(FACTOR_BASELINE_FIELDS))
  if (length(unknown) > 0)
    stop("Unknown factor baseline: ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(FACTOR_BASELINE_FIELDS), collapse = ", "))
  out <- list()
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value) || (length(value) == 1 && is.na(value) &&
                           !(is.numeric(value) && is.nan(value))))
      next
    field <- FACTOR_BASELINE_FIELDS[[name]]
    if (!is_valid_number(value, field$min, field$max))
      stop("Invalid ", name, ": ", paste(value, collapse = ", "),
           ". Must be a number between ", field$min, " and ", field$max,
           " ", field$units, ", or blank for the model's source population")
    out[[name]] <- as.numeric(value)
  }
  out
}

#' The baselines a drug's model reads
#'
#' @param drug the model function's name
#' @param factorBaselines the checked baselines (`checkFactorBaselines()`)
#' @returns a named list of the arguments to pass: each field the model's
#'   signature names, with its value or NULL for a blank field
#' @keywords internal
factorBaselineArgs <- function(drug, factorBaselines)
{
  if (!exists(drug, mode = "function")) return(list())
  formalNames <- names(formals(get(drug, mode = "function")))
  read <- intersect(names(FACTOR_BASELINE_FIELDS), formalNames)
  out <- list()
  for (name in read) out[name] <- list(factorBaselines[[name]])
  out
}

#' The baseline a model returned, checked
#'
#' @param x the model's `endogenousActivity`
#' @param drug the drug, for the message
#' @returns the baseline, a finite number of 0 or more, or NULL
#' @keywords internal
validateEndogenousActivity <- function(x, drug)
{
  if (is.null(x)) return(NULL)
  if (!is_valid_number(x, 0, Inf))
    stop("Invalid endogenousActivity for ", drug, ": must be a finite number of 0 or more.")
  as.numeric(x)
}

#' The baselines the Patient Profile holds
#'
#' @param input Shiny's input, or any list holding one entry per field
#' @returns a named list of the fields that hold a number (not checked against
#'   the bounds, which `checkNumericCovariates()` reports); a blank field, or
#'   a value no control sends, is left out, and the model's own baseline then
#'   applies
#' @keywords internal
factorBaselinesFromInput <- function(input)
{
  out <- list()
  for (name in names(FACTOR_BASELINE_FIELDS)) {
    value <- asScalarNumeric(input[[name]])
    if (length(value) == 1 && !is.na(value)) out[[name]] <- value
  }
  out
}

#' A Patient Profile field for each factor baseline
#'
#' @returns a tag list of `relevantField()`s, one per field, each shown only
#'   while a drug that reads it is in the dose table
#' @keywords internal
factorBaselineInputs <- function()
{
  lapply(names(FACTOR_BASELINE_FIELDS), function(name) {
    field <- FACTOR_BASELINE_FIELDS[[name]]
    relevantField(name, fieldHelp(
      blankableNumericInput(
        inputId = name,
        label = sprintf("%s (%s)", field$label, field$units),
        value = NA,
        min = field$min,
        max = field$max,
        step = field$step
      ),
      patientFieldTooltip(name),
      placement = "right"
    ))
  })
}
