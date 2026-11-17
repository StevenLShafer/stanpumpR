# -----------------------------------------------------------------------------
# The Patient Profile's Pulmonary hypertension
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-17, at the request of Steven L. Shafer: "If
# sildenafil is selected, create a patient covariate 'Pulmonary hypertension'
# that can be selected.  That will switch between the models used in patients
# with PAH and patients with erectile dysfunction.  The PAH switch only
# appears if there is a drug with use in PAH and not in PAH.  They will be
# very different PK in the two populations, as patients with PAH often have
# severe cardiac disease as a result."
#
# A drug given both for pulmonary arterial hypertension and for something
# else (sildenafil: Revatio and Viagra; tadalafil: Adcirca and Cialis) has
# been studied in both populations, and patients with PAH clear it more
# slowly.  Its model names `pulmonaryHypertension = FALSE` in its signature
# and returns the PAH population's parameters when it is TRUE.  getDrugPK()
# passes the flag only to the models that name it, so the Patient Profile's
# field is read, shown (relevantFields(), R/patient-fields.R) and listed on
# its hover tip only for those drugs: a drug given for PAH alone (riociguat,
# the endothelin antagonists, the prostacyclins) was fitted in patients with
# PAH and does not read it.
# -----------------------------------------------------------------------------

#' Check the pulmonary hypertension flag
#'
#' @param x TRUE, FALSE or NULL (FALSE)
#' @returns TRUE or FALSE
#' @keywords internal
checkPulmonaryHypertension <- function(x)
{
  if (is.null(x)) return(FALSE)
  if (!is.logical(x) || length(x) != 1 || is.na(x))
    stop("Invalid pulmonaryHypertension: ", paste(x, collapse = ", "),
         ". Must be TRUE or FALSE")
  x
}
