# -----------------------------------------------------------------------------
# Postmenstrual age, for the neonatal models whose clearance matures with it
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-27, at the request of Steven L. Shafer.
#
# Postmenstrual age is the gestational age at birth plus the postnatal age.
# The Patient Profile's Gestational age at birth field reaches only the models
# that name `gestationalAge` in their signature (getDrugPK()); a blank field is
# NULL, a term birth.  milrinonePediatric and tramadol predate the field and
# still take a term birth (40 + 52 x age).
# -----------------------------------------------------------------------------

#' Postmenstrual age in weeks
#'
#' @param age postnatal age in years
#' @param gestationalAge gestational age at birth in weeks, or NULL (or NA)
#'   for a term birth, \code{GESTATIONAL_AGE_TERM}
#' @returns the postmenstrual age in weeks: the gestational age at birth plus
#'   52 weeks per year of postnatal age
#' @keywords internal
postmenstrualAge <- function(age, gestationalAge = NULL) {
  if (is.null(gestationalAge) || (length(gestationalAge) == 1 && is.na(gestationalAge)))
    gestationalAge <- GESTATIONAL_AGE_TERM
  gestationalAge + 52 * age
}
