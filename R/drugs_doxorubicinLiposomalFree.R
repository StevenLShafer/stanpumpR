# -----------------------------------------------------------------------------
# Free doxorubicin released from pegylated liposomal doxorubicin (Hsu 2018):
# metabolite-only, formed by doxorubicinLiposomal
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# ug (the CSV row's ng/mL) as dosed (doxorubicin hydrochloride).
#
# Drafted by Claude Code, 2026-11-16, with R/drugs_doxorubicinLiposomal.R,
# whose header gives the source (Hsu 2018, Table I) and why this is not the
# library's doxorubicin: two compartments, V2 9.52 L, V3 35.1 L, Q 2.87 L/h,
# CLfree 2.68 L/h, fitted jointly with the release (METABOLITE_FORMATION_BASIS
# "joint"), so only the ratio of release to this clearance is identified.
# No covariates.  Scaled to fat-free mass with the switch on, at the
# reference anchor, exactly as doxorubicinLiposomal is (see its header's
# BODY SIZE and BODY SIZE ANCHOR; 2026-11-24); published values with it off.  Not offered on its own (no
# Units, no Category).  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

#' Free doxorubicin from liposomal doxorubicin (Hsu 2018), metabolite only
#'
#' Two compartments, formed by \code{doxorubicinLiposomal}; see the header of
#' \code{R/drugs_doxorubicinLiposomal.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinLiposomalFree <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DOXORUBICIN_LIPOSOMAL_HSU
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = p$v2 * size$volume,
    v2  = p$v3 * size$volume,
    v3  = 1,
    cl1 = p$clFree / 60 * size$clearance,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Hsu LF. Pharm Res 2018;35:106 (free doxorubicin released from pegylated liposomal ",
    "doxorubicin: two compartments, V 9.52 and 35.1 L, Q 2.87 L/h, CL 2.68 L/h, fitted ",
    "jointly with the release; not conventional doxorubicin). ",
    "https://doi.org/10.1007/s11095-018-2387-4"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
