# -----------------------------------------------------------------------------
# SN-38: the active metabolite of irinotecan (Klein 2002)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of SN-38.
#
# Drafted by Claude Code, 2026-11-06.  Metabolite only: it has no units of
# its own and is not dosed; R/drugs_irinotecan.R forms it, and its header
# gives the source, the checks and why the 466-L mixture group is used.
# Two compartments, every volume and clearance divided by the unknown
# fraction of irinotecan converted (Fmsn).  The volumes and clearances scale
# by the library's factors with the switch on and are fixed with it off.
# -----------------------------------------------------------------------------

# Klein 2002, Table I: SN-38, rate constants in /h
SN38_KLEIN <- list(
  v1 = 466,   # L, Vc/Fmsn of the group whose Fmsn was fixed at 12%
  k10 = 2.52, k12 = 2.6, k21 = 0.0584
)

#' SN-38, the active metabolite of irinotecan (Klein 2002)
#'
#' Two compartments, scaled by the unknown fraction formed; formed by
#' \code{irinotecan()}.  See the header of \code{R/drugs_irinotecan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
irinotecanSN38 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- SN38_KLEIN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = p$v1 * size$volume,
    v2  = p$v1 * p$k12 / p$k21 * size$volume,
    v3  = 1,                                 # two compartments
    cl1 = p$v1 * p$k10 / 60 * size$clearance,
    cl2 = p$v1 * p$k12 / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Klein CE, Gupta E, Reid JM, et al. Clin Pharmacol Ther 2002;72:638-647 (SN-38 ",
    "formed from irinotecan: two compartments, Vc/Fmsn 466 L, the group whose Fmsn ",
    "was fixed at 12%; k10 2.52, k12 2.6, k21 0.0584 /h). ",
    "https://doi.org/10.1067/mcp.2002.129502"
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
