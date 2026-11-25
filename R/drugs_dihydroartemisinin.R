# -----------------------------------------------------------------------------
# Dihydroartemisinin: the active metabolite of artemether, formed from it and
# not dosed directly
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is ng/mL (= mcg/L) of dihydroartemisinin
# in plasma.
#
# Drafted by Claude Code, 2026-11-24, with artemether (R/drugs_artemether.R),
# whose header gives the source, Ding 2026 (Br J Clin Pharmacol 92:589-605),
# Table 2: one compartment, CL/F 255 L/h and VC/F 64.1 L for the 45 kg
# patient, scaled by weight with the fixed exponents 0.75 and 1.  The
# parameters are apparent: artemether's bioavailability, fixed at 1, is
# inside them, and the whole of artemether's elimination forms this moiety.
# Its own half-life is about 10 minutes, so its curve is set by its
# formation and follows artemether's.
#
# No dihydroartemisinin product is approved in the United States (it is
# given abroad with piperaquine), so this entry has no dose units and no
# Category, as enalaprilat has none: it is drawn only as artemether's
# metabolite, which is what dihydroartemisininSet() is for, the one place its
# parameters are defined.
# -----------------------------------------------------------------------------

DIHYDROARTEMISININ_CL <- 255    # L/h, CL/F at 45 kg
DIHYDROARTEMISININ_V  <- 64.1   # L, VC/F at 45 kg

#' Dihydroartemisinin's disposition, the one place it is defined
#'
#' @inheritParams cefazolin
#' @returns a PK set: one compartment
#' @keywords internal
dihydroartemisininSet <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  f <- dingWeightFactors(weight, height, age, sex, adjustToFFM)
  list(
    v1  = DIHYDROARTEMISININ_V * f$volume,
    v2  = 1,
    v3  = 1,
    cl1 = DIHYDROARTEMISININ_CL * f$clearance / 60,
    cl2 = 0,
    cl3 = 0
  )
}


#' Dihydroartemisinin pharmacokinetics (metabolite of artemether)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dihydroartemisinin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  default <- dihydroartemisininSet(weight, height, age, sex, adjustToFFM)
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Ding J, Hoglund RM, van der Pluijm RW, et al. Br J Clin Pharmacol ",
      "2026;92:589-605, Table 2. THE ACTIVE METABOLITE OF ARTEMETHER, formed ",
      "from all of its elimination and not dosed directly. One compartment, ",
      "CL/F 255 L/h x (WT/45)^0.75, VC/F 64.1 L x WT/45 (apparent: artemether's ",
      "bioavailability, fixed at 1, is inside them). https://doi.org/10.1002/bcp.70301")
  )
}
