# -----------------------------------------------------------------------------
# ACT-333679, selexipag's active metabolite: two compartments, from Krause
# 2017 (see R/drugs_selexipag.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-15.  Metabolite only: it cannot be dosed
# (no Units and no Category in drugDefaults_global.csv) and is drawn when
# selexipag is.  Its parameters were fitted jointly with selexipag's in one
# source (METABOLITE_FORMATION_BASIS "joint"), Krause 2017, Table 1 and
# Table 2:
#
#     Vm/F 4.65 L x (bw/70)^0.88
#     k34 1.04 /h      k43 0.18 /h
#     km 0.49 /h in women, x exp(0.15) = 0.57 /h in men
#
# held at no PAH comedication (an endothelin receptor antagonist raised km to
# 0.57 /h, a PDE5 inhibitor to 0.52, both to 0.71).  ACT-333679 is about 37
# times as potent as selexipag at the IP receptor (Krause 2017), which makes
# it the moiety that carries the effect.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on Vm, at the pharmacokinetic weight with the
# switch on and at total weight with it off.  The rate constants carry none,
# so the clearances scale with Vm.
#
# THE BAND
# ========
# No therapeutic range.
# -----------------------------------------------------------------------------

# Krause 2017, Tables 1 and 2
ACT333679_VM    <- 4.65   # L at 70 kg
ACT333679_VM_WT <- 0.88
ACT333679_K34   <- 1.04   # /h
ACT333679_K43   <- 0.18   # /h
ACT333679_KM    <- 0.49   # /h, women
ACT333679_KM_MALE <- 0.15 # exp() on km in men

#' ACT-333679, the active metabolite of selexipag (Krause 2017)
#'
#' Two compartments; see the header of \code{R/drugs_ACT333679.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ACT333679 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / SELEXIPAG_WT_REF
  vm <- ACT333679_VM * wt^ACT333679_VM_WT
  km <- ACT333679_KM * (if (sex == SEX_MALE) exp(ACT333679_KM_MALE) else 1)

  default <- list(
    v1  = vm,
    v2  = vm * ACT333679_K34 / ACT333679_K43,
    v3  = 1,
    cl1 = vm * km / 60,
    cl2 = vm * ACT333679_K34 / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = SELEXIPAG_REFERENCE
  )
}
