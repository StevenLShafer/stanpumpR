# -----------------------------------------------------------------------------
# Colistin, formed from colistimethate (CMS): one compartment, from Garonzik
# 2011 (see R/drugs_colistimethate.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in mg
# of colistin base activity (see THE DOSE BASIS in R/drugs_colistimethate.R),
# so the plotted concentration is mg/L (= mcg/mL), total plasma colistin.
#
# Drafted by Claude Code, 2026-11-16.  Metabolite only: it cannot be dosed (no
# Units and no Category in drugDefaults_global.csv) and is drawn when
# colistimethate is.  Fitted jointly with CMS in one source
# (METABOLITE_FORMATION_BASIS "joint"); Table 2 and equation 8 of Garonzik
# 2011:
#
#     V3/fm 45.1 L       CLT_C/fm = 0.0147 x CrCL + 2.19 L/h
#
# with CrCL in mL/min/1.73 m2 (colistimethateCrCL()).  Both are apparent, the
# fraction of CMS's nonrenal clearance that forms colistin being unknown, and
# both are divided by COLISTIMETHATE_BASIS as CMS's are.  The renal term is
# not colistin's own renal clearance, which is small: "as CrCL declines a
# progressively larger fraction of a dose of CMS will be converted to
# colistin" (the source's discussion), and the fit carries it here.
#
# BODY SIZE: the renal term carries Cockcroft-Gault (weight inside) and is
# never scaled.  V3/fm and the non-renal 2.19 L/h have no size term in the
# source (it found no trend in clearance against body size); with the
# fat-free-mass switch on they are scaled, V3/fm by FFM / FFM_ref and the
# 2.19 L/h by its 0.75 power, anchored at Garonzik's cohort
# (COLISTIMETHATE_COHORT, colistimethateSizeFree()), as CMS's are, so that
# the formation of colistin (CMS's scaled non-renal clearance) and colistin's
# own parameters move together (Steven L. Shafer's first-principles rule,
# 2026-11-19; R/drugs_colistimethate.R, BODY SIZE).  With the switch off they
# are as published.
#
# THE MIC LINE (Shafer's decision, 2026-11-16) AND THE BAND
# ==========================================================
# The line is the library's rule for every antibiotic (R/antibioticThresholds.R):
# free drug at the MIC.  The EUCAST v16.1 (2026) breakpoint for
# Enterobacterales and Acinetobacter is 2 mg/L, and colistin's unbound
# fraction in critically ill patients is 0.49 (Nation 2017, by
# ultracentrifugation in 66 patients), so the line is 2 / 0.49 = 4.1 mg/L of
# TOTAL colistin.  It was drawn on Shafer's decision of 2026-11-16, knowing
# that it lies ABOVE the international consensus target (Tsuji 2019,
# Pharmacotherapy 39:10-39, doi 10.1002/phar.2209): an AUC of about 50
# mg.h/L, "a target average steady-state plasma concentration ... of ~2 mg/L
# for total drug", which "should be considered as a maximum tolerable
# exposure"; higher concentrations "increase both the incidence and severity
# of AKI".  They measure different things: the line is the instant free
# colistin equals the MIC, the target an average of total colistin over the
# day, set by its kidney toxicity.  The guideline expects that average to be
# bactericidal against an isolate at the 2 mg/L breakpoint (in the murine
# thigh model, binding about 50% in patients) but "very likely to be
# suboptimal for the systemic treatment of a lung infection" unless the MIC
# is well below the breakpoint.  So a regimen at that target stays below the
# line (this model: an average of 2 mg/L peaks at about 2.2, at a CrCL of 40
# or 80 on Nation 2017's look-up doses scaled to it), and a curve kept above
# the line averages more than twice the exposure the guideline calls
# tolerable; the help pages say so.
# There is no band (antibioticNoBands()): no MBC source has been read.
# -----------------------------------------------------------------------------

# Garonzik 2011, Table 2 and equation 8
COLISTIN_V       <- 45.1    # L (V3/fm)
COLISTIN_CL_RENAL <- 0.0147 # L/h per mL/min/1.73 m2 (CLRCSL_POP/fm)
COLISTIN_CL_NR   <- 2.19    # L/h (CLNRC_POP/fm)

#' Colistin, formed from colistimethate (Garonzik 2011)
#'
#' One compartment; see the header of \code{R/drugs_colistin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
colistin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL)
{
  w <- colistimethateWeight(weight, height, age, sex, adjustToFFM)
  crcl <- colistimethateCrCL(w, height, age, sex, creatinine)
  size <- colistimethateSizeFree(weight, height, age, sex, adjustToFFM)
  f <- COLISTIMETHATE_BASIS

  default <- list(
    v1  = COLISTIN_V * size$volume / f,
    v2  = 1,
    v3  = 1,
    cl1 = (COLISTIN_CL_RENAL * crcl + COLISTIN_CL_NR * size$clearance) / 60 / f,
    cl2 = 0,
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
    reference = COLISTIMETHATE_REFERENCE
  )
}
