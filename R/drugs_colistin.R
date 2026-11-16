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
# THE MIC LINE AND THE BAND (decision pending, recorded in the PR, 2026-11-16)
# ============================================================================
# None is drawn yet.  The library's rule puts an antibiotic's line at free
# drug at the MIC (R/antibioticThresholds.R): the EUCAST v16.1 (2026)
# breakpoint for Enterobacterales and Acinetobacter is 2 mg/L, and the
# unbound fraction in critically ill patients 0.49 (Nation 2017), so the line
# would be 4.1 mg/L of total colistin.  That is above the international
# consensus target (Tsuji 2019, Pharmacotherapy 39:10-39, doi
# 10.1002/phar.2209): an AUC of about 50 mg.h/L, "a target average
# steady-state plasma concentration ... of ~2 mg/L for total drug", which
# "should be considered as a maximum tolerable exposure"; higher
# concentrations "increase both the incidence and severity of AKI".  The band
# rule (free drug at the MBC, R/antibioticMbc.R) needs an MBC source, which
# has not been read.
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
  f <- COLISTIMETHATE_BASIS

  default <- list(
    v1  = COLISTIN_V / f,
    v2  = 1,
    v3  = 1,
    cl1 = (COLISTIN_CL_RENAL * crcl + COLISTIN_CL_NR) / 60 / f,
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
