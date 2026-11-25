# -----------------------------------------------------------------------------
# Fibrinogen concentrate (human): one compartment, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mg, concentrations in mg/dL.  Plotted as the patient's own plasma
# fibrinogen plus the fibrinogen infused (R/factorBaselines.R; Steven L.
# Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-25, at the request of
# Steven L. Shafer (docs/library-expansion/plan.csv, Biologics (surgery));
# digest card papers/digests/PMID_31658379.md.
#
# ONE DRUG FOR THE CONCENTRATES (Shafer, 2026-11-25, accepting the
# recommendation): the source's product is LFB's Clottafact (FibCLOT), which
# has no US label; the US products are RiaSTAP, Fibryga and Fesilty.  An
# infused fibrinogen is the patient's own protein, with the same half-life of
# about three days whichever plasma it was purified from, so the model stands
# for the class, and RiaSTAP's label for the US products (fdaLabels.csv).
#
# SOURCE
# ======
# Bellon A, Fuseau E, Roumanie O, et al.  Population pharmacokinetics of a
# triple-secured fibrinogen concentrate administered to afibrinogenaemic
# patients: observed age- and body weight-related differences and
# consequences for dose adjustment in children.  Br J Clin Pharmacol
# 2020;86:329-337.  Read on the PDF.  31 patients with afibrinogenaemia from
# three studies (2004-2015), 18 men and 13 women, aged 1.5 to 48.7 years and
# weighing 10.8 to 93.5 kg (Table 1), each given a single 0.06 g/kg; Clauss
# fibrinogen activity, 3 to 8 samples per patient up to 14 days; NONMEM.
#
# Table 2 and equations 4 and 5, weight (kg) not centred:
#
#     CL = 0.00288 x WT^0.556  L/h
#     V  = 0.0960  x WT^0.808  L
#
# One compartment (two were tested); age and sex did not improve the fit once
# weight was in.
#
# CHECKS (CL and V above): at 70 kg CL 0.44 mL/h/kg, V 42.5 mL/kg,
# half-life 67 h, incremental recovery 2.35 mg/dL per mg/kg; at 20 kg CL 0.76
# mL/h/kg, half-life 49 h, recovery 1.85.  Bellon's Table 4 (individual Bayes
# estimates): 40 kg and over CL 0.45 mL/h/kg, Vss 43.2 mL/kg, half-life 66.7
# h, recovery 2.31; under 40 kg 0.74, 52.2, 49.0 and 1.85.  RiaSTAP's label
# (12.3, 14 patients 8-61 years, 70 mg/kg): CL 0.59 +/- 0.13 mL/h/kg, Vss
# 52.7 mL/kg, half-life 78.7 h, recovery 1.7 mg/dL per mg/kg (the label's
# dosing rule).  The clearances agree within the label's SD; the recovery is
# higher, the volume smaller.
#
# BASELINE: the Patient Profile's Fibrinogen baseline (mg/dL), added to the
# infused fibrinogen.  Blank, the source population's: afibrinogenaemia, 0
# mg/dL (Shafer's decision of 2026-11-25).  A bleeding surgical patient's
# fibrinogen, measured before the doses in the dose table (a later level
# already contains them), should be entered: a normal adult's is 200 to 450
# mg/dL (RiaSTAP 12.2).
#
# BAND: from 100 mg/dL, RiaSTAP's target level, maintained until haemostasis
# (2.1), to 450 mg/dL, the top of normal (12.2), the typical 200, the bottom
# of normal.
#
# BODY SIZE: the source's weight terms, kept as fitted, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
#
# PREGNANCY: Hureau 2026 (PMID 42994620, abstract read) modelled plasma
# fibrinogen in 142 women bleeding at caesarean section, with the bleeding
# rate on the rate of loss; this model has no bleeding term, and nothing of
# that model is applied (inst/extdata/pregnancyRegistry.csv).  Endogenous
# fibrinogen rises to 400-600 mg/dL at term, which the baseline field carries.
#
# References
# ----------
# Bellon A et al., Br J Clin Pharmacol 2020;86:329-337.
#   https://doi.org/10.1111/bcp.14147
# RIASTAP prescribing information, BLA 125317, revised 12/2025.
# -----------------------------------------------------------------------------

FIBRINOGEN_CL_COEF <- 0.00288   # L/h per kg^0.556
FIBRINOGEN_CL_EXP  <- 0.556
FIBRINOGEN_V_COEF  <- 0.0960    # L per kg^0.808
FIBRINOGEN_V_EXP   <- 0.808
# The source population's baseline, afibrinogenaemia
FIBRINOGEN_BASELINE <- 0        # mg/dL

#' Fibrinogen concentrate (Bellon 2020)
#'
#' One compartment, allometric in weight, plotted as the patient's own
#' fibrinogen plus the fibrinogen infused, in mg/dL; see the header of
#' \code{R/drugs_fibrinogen.R}.
#'
#' @inheritParams cefazolin
#' @param fibrinogenBaseline the patient's own plasma fibrinogen in mg/dL, or
#'   NULL for the source population's (afibrinogenaemia, 0 mg/dL)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fibrinogen <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       fibrinogenBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl <- FIBRINOGEN_CL_COEF * pkW^FIBRINOGEN_CL_EXP   # L/h
  v  <- FIBRINOGEN_V_COEF  * pkW^FIBRINOGEN_V_EXP    # L

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  baseline <- if (is.null(fibrinogenBaseline)) FIBRINOGEN_BASELINE else fibrinogenBaseline

  reference <- paste0(
    "Bellon 2020 (31 patients with afibrinogenaemia, 1.5-48.7 years, Clottafact): ",
    "one compartment, CL 0.00288 x weight^0.556 L/h, V 0.0960 x weight^0.808 L; ",
    "standing for the fibrinogen concentrates (RiaSTAP, Fibryga, Fesilty in the US). ",
    "Plotted as the patient's own fibrinogen plus the infused: baseline ",
    signif(baseline, 3), " mg/dL",
    if (is.null(fibrinogenBaseline)) " (blank: afibrinogenaemia, the source population)" else "",
    ". Band: RiaSTAP's target, 100 mg/dL, to the top of normal, 450 mg/dL. ",
    "https://doi.org/10.1111/bcp.14147")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 200, upperTypical = 450, lowerTypical = 100,
    reference = reference,
    endogenousActivity = baseline
  )
}
