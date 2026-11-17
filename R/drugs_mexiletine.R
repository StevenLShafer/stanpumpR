# -----------------------------------------------------------------------------
# Mexiletine (Vozeh 1982): one compartment, apparent oral, proportional to
# body weight
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# supplied the source's PDF.  The numbers below were read in that PDF
# (Methods, equation 4, Table 1 on the page image, Table 2 and the
# Discussion).
#
# THE SOURCE
# ==========
# Vozeh S, Katz G, Steiner V, Follath F.  Population pharmacokinetic
# parameters in patients treated with oral mexiletine.  Eur J Clin Pharmacol
# 1982;23:445-451 (doi 10.1007/BF00605996).  58 patients with arrhythmias
# (65.8 +/- 11.8 years; 8 women; 27 with heart failure, 8 with abnormal liver
# tests), 452 serum concentrations after oral loading doses of 400 or 600 mg
# and during maintenance on 200 or 250 mg every 8 h, gas chromatography;
# NONMEM.  One compartment with first-order absorption after a lag, apparent
# oral parameters (no bioavailability in the model), clearance and volume
# proportional to body weight ("only the use of the body weight
# significantly improved the fit").  Table 1:
#
#     Cl 0.38 L/h/kg    Vd 5.3 L/kg    ka 3.1 /h    lag (to) 0.3 h
#
# so at 70 kg CL/F 26.6 L/h, V/F 371 L and a half-life of 9.7 h, as the
# paper says.  Heart failure, liver disease and sex did not improve the fit.
# The paper's own check: after 500 mg the typical patient's concentration is
# 1.25 mg/L at 1.5 h and 0.95 at 6 h, and 250 mg every 8 h averages about
# 1.2 mg/L; this model gives 1.23, 0.91 and 1.17.
#
# DOSE BASIS
# ==========
# The paper gives doses as "mg" of mexiletine without naming the salt.  The
# drug has only ever been marketed as the hydrochloride (Mexitil, and the US
# generic capsules of 150, 200 and 250 mg mexiletine hydrochloride), so the
# doses are read as the hydrochloride as prescribed, and a dose is entered as
# the capsule strength.  (Base is 83.1% of the hydrochloride.)
#
# AGAINST AN INTRAVENOUS STUDY (not used in the fit)
# ==================================================
# Campbell 1978 (Br J Clin Pharmacol 6:372, read in full; five young men):
# clearance 516 mL/min (31.0 L/h), half-life 11.8 h, bioavailability 0.80, so
# CL/F 38.7 L/h, about 45% above this model's at 70 kg.  Older patients with
# heart disease clear it more slowly (AGENTS.md: compare clearance first).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms, at the pharmacokinetic weight with the
# fat-free-mass switch on and at total body weight with it off.
#
# NOT MODELLED
# ============
# CYP2D6 (poor metabolisers clear about half as much, Lledo 1993, abstract),
# CYP1A2 and smoking, urine pH (acid urine speeds renal clearance), liver
# disease, and the slower absorption after a myocardial infarction.
#
# THE BAND
# ========
# The source: "therapeutic serum concentration of between 0.8 and 2 mg/l"
# (its references 2, 5 and 10), central nervous system effects becoming more
# frequent above 2 mg/L.  The current label's range was not reachable.
# -----------------------------------------------------------------------------

# Vozeh 1982, Table 1
MEXILETINE_CL_PER_KG <- 0.38   # L/h/kg
MEXILETINE_V_PER_KG  <- 5.3    # L/kg
MEXILETINE_KA        <- 3.1    # 1/h
MEXILETINE_LAG       <- 0.3    # h

#' Mexiletine (Vozeh 1982)
#'
#' One compartment, apparent oral, proportional to body weight; see the
#' header of \code{R/drugs_mexiletine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mexiletine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  default <- list(
    v1 = MEXILETINE_V_PER_KG * pkW, v2 = 1, v3 = 1,
    cl1 = MEXILETINE_CL_PER_KG * pkW / 60, cl2 = 0, cl3 = 0,
    ka_PO = MEXILETINE_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = MEXILETINE_LAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Vozeh S et al., Eur J Clin Pharmacol 1982;23:445-451: one compartment, ",
    "apparent oral, CL/F 0.38 L/h/kg, V/F 5.3 L/kg, ka 3.1 /h, lag 0.3 h, in ",
    "58 patients with arrhythmias; doses as mexiletine hydrochloride. ",
    "https://doi.org/10.1007/BF00605996"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 1.4,
    upperTypical = 2,
    lowerTypical = 0.8,
    reference = reference
  )
}
