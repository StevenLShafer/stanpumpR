# -----------------------------------------------------------------------------
# Scopolamine by transdermal patch (Transderm Scop): two-compartment
# intravenous disposition (Liem-Moolenaar 2011) fed by a skin depot
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# ng, so the plotted concentration is pg/mL.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# on 2026-11-13 chose a patch route on the intravenous disposition, since the
# patch is the only FDA-approved scopolamine product in the US.  The numbers
# below were read in the full text of the source (the PubMed Central page of
# PMC3099375: Methods and Table 2) and in the Transderm Scop label (Reference
# ID 5578984, 2025), read in full.
#
# THE DISPOSITION
# ===============
# Liem-Moolenaar M, de Boer P, Timmers M, et al.  Pharmacokinetic-
# pharmacodynamic relationships of central nervous system effects of
# scopolamine in healthy subjects.  Br J Clin Pharmacol 2011;71:886-898 (doi
# 10.1111/j.1365-2125.2011.03936.x).  90 healthy men (18-55 years, BMI
# 18-28.5), "Scopolamine (0.5 mg) ... administered as a short-term i.v.
# infusion over 15-min", plasma at 0.5, 0.75, 1, 2.5 and 6.5 h, LC-MS/MS
# (LLOQ 10 pg/mL), NONMEM, two compartments, no covariates.  Table 2:
#
#     CL 2.53 L/min    V1 66.3 L    Q 4.78 L/min    Vss 250 L
#
# so V2 = 250 - 66.3 = 183.7 L (the paper prints no V2); half-lives 5.4 and
# 90 min, against the paper's "mean terminal half-life of 1.5 (range
# 1.2-2.49) h".  V1 is weakly identified, the first sample being 15 min after
# the infusion.
#
# THE DOSE BASIS
# ==============
# The paper says "0.5-mg scopolamine" and never names the salt.  Read as
# base, this disposition gives the patch's labelled plateau: 1 mg over 72 h
# at a clearance of 151.8 L/h is 91.5 pg/mL, against the label's "average
# plasma concentration produced is 87 pg/mL (0.28 nM) for free scopolamine"
# (read as the hydrobromide, 21-31% higher), which settles it (AGENTS.md: a
# basis a source leaves open is often decidable from a study that never
# entered the fit).  The patch is labelled in scopolamine base.
#
# THE PATCH
# =========
# The label: "The system is formulated to deliver approximately 1 mg of
# scopolamine to the systemic circulation over 3 days"; "circulating plasma
# concentrations are detected within 4 hours with peak concentrations being
# obtained, on average, within 24 hours"; "Following removal of the used
# transdermal system, there is some degree of continued systemic absorption
# of scopolamine bound in the skin layers ... plasma concentrations of
# scopolamine decline in a log linear fashion with an observed half-life of
# 9.5 hours", far longer than the 1.5-h systemic half-life, so the decline
# after removal is the skin depot emptying.  The model is therefore the
# patch's delivery as a constant rate into a skin depot (the tissue depot,
# ROUTE_TD, "mg/day TD": one patch is 1/3 mg/day, entered when it is applied
# and set to 0 when it is removed) that releases first-order with a 9.5-h
# half-life, bioavailability 1 (the label's 1 mg is what reaches the
# circulation).  It reaches 80% of its plateau at 24 h; the label's earlier
# peak would come from a priming dose in the patch's adhesive (older reviews,
# abstracts only), which is not modelled.  For postoperative nausea the label
# applies the patch the evening before surgery and removes it 24 hours after.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No weights were printed or tested, so the values are written for the
# library's reference man and scaled by fat-free mass with the switch on,
# fixed with it off.  The patch's delivery does not scale.
#
# NOT MODELLED
# ============
# The effect site: Liem-Moolenaar's equilibration half-lives run from 17 min
# (heart rate) to 199 min (alertness) and 649 min (finger tapping), one per
# endpoint, so the curve is plasma only.  Intravenous scopolamine is not
# offered (no intravenous product is marketed in the US).  Conjugates (the
# label's total, 354 pg/mL) are not plotted.
#
# THE BAND
# ========
# None: no therapeutic range is established.  87 pg/mL is what the patch
# delivers, not a target.
# -----------------------------------------------------------------------------

# Liem-Moolenaar 2011, Table 2
SCOPOLAMINE_CL  <- 2.53     # L/min
SCOPOLAMINE_V1  <- 66.3     # L
SCOPOLAMINE_Q   <- 4.78     # L/min
SCOPOLAMINE_VSS <- 250      # L
# Transderm Scop label: decline after removal
SCOPOLAMINE_SKIN_HALF_LIFE <- 9.5   # h

#' Scopolamine by transdermal patch (Liem-Moolenaar 2011, Transderm Scop)
#'
#' Two-compartment intravenous disposition fed by a skin depot; see the
#' header of \code{R/drugs_scopolamine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
scopolamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  default <- list(
    v1  = SCOPOLAMINE_V1 * size$volume,
    v2  = (SCOPOLAMINE_VSS - SCOPOLAMINE_V1) * size$volume,
    v3  = 1,
    cl1 = SCOPOLAMINE_CL * size$clearance,
    cl2 = SCOPOLAMINE_Q * size$clearance,
    cl3 = 0,
    ka_TD = log(2) / (SCOPOLAMINE_SKIN_HALF_LIFE * 60),
    bioavailability_TD = 1,
    tlag_TD = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Liem-Moolenaar M et al., Br J Clin Pharmacol 2011;71:886-898: two ",
    "compartments, CL 2.53 L/min, V1 66.3 L, Q 4.78 L/min, V2 183.7 L ",
    "(Vss 250 L), healthy men; Transderm Scop label (2025): 1 mg delivered ",
    "over 3 days, released from the skin with a 9.5-h half-life. ",
    "https://doi.org/10.1111/j.1365-2125.2011.03936.x"
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
