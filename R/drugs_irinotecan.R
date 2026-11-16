# -----------------------------------------------------------------------------
# Irinotecan, intravenous (Klein 2002): plasma irinotecan, forming SN-38
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, who
# chose this source (2026-11-06; docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source P19).  Every number below
# was read from the PDF, which Shafer supplied (2026-11-06): Table I, Table
# II and Figure 3, on the page images.
#
# THE SOURCE
# ==========
# Klein CE, Gupta E, Reid JM, Atherton PJ, Sloan JA, Pitot HC, Ratain MJ,
# Kastrissios H.  Population pharmacokinetic model for irinotecan and two of
# its metabolites, SN-38 and SN-38 glucuronide.  Clin Pharmacol Ther
# 2002;72:638-647 (doi 10.1067/mcp.2002.129502).  78 patients from two
# dose-escalation studies (Chicago 100-175 mg/m2, Mayo 240-340 mg/m2), 90-min
# infusions, plasma to 25-49.5 h, HPLC (LLOQ at least 5 ng/mL); NONMEM, FOCE.
#
# THE MODEL (Table I, the structural model; Figure 3)
# ===================================================
# A linear cascade: irinotecan in three compartments, all of its elimination
# forming SN-38 (two compartments) scaled by the unknown fraction Fmsn, which
# forms SN-38G (one compartment):
#
#     irinotecan  Vc 5.52 L;  k10 4.56, k12 57.8, k21 2.58, k13 1.96, k31 0.0812 /h
#     SN-38       Vc/Fmsn 466 L (see below);  k10 2.52, k12 2.6, k21 0.0584 /h
#     SN-38G      Vc/Fmsng 166 L;  k10 1.37 /h                (not carried)
#
# Checks: irinotecan CL 25.2 L/h, Vss 262.4 L and half-lives 0.6 min, 2.5 h
# and 13.5 h (the paper's 25.2, 262.5, 0.01, 2.5 and 14 h); SN-38's CL/Fmsn
# 1174 L/h and half-lives 0.135 and 24.3 h (the paper's 1174, 0.135, 24.3).
#
# SN-38's volume: the fit is a two-group mixture, Vc/Fmsn 200 L (Table I: 58%)
# or 466 L (42%).  The 466-L group is used: Table I's proportions appear
# swapped (Figure 2 puts "more than one half" of the patients at the low
# SN-38 peak per dose, about 0.09 ng/mL per mg, which is the 466-L group's
# 0.070 and not the 200-L group's 0.162; the validation set's split is 31%
# against 69% the same way), and the 466-L group is the one whose Fmsn the
# authors anchored (fixed at 12%, from Slatter 2000), giving an absolute
# central volume of about 56 L.  A patient in the other group would have
# about 2.3 times the SN-38 shown.
#
# In the library's metabolite interface kFormation is irinotecan's own k10
# (all of the elimination, Fmsn-scaled, as for aripiprazole), with mwRatio 1:
# the paper prints no molar conversion, and a constant factor would be
# absorbed into the apparent Vc/Fmsn it fitted.  SN-38 is the metabolite-only
# drug irinotecanSN38 (R/drugs_irinotecanSN38.R), fitted jointly
# (METABOLITE_FORMATION_BASIS "joint").  SN-38G, a third generation, is not
# carried: the closed-form metabolite route forms one metabolite.
#
# WHAT IS NOT REPRESENTED
# =======================
# No covariate: the paper gives its covariate effects (performance status
# and age on irinotecan clearance, sex and bilirubin on SN-38, weight on
# volumes) only as percentages whose references contradict Table I, so they
# cannot be rebuilt; UGT1A1*28, which raises SN-38 exposure, was not studied;
# no lactone-carboxylate split (the assay most likely measured both).  The
# model is fixed in absolute litres for the cohort; volumes and clearances
# scale by the library's factors with the switch on.
#
# DOSE BASIS
# ==========
# Not stated.  Doses are entered as labelled (Camptosar is labelled as
# irinotecan hydrochloride trihydrate, base 0.866 of it), with no conversion.
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Klein 2002, Table I (structural model); rate constants in /h
IRINOTECAN_KLEIN <- list(
  v1 = 5.52, k10 = 4.56, k12 = 57.8, k21 = 2.58, k13 = 1.96, k31 = 0.0812
)
#' Irinotecan, intravenous (Klein 2002)
#'
#' Three compartments, forming SN-38; see the header of
#' \code{R/drugs_irinotecan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
irinotecan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- IRINOTECAN_KLEIN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- p$v1 * size$volume
  cl1 <- p$v1 * p$k10 / 60 * size$clearance

  default <- list(
    v1  = v1,
    v2  = p$v1 * p$k12 / p$k21 * size$volume,
    v3  = p$v1 * p$k13 / p$k31 * size$volume,
    cl1 = cl1,
    cl2 = p$v1 * p$k12 / 60 * size$clearance,
    cl3 = p$v1 * p$k13 / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Klein CE, Gupta E, Reid JM, et al. Population pharmacokinetic model for ",
    "irinotecan and two of its metabolites, SN-38 and SN-38 glucuronide. Clin ",
    "Pharmacol Ther 2002;72:638-647. 78 patients: irinotecan in three compartments ",
    "(Vc 5.52 L, k10 4.56, k12 57.8, k21 2.58, k13 1.96, k31 0.0812 /h), all of its ",
    "elimination forming SN-38 (Vc/Fmsn 466 L, k10 2.52, k12 2.6, k21 0.0584 /h); ",
    "no covariates; SN-38 glucuronide not carried. Doses as labelled. ",
    "https://doi.org/10.1067/mcp.2002.129502"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "irinotecanSN38",
      kFormation        = cl1 / v1,          # all elimination, Fmsn-scaled
      firstPassFraction = 0,
      mwRatio           = 1                  # as fitted (see the header)
    )
  )
}
