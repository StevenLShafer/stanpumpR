# -----------------------------------------------------------------------------
# Metoprolol, intravenous and oral (Lammers 2017): healthy men, by CYP2D6
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv), from
# the full text, which he supplied.
#
# THE SOURCE
# ==========
# Lammers LA, Achterbergh R, van Schaik RHN, Romijn JA, Mathot RAA.  Effect
# of short-term fasting on systemic cytochrome P450-mediated drug metabolism
# in healthy subjects: a randomized, controlled, crossover study using a
# cocktail approach.  Clin Pharmacokinet 2017;56:1231-1244
# (doi 10.1007/s40262-017-0515-7).  Twelve healthy white
# men (19-33 years, 68-103 kg, median 80) given a probe cocktail containing
# metoprolol 20 mg intravenously (1 mg/mL ampoules, AstraZeneca) and, on
# another occasion, a 100 mg tablet, each after an overnight fast (control)
# and after 36 hours of fasting; LC-MS/MS plasma, sampled for 9 hours after
# the intravenous dose and 10 after the oral.  NONMEM, two compartments with
# first-order absorption (Table 3):
#
#     CL 65.8 L/h     V1 187 L     Q 74.5 L/h     V2 97.6 L
#     ka 1.19 /h      F 0.45
#
# CYP2D6: one poor metaboliser (*4/*4) had 0.44 x the clearance and 2.28 x
# the bioavailability (so F 1.03, held at 1 here); one ultrarapid metaboliser
# (*1/*1xN) had 2.01 x the clearance and 0.43 x the bioavailability.  Both
# factors were estimated from those two men alone and then FIXED, so they
# are the right direction and roughly the right size, nothing more.  The
# typical subject is the rest of the group, five of whose nine genotyped men
# were intermediate metabolisers (*1/*3, *1/*4) and three normal, so the
# intermediate and normal phenotypes both take the typical values.
#
# The 36-hour fast raised clearance 13% and is not applied: these are the
# overnight-fasted control values.  The doses are the tartrate, as the
# products were labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed values with no size term, for the source's young men, scaled to
# fat-free mass with the switch on; with it off they are the published values
# for everyone (legacyVolume = 1).
#
# NO EFFECT SITE, NO BAND
# =======================
# The source measured no effect.  Plasma only.
# -----------------------------------------------------------------------------

# Lammers 2017, Table 3, control (overnight fast)
METOPROLOL_CL <- 65.8      # L/h
METOPROLOL_V1 <- 187       # L
METOPROLOL_Q  <- 74.5      # L/h
METOPROLOL_V2 <- 97.6      # L
METOPROLOL_KA <- 1.19      # /h
METOPROLOL_F  <- 0.45
# Fixed from one poor and one ultrarapid metaboliser
METOPROLOL_CYP2D6_CL <- c(poor = 0.44, intermediate = 1, normal = 1,
                          ultrarapid = 2.01)
METOPROLOL_CYP2D6_F  <- c(poor = 2.28, intermediate = 1, normal = 1,
                          ultrarapid = 0.43)

#' Metoprolol, intravenous and oral (Lammers 2017)
#'
#' Two compartments with first-order oral absorption in healthy men, with
#' the CYP2D6 poor and ultrarapid metaboliser factors; see the header of
#' \code{R/drugs_metoprolol.R}.
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metoprolol <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       cyp2d6 = CYP2D6_DEFAULT)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = METOPROLOL_V1 * size$volume,
    v2  = METOPROLOL_V2 * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = METOPROLOL_CL * METOPROLOL_CYP2D6_CL[[cyp2d6]] / 60 * size$clearance,
    cl2 = METOPROLOL_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = METOPROLOL_KA / 60,
    bioavailability_PO = min(1, METOPROLOL_F * METOPROLOL_CYP2D6_F[[cyp2d6]]),
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Lammers LA, Achterbergh R, van Schaik RHN, Romijn JA, Mathot RAA, Clin Pharmacokinet ",
    "2017;56:1231-1244.  Two compartments in 12 healthy men (20 mg IV, 100 mg ",
    "PO): CL 65.8 L/h, V1 187 L, Q 74.5 L/h, V2 97.6 L, ka 1.19 /h, F 0.45; ",
    "CYP2D6 poor metaboliser CL x 0.44 and F x 2.28, ultrarapid CL x 2.01 and ",
    "F x 0.43 (one subject each); CYP2D6 ", cyp2d6, ". ",
    "https://doi.org/10.1007/s40262-017-0515-7"
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
