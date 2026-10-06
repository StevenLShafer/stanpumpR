# -----------------------------------------------------------------------------
# Tramadol: modelled as a prodrug, because only its metabolite's opioid
# activity is of interest here
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# WHY TRAMADOL CARRIES NO EFFECT SITE
# ===================================
# Tramadol is NOT pharmacologically inert.  It has weak mu-opioid activity and,
# more importantly, inhibits serotonin and noradrenaline reuptake, and human
# antagonist studies show both mechanisms contribute to its analgesia.
#
# This model deliberately represents only the mu-opioid activity of the
# metabolite, at Steven L. Shafer's instruction on 2026-10-06: the
# monoaminergic action of the parent is out of scope for a package whose
# opioid total is a mu-opioid quantity, and folding a reuptake-inhibition
# effect into an opioid MEAC would misrepresent both.
#
# So tramadol is given tPeak 0 and MEAC 0, the same treatment codeine gets,
# and the effect appears on the desmetramadol row.  The difference from
# codeine is worth stating plainly: codeine is a prodrug as a matter of
# pharmacology, whereas tramadol is a prodrug only as a matter of what this
# model chooses to represent.  A patient's response to tramadol is NOT fully
# described by the desmetramadol curve, and a poor metaboliser forming almost
# no metabolite still gets monoaminergic analgesia that does not appear here.
#
# DISPOSITION
# ===========
# Holford 2014, which fitted parent and metabolite jointly to intravenous
# tramadol in 57 healthy and 56 postoperative adults.  Parent: central volume
# 90 L, peripheral 79 L, intercompartmental clearance 105 L/h, residual
# elimination clearance 18.4 L/h and formation clearance to the metabolite
# 10.5 L/h at 70 kg, so total clearance is 28.9 L/h.  Clearances scale as
# (W/70)^0.75 and volumes as W/70.
#
# Holford rather than Brvar for the disposition, because Holford is
# intravenous, physical and jointly fitted with the metabolite this model
# exists to produce.  Brvar's oral vector is apparent, divided by an
# unmeasured bioavailability, and carries no metabolite at all.
#
# ABSORPTION
# ==========
# ka is set so the oral plasma peak falls at 0.99 h, which is where Brvar's
# own complete model puts it for immediate-release capsules.
#
# Note what was NOT done.  Brvar fitted an inverse-Gaussian input with a mean
# absorption time of 0.549 h, and transplanting that number directly would be
# wrong twice over: this package integrates first-order absorption rather than
# an inverse Gaussian, and Brvar's parameter was fitted against Brvar's own
# apparent volumes, which are roughly twice Holford's physical ones.  Dropped
# onto this disposition it would peak at 0.85 h instead of 0.99.  Anchoring on
# the predicted observable rather than the other model's parameter is what
# keeps the two sources from being silently merged.
#
# Bioavailability is 0.70, the regulatory figure for capsules.  A 100 mg
# immediate-release tablet is quoted at 0.75, and a 24-hour extended-release
# product at 85 to 90 per cent of its immediate-release comparator; those are
# different products and are not represented.  Only an immediate-release oral
# unit is offered, alongside intravenous.
#
# CYP2D6 AND METABOLITE FORMATION
# ===============================
# Formation to desmetramadol is CYP2D6-mediated and is the whole reason
# phenotype matters for tramadol.
#
# The relative weights come from Stamer 2007, which measured early (+)-M1
# exposure after intravenous tramadol in postoperative patients grouped by
# number of active CYP2D6 genes.  Normalising its medians to the two-gene
# group gives 0, 0.58, 1 and 2.25.
#
# The poor-metaboliser value is NOT taken as the literal zero median.  Stamer's
# own third quartile for that group is 0.17 of the normal median, Holford's
# slow latent class sits at 0.16 of its fast class, and a structural zero
# would assert that no CYP2D6-independent route exists.  It is set to 0.10,
# between the measured median and those two upper estimates, and this is the
# least well supported number in the file.
#
# The intermediate value has independent support: Lee 2019 fitted apparent
# formation clearance in *10/*10 subjects at 0.472 of its comparator, against
# Stamer's 0.580.  Two different designs within 20 per cent of each other.
#
# Formation is a branch of total clearance, so changing phenotype changes the
# parent's total clearance too, from 19.5 L/h in a poor metaboliser to 42.0 in
# an ultrarapid one.  That is a real and clinically familiar effect: poor
# metabolisers have higher tramadol concentrations and less metabolite.
#
# Caveats the sources themselves raise.  Stamer's numbers are a finite 0 to 3
# hour window, for the (+) enantiomer, not whole-exposure formation
# multipliers.  Holford's two classes are a latent mixture, not assigned
# genotypes, and Allegaert found only 32 per cent of low-activity subjects
# fell in the slow class.  These weights should be treated as the best
# available ordering, not a calibrated four-group vector.
#
# References
# ----------
# Holford S et al., J Pharmacol Clin Toxicol 2014;2(1):1023.
#   https://www.jscimedcentral.com/public/assets/articles/pharmacology-spidpharmacokinetics-1023.pdf
# Brvar N et al., Int J Pharm 2014;473:170-178.
#   https://doi.org/10.1016/j.ijpharm.2014.07.013
# Stamer UM et al., Clin Pharmacol Ther 2007;82:41-47.
#   https://doi.org/10.1038/sj.clpt.6100152
# Lee J et al., Drug Des Devel Ther 2019.
#   https://doi.org/10.2147/DDDT.S199574
# Allegaert K et al., Clin Pharmacokinet 2015;54:167-178.
#   https://doi.org/10.1007/s40262-014-0191-9
# Desmeules JA et al., Br J Clin Pharmacol 1996;41:7-12.
#   https://doi.org/10.1111/j.1365-2125.1996.tb00152.x
# Enggaard TP et al., Anesth Analg 2006;102:146-150.
#   https://doi.org/10.1213/01.ane.0000189613.61910.32
# -----------------------------------------------------------------------------


# Relative CYP2D6 formation activity, normal metaboliser = 1.  Stamer 2007
# early (+)-M1 AUC medians normalised to the two-active-gene group, with the
# poor value lifted off its literal zero; see the header.
TRAMADOL_CYP2D6_WEIGHT <- c(
  poor         = 0.10,
  intermediate = 0.580451,
  normal       = 1.0,
  ultrarapid   = 2.251128
)

#' Tramadol pharmacokinetics
#'
#' Modelled as a prodrug: only the mu-opioid activity of its metabolite
#' desmetramadol is represented, so tramadol itself carries no effect site.
#' See the file header, because that is a scope decision rather than a
#' statement about the drug.
#'
#' @param weight weight in kg
#' @param height height in cm (not used)
#' @param age age in years (not used)
#' @param sex sex as a string (not used)
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#'   Scales formation of desmetramadol, and with it the parent's own total
#'   clearance.
#'
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   desmetramadol as the formed active species
#' @export
tramadol <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  activity <- unname(TRAMADOL_CYP2D6_WEIGHT[[cyp2d6]])

  # Holford 2014 allometry
  perKg   <- weight / 70
  perKg75 <- perKg^0.75

  # Clearance splits into a residual pathway and the CYP2D6 branch, so the
  # total moves with phenotype.
  CL_RESIDUAL  <- 18.4   # L/h at 70 kg, everything except formation
  CL_FORMATION <- 10.5   # L/h at 70 kg, normal metaboliser
  clFormation  <- CL_FORMATION * activity * perKg75   # L/h
  clTotal      <- CL_RESIDUAL * perKg75 + clFormation # L/h

  v1  <- 90 * perKg
  v2  <- 79 * perKg
  v3  <- 1                          # unused third compartment
  cl1 <- clTotal / 60               # L/min
  cl2 <- 105 * perKg75 / 60         # L/min
  cl3 <- 0

  # --- Absorption: oral plasma peak at 0.99 h (see header) ---
  ka_PO              <- 0.0254568712   # 1/min, = 1.527 /h
  bioavailability_PO <- 0.70
  tlag_PO            <- 0

  # --- Formation to desmetramadol ---
  # kFormation is a first-order transfer out of the central compartment, so it
  # is the formation clearance over the central volume.  Clearance scales as
  # W^0.75 and volume as W, so this falls slowly with weight rather than being
  # constant.
  kFormation <- (clFormation / 60) / v1   # per minute

  tPeak <- 0   # see the header: the parent's own activity is out of scope
  MEAC  <- 0

  # Display band, ng/mL: tramadol runs a few hundred ng/mL after ordinary doses
  typical      <- 300
  upperTypical <- 600
  lowerTypical <- 100

  reference <- paste0(
    "Holford S et al., J Pharmacol Clin Toxicol 2014;2(1):1023 ",
    "(joint intravenous parent and metabolite disposition); ",
    "Brvar N et al., Int J Pharm 2014;473:170-178. ",
    "https://pubmed.ncbi.nlm.nih.gov/25014373/ (oral input timing); ",
    "Stamer UM et al., Clin Pharmacol Ther 2007;82:41-47. ",
    "https://pubmed.ncbi.nlm.nih.gov/17361124/ (CYP2D6)"
  )

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      metabolite = list(
        name              = "desmetramadol",
        kFormation        = kFormation,
        # First pass, added 2026-10-06.  Holford's model is intravenous and
        # has no presystemic term, so with formation alone the metabolite
        # peaked at 4 h.  Peak analgesia after oral tramadol is observed at
        # 2.5 h, and the effect site cannot precede the curve driving it, so
        # that was unreachable: the metabolite was appearing too late because
        # presystemic formation was missing.
        #
        # 0.10 is inferred rather than fitted, and three independent routes
        # agree on it:
        #   - Lee 2019 reports 20 to 30% of an oral dose undergoing first
        #     pass.  Holford puts CYP2D6 at 10.5 of 28.9 L/h, a 36% share of
        #     tramadol clearance.  If presystemic extraction splits in the
        #     same proportion, first-pass M1 is 0.073 to 0.109.
        #   - At 0.10 a 100 mg oral dose gives a metabolite peak of 42 ng/mL,
        #     which sits in the range oral tramadol studies report.
        #   - It brings the metabolite peak to 93 min, which makes the
        #     observed 2.5 h peak analgesia reachable with a 24 min
        #     equilibration half-time rather than an implausible one.
        #
        # It raises total metabolite exposure by about 1.4 fold against
        # formation alone, taking the dose fraction converted from 0.25 to
        # 0.35.  That is a consequence of the change, not an independent
        # check, and is the thing to revisit if measured oral M1 exposure
        # turns out lower.
        #
        # Scaled by the SAME phenotype activity as the systemic route, since
        # presystemic O-demethylation is the same CYP2D6 reaction.  Leaving
        # it constant was a bug in the first version of this change: a poor
        # metaboliser then received full first-pass metabolite and reached
        # 71% of a normal metaboliser's peak instead of about a tenth.
        firstPassFraction = 0.10 * activity,
        # O-desmethyltramadol 249.38, tramadol base 263.38 g/mol
        mwRatio           = 249.38 / 263.38
      )
    )
  )
}
