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
# Allegaert 2015 (Clin Pharmacokinet 54:167-178, read in full), a pooled
# population analysis of intravenous tramadol and its metabolite in 295
# people from 25 weeks postmenstrual age to 84.8 years (0.5 to 186 kg), with
# two dog studies that supply the metabolite's central volume.  Table 3,
# original estimates at 70 kg: parent central volume 51.0 L, peripheral
# 134 L, intercompartmental clearance 227 L/h, clearance by other pathways
# 21.6 L/h and formation clearance to the metabolite 10.5 L/h (faster
# metabolisers), so total clearance is 32.1 L/h.  Clearances scale as
# (W/70)^0.75 and volumes as W/70 (Eq. 1).
#
# MATURATION (Allegaert 2015 Table 8, Eq. 2)
# ==========================================
# Each clearance pathway is multiplied by
#
#     Fmat = PMA^Hill / (TM50^Hill + PMA^Hill)
#
# with PMA the postmenstrual age in weeks: other pathways TM50 39.0 weeks,
# Hill 6.76; formation of M1 (CYP2D6) TM50 39.9, Hill 9.00; and the
# metabolite's elimination TM50 47.7, Hill 3.4, fixed to glomerular
# filtration (desmetramadol's file).  The original estimates are used, as
# for Table 3; the paper's text quotes the bootstrap averages (39.1, 6.81;
# 39.8, 9.39).  The intercompartmental clearances do not mature.  PMA is
# 40 + 52 x age in years, a term birth, as for milrinonePediatric and
# propofol, so a premature infant's gestational age cannot be entered.  At
# term the factors are 0.54 (other pathways), 0.51 (formation) and 0.36
# (metabolite elimination); at one year 0.997, 0.999 and 0.90; by two years
# the metabolite's is 0.98 and the parent's are 1.
#
# WHY ALLEGAERT AND NOT HOLFORD 2014
# ==================================
# Until 2026-10-27 the disposition was Holford 2014 (J Pharmacol Clin
# Toxicol 2:1023), the same group's earlier fit to the 113 adults (57
# healthy, 56 postoperative) alongside six other species: central volume 90
# L, peripheral 79 L, intercompartmental clearance 105 L/h, clearances 18.4
# and 10.5 L/h, and for M1 84.2 L/h, 274 L/h and 131 L.  Allegaert began from
# those adult estimates and re-estimated them with the children added, under
# the maturation model.  In an adult the two agree closely after the first
# quarter hour of an intravenous dose: the same 4.3 h terminal half-life,
# M1 exposure within 6%, and Vss 185 against 169 L.  The difference is the
# first ten minutes, where the smaller central volume (51 against 90 L)
# gives a higher early peak, and the first year of life, which Holford did
# not model: scaled by size alone, a term neonate's clearances were about
# twice Allegaert's.  Switched by decision of Steven L. Shafer, 2026-10-27,
# for the maturation, and adopted whole rather than grafting Allegaert's
# maturation onto Holford's adult values, which would mix two fits.
#
# One finding of Allegaert's is not carried: in one paediatric study (study
# code 2) the clearance by other pathways was 0.337 and the metabolite's
# elimination 0.527 of the others', unexplained by the authors.  The
# typical values above are for every other study.
#
# DOSE BASIS: HYDROCHLORIDE AS LABELLED, BASE CONCENTRATIONS
# ===========================================================
# Tramadol is prescribed and labelled as the hydrochloride (50 mg tramadol
# HCl = 43.9 mg tramadol).  Allegaert 2015's Methods say: "Reported tramadol
# hydrochloride doses were converted to base tramadol, where 1 mg tramadol
# hydrochloride is equal to 0.8784 mg of tramadol", with M1 converted to
# tramadol milligram equivalents by the molar ratio 249.38/263.38 (Holford
# 2014 says the same).  Every parameter is therefore on the base scale.
# Until 2026-10-27 the library fed a labelled hydrochloride dose to
# base-scale parameters, which overstated every tramadol and desmetramadol
# concentration by 1/0.8784 = 1.138 (opioid potency audit of 2026-10-27,
# docs/opioid-review-2026-10-27.md).
#
# A dose is entered as the HYDROCHLORIDE, as dispensed, and the plotted
# concentrations are tramadol base.  As for methadone, the volumes and
# clearances are divided by TRAMADOL_BASE_FRACTION (263.38 / 299.84), so the
# central amount is in hydrochloride milligrams.  Formation is a ratio of a
# clearance to a volume, which the division leaves alone; the mass that
# formation hands to desmetramadol is converted by mwRatio 249.38 / 299.84,
# M1 per milligram of the hydrochloride, which is the base molar ratio
# 249.38 / 263.38 times 0.8784.  Desmetramadol's own parameters are
# unchanged.  Oral bioavailability is a ratio of exposures to the same salt,
# so 0.70 is unchanged.
#
# Allegaert rather than Brvar for the disposition, because Allegaert is
# intravenous, physical and jointly fitted with the metabolite this model
# exists to produce.  Brvar's oral vector is apparent, divided by an
# unmeasured bioavailability, and carries no metabolite at all.
#
# ABSORPTION
# ==========
# ka is set so the oral plasma peak falls at 0.99 h, which is where Brvar's
# own complete model puts it for immediate-release capsules: 1.232 /h on
# Allegaert's disposition at 70 kg (1.527 /h on Holford's, until 2026-10-27).
#
# Note what was NOT done.  Brvar fitted an inverse-Gaussian input with a mean
# absorption time of 0.549 h, and transplanting that number directly would be
# wrong twice over: this package integrates first-order absorption rather than
# an inverse Gaussian, and Brvar's parameter was fitted against Brvar's own
# apparent volumes, which are roughly twice the physical ones.  Dropped onto
# Holford's disposition it peaked at 0.85 h instead of 0.99.  Anchoring on
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
# slow latent class sits at 0.16 of its fast class (Allegaert's at 0.17,
# Table 3, 1.83 against 10.5 L/h), and a structural zero
# would assert that no CYP2D6-independent route exists.  It is set to 0.10,
# between the measured median and those two upper estimates, and this is the
# least well supported number in the file.
#
# The intermediate value has independent support: Lee 2019 fitted apparent
# formation clearance in *10/*10 subjects at 0.472 of its comparator, against
# Stamer's 0.580.  Two different designs within 20 per cent of each other.
#
# Formation is a branch of total clearance, so changing phenotype changes the
# parent's total clearance too, from 22.7 L/h in a poor metaboliser to 45.2 in
# an ultrarapid one (adult, 70 kg).  That is a real and clinically familiar effect: poor
# metabolisers have higher tramadol concentrations and less metabolite.
#
# Caveats the sources themselves raise.  Stamer's numbers are a finite 0 to 3
# hour window, for the (+) enantiomer, not whole-exposure formation
# multipliers.  Allegaert's two classes are a latent mixture, not assigned
# genotypes, and only 32 per cent of its low-activity subjects fell in the
# slow class.  Its exploratory genotype analysis put the low-activity group's
# formation clearance at 0.236 of normal (95% CI 0.130 to 0.461), between
# this file's poor and intermediate weights.  These weights should be treated as the best
# available ordering, not a calibrated four-group vector.
#
# References
# ----------
# Allegaert K et al., Clin Pharmacokinet 2015;54:167-178.
#   https://doi.org/10.1007/s40262-014-0191-9
# Holford S et al., J Pharmacol Clin Toxicol 2014;2(1):1023.
#   https://www.jscimedcentral.com/public/assets/articles/pharmacology-spidpharmacokinetics-1023.pdf
# Brvar N et al., Int J Pharm 2014;473:170-178.
#   https://doi.org/10.1016/j.ijpharm.2014.07.013
# Stamer UM et al., Clin Pharmacol Ther 2007;82:41-47.
#   https://doi.org/10.1038/sj.clpt.6100152
# Lee J et al., Drug Des Devel Ther 2019;13:1751-1761.
#   https://doi.org/10.2147/DDDT.S199574
# Desmeules JA et al., Br J Clin Pharmacol 1996;41:7-12.
#   https://doi.org/10.1111/j.1365-2125.1996.tb00152.x
# Enggaard TP et al., Anesth Analg 2006;102:146-150.
#   https://doi.org/10.1213/01.ane.0000189613.61910.32
# -----------------------------------------------------------------------------


# Tramadol base per milligram of tramadol hydrochloride: 263.38 / 299.84 =
# 0.8784, Allegaert 2015's (and Holford 2014's) factor.  See DOSE BASIS in
# the header.
TRAMADOL_BASE_FRACTION <- 263.38 / 299.84

# Allegaert 2015 Table 3, original estimates at 70 kg (L/h, L).
TRAMADOL_CL_OTHER     <- 21.6   # clearance by other pathways (CLPO)
TRAMADOL_CL_FORMATION <- 10.5   # clearance to M1, faster metabolisers (CLPM)
TRAMADOL_Q            <- 227    # intercompartmental clearance (CLICP)
TRAMADOL_V1           <- 51.0   # central volume (VP1)
TRAMADOL_V2           <- 134    # peripheral volume (VP2)

# Allegaert 2015 Table 8, original estimates: maturation of each clearance
# pathway with postmenstrual age, TM50 in weeks.  The metabolite's
# elimination (CLMO) is fixed to glomerular filtration's.
TRAMADOL_MATURATION <- list(
  other     = c(tm50 = 39.0, hill = 6.76),   # CLPO
  formation = c(tm50 = 39.9, hill = 9.00),   # CLPM
  m1        = c(tm50 = 47.7, hill = 3.4)     # CLMO (desmetramadol)
)

#' Maturation of tramadol's clearance pathways (Allegaert 2015)
#'
#' The sigmoid maturation fraction of Allegaert 2015's Eq. 2 for each
#' clearance pathway, at the postmenstrual age of a term birth
#' (40 + 52 x age in years).
#'
#' @param age age in years
#' @returns a named numeric vector: \code{other}, \code{formation}, \code{m1}
#' @keywords internal
tramadolMaturation <- function(age)
{
  pma <- 40 + 52 * age
  vapply(TRAMADOL_MATURATION, function(m)
    unname(pma^m[["hill"]] / (m[["tm50"]]^m[["hill"]] + pma^m[["hill"]])),
    numeric(1))
}

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
#' @param height height in cm (fat-free mass only)
#' @param age age in years: the maturation of each clearance pathway, and
#'   fat-free mass
#' @param sex sex as a string (fat-free mass only)
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#'   Scales formation of desmetramadol, and with it the parent's own total
#'   clearance.
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#'
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   desmetramadol as the formed active species
#' @export
tramadol <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                     adjustToFFM = TRUE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  activity <- unname(TRAMADOL_CYP2D6_WEIGHT[[cyp2d6]])

  # Allegaert 2015 allometry: clearances (W/70)^0.75, volumes W/70.  With the
  # fat-free-mass switch on (docs/weight-adjustment.md) the same exponents
  # apply to the fat-free-mass ratio instead.  Tramadol and desmetramadol
  # must make this call identically: the formation constant is a clearance
  # over a volume, and the pair was fitted jointly.
  size    <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                           legacyClearance = (weight / 70)^0.75)
  perKg   <- size$volume
  perKg75 <- size$clearance

  # Clearance splits into the other pathways and the CYP2D6 branch, so the
  # total moves with phenotype; each matures with postmenstrual age.
  mat          <- tramadolMaturation(age)
  clFormation  <- TRAMADOL_CL_FORMATION * activity * perKg75 *
    mat[["formation"]]                                         # L/h
  clTotal      <- TRAMADOL_CL_OTHER * perKg75 * mat[["other"]] +
    clFormation                                                # L/h

  # Hydrochloride doses, base concentrations (see DOSE BASIS in the header).
  f   <- TRAMADOL_BASE_FRACTION
  v1  <- TRAMADOL_V1 * perKg / f
  v2  <- TRAMADOL_V2 * perKg / f
  v3  <- 1                                    # unused third compartment
  cl1 <- clTotal / 60 / f                     # L/min
  cl2 <- TRAMADOL_Q * perKg75 / 60 / f        # L/min
  cl3 <- 0

  # --- Absorption: oral plasma peak at 0.99 h (see header) ---
  ka_PO              <- 0.0205398742   # 1/min, = 1.232 /h
  bioavailability_PO <- 0.70
  tlag_PO            <- 0

  # --- Formation to desmetramadol ---
  # kFormation is a first-order transfer out of the central compartment, so it
  # is the formation clearance over the central volume.  Clearance scales as
  # W^0.75 and volume as W, so this falls slowly with weight rather than being
  # constant.
  kFormation <- (clFormation / 60 / f) / v1   # per minute

  tPeak <- 0   # see the header: the parent's own activity is out of scope
  MEAC  <- 0

  # Display band, ng/mL: tramadol runs a few hundred ng/mL after ordinary doses
  typical      <- 300
  upperTypical <- 600
  lowerTypical <- 100

  reference <- paste0(
    "Allegaert K et al., Clin Pharmacokinet 2015;54:167-178 ",
    "(joint intravenous parent and metabolite disposition with maturation, ",
    "Tables 3 and 8, on tramadol base; doses are the hydrochloride, x 0.8784, ",
    "as Allegaert converted them). https://pubmed.ncbi.nlm.nih.gov/25258277/; ",
    "Brvar N et al., Int J Pharm 2014;473:170-178. ",
    "https://pubmed.ncbi.nlm.nih.gov/25014367/ (oral input timing); ",
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
        # First pass, added 2026-10-06.  The disposition model is
        # intravenous and has no presystemic term, so with formation alone
        # the metabolite peaks at 4.6 h (4 h on Holford's, before
        # 2026-10-27).  Peak analgesia after oral tramadol is observed at
        # 2.5 h, and the effect site cannot precede the curve driving it, so
        # that was unreachable: the metabolite was appearing too late because
        # presystemic formation was missing.
        #
        # 0.10 is inferred rather than fitted, and three independent routes
        # agree on it:
        #   - Lee 2019 reports 20 to 30% of an oral dose undergoing first
        #     pass.  Allegaert puts CYP2D6 at 10.5 of 32.1 L/h, a 33% share
        #     of tramadol clearance (Holford 36%).  If presystemic extraction
        #     splits in the same proportion, first-pass M1 is 0.065 to
        #     0.098.
        #   - At 0.10, 100 mg of the hydrochloride by mouth gives a
        #     metabolite peak of 37 ng/mL, which sits in the range oral
        #     tramadol studies report.
        #   - It brings the metabolite peak to 134 min (93 on Holford's
        #     disposition), which makes the observed 2.5 h peak analgesia
        #     reachable, with a 9.8 min equilibration half-time.
        #
        # It raises total metabolite exposure by about 1.4 fold against
        # formation alone, taking the dose fraction converted from 0.23 to
        # 0.33 (0.25 to 0.35 on Holford's).  That is a consequence of the change, not an independent
        # check, and is the thing to revisit if measured oral M1 exposure
        # turns out lower.
        #
        # Scaled by the SAME phenotype activity and maturation as the
        # systemic route, since presystemic O-demethylation is the same
        # CYP2D6 reaction.  Leaving
        # it constant was a bug in the first version of this change: a poor
        # metaboliser then received full first-pass metabolite and reached
        # 71% of a normal metaboliser's peak instead of about a tenth.
        # Matured with CYP2D6, as the systemic formation is (2026-10-27).
        firstPassFraction = 0.10 * activity * mat[["formation"]],
        # O-desmethyltramadol 249.38, tramadol hydrochloride 299.84 g/mol:
        # the central amount is in hydrochloride milligrams (see the header)
        mwRatio           = 249.38 / 299.84
      )
    )
  )
}
