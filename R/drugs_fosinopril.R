# -----------------------------------------------------------------------------
# Fosinopril (Monopril) -- the curve is FOSINOPRILAT, the active diacid
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-29, at the request of Steven L. Shafer, who
# settled the design that day.  Sources: Hui KK, Duchin KL, Kripalani KJ,
# Chan D, Kramer PK, Yanagawa N, Clin Pharmacol Ther 1991;49:457-467 (PMID
# 1826651, read in full) for the disposition, and Murdoch D, McTavish D,
# Drugs 1992;43:123-140 section 2.1 for the absorption, which quotes Singhvi
# et al. 1988 in healthy volunteers.  Tests:
# tests/testthat/test-drugs-fosinopril.R.
#
# ONE DRUG, NOT TWO, AND WHY IT IS NOT THE ENALAPRIL PATTERN
# ==========================================================
# Fosinopril is an ester prodrug of fosinoprilat, the active moiety, and only
# the prodrug is marketed.  Shafer's design (2026-10-29): "The model should
# assume that it instantly converts to fosinoprilat in the stomach, and is
# then absorbed. There is no reason that impaired patients would have impaired
# absorption, and since the ester hydrolysis occurs in the gut prior to
# absorption, that would not be affected. I think the correct choice is to
# convert oral fosinopril to the molar equivalent of fosinoprilat, and then use
# the Hui model."  So there is ONE drug here, dosed orally as prescribed, and
# deliberately NO separate `fosinoprilat` entry: unlike enalaprilat, which the
# library carries because Faisal 2019 fitted parent and metabolite together,
# fosinoprilat "does not exist as a drug, other than for research (e.g., Hui)".
# The parent is not plotted at all, as benazepril's is not.
#
# The label supports the gut-wall conversion: fosinopril "is absorbed slowly",
# "the primary site of absorption is the proximal small intestine
# (duodenum/jejunum)", and it is "rapidly converted to its active diacid,
# fosinoprilat, by esterases in the gastrointestinal mucosa and liver"
# (Murdoch section 2).  Less than 1% of plasma radioactivity after an oral dose
# is intact prodrug (Hui, PDF p6), so treating the conversion as instantaneous
# and complete before absorption costs nothing the data could detect.
#
# THE DOSE BASIS: 0.75 MG FOSINOPRILAT PER MG FOSINOPRIL SODIUM, READ
# ===================================================================
# Hui's Methods state the intravenous dose as "7.5 mg 14C-fosinoprilat
# (equimolar to 10 mg fosinopril sodium)" (PDF p2).  That ratio, 0.75, is taken
# from the source rather than computed from molecular weights, and deliberately:
# the disposition below was fitted to that 7.5 mg dose, and Hui's oral
# bioavailability percentages are expressed against the 10 mg fosinopril dose,
# so 0.75 keeps the whole chain internally consistent.  The archived US label
# gives fosinopril sodium's molecular weight as 585.65; exact molar arithmetic
# would differ from 0.75 by under 1%, and the label prints no molecular weight
# for the diacid, so there is nothing to check it against here.
#
# The factor is folded into `bioavailability_PO` together with the
# bioavailability itself.  A dose is entered as prescribed, in milligrams of
# fosinopril sodium; neither factor must be applied again anywhere.
#
# THE DISPOSITION: SIX PRINTED NUMBERS, SOLVED LINEARLY
# =====================================================
# The registry had fosinopril as NOT PURSUED partly on an identifiability
# failure.  That reasoning was about Gehr 1993 (clearance, steady-state volume,
# mean residence time and a terminal half-life, where MRT is Vss/CL, so three
# independent numbers and not the four two compartments need) and it was never
# applied to Hui, which is a different kind of paper.  Hui Table IV prints SIX
# numbers per group for a three-compartment fit of intravenous fosinoprilat --
# V_init, CL, Vss and the three half-lives -- which is exactly the six a
# three-compartment model has, and the solve is LINEAR.
#
# Hui's Methods define the two quantities that make it linear: "Area under the
# curve was calculated as A/alpha + B/beta + C/gamma, and the initial volume of
# distribution (V_init) as Dose/(A + B + C)."  With the eigenvalues fixed by the
# half-lives, the three intercepts solve
#
#     A + B + C           = Dose / V_init
#     sum(A_i / lambda_i) = Dose / CL                 ( = AUC )
#     sum(A_i / lambda_i^2) = Vss * Dose / CL^2       ( = MRT x AUC, MRT = Vss/CL )
#
# At the normal-renal-function group (V_init 3.1 L, CL 26.5 mL/min, Vss 10.2 L,
# half-lives 0.2, 2.1, 12.4 h) and the 7500 mcg dose, that gives
#
#     A, B, C = 1299.8, 1055.6, 63.9 ng/mL
#
# all three positive, and through `threeCompartmentFromTriexponential()` (the
# same helper propranolol uses)
#
#     V1 3.10, V2 2.43, V3 4.67 L;  CL 0.0265, Q2 0.0717, Q3 0.00547 L/min
#
# every micro rate constant positive.  CL and Vss come back exactly, being
# inputs.  The third phase is a thin, long tail: C is only 2.6% of the intercept
# sum, yet because it decays so slowly it still carries 24% of the area -- much
# as benazeprilat's 22.3 h phase carries 17% at 1 to 3 per cent of peak
# concentrations.  That is the converting-enzyme binding which gives every ACE
# inhibitor a long flat tail, and a third compartment is its linearisation at low
# concentration, the same reduction benazepril's and lisinopril's headers
# describe.  It is also why fosinopril is given once daily on a 12.4 h
# half-life.
#
# CONDITIONING: NOTHING LIKE ESMOLOL
# ==================================
# Section 3.3 of docs/antihypertensives.md rejected esmolol's decomposition for
# conditioning rather than identifiability, so this one was stress-tested the
# same way: the solve was repeated at all 3^6 = 729 corners of Hui's printed
# +/- SEM box, including t1/2,alpha at 0.1 and 0.3 h where its SEM is half the
# estimate.  Every one of the 729 gives three positive intercepts and a
# physically admissible mammillary model; none is refused.  Across that box
#
#     V1 2.70-3.50 L (1.30x)   CL 23.5-29.5 mL/min (1.26x)
#     V2 1.04-4.18 L (4.03x)   Q2 29.0-193.6 mL/min (6.66x)
#     V3 2.78-6.57 L (2.36x)   Q3 2.74-10.45 mL/min (3.81x)
#
# V1 and CL are inputs and move only as much as their own SEMs; the DERIVED
# distribution parameters are soft, Q2 worst because it is driven almost
# entirely by t1/2,alpha, the one number whose SEM is 50% of its estimate.
# V2 + V3 is pinned, being Vss - V1.  So the AREA and the shape of the first
# minutes are firm and the split of the middle phase is not -- and for an oral
# drug absorbed over hours, almost nothing of the plotted curve depends on it.
#
# WHAT THE SOURCE IS, AND IS NOT
# ==============================
# Four to six patients per group, aged 54 to 68, mostly hypertensive, weighing
# 77 to 81 kg; dispersion printed as SEM, not SD; radiolabel assay (TLC
# radiochromatography, sensitivity 2 ng/mL); one patient withdrawn and four
# excluded from some calculations, one of them for a poor fit.  Table IV's
# parameters are MEANS OF INDIVIDUAL FITS, so they need not form one consistent
# model, and they do not quite: the area the derived vector implies is
# 4717 ng.h/mL against Table IV's own model AUC of 4971, about 5% apart.
# Shafer's two-stage rule (docs/) says to refit the individual data; it is not
# published, so the inconsistency is stated rather than removed.
#
# Protein binding is 97.4 to 98.1% (Hui), 95% (Singhvi) or "approximately
# 99.4%" (the label), and the plotted curve is TOTAL fosinoprilat.
#
# THE RENAL COVARIATE: A PLATEAU, AND A CONFLICT WITH THE LABEL
# =============================================================
# Hui's four groups, by creatinine clearance in mL/min/1.73 m^2 (Table I) and
# model-dependent total clearance in mL/min (Table IV):
#
#     98.8 -> 26.5      59.6 -> 14.6      31.8 -> 13.7      16.3 -> 18.2
#
# Clearance falls 42% from normal to impaired -- Hui's ANOVA calls that
# significant at p < 0.001 -- and is then FLAT within impairment: "in patients
# with renal impairment, there was no relationship between baseline creatinine
# clearance (11 to 72 ml/min/1.73 m2) and body clearance" (r = 0.23, p > 0.05,
# Fig. 3), and the severe group's 18.2 is the HIGHEST of the three.  The reason
# is in Fig. 4: hepatic clearance is 63%, 79% and 85% of body clearance in the
# mild, moderate and severe groups, so as filtration falls the hepatobiliary
# route carries progressively more of a smaller total.  So the covariate must
# plateau, and clearance must NOT be scaled in proportion to creatinine
# clearance (Shafer, 2026-10-29).
#
# The implemented covariate is a two-plateau ramp on Cockcroft-Gault indexed to
# 1.73 m^2, the unit Hui reports:
#
#     CL = 26.5 mL/min        at CrCl >= 80
#     CL = 15.5 mL/min        at CrCl <= 59.6
#     linear in between
#
# Every number in it is read.  26.5 is Hui's normal group.  15.5 is the
# unweighted mean of his three impaired group means (14.6, 13.7, 18.2),
# unweighted because Table IV prints no per-cell n, only "n = 4 to 6 per
# group"; the mean is flat across his whole impaired range, which is what the
# data say.  59.6 is his mild group's own creatinine clearance, the top of the
# range that mean belongs to.  80 is the US LABEL's boundary for normal renal
# function: it defines "mild to severe renal insufficiency" as creatinine
# clearance 10 to 80 mL/min/1.73 m^2.  The 59.6-to-80 interval that the ramp
# crosses is the one interval neither source measures.
#
# ONLY THE CLEARANCE CARRIES THE COVARIATE, and that is worth dwelling on,
# because it buys a check.  Hui's V_init (3.1, 3.3, 3.4, 4.2 L) and Vss (10.2,
# 8.8, 8.9, 13.0 L) show no trend worth modelling, so the volumes and both
# intercompartmental clearances are held at the normal group's.  The model is
# therefore never told the impaired half-lives -- and at the impaired plateau it
# PREDICTS a terminal half-life of 14.7 h against Hui's measured 14.0, 14.1 and
# 16.9 h (mean 15.0), and a second-phase half-life of 2.9 h against his 3.1,
# 3.9, 3.1.  A clearance-only covariate on a fixed structure reproduces the
# half-lives that structure was not fitted to.
#
# How much that is worth differs between the two, and the paper's own statistics
# are what separate them.  Hui found the DISTRIBUTION and ELIMINATION (beta)
# half-lives significantly lower in normal renal function, so the 2.9 h against
# 3.37 h is the model reproducing a difference the paper established -- within
# 15%, on the phase that carries 68% of the area.  He did NOT report the TERMINAL
# half-life as significantly different (Table IV marks t1/2 gamma with no
# significance symbol, and the text's list of what differed omits it), so the
# 14.7 h against 15.0 is a match to a trend rather than to a demonstrated
# effect.  Both are checks the model was not given; only one of them is a check
# against something the source itself calls real.
#
# THE CONFLICT.  The archived label says the opposite in words: "In patients
# with mild to severe renal insufficiency (creatinine clearance 10 to 80
# mL/min/1.73 m2), the clearance of fosinoprilat does not differ appreciably
# from normal, because of the large contribution of hepatobiliary elimination",
# and only below CrCl 10 is total clearance "approximately one-half of that in
# patients with normal renal function".  Hui is the study that sentence rests
# on, so the two can be reconciled only by reading it as flatness ACROSS 10-80
# rather than equality WITH normal -- which the label's own arithmetic supports,
# since it puts healthy body clearance at "between 26 and 39 mL/min" and half of
# that is about Hui's 15.5.  On that reading the label and Hui agree and the
# ramp is right.  SETTLED BY STEVEN L. SHAFER, 2026-10-29: keep Hui's halving,
# which is what this model does.  Two reasons, and the second is general.
#
# Where a label's PROSE and the study it rests on disagree, the study's NUMBERS
# are what a simulation can be built on and the prose is what the user must be
# warned about.  A label sentence is a regulatory summary written to guide
# dosing: "does not differ appreciably from normal" is a clinical judgment,
# where Hui's 42% at p < 0.001 is a measurement.
#
# And a label is frozen at approval (AGENTS.md): it is published on the best
# science of its day, after which the science advances and the label does not,
# because asking to change one invites fresh regulatory scrutiny of settled
# matters.  So a finding published after a label outranks it.  This drug shows
# the sharp edge of that: the archived label reads "Revised: 10/2021", yet its
# renal-impairment paragraph is the 1991-era summary of this very paper.  A
# REVISION DATE IS NOT A REVIEW DATE, and the date on the front of a label says
# nothing about which paragraphs were revisited.
#
# The conflict itself stays on the record, because the reconciling reading is an
# inference neither document states: a user comparing this curve with the label
# should know that the label would have them expect no change down to a
# creatinine clearance of 10, and that this model follows the measurements
# instead.
#
# A blank Serum creatinine field means the assumed normal value for the
# patient's age and sex (0.9 mg/dL for a man, 0.8 for a woman from 18 years;
# `assumedCreatinine()`), NOT a normal clearance: Cockcroft-Gault's age term
# then puts a 70-year-old at an indexed clearance near 72, on the ramp, and so
# about 16% below the normal-function clearance.  Whether that is right is
# precisely what Hui cannot resolve and the label denies -- and the label
# separately reports no pharmacokinetic difference in men aged 65 to 74.
#
# ABSORPTION: FROM A SECOND STUDY, BECAUSE HUI'S NORMAL GROUP GOT NO ORAL DOSE
# ============================================================================
# Hui gave the oral dose only to his impaired patients, so the absorption of a
# patient with normal renal function has to come from elsewhere.  Murdoch's
# section 2.1 quotes Singhvi 1988, single oral fosinopril 10 mg in healthy
# volunteers: about one third of the drug absorbed (32 to 36%), Cmax 99 to
# 140 mcg/L, tmax 2.8 to 3.1 h, AUC(0-24h) 1035 to 1132 mcg/L.h, and
# "fosinoprilat bioavailability from 25 to 29%".  Over 10 to 640 mg, tmax ran
# 2.4 to 4.2 h; the label says tmax is dose-independent at "approximately 3
# hours".  Food slows the rate and not the extent.  No lag was fitted anywhere
# and none is used.
#
# ka = 0.2987 /h puts the model's peak at 2.95 h, the midpoint of Singhvi's
# 2.8 to 3.1 h, on Hui's disposition.
#
# F = 0.26 is DERIVED against Singhvi's two measurements rather than read,
# because the two routes to it disagree slightly and a measurement is the better
# anchor: reproducing his Cmax (midpoint 119.5 ng/mL) needs 0.264 and
# reproducing his AUC(0-24h) (midpoint 1083.5 ng.h/mL) needs 0.250.  At 0.26 the
# model gives Cmax 117.8 and AUC(0-24h) 1126.9, both inside the reported ranges,
# and the value falls inside the reported "25 to 29%" as well.  Two independent
# corroborations, and one honest caution:
#
#   * Hui's own crossover gives fosinoprilat bioavailability as 22.5, 22.0 and
#     27.9% in his three impaired groups, mean 24.1% -- 8% below the value used,
#     from the same patients, assay and analysis as the disposition.  That it
#     brackets Singhvi's healthy 25 to 29% is the evidence for Shafer's point
#     that renal impairment leaves the EXTENT of absorption alone, and it is why
#     one absorption model serves every patient here.
#   * The agreement with the reported percentage is partly a coincidence of two
#     offsetting discrepancies, and saying so is more useful than claiming a
#     match.  Singhvi's own intravenous and oral areas (3166 and 1035-1132
#     mcg/L.h over 0-24 h for equimolar doses) imply an availability of 33 to
#     36% against HIS clearance, not 25 to 29%; his clearance was about 40%
#     above Hui's, which is why 0.26 against Hui's clearance lands where it
#     does.  See the next section.
#
# WHAT HUI'S "NORMAL RENAL FUNCTION" IS, AND WHY THE CURVE RUNS HIGH
# ==================================================================
# Hui's normal group were hypertensive patients averaging 61 years, and their
# clearance of 26.5 mL/min sits at the BOTTOM of the label's healthy range of
# 26 to 39 mL/min.  Singhvi's young healthy volunteers were at the top: his
# intravenous AUC(0-24h) of 3166 ng.h/mL for 7.5 mg implies about 37 mL/min
# (derived here, extrapolating his truncated area with this model's own 0-24 h
# share of the total, 94%), and this model gives 4417 for the same dose -- 40% higher
# exposure, and a 5-minute concentration of 2064 ng/mL against his measured
# 1556.  Both are inside the label's quoted healthy range, so neither is wrong;
# the model is the older patient.  By the rule in AGENTS.md only the clearances
# are compared here, and they differ by exactly the factor the exposures do.
# A user comparing this curve with a healthy-volunteer concentration should
# expect it to sit high after an intravenous dose -- which no one can give,
# since only the prodrug is marketed -- and to be about right orally, where the
# derived F absorbs the difference.
#
# The terminal half-life, 12.4 h, needs no such caveat: the label gives
# approximately 12 h in healthy subjects and an effective 11.5 h for
# accumulation in hypertensive patients, and Murdoch 11.5 to 12 h.
#
# AN INDEPENDENT CHECK ON THE VOLUME
# ==================================
# Murdoch reports Singhvi's steady-state volume of distribution as 9.8 to
# 10.6 L after the same 7.5 mg intravenous dose.  Hui's 10.2 L -- the number
# this model's Vss reproduces exactly, being an input -- sits in the middle of
# it.  A different laboratory and population agree on the volume while
# disagreeing on the clearance, which is the pattern AGENTS.md predicts in
# reverse and worth noting: here it is the construct that agrees.
#
# WHAT IS NOT MODELLED
# ====================
# * The absorption RATE in renal impairment.  Hui's impaired patients had tmax
#   4.0 to 4.2 h and mean absorption times of 7.1, 11.1 and 12.3 h, against
#   Singhvi's healthy 2.8 to 3.1 h.  Shafer's design is one absorption model for
#   every patient; the extent is what Hui shows unchanged, and the rate is not
#   the same claim.  MAT is also confounded, since the mean residence time after
#   the intravenous dose rose in the same patients.  Recorded, not applied.
# * p-hydroxyfosinoprilat.  The label puts active fosinoprilat at 75% of plasma
#   radioactivity after an oral dose, its glucuronide at 20 to 30% and the
#   p-hydroxy metabolite at 1 to 5%.  The glucuronide is inactive, but "in rats,
#   the p-hydroxy metabolite of fosinoprilat is as potent an inhibitor of ACE as
#   fosinoprilat".  It is a minor, equipotent, unmodelled active metabolite; the
#   plotted curve is fosinoprilat alone, as Hui's assay measured it.
# * Hepatic impairment.  The label puts apparent total body clearance at about
#   half of normal in alcoholic or biliary cirrhosis, with hydrolysis slowed but
#   not reduced in extent.  The Patient Profile has no hepatic covariate.
# * Dialysis.  Fosinopril is poorly dialysed (2% and 7% of urea clearance by
#   haemodialysis and peritoneal dialysis), and dialysis patients had clearance
#   reduced by half.  The ramp's plateau is already about half of normal, so it
#   happens to land near the right place, but nothing here models dialysis.
# * Children.  The label reports 20 patients aged 6 to 16 with GFR >= 25 whose
#   AUC and Cmax at 0.3 mg/kg matched adults at the same dose per kilogram, with
#   a terminal half-life of 11 to 13 hours.  That is consistent with ordinary
#   size scaling and no paediatric covariate, which is what this model does.
# * Age, beyond what Cockcroft-Gault's age term does to the renal covariate.
#   The label found no pharmacokinetic difference in men aged 65 to 74 with
#   normal renal and hepatic function.
# * Blood pressure, ACE inhibition and renin: nothing in this library plots a
#   pressure (docs/antihypertensives.md).  tPeak, MEAC and the band are zero.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Hui fitted no size covariate: absolute litres and mL/min in patients
# averaging 78.6 kg.  The published parameters are therefore unscaled with the
# switch off (legacyVolume = 1) and take the library's fat-free-mass factors
# with it on.  The renal term sees the pharmacokinetic weight with the switch on
# and total body weight with it off, as every Cockcroft-Gault model here does.
# -----------------------------------------------------------------------------

# Hui 1991 Table IV, normal renal function (PDF p7, printed 463)
FOSINOPRIL_IV_DOSE     <- 7500          # mcg fosinoprilat, the dose fitted
FOSINOPRIL_V_INIT      <- 3.1           # L,      Dose/(A+B+C), per the Methods
FOSINOPRIL_VSS         <- 10.2          # L,      model-dependent
FOSINOPRIL_HALF_LIVES  <- c(0.2, 2.1, 12.4)   # h,  alpha, beta, gamma

# The renal covariate (see the header).  Clearances in mL/min, creatinine
# clearances in mL/min/1.73 m^2.
FOSINOPRIL_CL_NORMAL    <- 26.5   # Hui's normal group
FOSINOPRIL_CL_IMPAIRED  <- 15.5   # unweighted mean of his 14.6, 13.7, 18.2
FOSINOPRIL_CRCL_NORMAL  <- 80     # the US label's boundary for normal function
FOSINOPRIL_CRCL_IMPAIRED <- 59.6  # Hui's mild group's own creatinine clearance

# Absorption, from Singhvi 1988 via Murdoch & McTavish 1992 section 2.1
FOSINOPRIL_KA <- 0.2987           # /h, set to put the peak at 2.95 h
FOSINOPRIL_F  <- 0.26             # fraction of the fosinoprilat-equivalent dose

# Hui's Methods: 7.5 mg fosinoprilat is equimolar to 10 mg fosinopril sodium
FOSINOPRILAT_PER_FOSINOPRIL <- 0.75    # mg fosinoprilat per mg fosinopril sodium

#' Fosinoprilat total body clearance at a given creatinine clearance
#'
#' The two-plateau ramp of Hui 1991's four groups: the normal group's clearance
#' at and above the label's boundary for normal renal function, the mean of his
#' three impaired groups at and below his mild group's creatinine clearance, and
#' linear across the one interval neither source measures.  See the header of
#' `R/drugs_fosinopril.R`.
#'
#' @param crcl creatinine clearance in mL/min/1.73 m^2
#' @return total body clearance in mL/min
#' @keywords internal
fosinoprilatClearance <- function(crcl)
{
  f <- pmin(1, pmax(0, (crcl - FOSINOPRIL_CRCL_IMPAIRED) /
                       (FOSINOPRIL_CRCL_NORMAL - FOSINOPRIL_CRCL_IMPAIRED)))
  FOSINOPRIL_CL_IMPAIRED + (FOSINOPRIL_CL_NORMAL - FOSINOPRIL_CL_IMPAIRED) * f
}

#' Fosinoprilat disposition from Hui 1991's printed summary
#'
#' The unique three-compartment model behind one group of Hui Table IV.  Its
#' six printed numbers close the system linearly: with the eigenvalues fixed by
#' the half-lives, the three intercepts solve `A+B+C = Dose/V_init`,
#' `sum(A/lambda) = Dose/CL` and `sum(A/lambda^2) = Vss*Dose/CL^2`, the last two
#' being the area and the mean residence time the paper's Methods define.  The
#' intercepts then go through `threeCompartmentFromTriexponential()`.
#'
#' @param cl total body clearance in L/min
#' @param vInit the initial volume of distribution in L
#' @param vss the steady-state volume of distribution in L
#' @param halfLives the three disposition half-lives in hours
#' @param dose the dose the intercepts are expressed for, in mcg
#' @return a list of `v1`, `v2`, `v3`, `cl1`, `cl2`, `cl3` (L, L/min) and the
#'   intercepts `coefficients` in ng/mL at `dose` with their `exponents` in /min
#' @keywords internal
fosinoprilatDisposition <- function(cl, vInit = FOSINOPRIL_V_INIT,
                                    vss = FOSINOPRIL_VSS,
                                    halfLives = FOSINOPRIL_HALF_LIVES,
                                    dose = FOSINOPRIL_IV_DOSE)
{
  lambda <- log(2) / (halfLives * 60)                 # /min
  auc <- dose / cl                                    # mcg.min/L
  mrt <- vss / cl                                     # min
  coefficients <- solve(
    rbind(c(1, 1, 1), 1 / lambda, 1 / lambda^2),
    c(dose / vInit, auc, mrt * auc)
  )
  if (!all(coefficients > 0))
    stop("These numbers have no triexponential with positive intercepts")
  # ng/mL is mcg/L, so the intercepts per mcg are already 1/L
  p <- threeCompartmentFromTriexponential(coefficients / dose, lambda)
  c(p, list(coefficients = coefficients, exponents = lambda))
}

#' Fosinopril pharmacokinetics, plotted as fosinoprilat
#'
#' Three compartments from Hui 1991's Table IV in patients with normal renal
#' function, with a two-plateau clearance covariate on creatinine clearance and
#' an absorption taken from Singhvi 1988 through the Murdoch & McTavish review.
#' The plotted moiety is fosinoprilat, the active diacid; a dose is entered as
#' prescribed, in milligrams of fosinopril sodium, and converted to its molar
#' equivalent of fosinoprilat inside the bioavailability.  See the header of
#' `R/drugs_fosinopril.R`.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for the patient's age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fosinopril <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # Hui fitted no size covariate, so the published parameters stand unscaled
  # with the switch off and take the library's factors with it on.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1)
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Hui reports creatinine clearance indexed to 1.73 m^2 and names no body
  # surface area formula; Du Bois is the one of its day.
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * 1.73 /
    bsaDuBois(pkW, height)
  cl <- fosinoprilatClearance(crcl) / 1000              # mL/min -> L/min

  # The STRUCTURE is the normal group's and the covariate moves nothing but
  # elimination, which is what lets the impaired half-lives be a prediction
  # (see the header).  Deriving the vector afresh at the impaired clearance
  # instead would hold the half-lives fixed and move the distribution
  # parameters, which is the opposite of what Hui's Table IV shows.
  p <- fosinoprilatDisposition(FOSINOPRIL_CL_NORMAL / 1000)

  default <- list(
    v1  = p$v1 * size$volume,
    v2  = p$v2 * size$volume,
    v3  = p$v3 * size$volume,
    cl1 = cl * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = p$cl3 * size$clearance,
    ka_PO = FOSINOPRIL_KA / 60,                         # /h -> /min
    # Both factors live here: the molar conversion of the prodrug to the active
    # diacid and the bioavailability of the diacid.  Neither is applied again.
    bioavailability_PO = FOSINOPRILAT_PER_FOSINOPRIL * FOSINOPRIL_F,
    tlag_PO = 0                                         # none was fitted
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration constant published; plasma only
  MEAC  <- 0

  # No established therapeutic plasma range for fosinoprilat: it is dosed to
  # blood pressure, and Hart 2026 Table 5 does not list it.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Hui KK, Duchin KL, Kripalani KJ, Chan D, Kramer PK, Yanagawa N. ",
    "Pharmacokinetics of fosinopril in patients with various degrees of renal ",
    "function. Clin Pharmacol Ther 1991;49:457-67. THE PLOTTED MOIETY IS ",
    "FOSINOPRILAT, the active diacid; fosinopril is an ester prodrug, only the ",
    "prodrug is marketed, and the model takes the conversion to be complete in ",
    "the gut before absorption, so a dose is entered as prescribed in ",
    "milligrams of fosinopril sodium and converted at the source's own ",
    "0.75 mg fosinoprilat per mg fosinopril sodium (its intravenous 7.5 mg ",
    "was 'equimolar to 10 mg fosinopril sodium'). Three compartments DERIVED ",
    "from Table IV's normal-renal-function group (n = 4 to 6): its six printed ",
    "numbers -- initial volume 3.1 L, clearance 26.5 mL/min, steady-state ",
    "volume 10.2 L and half-lives of 0.2, 2.1 and 12.4 h -- close a ",
    "three-compartment model by a linear solve for the intercepts, giving V1 ",
    "3.10, V2 2.43, V3 4.67 L, CL 26.5, Q2 71.7, Q3 5.47 mL/min. All 729 ",
    "corners of the printed SEM box give a physically admissible model. Total ",
    "clearance carries the only covariate, a two-plateau ramp on ",
    "Cockcroft-Gault creatinine clearance indexed to 1.73 m2: 26.5 mL/min at ",
    "80 and above, which is the US label's boundary for normal renal function, ",
    "falling to 15.5 mL/min at 59.6 and below, the mean of Hui's three ",
    "impaired groups at his mild group's creatinine clearance. A BLANK SERUM ",
    "CREATININE means the assumed normal value for the patient's age and sex, ",
    "not a normal clearance. Clearance halves from normal to impaired and is ",
    "then flat across it, because the hepatobiliary route takes 63 to 85% of a ",
    "smaller total; the label states instead that clearance 'does not differ ",
    "appreciably from normal' over a creatinine clearance of 10 to 80, which ",
    "can be read as flatness across that range rather than equality with ",
    "normal. With the volumes held fixed the model predicts a terminal ",
    "half-life of 14.7 h in impairment against Hui's measured 14.0, 14.1 and ",
    "16.9 h, which it was not told. Absorption comes from Singhvi 1988 in ",
    "healthy volunteers through Murdoch D, McTavish D, Drugs 1992;43:123-40: ",
    "ka 0.2987 /h puts the peak at 2.95 h, the midpoint of its 2.8 to 3.1 h, ",
    "and a bioavailability of 0.26 of the fosinoprilat-equivalent dose ",
    "reproduces its peak of 99 to 140 ng/mL and its 0-24 h area of 1035 to ",
    "1132 ng.h/mL; the same review's reported availability is 25 to 29% and ",
    "Hui's own impaired patients gave 22.5, 22.0 and 27.9%, which is the ",
    "evidence that renal impairment leaves the extent of absorption alone. Its ",
    "RATE is not modelled: Hui's impaired patients peaked at 4.0 to 4.2 h. ",
    "Murdoch reports Singhvi's steady-state volume as 9.8 to 10.6 L, an ",
    "independent check on the 10.2 L used. Hui's normal group were ",
    "hypertensive patients averaging 61 years whose clearance sits at the ",
    "bottom of the label's healthy range of 26 to 39 mL/min, so the curve runs ",
    "high against young healthy volunteers. Table IV's parameters are means of ",
    "individual fits and do not form one exactly consistent model: the derived ",
    "area is 4717 ng.h/mL against its own 4971. The equipotent minor ",
    "metabolite p-hydroxyfosinoprilat, hepatic impairment and dialysis are not ",
    "modelled. https://doi.org/10.1038/clpt.1991.54"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
