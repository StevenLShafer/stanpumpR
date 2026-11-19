# -----------------------------------------------------------------------------
# Insulin: one effect site for every insulin product
# -----------------------------------------------------------------------------
# Steven L. Shafer's decision, 2026-11-19: every insulin in the library
# (insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70,
# insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine,
# insulinAspart, insulinAspartFiasp, insulinDegludec) shares ONE effect site,
# whose rate constant is a property of insulin's action, not of the product.
# Each product reaches it through its own subcutaneous absorption, which is
# what makes the effect peak of regular insulin, lispro and NPH differ.
# Drafted by Claude Code, 2026-11-19.
#
# THE MEASUREMENT: the time to peak effect after an intravenous bolus
# ===============
# Howey DC, Bowsher RR, Brunelle RL, Woodworth JR.  [Lys(B28), Pro(B29)]-
# human insulin.  A rapidly absorbed analogue of human insulin.  Diabetes
# 1994;43:396-402 (PMID 8314011, read in full).  Ten healthy men (70.9 kg),
# euglycaemic glucose clamp, 10 U of human regular insulin intravenously:
# the glucose infusion rate peaked at 23 +/- 5 min (Table 1, TRmax).
# Galloway JA, Hooper SA, Spradlin CT, et al.  Biosynthetic human proinsulin.
# Diabetes Care 1992;15:666-692 (PMID 1516487, read in full), Table 7, the
# same Lilly laboratory's Pharmacology Study 3: 0.2 U/kg intravenously,
# median time of maximum glucose infusion rate 0.33 h (20 min).
#
# THE DISPOSITION IT IS SOLVED ON.  ke0 depends on how fast plasma insulin
# falls after the bolus, and insulin's intravenous disposition is not one
# exponential: Howey's noncompartmental terminal half-life is 46.3 min
# (harmonic mean; terminal rate 0.0243 /min) but his mean residence time is
# 32 min, and Galloway's Table 4 gives a distribution half-life of 5.1 min
# and an elimination half-life of 0.92 h.  ke0 is solved on two exponentials
# with Galloway's 5.1-min fast phase and Howey's 46.3-min terminal phase,
# their shares set so that the mean residence time is Howey's 32 min (93% of
# the area-weighted intercept on the fast phase).  A 23-min peak then needs
# ke0 = 0.876 /h (equilibration half-time 47 min); 20 min needs 1.17 /h.
# On one exponential of 46.3 min the same peak would need 5.7 /h, and on
# one of 32 min 3.5 /h: the answer turns on the fast phase.
#
# THE CHECK, against effect peaks after SUBCUTANEOUS doses, which no part of
# the derivation used (computed 2026-11-19 on the library's own Tham 2017
# absorption models, a 70 kg man):
#
#                      measured peak glucose infusion rate     model, ke0 0.876
#   regular, 10 U      179 min (Howey 1994), 3.0 h (Woodworth   2.5 h
#                      1994, PMID 8112192)
#   regular, 0.2 U/kg  172-191 min (Rave 2001, PMID 11347749)   2.7 h
#   lispro, 10-14 U    99 min (Howey), 102-107 (Rave; Heise     2.2 h
#                      1998, PMID 9589244), 116 (Cengiz 2010,
#                      PMID 20150302)
#   NPH, 21-25 U       5.4 h (Lepore 2000, PMID 11118018),      4.9-5.0 h
#                      5.9 h (Woodworth)
#
# Every product lands within an hour of its measured peak (lispro 0.3-0.6 h
# late, regular 0.3-0.7 h and NPH 0.4-0.9 h early), and
# the faster values that one exponential would give fail the check: at 5.7
# /h regular insulin's effect would peak at 1.5 h, half the measured 3 h.
# The residual errors follow the absorption models (Tham's regular insulin
# peaks in plasma at 1.4 h against Howey's 1.7 h, lispro at 1.1 h against
# 0.7 h), not the effect site.  Woodworth 1994 fitted an effect compartment
# to the subcutaneous data but printed only EC50 and gamma, not ke0; its
# intravenous effect-compartment model is a 1991 book chapter (Hooper,
# Bowsher and Howey) not in the archive.  The minimal model's remote insulin
# compartment, p2 about 0.02-0.03 /min, is the same order of delay.
#
# AN INDEPENDENT ESTIMATE agrees.  The FDA's Clinical Pharmacology review of
# Fiasp (NDA 208751, 2017, read in full; the source of insulinAspart's PK)
# reproduces the sponsor's PK-PD model of 72 clamps after subcutaneous
# insulin aspart (Fiasp and NovoLog): an effect compartment driving a
# sigmoid Emax glucose infusion rate, whose rate constant ("turnover rate
# constant", P2) is 0.996 /h, 95% CI 0.88 to 1.11 (Table 6, printed p. 92).
# Fitted to different data, by a different method, for a different insulin,
# it brackets the 0.876 /h derived here from regular insulin intravenously.
#
# ke0 is supplied directly (getDrugPK() then solves each product's own time
# to peak effect after a bolus on its disposition), not as a tPeak, because
# the products' one-compartment disposition models are apparent fits to
# subcutaneous data and do not describe an intravenous bolus.  MEAC and the
# band stay 0: no glucose endpoint is plotted.
# -----------------------------------------------------------------------------

INSULIN_KE0 <- 0.876   # /h
