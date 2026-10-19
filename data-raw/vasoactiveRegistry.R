# -----------------------------------------------------------------------------
# Build inst/extdata/vasoactiveRegistry.csv and vasoactiveParameters.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, who
# asked for the intravenous vasoactive drugs of intensive care: clevidipine,
# sodium nitroprusside, nitroglycerin, epinephrine, norepinephrine,
# vasopressin, ephedrine, phenylephrine, hydralazine, nicardipine, dopamine,
# methylene blue, atropine, adenosine, sodium bicarbonate, magnesium sulfate,
# procainamide, verapamil, diltiazem, propranolol, metoprolol, esmolol and
# labetalol, with references, and the references still to be obtained.
#
# The registry has one row per drug and candidate source.  A row is
# "implemented" only where a complete model is in the library as a
# selectable drug (R/drugs_<name>.R, a CSV row, a test and a help page).
# Every other row says what blocks it and, where a paper would unblock it,
# names that paper in `to_obtain`.  Readiness:
#
#   implemented        in the library
#   needs_full_text    a candidate model exists, but the numbers that fix it
#                      could not be read here (abstract only, or a full text
#                      whose tables were stripped)
#   complete_not_implemented
#                      a complete model, read and checked, of a drug the
#                      library already offers from another source; kept as
#                      the record of an alternative, not a second curve
#   no_suitable_model  no published model this engine can represent: the
#                      drug is not measured (nitroprusside), its kinetics are
#                      sub-second and dose-dependent (adenosine), or it has no
#                      pharmacokinetics in the usual sense (bicarbonate)
#
# `evidence_read` records what was actually read in this environment: the
# full text (from PubMed Central), the abstract only, or nothing beyond a
# citation.  Literature access from here is the PubMed server alone; scanned
# PubMed Central papers (most British Journal of Clinical Pharmacology papers
# before 2000) return an empty body, although they are free to download.
#
# Run with: Rscript data-raw/vasoactiveRegistry.R
# Checked by tests/testthat/test-vasoactive-registry.R.
# -----------------------------------------------------------------------------

READINESS <- c("implemented", "needs_full_text", "complete_not_implemented",
               "no_suitable_model")
EVIDENCE  <- c("full_text", "abstract", "none")

pubmed <- function(pmid) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
doi    <- function(d) paste0("https://doi.org/", d)

reg <- function(model_id, drug, population, analyte, matrix, readiness,
                stanpumpr_drug, evidence_read, blocking_issue, to_obtain,
                source, source_url, antihypertensive_model_id = NA)
  data.frame(model_id, drug, route = "IV", population, analyte, matrix,
             readiness, stanpumpr_drug, evidence_read, blocking_issue,
             to_obtain, source, source_url, antihypertensive_model_id,
             stringsAsFactors = FALSE)

registry <- rbind(
  # --- Implemented ------------------------------------------------------------
  reg("epinephrine__septic_shock__abboud2009", "epinephrine",
      "38 adults in septic shock, 0.026-1.7 mcg/kg/min", "epinephrine (exogenous)", "plasma",
      "implemented", "epinephrine", "full_text",
      "None for the curve.  SAPS II (on clearance) has no Patient Profile field and is held at 50; the volume is poorly determined (SE 36%) because the data are steady-state.",
      NA,
      "Abboud I, Lerolle N, Urien S, et al. Pharmacokinetics of epinephrine in patients with septic shock: modelization and interaction with endogenous neurohormonal status. Crit Care 2009;13:R120. PMID 19622169, PMC2750169.",
      doi("10.1186/cc7972")),
  reg("magnesium_sulfate__preeclampsia__du2019", "magnesium sulfate",
      "92 women with preeclampsia, 4 g over 20 min then 2 g/h", "rise in serum magnesium above baseline", "serum",
      "implemented", "magnesiumSulfate", "full_text",
      "None for the curve.  Fitted in pregnancy; other adults are an extrapolation.  The paper's 24-hour exposure and all six Table 4 maintenance doses are reproduced.",
      NA,
      "Du L, Wenning LA, Carvalho B, et al. Population pharmacokinetic modeling to evaluate standard magnesium sulfate treatments and alternative dosing regimens for women with preeclampsia. J Clin Pharmacol 2019;59:374-385. PMID 30422321, PMC6518930.",
      doi("10.1002/jcph.1328")),
  reg("labetalol__healthy__kanto1981", "labetalol",
      "4 healthy volunteers, 1.5 mg/kg", "labetalol", "plasma",
      "implemented", "labetalol", "abstract",
      "Built from the abstract's clearance, central volume and two half-lives; the printed Vss checks it to 4%.  Dose basis (HCl or base) and assay are unread.  Surgical patients clear it about half as fast.",
      "Kanto J, Allonen H, Kleimola T, Mantyla R. Pharmacokinetics of labetalol in healthy volunteers. Int J Clin Pharmacol Ther Toxicol 1981;19:41-44. PMID 7203731 (full text, to confirm dose basis and assay).",
      "Kanto J, Allonen H, Kleimola T, Mantyla R. Pharmacokinetics of labetalol in healthy volunteers. Int J Clin Pharmacol Ther Toxicol 1981;19:41-44. PMID 7203731.",
      pubmed(7203731), "labetalol__injection__iv__catalog__none__label"),
  reg("diltiazem__healthy__hoglund1988", "diltiazem",
      "10 healthy volunteers, 20 mg over 20 min", "diltiazem", "plasma",
      "implemented", "diltiazem", "abstract",
      "Built from the abstract's clearance, central volume and two half-lives; the printed Vss checks it to 2.6%.  Clearance falls at higher infusion rates (label; Dias 1992), so long high-rate infusions run higher than plotted.  Dose basis unread.",
      "Hoglund P, Nilsson LG. Physiological disposition of intravenously administered 14C-labeled diltiazem in healthy volunteers. Ther Drug Monit 1988;10:401-409. PMID 3201524 (full text, to confirm dose basis and weights).",
      "Hoglund P, Nilsson LG. Physiological disposition of intravenously administered 14C-labeled diltiazem in healthy volunteers. Ther Drug Monit 1988;10:401-409. PMID 3201524.",
      doi("10.1097/00007691-198804000-00006")),
  reg("nitroglycerin__ccf__armstrong1983", "nitroglycerin",
      "10 patients with congestive cardiac failure, 21 mcg/min", "glyceryl trinitrate", "plasma",
      "implemented", "nitroglycerin", "abstract",
      "One compartment from the abstract's clearance (6.2 L/min) and time constant (8.1 min).  Healthy volunteers' clearance is 3 to 10 times higher (McNiff 1981); PVC tubing adsorption and sampling site are unread.",
      "Armstrong PW, Watts DG, Moffat JA. Steady state pharmacokinetic haemodynamic studies of intravenous nitroglycerin in congestive cardiac failure. Br J Clin Pharmacol 1983;16:385-390. PMID 6414501; free scan PMC1428029.",
      "Armstrong PW, Watts DG, Moffat JA. Br J Clin Pharmacol 1983;16:385-390. PMID 6414501.",
      doi("10.1111/j.1365-2125.1983.tb02182.x"), "nitroglycerin__injection__iv__catalog__none__label"),
  reg("clevidipine__healthy__ericsson1999", "clevidipine",
      "25 healthy men, 20-min infusions", "clevidipine", "whole blood",
      "implemented", "clevidipine", "abstract",
      "Implemented earlier, in the Antihypertensives category (R/drugs_clevidipine.R; docs/antihypertensives.md).",
      NA,
      "Ericsson H, Fakt C, Jolin-Mellgard A, et al. Br J Clin Pharmacol 1999;47:531-538.",
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC2014189/",
      "clevidipine__emulsion__iv__healthy_men__concentration__ericsson1999"),

  # --- A candidate model exists; the paper is needed ---------------------------
  reg("norepinephrine__volunteers__li2024", "norepinephrine",
      "36 healthy volunteers, 18-70 y, 0.04-0.20 mcg/kg/min, awake and anaesthetised", "norepinephrine (exogenous)", "plasma (radial arterial)",
      "implemented", "norepinephrine", "full_text",
      "None for the curve.  Read from the published PDF and supplement (the PubMed Central copy is stripped).  Evaluated awake: the library cannot feed a propofol concentration into clearance, which falls 11.8% at 3.53 mcg/mL.  The 13.7 s lag is not represented.  Covariate coefficients read as per 100 and the reference weight as 70 kg, both checked against the text.",
      NA,
      "Li Y, Koomen JV, Eleveld DJ, et al. Population pharmacokinetic modelling of norepinephrine in healthy volunteers prior to and during general anesthesia. Clin Pharmacokinet 2024;63:1597-1608. PMID 39465453, PMC11573843.",
      doi("10.1007/s40262-024-01430-y")),
  reg("norepinephrine__septic_trauma__beloeil2005", "norepinephrine",
      "12 septic shock and 11 trauma patients, sedated and ventilated", "norepinephrine", "plasma (arterial)",
      "complete_not_implemented", NA, "full_text",
      "Read from the supplied article: one compartment, CL = 59 600 / SAPS II mL/min and V 8840 mL (Table 3), which reproduces the paper's three worked clearances and half-lives (SAPS II 20, 30, 66).  Not offered beside Li 2024: SAPS II is not a Patient Profile field, and the dose basis is unstated in a French study, where norepinephrine is often dosed as the tartrate, twice the base.  Clearance at SAPS II 50 is 1.19 L/min, about half the healthy volunteers' 2.1.",
      NA,
      "Beloeil H, Mazoit JX, Benhamou D, Duranteau J. Norepinephrine kinetics and dynamics in septic shock and trauma patients. Br J Anaesth 2005;95:782-788. PMID 16227334.",
      doi("10.1093/bja/aei259")),
  reg("norepinephrine__children__oualha2014", "norepinephrine",
      "38 hypotensive critically ill children, 2-85 kg (median 6.7)", "norepinephrine", "plasma",
      "complete_not_implemented", NA, "full_text",
      "Read from the supplied article and supplement (definitions only): one compartment, CL 6.6 L/h x BW^0.75, endogenous production 3.12 mcg/h x BW^0.75, V fixed at the circulating volume 0.08 L/kg (Table 2); it reproduces its own 10 kg example and Figure 8.  A candidate for a paediatric norepinephrine; not offered yet because the dose basis is unstated and the product used (noradrenaline 2 mg/mL, Renaudin) may be labelled as the tartrate, a factor of two.",
      NA,
      "Oualha M, Treluyer JM, Lesage F, et al. Population pharmacokinetics and haemodynamic effects of norepinephrine in hypotensive critically ill children. Br J Clin Pharmacol 2014;78:886-897. PMID 24802558.",
      doi("10.1111/bcp.12412")),
  reg("norepinephrine__joachim2024", "norepinephrine",
      "adults", "norepinephrine", "plasma",
      "needs_full_text", NA, "abstract",
      "Two compartments with a depot; no numbers in the abstract.",
      "Joachim J et al. Br J Clin Pharmacol 2024;90:2861-2869. PMID 39039779, doi 10.1111/bcp.16180.",
      "Joachim J et al. Br J Clin Pharmacol 2024;90:2861-2869. PMID 39039779.",
      doi("10.1111/bcp.16180")),
  reg("phenylephrine__pooled__anderson2017", "phenylephrine",
      "pooled children and 4 adult volunteers given IV", "phenylephrine", "plasma",
      "needs_full_text", NA, "abstract",
      "The abstract prints a complete two-compartment set (CL 160 L/h, V1 13.3 L, Q 25.3 L/h, V2 225 L at 70 kg) but labels it for the nasal formulation, prints no allometric exponents or dose basis, and its 7-hour terminal half-life conflicts with Hengstmann's 2-3 h.",
      "Anderson BJ et al. The phenylephrine concentration-response relationship for blood pressure after nasal delivery. Paediatr Anaesth 2017;27:1050-1055. PMID 28868789, doi 10.1111/pan.13221 (top priority).",
      "Anderson BJ et al. Paediatr Anaesth 2017;27:1050-1055. PMID 28868789.",
      doi("10.1111/pan.13221")),
  reg("phenylephrine__healthy__hengstmann1982", "phenylephrine",
      "healthy volunteers", "phenylephrine", "plasma",
      "needs_full_text", NA, "abstract",
      "Half-life 2-3 h and V 340 L in the abstract; its printed clearance (2 L/h) is evidently a misprint for 2 L/min, and no distribution phase is given.",
      "Hengstmann JH et al. Pharmacokinetics of 3H-phenylephrine in man. Eur J Clin Pharmacol 1982;21:335-341. PMID 7056280, doi 10.1007/BF00637623.",
      "Hengstmann JH et al. Eur J Clin Pharmacol 1982;21:335-341. PMID 7056280.",
      doi("10.1007/BF00637623")),
  reg("dopamine__healthy__macgregor2000", "dopamine",
      "9 healthy men, 10 then 3 mcg/kg/min", "dopamine", "plasma (radial arterial)",
      "implemented", "dopamine", "full_text",
      "Read from the supplied article.  Table 4's micro constants reproduce every derived value and Equation 3's 3.77 steady-state constant.  The 0.5 min lag and the endogenous baseline are not represented; concentrations at the same rate varied 10- to 75-fold, and the model overpredicted the 10 mcg/kg/min infusion.  Dose basis unstated (hydrochloride assumed).",
      NA,
      "MacGregor DA, Smith TE, Prielipp RC, et al. Pharmacokinetics of dopamine in healthy male subjects. Anesthesiology 2000;92:338-346. PMID 10691218.",
      doi("10.1097/00000542-200002000-00013")),
  reg("dopamine__critically_ill__lecorre1993", "dopamine",
      "critically ill adults, 3 and 6 mcg/kg/min", "dopamine", "plasma",
      "needs_full_text", NA, "abstract",
      "CL, Vss and the terminal half-life only (underdetermined), and Vss and half-life roughly double between the two rates, so the kinetics are dose-dependent.",
      "Le Corre P et al. Steady-state pharmacokinetics of dopamine in adult patients. Crit Care Med 1993;21:1652-1657. PMID 8222680, doi 10.1097/00003246-199311000-00013.",
      "Le Corre P et al. Crit Care Med 1993;21:1652-1657. PMID 8222680.",
      doi("10.1097/00003246-199311000-00013")),
  reg("nicardipine__cardiac_surgery__cheung1999", "nicardipine",
      "40 adults undergoing cardiac surgery, boluses 0.25-2 mg", "nicardipine", "plasma (mixed venous)",
      "no_suitable_model", NA, "full_text",
      "Read from the supplied article: Table 2 is not a coherent two-compartment model.  Its 'redistribution half-time' is ln2 Vc/CL (3.5 min at 1 mg exactly), not an independent rate; Varea is 1.3-2.2 times smaller than CL/beta in every group; the central volumes (0.02-0.12 L/kg) are below plasma volume, the bolus having gone into the right atrium and the samples drawn from the pulmonary artery; clearance doubles across the dose groups; and sampling stopped at 4 h.",
      NA,
      "Cheung AT, Guvakov DV, Weiss SJ, Savino JS, Salgo IS, Meng QC. Nicardipine intravenous bolus dosing for acutely decreasing arterial blood pressure during general anesthesia for cardiac operations. Anesth Analg 1999;89:1116-1123. PMID 10553821.",
      pubmed(10553821), "nicardipine__injection__iv__cardiac_surgery_adults__concentration__page2007"),
  reg("nicardipine__healthy__guerret1989", "nicardipine",
      "healthy volunteers", "nicardipine", "plasma",
      "needs_full_text", NA, "abstract",
      "Three compartments (CL about 800 mL/min, Vd about 1 L/kg, terminal half-life 4-5 h) without a printed volume and clearance set.",
      "Guerret M et al. Simultaneous study of the pharmacokinetics of intravenous and oral nicardipine using a stable isotope. Eur J Clin Pharmacol 1989;37:381-385. PMID 2598970, doi 10.1007/BF00558504.  Also Modi 1993, J Pharm Sci 82:705-713, PMID 8360844, doi 10.1002/jps.2600820707.",
      "Guerret M et al. Eur J Clin Pharmacol 1989;37:381-385. PMID 2598970.",
      doi("10.1007/BF00558504")),
  reg("methylene_blue__healthy__walter_sack2009", "methylene blue",
      "16 healthy volunteers, 50 mg IV over 9.5 min and 500 mg oral", "methylene blue", "plasma and whole blood",
      "needs_full_text", NA, "full_text",
      "Read from the supplied article: non-compartmental only (AUC, Cmax, terminal half-life 18.5 h, range 7.7-48).  The plasma curve is clearly multiphasic, so a one-compartment model from CL and the half-life would misplace the early concentrations badly; no compartmental parameters are reported.  A three-compartment adult population model is reported in the EMA ProveBlue assessment.",
      "EMA ProveBlue (methylthioninium chloride) Article 46 assessment report P46-010; FDA ProvayBlue NDA 204630 clinical pharmacology review (a population model on a modern assay would settle the tenfold conflict with Peter 2000).",
      "Walter-Sack I, Rengelshausen J, Oberwittler H, et al. High absolute bioavailability of methylene blue given as an aqueous oral formulation. Eur J Clin Pharmacol 2009;65:179-189. PMID 18810398.",
      doi("10.1007/s00228-008-0563-x")),
  reg("methylene_blue__healthy__peter2000", "methylene blue",
      "7 healthy volunteers, 100 mg IV over 30 s", "methylene blue", "whole blood",
      "no_suitable_model", NA, "full_text",
      "Read from the supplied article: a complete triexponential fit to the pooled data, C = 9.4 exp(-0.237 t) + 3.8 exp(-0.065 t) + 0.3 exp(-0.0022 t) umol/L, which would fix three compartments.  But its AUC per mg is about a tenth of Walter-Sack 2009's LC-MS/MS measurements (1250 against about 12 900 ng h/mL per 100 mg in whole blood), whose authors suggest earlier assays measured only unbound drug; and its terminal phase is extrapolated from 4 h of sampling.  The two sources cannot both be right, and neither alone is a defensible model.",
      NA,
      "Peter C, Hongwan D, Kupfer A, Lauterburg BH. Pharmacokinetics and organ distribution of intravenous and oral methylene blue. Eur J Clin Pharmacol 2000;56:247-250. PMID 10952480.",
      doi("10.1007/s002280000124")),
  reg("atropine__healthy__hinderling1985", "atropine",
      "3 healthy men, 1.35 and 2.15 mg (6 experiments)", "atropine (racemic)", "plasma",
      "implemented", "atropine", "full_text",
      "Read from the supplied article (and Part II, its pharmacodynamics).  Built from Table I's V_P, CL, k_PT and k_TP; Vdss, alpha, the terminal half-life and Vd_ps check it within 4%.  Doses are the base in the source and the sulfate as labelled.  Three young men; distribution was dose-dependent; age and urine flow are not covariates.",
      NA,
      "Hinderling PH, Gundert-Remy U, Schmidlin O. Integrated pharmacokinetics and pharmacodynamics of atropine in healthy humans. I: Pharmacokinetics. J Pharm Sci 1985;74:703-710. PMID 4032240; II: Pharmacodynamics, 711-717, PMID 4032241.",
      doi("10.1002/jps.2600740702")),
  reg("atropine__parturients__kanto1981", "atropine",
      "32 parturients given IV atropine", "atropine", "plasma",
      "needs_full_text", NA, "abstract",
      "Half-lives 1.02 min and 2.56 h, Vd 1.0 L/kg and CL 6.36 mL/min/kg in the abstract, but which volume is meant is unread, and the population is pregnant.",
      "Kanto J et al. Placental transfer and pharmacokinetics of atropine after a single maternal intravenous and intramuscular administration. Acta Anaesthesiol Scand 1981;25:85-88. PMID 7324829, doi 10.1111/j.1399-6576.1981.tb01613.x.",
      "Kanto J et al. Acta Anaesthesiol Scand 1981;25:85-88. PMID 7324829.",
      doi("10.1111/j.1399-6576.1981.tb01613.x")),
  reg("digoxin__hf_af__yukawa2001", "digoxin",
      "106 Japanese patients with heart failure and atrial fibrillation, oral tablets", "digoxin", "serum",
      "implemented", "digoxin", "full_text",
      "Read from the supplied article.  Apparent (/F) parameters with no absorption phase, so offered only as a constant oral daily rate (mg/day PO), with no intravenous unit.  Reproduces Table VI's clearances and doses.  Spironolactone and calcium antagonists held at none.  An intravenous model needs an intravenous study.",
      "Cheng JW, Charland SL, Shaw LM, et al. Is the volume of distribution of digoxin reduced in patients with renal dysfunction? Pharmacotherapy 1997;17:584-590 (PMID 9165563; intravenous, two compartments by renal function); Koup JR et al. Clin Pharmacol Ther 1975;18:9-21; Kramer WG et al. J Pharmacokinet Biopharm 1974;2:299-312.",
      "Yukawa E, Suematu F, Yukawa M, et al. Population pharmacokinetics of digoxin in Japanese patients: a 2-compartment pharmacokinetic model. Clin Pharmacokinet 2001;40:773-781. PMID 11707062.",
      doi("10.2165/00003088-200140100-00005")),
  reg("procainamide__clinical__grasela1984", "procainamide",
      "39 inpatients, routine clinical data", "procainamide (NAPA in urine)", "plasma",
      "implemented", "procainamide", "full_text",
      "Read from the supplied article.  Equation 7's sex term is used as printed, which reproduces Table IV's average patient but runs opposite to Cockcroft-Gault (5 of 39 were women).  The doses are the base (Figure 2) and the hydrochloride as labelled.  Heart failure (11% on renal and acetylation clearance) and acetylator phenotype are not covariates; NAPA is not plotted; one compartment, so a fast loading bolus is outside it.",
      NA,
      "Grasela TH, Sheiner LB. Population pharmacokinetics of procainamide from routine clinical data. Clin Pharmacokinet 1984;9:545-554. PMID 6509861.",
      doi("10.2165/00003088-198409060-00004")),
  reg("verapamil__healthy__dominic1981", "verapamil",
      "8 healthy men", "racemic verapamil", "plasma",
      "needs_full_text", NA, "abstract",
      "Two compartments, but the abstract's V (178 L) exceeds CL/beta and so cannot be Vss: three usable numbers for four parameters, and no central volume.  Enantiomer kinetics differ (Eichelbaum 1984).  Three supplied papers, Fuenmayor 1992 (Drugs 44 Suppl 1:1-11), Saseen 2013 (J Clin Pharmacol) and Kang 2003 (Clin Pharmacol Ther 73:31-40, a population model of oral sustained-release verapamil that estimates only an apparent steady-state clearance, 23.8 mL/min/kg in women and 18.6 in men), give no intravenous model.  Kang's references include Schwartz 1990, \"Aging alters verapamil elimination and dynamics: single dose and steady-state responses\" (J Pharmacol Exp Ther 255:364-373).",
      "Schomerus M, Spiegelhalder B, Stieren B, Eichelbaum M. Physiological disposition of verapamil in man. Cardiovasc Res 1976;10:605-612; Dominic JA et al. The pharmacology of verapamil. III. Pharmacokinetics in normal subjects after intravenous and oral dosing. J Cardiovasc Pharmacol 1981;3:25-38. PMID 6160352, doi 10.1097/00005344-198101000-00003.  Also Kates 1981 (Clin Pharmacol Ther 30:44, doi 10.1038/clpt.1981.125), McAllister 1982 (doi 10.1038/clpt.1982.54), Eichelbaum 1984 (PMC1463390).",
      "Dominic JA et al. J Cardiovasc Pharmacol 1981;3:25-38. PMID 6160352.",
      doi("10.1097/00005344-198101000-00003")),
  reg("diltiazem__healthy__kolle1983", "diltiazem",
      "6 healthy subjects, 20 mg over 20 min", "diltiazem", "plasma",
      "needs_full_text", NA, "abstract",
      "An alternative to the implemented model: three half-lives, V1, Vss and CL fix a three-compartment model exactly, but with no number left to check it, and the companion paper gives a different infusion time.",
      "Kolle EU, Ochs HR, Vollmer KO. Pharmacokinetic model of diltiazem. Arzneimittelforschung 1983;33:972-977. PMID 6684932; Ochs HR, Knuchel M. Klin Wochenschr 1984;62:303 (doi 10.1007/BF01716446); Dias VC et al. Circulation 1992;86:1421 (doi 10.1161/01.cir.86.5.1421).",
      "Kolle EU, Ochs HR, Vollmer KO. Arzneimittelforschung 1983;33:972-977. PMID 6684932.",
      pubmed(6684932)),
  reg("propranolol__healthy__fagan1982", "propranolol",
      "6 healthy men, 0.05 mg/kg at 1 mg/min", "propranolol (racemic)", "plasma",
      "implemented", "propranolol", "full_text",
      "Read from the supplied letter: three compartments derived exactly from Table 1's mean triexponential (V1 11.1 L, CL 1.11 L/min, Vss 264 L); the mean curve's AUC matches the printed 57.6 ng h/mL.  Checked against Cheymol 1987's normal-weight controls (CL 75.9 L/h, Vss 318.6 L, 3.1 h), all within 17%.  Six young men; clearance follows hepatic blood flow.",
      NA,
      "Fagan TC, Walle T, Walle UK, Conradi EC, Harmon G, Gaffney TE. Early kinetics of intravenous propranolol. Br J Clin Pharmacol 1982;13:571-574. PMID 7066173.",
      doi("10.1111/j.1365-2125.1982.tb01424.x")),
  reg("propranolol__obese_controls__cheymol1987", "propranolol",
      "12 obese and 12 normal-weight volunteers, 8 mg over 10 min", "propranolol (racemic)", "plasma",
      "complete_not_implemented", NA, "full_text",
      "Read from the supplied article: two compartments, but only CL, Vss, V-beta and the terminal half-life (Table II), one short of a model.  Used as the external check on Fagan 1982, and its obese subjects' lower clearance and volume support scaling to fat-free mass.",
      NA,
      "Cheymol G, Poirier JM, Barre J, Pradalier A, Dry J. Comparative pharmacokinetics of intravenous propranolol in obese and normal volunteers. J Clin Pharmacol 1987;27:874-879. PMID 3429695.",
      doi("10.1002/j.1552-4604.1987.tb05582.x")),
  reg("metoprolol__cyp2d6__leemann1993", "metoprolol",
      "7 healthy men, 4 extensive and 3 poor CYP2D6 metabolisers, 20 mg tartrate over 20 min", "metoprolol (racemic)", "whole blood",
      "needs_full_text", NA, "full_text",
      "Read from the supplied article: biexponential fits, but only clearance and Vss are reported (Table 1; extensive 1.08 L/h/kg and 4.84 L/kg, poor 0.43 and 2.83), not the coefficients and exponents, so no model can be rebuilt.  It does show that a CYP2D6 covariate must act on volume as well as clearance.",
      "Regardh CG, Johnsson G. Clinical pharmacokinetics of metoprolol. Clin Pharmacokinet 1980;5:557-569; Regardh CG, Johnsson G, Jordo L, et al. Plasma concentrations and beta-blocking effects in normal volunteers after intravenous doses of metoprolol and propranolol. J Cardiovasc Pharmacol 1980;2:715-723; Richard J, Cardot J, Godbillon J. Inter- and intra-subject variability of metoprolol kinetics after intravenous administration. Eur J Drug Metab Pharmacokinet 1994;19:157-162 (doi 10.1007/BF03188836); Regardh 1974 (PMID 4155762) was unavailable.  Zamir 2022's systematic review (Clin Pharmacokinet 61:1095, read in full) tabulates only CL and ke for the IV studies, and its ke of 1.01 /h beside CL 48 L/h is not a disposition rate.",
      "Leemann TD, Devi KP, Dayer P. Similar effect of oxidation deficiency (debrisoquine polymorphism) and quinidine on the apparent volume of distribution of (+/-)-metoprolol. Eur J Clin Pharmacol 1993;45:65-71. PMID 8405032.",
      doi("10.1007/BF00315352"), "metoprolol__tartrate_injection__iv__catalog__none__label"),
  reg("esmolol__cabg__debruijn1987", "esmolol",
      "13 of 19 patients anaesthetised for CABG on chronic beta blockers", "esmolol", "whole blood (internal jugular)",
      "implemented", "esmolol", "full_text",
      "Read from the supplied article: 'V epsilon' is the central volume (0.413 L/kg).  Built from the means of CL, Vc and both half-lives; V-beta checks it to 4%.  Central venous whole blood, infused peripherally; arterial esmolol runs several times higher.  Four of the 13 individual parameter sets admit no compartmental model.",
      NA,
      "de Bruijn NP, Reves JG, Croughwell N, Clements F, Drissel DA. Pharmacokinetics of esmolol in anesthetized patients receiving chronic beta blocker therapy. Anesthesiology 1987;66:323-326. PMID 2881504.",
      doi("10.1097/00000542-198703000-00010"), "esmolol__injection__iv__healthy_adults__concentration__sum1983"),

  # --- No model this engine can represent --------------------------------------
  reg("nitroprusside__review__schulz1984", "sodium nitroprusside",
      "review", "none measured (nitroprusside decomposes in blood within minutes)", "not applicable",
      "no_suitable_model", NA, "abstract",
      "Intact nitroprusside is not measured; its cyanide and thiocyanate products have kinetics of their own (thiocyanate Vd about 0.25 L/kg, half-life about 3 days) but no transferable dose-to-cyanide input model.  The children's model (Barrett 2015) is kinetic-dynamic with a nominal volume.",
      NA,
      "Schulz V. Clinical pharmacokinetics of nitroprusside, cyanide, thiosulphate and thiocyanate. Clin Pharmacokinet 1984;9:239-251. PMID 6375932.",
      doi("10.2165/00003088-198409030-00005"), "nitroprusside__injection__iv__anaesthetised_children__map__barrett2015"),
  reg("vasopressin__healthy__baumann1976", "vasopressin",
      "hydrated normal subjects (tracer)", "arginine vasopressin", "plasma",
      "no_suitable_model", NA, "abstract",
      "No compartmental population model of IV vasopressin in humans was found: a metabolic clearance (4.1 mL/min/kg) and half-life (24 min) in volunteers, and the label's 9-25 mL/min/kg in vasodilatory shock.  Dosed in units, endogenous, and with no reachable units-to-mass conversion.",
      NA,
      "Baumann G, Dingman JF. Distribution, blood transport, and degradation of antidiuretic hormone in man. J Clin Invest 1976;57:1109-1116. PMID 1262458, PMC436762.",
      doi("10.1172/JCI108377")),
  reg("ephedrine__oral__csajka2005", "ephedrine",
      "oral only", "ephedrine", "plasma",
      "no_suitable_model", NA, "abstract",
      "No human intravenous pharmacokinetics was found; the FDA review for the IV product (NDA 208289) itself extrapolates from oral data.  Csajka 2005 is oral, a four-compartment ephedrine-norephedrine model with saturable conversion (ephedrine CL 0.34 L/min, V 181 L).",
      NA,
      "Csajka C, Haller CA, Benowitz NL, Verotta D. Mechanistic pharmacokinetic modelling of ephedrine, norephedrine and caffeine in healthy subjects. Br J Clin Pharmacol 2005;59:335-345. PMID 15752380, PMC1884794.",
      doi("10.1111/j.1365-2125.2005.02254.x")),
  reg("hydralazine__hypertensive__ludden1980", "hydralazine",
      "8 hypertensive patients, 0.3 mg/kg, specific assay", "hydralazine", "plasma",
      "no_suitable_model", NA, "abstract",
      "CL, Vss and the terminal half-life only (Varea is implied by the other two): one number short of a two-compartment model, and the paper reports no distribution phase, so its full text would not close it either.",
      NA,
      "Ludden TM, Shepherd AM, McNay JL, Lin MS. Hydralazine kinetics in hypertensive patients after intravenous administration. Clin Pharmacol Ther 1980;28:736-742. PMID 7438689.",
      doi("10.1038/clpt.1980.229"), "hydralazine__injection__iv__hypertensive_adults__concentration__ludden1980"),
  reg("adenosine__healthy__blardi1993", "adenosine",
      "healthy volunteers, 1-min infusions", "adenosine", "plasma",
      "no_suitable_model", NA, "abstract",
      "Whole-blood half-life of seconds (red-cell and endothelial uptake), and a clearance that falls with dose (10.7 to 4.1 L/min): neither linear nor on the engine's time scale.",
      NA,
      "Blardi P et al. Pharmacokinetics of exogenous adenosine in man after infusion. Eur J Clin Pharmacol 1993;44:505-507. PMID 8359193; Moser GH, Schrader J, Deussen A. Turnover of adenosine in plasma of human and dog blood. Am J Physiol 1989;256:C799-C806 (doi 10.1152/ajpcell.1989.256.4.C799).",
      doi("10.1007/BF00315554")),
  reg("sodium_bicarbonate__space__adrogue1983", "sodium bicarbonate",
      "physiology (dogs and patients)", "bicarbonate", "plasma",
      "no_suitable_model", NA, "abstract",
      "No clearance-and-volume model exists: the apparent bicarbonate space itself changes with the baseline bicarbonate and PCO2, and elimination is buffering, CO2 exhalation and renal handling.",
      NA,
      "Adrogue HJ, Brensilver J, Cohen JJ, Madias NE. Influence of steady-state alterations in acid-base equilibrium on the fate of administered bicarbonate in the dog. J Clin Invest 1983;71:867-883. PMID 6300190; and Am J Nephrol 2020 (PMID 32069452).",
      doi("10.1172/jci110841"))
)

stopifnot(all(registry$readiness %in% READINESS),
          all(registry$evidence_read %in% EVIDENCE),
          !any(duplicated(registry$model_id)))

par <- function(drug, parameter, value, units, provenance, evidence, source_url)
  data.frame(drug, parameter, value = as.character(value), units, provenance,
             evidence, source_url, stringsAsFactors = FALSE)

AB  <- doi("10.1186/cc7972")
DU  <- doi("10.1002/jcph.1328")
KA  <- pubmed(7203731)
HO  <- doi("10.1097/00007691-198804000-00006")
AR  <- doi("10.1111/j.1365-2125.1983.tb02182.x")
LI  <- doi("10.1007/s40262-024-01430-y")
GS  <- doi("10.2165/00003088-198409060-00004")
DB  <- doi("10.1097/00000542-198703000-00010")
MG  <- doi("10.1097/00000542-200002000-00013")
HI  <- doi("10.1002/jps.2600740702")
FA  <- doi("10.1111/j.1365-2125.1982.tb01424.x")
YU  <- doi("10.2165/00003088-200140100-00005")

parameters <- rbind(
  par("epinephrine", "CL", 127, "L/h at 70 kg, SAPS II 50", "estimated", "full_text", AB),
  par("epinephrine", "BW exponent on CL", 0.60, "(BW/70)^0.60", "estimated", "full_text", AB),
  par("epinephrine", "SAPS II exponent on CL", -0.67, "(SAPS II/50)^-0.67; held at 50", "estimated", "full_text", AB),
  par("epinephrine", "V", 7.9, "L", "estimated", "full_text", AB),
  par("epinephrine", "R0", 43.5, "nmol/h endogenous input (not plotted)", "estimated", "full_text", AB),

  par("magnesiumSulfate", "CL", 3.72, "L/h at 85 kg, creatinine 0.8 mg/dL", "estimated", "full_text", DU),
  par("magnesiumSulfate", "creatinine exponent on CL", -0.731, "(Cr/0.8)^-0.731", "estimated", "full_text", DU),
  par("magnesiumSulfate", "Vc", 15.4, "L at 85 kg (x WT/85)", "estimated", "full_text", DU),
  par("magnesiumSulfate", "Q", 3.66, "L/h at 85 kg (x (WT/85)^0.75)", "estimated", "full_text", DU),
  par("magnesiumSulfate", "Vp", 17.0, "L at 85 kg (x WT/85)", "estimated", "full_text", DU),
  par("magnesiumSulfate", "weight exponents", "0.75 (CL, Q); 1 (Vc, Vp)", "fixed", "fixed", "full_text", DU),
  par("magnesiumSulfate", "MgSO4.7H2O per mol Mg", 246.47, "g/mol", "derived", "full_text", DU),

  par("labetalol", "CL", 24.80, "mL/min/kg", "estimated", "abstract", KA),
  par("labetalol", "V1 (Vdc)", 1.1, "L/kg", "estimated", "abstract", KA),
  par("labetalol", "distribution half-life", 4.9, "min", "estimated", "abstract", KA),
  par("labetalol", "elimination half-life", 4.9, "h", "estimated", "abstract", KA),
  par("labetalol", "Vss (check only)", 9.41, "L/kg", "estimated", "abstract", KA),
  par("labetalol", "V2", 7.918, "L/kg", "derived", "abstract", KA),
  par("labetalol", "Q", 0.1171, "L/min/kg", "derived", "abstract", KA),

  par("diltiazem", "CL", 992, "mL/min", "estimated", "abstract", HO),
  par("diltiazem", "V1", 119, "L", "estimated", "abstract", HO),
  par("diltiazem", "alpha half-life", 0.40, "h", "estimated", "abstract", HO),
  par("diltiazem", "beta half-life", 2.77, "h", "estimated", "abstract", HO),
  par("diltiazem", "Vss (check only)", 209, "L", "estimated", "abstract", HO),
  par("diltiazem", "V2", 84.6, "L", "derived", "abstract", HO),
  par("diltiazem", "Q", 1.222, "L/min", "derived", "abstract", HO),

  par("norepinephrine", "CL", 2.1, "L/min at 70 kg, age 35, awake (x (BW/70)^0.75)", "estimated", "full_text", LI),
  par("norepinephrine", "Vc", 2.4, "L at 70 kg (x BW/70)", "estimated", "full_text", LI),
  par("norepinephrine", "Q", 0.6, "L/min at 70 kg (x (BW/70)^0.75)", "estimated", "full_text", LI),
  par("norepinephrine", "Vp", 3.6, "L at 70 kg (x BW/70)", "estimated", "full_text", LI),
  par("norepinephrine", "Prod", 497.7, "ng/min endogenous production (not plotted)", "estimated", "full_text", LI),
  par("norepinephrine", "AGE~CL", -0.344, "exp(-0.344/100 (age - 35))", "estimated", "full_text", LI),
  par("norepinephrine", "CPROP~CL", -3.57, "exp(-3.57/100 propofol mcg/mL); held at no propofol", "estimated", "full_text", LI),
  par("norepinephrine", "lag time", 13.7, "s (not represented)", "estimated", "full_text", LI),

  par("procainamide", "CL_R", 14.4, "L/h, 70 kg, x renal fraction (Eq. 7)", "estimated", "full_text", GS),
  par("procainamide", "CL_A", 10.1, "L/h, 70 kg (acetylation)", "estimated", "full_text", GS),
  par("procainamide", "CL_O", 1.2, "L/h, 70 kg (other metabolism)", "estimated", "full_text", GS),
  par("procainamide", "Vd", 136.0, "L, 70 kg", "estimated", "full_text", GS),
  par("procainamide", "CHF on CL_R and CL_A", 0.11, "fractional fall; not applied", "estimated", "full_text", GS),
  par("procainamide", "base per hydrochloride", 0.8658, "235.33/271.79", "derived", "full_text", GS),

  par("esmolol", "CL", 128, "mL/kg/min", "estimated", "full_text", DB),
  par("esmolol", "Vc", 0.413, "L/kg", "estimated", "full_text", DB),
  par("esmolol", "distribution half-life", 1.34, "min", "estimated", "full_text", DB),
  par("esmolol", "elimination half-life", 9.9, "min", "estimated", "full_text", DB),
  par("esmolol", "V-beta (check only)", 1.90, "L/kg", "estimated", "full_text", DB),
  par("esmolol", "V2", 0.567, "L/kg", "derived", "full_text", DB),
  par("esmolol", "Q", 0.0663, "L/min/kg", "derived", "full_text", DB),

  par("dopamine", "k10", 1.10, "/min", "estimated", "full_text", MG),
  par("dopamine", "k12", 0.244, "/min", "estimated", "full_text", MG),
  par("dopamine", "k21", 0.070, "/min", "estimated", "full_text", MG),
  par("dopamine", "V1", 0.241, "L/kg", "estimated", "full_text", MG),
  par("dopamine", "lag", 0.508, "min (not represented)", "estimated", "full_text", MG),
  par("dopamine", "CL", 0.265, "L/kg/min (Cl10)", "derived", "full_text", MG),
  par("dopamine", "V2", 0.840, "L/kg", "derived", "full_text", MG),

  par("atropine", "V_P", 19.1, "L", "estimated", "full_text", HI),
  par("atropine", "CL", 1175, "mL/min", "estimated", "full_text", HI),
  par("atropine", "k_PT", 0.560, "/min", "estimated", "full_text", HI),
  par("atropine", "k_TP", 0.0557, "/min", "estimated", "full_text", HI),
  par("atropine", "Vdss (check only)", 210, "L", "estimated", "full_text", HI),
  par("atropine", "V2", 192.0, "L", "derived", "full_text", HI),
  par("atropine", "base per sulfate", 0.8329, "2 x 289.37/694.83", "derived", "full_text", HI),

  par("propranolol", "P", 317, "ng/mL per 3.8 mg (bolus-equivalent)", "estimated", "full_text", FA),
  par("propranolol", "A", 17.2, "ng/mL per 3.8 mg", "estimated", "full_text", FA),
  par("propranolol", "B", 9.3, "ng/mL per 3.8 mg", "estimated", "full_text", FA),
  par("propranolol", "pi", 0.856, "/min", "estimated", "full_text", FA),
  par("propranolol", "alpha", 0.0548, "/min", "estimated", "full_text", FA),
  par("propranolol", "beta", 0.203, "/h", "estimated", "full_text", FA),
  par("propranolol", "V1", 11.06, "L", "derived", "full_text", FA),
  par("propranolol", "CL", 1.107, "L/min", "derived", "full_text", FA),

  par("digoxin", "CL/F per weight", 0.036, "L/h per kg total body weight", "estimated", "full_text", YU),
  par("digoxin", "CL/F per CLcr", 0.112, "L/h per mL/min (Cockcroft-Gault)", "estimated", "full_text", YU),
  par("digoxin", "V1/F", 1.83, "L/kg", "estimated", "full_text", YU),
  par("digoxin", "Q/F", 0.629, "L/h/kg", "estimated", "full_text", YU),
  par("digoxin", "V2/F", 22.6, "L/kg", "estimated", "full_text", YU),
  par("digoxin", "spironolactone on CL", 0.770, "factor; held at none", "estimated", "full_text", YU),
  par("digoxin", "calcium antagonist on CL", 0.784, "factor; held at none", "estimated", "full_text", YU),

  par("nitroglycerin", "CL", 6.2, "L/min", "estimated", "abstract", AR),
  par("nitroglycerin", "time constant", 8.1, "min (V/CL)", "estimated", "abstract", AR),
  par("nitroglycerin", "V", 50.2, "L", "derived", "abstract", AR)
)

utils::write.csv(registry, "inst/extdata/vasoactiveRegistry.csv", row.names = FALSE)
utils::write.csv(parameters, "inst/extdata/vasoactiveParameters.csv", row.names = FALSE)
message("Wrote ", nrow(registry), " registry rows and ", nrow(parameters),
        " parameter rows.")
