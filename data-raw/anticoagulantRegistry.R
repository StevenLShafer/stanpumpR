# -----------------------------------------------------------------------------
# Build inst/extdata/anticoagulantRegistry.csv and anticoagulantParameters.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer.
#
# The registry is the machine-readable audit the perioperative-anticoagulant
# specification asks for: one row per anticoagulant model named in that
# specification, with its predicted output, route, analyte and assay, its
# readiness, the gate that must close before a pending model becomes a curve,
# and a clickable primary source.  A model is marked implemented only where a
# full, source-checked model is in the library: either its own selectable drug
# (R/drugs_<name>.R, a row in drugDefaults_global.csv, a unit test and a help
# page) or a companion overlay of one (R/alternativeModels.R, a unit test and a
# help page), such as Hamberg's warfarin model beside Xue's.  Everything else is
# a research placeholder with an explicit readiness and a named blocking issue,
# never a plausible default.  This mirrors inst/extdata/antiseizureRegistry.csv.
#
# The parameter audit lists every numeric field of the implemented models with
# its value, units, reference population, source URL and provenance.
#
# Run with: Rscript data-raw/anticoagulantRegistry.R
# Checked by tests/testthat/test-anticoagulant-registry.R.
# -----------------------------------------------------------------------------

# Readiness vocabulary (specification):
#   research_ready   a coherent source supplies enough typical parameters to
#                    implement its stated endpoint (NOT externally validated
#                    for clinical decisions): implemented in the library
#   provisional      a published model with a material unverified transcription,
#                    population limitation or supplement dependency
#   pending          numerical parameters needed for dose-to-endpoint
#                    prediction are absent
READY <- c("research_ready", "provisional", "pending")

reg <- function(model_id, drug_class, context, analyte, assay, route,
                readiness, stanpumpr_drug, blocking_issue, source_url)
  data.frame(model_id, drug_class, context, analyte, assay, route,
             readiness, stanpumpr_drug, blocking_issue, source_url,
             stringsAsFactors = FALSE)

registry <- rbind(
  # --- Implemented (research-ready; concentration or INR only) ---------------
  reg("ufh_jia_2015_cpb", "unfractionated heparin",
      "IV, cardiopulmonary bypass", "anti-IIa activity", "chromogenic anti-IIa (IU/mL)",
      "IV", "research_ready", "heparin",
      "Final-model Table 2 estimates, confirmed against the published article; no ACT fit, and no ward/low-dose or hemodilution covariate.",
      "https://doi.org/10.1186/s12967-015-0404-5"),
  reg("biva_zhang_2012_healthy", "direct thrombin inhibitor",
      "IV, young healthy volunteers", "bivalirudin plasma", "ACT (not plotted)",
      "IV", "research_ready", "bivalirudin",
      "Healthy volunteers only; no renal covariate, so PCI/CPB and renal impairment are out of range.",
      "https://doi.org/10.1038/aps.2012.37"),
  reg("apix_cirincione_2018_nvaf", "factor Xa inhibitor",
      "oral, nonvalvular atrial fibrillation", "apixaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "apixaban",
      "Indication/race/comedication/clock-time fixed to the NVAF morning-dose patient; renal term and F(dose) equation confirmed against the published supplement (no CrCl cap, fixed exponent 1).",
      "https://doi.org/10.1002/psp4.12347"),
  reg("riva_willmann_2018", "factor Xa inhibitor",
      "oral, pooled indications (AF here)", "rivaroxaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "rivaroxaban",
      "Covariate centring (medians 81 kg / 93 mL/min / 61 y) and F(dose) equation confirmed against the published NONMEM control stream; indication fixed to AF; fed dosing.",
      "https://doi.org/10.1002/psp4.12288"),
  reg("warf_xue_2017_cardiac", "vitamin K antagonist",
      "oral, cardiac surgery", "S/R-warfarin (mg/L) -> INR", "INR (theory-based 1/PCA)",
      "PO", "research_ready", "warfarin",
      "Eq.2 multiplicative C50_app is the form that reproduces the paper's worked example (the erratum, BCP 2017;83:1602, was not itself read); FFM reference 56.1 vs 56.6 kg resolved to the formula-consistent 56.1; R antagonism study-specific.",
      "https://doi.org/10.1111/bcp.13157"),
  reg("warf_hamberg_2007", "vitamin K antagonist",
      "oral", "S/R-warfarin (mg/L) -> INR", "INR (two-chain transit compartments)",
      "PO", "research_ready", "warfarin",
      "Implemented as the Hamberg companion of warfarin (Graph Options, R/alternativeModels.R), a second INR line, not a separate selectable curve; Tables 2-4 and Appendix eq 8-10 read from the typeset original (CYP2C9 and age on CL_S, VKORC1 on EC50, ktr = 1/MTT, INR = BASE + 20(1-A6 A7)^3.61 with INR_MAX fixed at 20).",
      "https://doi.org/10.1038/sj.clpt.6100084"),
  reg("edox_niebecker_2015", "factor Xa inhibitor",
      "oral, VTE (Hokusai)", "edoxaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "edoxaban",
      "Full Table 3 final-model parameters read from the paper; phase 3 above-90 renal slope is 0.0321 x (1 + 2.74) = 0.120 (the abstract's +274% increase; nlmixr2lib agrees) and Q/F x (1 + 0.646) = 9.73 (+64.6%); CLcr truncated at 150 mL/min; allometric exponents per the footnote (CL/F and Vp/F ^0.75, Vc/F and Q/F ^1); fixed to the non-Asian non-dose-reduced patient on no P-gp inhibitor.",
      "https://doi.org/10.1111/bcp.12727"),
  reg("dabi_liesenfeld_2011_rely", "direct thrombin inhibitor",
      "oral, NVAF (RE-LY)", "total active dabigatran (ng/mL)", "none plotted",
      "PO", "research_ready", "dabigatran",
      "Full Table 5 final-model parameters and equations read from the paper; Emax CrCl-CL/F reproduces the 69.6 L/h typical clearance; fixed to the non-South-Asian no-heart-failure median-haemoglobin patient on no interacting co-medication; doses as mg dabigatran etexilate.",
      "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  reg("arg_cox_2004_pci", "direct thrombin inhibitor",
      "IV, PCI", "argatroban plasma (ng/mL)", "none plotted",
      "IV", "research_ready", "argatroban",
      "Full Table II final model read from the paper (CL = 22.0 + 0.018 x (WT-87), V1 11, V2 13, Q 10); PCI patients on a GP IIb/IIIa antagonist; no hepatic covariate (under-predicts in liver disease); ACT Emax not plotted.",
      "https://doi.org/10.1177/0091270004267651"),
  reg("enox_berges_2007_elderly", "low molecular weight heparin",
      "SC, elderly prophylaxis", "anti-Xa activity (IU/mL)", "none plotted",
      "SC", "research_ready", "enoxaparin",
      "Full Table 3 final model read from the paper (ka 0.63/h; CL 0.70 x (WT/65)^0.78 x (CLcr/69)^0.25; V2 6.43 x (WT/65)^1.25; Q 0.34; V3 8.18); subcutaneous anti-Xa in IU, simplified-MDRD CLcr; elderly (>75) prophylaxis population; apparent parameters.",
      "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  reg("fonda_delavenne_2010", "factor Xa inhibitor (pentasaccharide)",
      "SC, orthopedic prophylaxis", "fondaparinux (mg/L)", "none plotted",
      "SC", "research_ready", "fondaparinux",
      "Full Table 2 / Eqs 4-5 final model read from the paper (ka 0.716/h; Cl/F 0.278 x (WT/68)^0.534 x (CLcr/87)^0.532 x (Age/74)^-0.461; V2/F 8.14 x (WT/68)^0.522; Q/F 0.082; V3/F 15.6); subcutaneous, simplified-MDRD CLcr; post-orthopedic-surgery population; apparent parameters.",
      "https://doi.org/10.1160/TH10-02-0127"),
  reg("flu_verstuyft_2012", "vitamin K antagonist",
      "oral, induction (healthy volunteers)", "fluindione (mg/L) -> INR", "INR (indirect response on 1/INR)",
      "PO", "research_ready", "fluindione",
      "Full Tables II and IV and Eq.1 read from the typeset original (apparent two-compartment PK; indirect-response INR, E = C^2.32/(C^2.32 + C50^2.32), kin 0.0385/h, kout 0.0391/h); CYP2C9 *3 on CL (x0.637), VKORC1 on C50 (x1.30 for the less-sensitive genotype); CYP1A2/smoking covariate fixed at the median (no field); single-dose healthy-volunteer fit, not a maintenance model.",
      "https://doi.org/10.2165/11595560-000000000-00000"),
  reg("aceno_verstuyft_2012", "vitamin K antagonist",
      "oral, induction (healthy volunteers)", "S-acenocoumarol (mg/L) -> INR", "INR (indirect response on 1/INR)",
      "PO", "research_ready", "acenocoumarol",
      "Full Tables III and IV and Eq.1 read from the typeset original (S-enantiomer only, half the racemic dose; apparent two-compartment PK; indirect-response INR, E = C^1.2/(C^1.2 + C50^1.2), kin 0.0269/h, kout 0.0268/h); CYP2C9 *3 on CL (x0.7), VKORC1 on C50 (x2.29 for the less-sensitive genotype); fast clearance makes the simulated steady-state INR about 20-30% from the paper's Table IV (no stated maintenance dose/definition); single-dose healthy-volunteer fit.",
      "https://doi.org/10.2165/11595560-000000000-00000"),

  # --- Provisional (published, but a material gap) ---------------------------
  reg("ufh_delavenne_2017_cpb", "unfractionated heparin",
      "IV, cardiopulmonary bypass", "anti-Xa activity -> ACT", "Hemochron Jr ACT",
      "IV", "pending", NA,
      "Numeric PK and Emax/EC50 table not available from a verified publisher source; never combine with Jia anti-IIa.",
      "https://doi.org/10.1093/bja/aex044"),
  reg("enox_barras_2009", "low molecular weight heparin",
      "SC, bleeding/bruising PD", "enoxaparin-induced bleeding/bruising", "PD (not plotted)",
      "SC", "pending", NA,
      "A bleeding/bruising occurrence-and-severity PD model; the project's rule forbids plotting a bleeding probability, so it stays documentation-only (the enoxaparin PK curve is the Berges 2007 model).",
      "https://doi.org/10.1111/j.1365-2125.2009.03518.x"),
  reg("biva_label_pci", "direct thrombin inhibitor",
      "IV, PCI", "bivalirudin plasma", "ACT/ECT",
      "IV", "pending", NA,
      "Label CL (0.204 L/h/kg) differs from the healthy-volunteer fit; no full V/Q/PD vector for PCI patients.",
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=72c5413f-882a-4784-9be6-312c0a0a2a97"),
  reg("apix_byon_2017_vte", "factor Xa inhibitor",
      "oral, VTE treatment", "apixaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "apixabanVTE",
      "Final Table 4 read from the full text; a separate VTE-treatment fit from the NVAF model. Race/comedication/clock-time fixed to the non-Asian morning-dose patient; Cockcroft-Gault renal term with a 150 mL/min cap; the >10 mg dose-dependent F (parameters not printed) is not applied, so doses >10 mg over-predict; the anti-Xa map (0.0159 IU/mL per ng/mL, one reagent) is documentation only.",
      "https://doi.org/10.1002/psp4.12184"),
  reg("dabi_troconiz_2007_hip", "direct thrombin inhibitor",
      "oral, post-hip-replacement surgery", "total active dabigatran (ng/mL)", "none plotted",
      "PO", "research_ready", "dabigatranHip",
      "Full Tables III-IV read from the typeset original; time-varying apparent PK (lower absorption and clearance on days 0-1 than days 2-10), gastrin fixed at the median (no field), Cockcroft-Gault CrCl and serum creatinine and age on the later phase; dose-time phase split at 24 h because the surgery-relative breakpoint is not a simulator input; aPTT/ECT PD not plotted; doses as mg dabigatran etexilate.",
      "https://doi.org/10.1177/0091270006297228"),
  # --- Pending: label-only or specialty extensions, no portable fit ----------
  reg("dalteparin_label", "low molecular weight heparin",
      "SC", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "No general adult population ka/CL/V/Q vector; label PK is a validation envelope only.",
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=23527b8b-9b28-4e6d-9751-33b143975ac7"),
  reg("tinzaparin_label", "low molecular weight heparin",
      "SC", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "Barrett abstract gives CL/F and Vc/F but not ka, Q or Vp; no full population fit.",
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=d3e5fbbf-4a1f-4aa2-8e80-9729f6cf04ee"),
  reg("phenprocoumon", "vitamin K antagonist",
      "oral", "phenprocoumon -> INR", "INR",
      "PO", "pending", NA,
      "Steady-state genotype CL only; no validated stop/restart dynamic model.",
      "https://pubmed.ncbi.nlm.nih.gov/23519598/"),
  reg("nadroparin", "low molecular weight heparin",
      "SC", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "No drug-specific dynamic fit; do not alias another LMWH.",
      "https://www.ema.europa.eu/en/documents/psusa/nadroparin-list-nationally-authorised-medicinal-products-psusa00002104201803_en.pdf"),
  reg("bemiparin", "low molecular weight heparin",
      "SC", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "No drug-specific dynamic fit; jurisdiction metadata only.",
      "https://cima.aemps.es/cima/dochtml/ft/65673/FT_65673.html"),
  reg("danaparoid", "heparinoid",
      "SC/IV", "anti-Xa activity (IU/mL)", "danaparoid anti-Xa",
      "SC", "pending", NA,
      "No drug-specific dynamic fit; distinct anti-Xa calibration; do not alias an LMWH or DTI.",
      "https://www.medicines.org.uk/emc/product/13201/smpc"),
  reg("desirudin", "direct thrombin inhibitor",
      "SC", "desirudin plasma", "aPTT",
      "SC", "pending", NA,
      "No drug-specific dynamic fit; do not alias bivalirudin or argatroban.",
      "https://www.accessdata.fda.gov/drugsatfda_docs/label/2014/021271s006lbl.pdf")
)
stopifnot(all(registry$readiness %in% READY))

# The parameter audit for the implemented (research-ready) models.
par <- function(drug, parameter, value, units, provenance, population, source_url)
  data.frame(drug, parameter, value = as.character(value), units, provenance,
             population, source_url, stringsAsFactors = FALSE)

JIA  <- "41 enrolled (32 completed) cardiac-bypass patients, Jia 2015"
ZH   <- "36 young healthy Chinese volunteers, Zhang 2012"
CIR  <- "4385 subjects (phase I-III), Cirincione 2018 NVAF reference"
BYON <- "970 subjects (phase I-III VTE), Byon 2017 reference 60 y, 85 kg, 100 mL/min"
WIL  <- "4918 patients, 7 trials, Willmann 2018 (population medians: 81 kg, 93 mL/min, 61 y)"
XUE  <- "264 cardiac-surgery patients, Xue 2017"
NIE  <- "4130 individuals (13 phase 1 + Hokusai-VTE), Niebecker 2015"
LIE  <- "9522 atrial-fibrillation patients (RE-LY), Liesenfeld 2011"
TRO  <- "287 total-hip-replacement patients, Trocóniz 2007"
COX  <- "152 PCI patients on a GP IIb/IIIa antagonist, Cox 2004"
BER  <- "189 patients over 75 (PROPHRE.75), Berges 2007"
DEL  <- "orthopaedic-surgery patients (GETHCAM/POP-A-RIX), Delavenne 2010"
HAM  <- "150 patients (Study I+II), Hamberg 2007"
VER  <- "24 healthy White subjects (single dose), Verstuyft 2012"

parameters <- rbind(
  par("heparin", "CL", 1.18, "L/h", "estimated", JIA, "https://doi.org/10.1186/s12967-015-0404-5"),
  par("heparin", "Vc", 3.04, "L", "estimated", JIA, "https://doi.org/10.1186/s12967-015-0404-5"),
  par("heparin", "Q", 0.171, "L/h", "estimated", JIA, "https://doi.org/10.1186/s12967-015-0404-5"),
  par("heparin", "Vp", 8.01, "L", "estimated", JIA, "https://doi.org/10.1186/s12967-015-0404-5"),

  par("bivalirudin", "CL", 0.323, "L/h/kg", "estimated", ZH, "https://doi.org/10.1038/aps.2012.37"),
  par("bivalirudin", "V1", 0.086, "L/kg", "estimated", ZH, "https://doi.org/10.1038/aps.2012.37"),
  par("bivalirudin", "Q", 0.0957, "L/h/kg", "estimated", ZH, "https://doi.org/10.1038/aps.2012.37"),
  par("bivalirudin", "V2", 0.0554, "L/kg", "estimated", ZH, "https://doi.org/10.1038/aps.2012.37"),

  par("apixaban", "CL_R/F", 1.57, "L/h (x CrCl/80)", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "CL_NR/F", 2.02, "L/h (x (age/65)^-0.429, x0.784 female)", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "NVAF factor on CL", 0.861, "fraction", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "Vc/F", 30.0, "L (x (WT/70)^0.790, x0.960 NVAF)", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "Q/F", 1.91, "L/h", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "Vp/F", 27.0, "L", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "ka", 0.473, "1/h", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),
  par("apixaban", "F(dose) I50, gamma", "0.420, 0.857", "logit I50 -0.322", "estimated", CIR, "https://doi.org/10.1002/psp4.12347"),

  par("apixabanVTE", "CL_R/F", 1.83, "L/h (x min(CrCl,150)/100)", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "CL_NR/F", 2.52, "L/h (x (age/60)^-0.267, x0.777 female)", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "Vc/F", 32.1, "L (x (WT/85)^0.523)", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "Q/F", 1.62, "L/h", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "Vp/F", 19.8, "L", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "ka", 0.440, "1/h (morning; 0.239 evening)", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),
  par("apixabanVTE", "anti-Xa slope", 0.0159, "IU/mL per ng/mL (documented, not plotted)", "estimated", BYON, "https://doi.org/10.1002/psp4.12184"),

  par("rivaroxaban", "CL/F", 6.58, "L/h (x (CrCl/93)^0.406 x (WT/81)^-0.278 x0.849 AF)", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),
  par("rivaroxaban", "V/F", 62.5, "L (x (WT/81)^0.216 x (age/61)^-0.189 x0.889 female)", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),
  par("rivaroxaban", "ka", 0.821, "1/h", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),
  par("rivaroxaban", "F(dose) Fmin,Fmax,D50", "0.590, 1.25, 14.4", "exp(-ln2 D/D50)", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),

  par("warfarin", "CL_S", 0.224, "L/h (x (FFM/56.1)^0.75)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "V_S", 24.0, "L (x FFM/56.1, x1.33 female)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "CL_R", 0.124, "L/h (x (FFM/56.1)^0.75)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "V_R", 16.2, "L (x FFM/56.1, x1.22 female)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "ka", 1.80, "1/h", "fixed", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "PCA0", 0.979, "fraction", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "C50_S", 0.378, "mg/L (x0.725 VKORC1 AA, x0.770 CYP4F2 C)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "Hill", 2.720, "dimensionless", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "T2PCA", 11.4, "h (turnover half-life)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "RIC50", 2.387, "mg/L (R competition)", "estimated", XUE, "https://doi.org/10.1111/bcp.13157"),
  par("warfarin", "Emax", 1, "fixed", "fixed", XUE, "https://doi.org/10.1111/bcp.13157"),

  # The Hamberg (2007) companion of warfarin (Graph Options), a second INR line
  # (R/alternativeModels.R, warfarinHamberg). S- and R-warfarin PK and a
  # two-chain transit-compartment INR model; apparent (/F) parameters, no
  # body-size covariate.
  par("warfarin (Hamberg)", "CL_S", 0.314, "L/h (x (1 - 0.0091 (age-71)) x CYP2C9 factor)", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "V1_S", 13.8, "L", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "Q_S", 0.131, "L/h", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "V2_S", 6.59, "L", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "ka", 2, "1/h (both enantiomers)", "fixed", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "CL_R", 0.139, "L/h (x (1 - 0.0098 (age-71)))", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "V_R", 12.8, "L", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "EC50", "4.61 / 3.02 / 2.20", "mg/L (VKORC1 GG / GA / AA)", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "gamma", 0.424, "dimensionless", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "MTT1", 11.6, "h (six-compartment chain; ktr = 1/MTT)", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "MTT2", 120, "h (one-compartment chain; ktr = 1/MTT)", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "lambda", 3.61, "dimensionless", "estimated", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "Emax", 1, "fixed", "fixed", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),
  par("warfarin (Hamberg)", "INR_max", 20, "fixed (INR = baseline + 20 (1 - A6 A7)^lambda)", "fixed", HAM, "https://doi.org/10.1038/sj.clpt.6100084"),

  par("edoxaban", "CL_NR/F", 15.2, "L/h (x (WT/70)^0.75)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "CL_R/F slope1", 0.202, "L/h per mL/min (CLcr<=90)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "CL_R/F slope2 (phase 3)", 0.120, "L/h per mL/min (CLcr>90, 0.0321x(1+2.74), +274%)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "CLcr cap", 150, "mL/min (CLcr truncated here)", "fixed", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "Vc/F", 209, "L (x WT/70, x1.226 Asian)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "Vp/F", 92.3, "L (x (WT/70)^0.75)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "Q/F (phase 3)", 9.73, "L/h (5.91 x1.646, +64.6%; x WT/70)", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "ka", 3.36, "1/h", "estimated", NIE, "https://doi.org/10.1111/bcp.12727"),
  par("edoxaban", "tlag", 0.250, "h", "fixed", NIE, "https://doi.org/10.1111/bcp.12727"),

  par("dabigatran", "CLmax/F", 124, "L/h (Emax; x (1-0.0041 (age-72)) x0.917 female)", "estimated", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "EC50_CrCl, power", "56.7, 1.29", "mL/min, Hill", "estimated", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "V2/F", 673, "L (x (1+0.0077 (WT-80.3)))", "estimated", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "Q/F", 35.5, "L/h", "fixed", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "V3/F", 345, "L", "fixed", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "ka", 0.754, "1/h", "fixed", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  par("dabigatran", "ALAG", 0.634, "h", "fixed", LIE, "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),

  # The Trocóniz (2007) post-hip-replacement model, a second dabigatran drug
  # (dabigatranHip).  Time-varying apparent two-compartment PK: days 0-1 and
  # days 2-10 take different CL/F and ka; gastrin fixed at its median 34.58.
  par("dabigatranHip", "CL/F (days 0-1)", 43.4, "L/h (x (1 + 0.633 GAST/34.58); 70.9 at median)", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "CL/F (days 2-10)", 82.1, "L/h (x CRCL/76.17 x (1 + 0.294 GAST/34.58); 106.2 at median)", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "V2/F (central)", 30.8, "L", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "Q/F", 13.6, "L/h", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "V3/F (peripheral)", 136, "L", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "ka (days 0-1)", 0.022, "1/h", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "ka (days 2-10)", 0.265, "1/h (x (1-0.363 SCR/0.964) x (1-0.447 AGE/66.97); 0.093 at median)", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),
  par("dabigatranHip", "ALAG1", 0.4, "h", "estimated", TRO, "https://doi.org/10.1177/0091270006297228"),

  par("argatroban", "CL", 22.0, "L/h (+ 0.018 x (WT-87))", "estimated", COX, "https://doi.org/10.1177/0091270004267651"),
  par("argatroban", "V1", 11.0, "L", "estimated", COX, "https://doi.org/10.1177/0091270004267651"),
  par("argatroban", "V2", 13.0, "L", "estimated", COX, "https://doi.org/10.1177/0091270004267651"),
  par("argatroban", "Q", 10.0, "L/h", "estimated", COX, "https://doi.org/10.1177/0091270004267651"),
  par("argatroban", "WT on CL", 0.018, "L/h per kg (linear, about 87 kg median)", "estimated", COX, "https://doi.org/10.1177/0091270004267651"),

  par("enoxaparin", "ka", 0.63, "1/h (SC)", "estimated", BER, "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  par("enoxaparin", "CL", 0.70, "L/h (x (WT/65)^0.78 x (CLcr/69)^0.25)", "estimated", BER, "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  par("enoxaparin", "V2 (central)", 6.43, "L (x (WT/65)^1.25)", "estimated", BER, "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  par("enoxaparin", "Q", 0.34, "L/h", "fixed", BER, "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  par("enoxaparin", "V3 (peripheral)", 8.18, "L", "estimated", BER, "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),

  par("fondaparinux", "Ka", 0.716, "1/h (SC)", "estimated", DEL, "https://doi.org/10.1160/TH10-02-0127"),
  par("fondaparinux", "Cl/F", 0.278, "L/h (x (WT/68)^0.534 x (CLcr/87)^0.532 x (Age/74)^-0.461)", "estimated", DEL, "https://doi.org/10.1160/TH10-02-0127"),
  par("fondaparinux", "V2/F (central)", 8.14, "L (x (WT/68)^0.522)", "estimated", DEL, "https://doi.org/10.1160/TH10-02-0127"),
  par("fondaparinux", "Q/F", 0.082, "L/h", "fixed", DEL, "https://doi.org/10.1160/TH10-02-0127"),
  par("fondaparinux", "V3/F (peripheral)", 15.6, "L", "estimated", DEL, "https://doi.org/10.1160/TH10-02-0127"),

  par("fluindione", "CL/F", 0.17, "L/h (x (WT/69)^0.75 x 0.637 CYP2C9 *3)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "V1/F", 5.19, "L (x WT/69)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "Q/F", 0.173, "L/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "V2/F", 2.77, "L", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "ka", 1.83, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "C50", 3.63, "mg/L (x1.30 VKORC1 less-sensitive)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "gamma", 2.32, "dimensionless", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "kin", 0.0385, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "kout", 0.0391, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("fluindione", "Emax", 1, "fixed", "fixed", VER, "https://doi.org/10.2165/11595560-000000000-00000"),

  par("acenocoumarol", "CL_S/F", 15.3, "L/h (x 0.7 CYP2C9 *3)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "V1_S/F", 24.7, "L (x WT/69)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "Q_S/F", 4.85, "L/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "V2_S/F", 52.5, "L", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "ka", 0.936, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "tlag", 0.269, "h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "dose fraction (S)", 0.5, "fraction of racemate", "fixed", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "C50", 0.0023, "mg/L (x2.29 VKORC1 less-sensitive)", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "gamma", 1.2, "dimensionless", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "kin", 0.0269, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "kout", 0.0268, "1/h", "estimated", VER, "https://doi.org/10.2165/11595560-000000000-00000"),
  par("acenocoumarol", "Emax", 1, "fixed", "fixed", VER, "https://doi.org/10.2165/11595560-000000000-00000")
)

utils::write.csv(registry,
  "inst/extdata/anticoagulantRegistry.csv", row.names = FALSE)
utils::write.csv(parameters,
  "inst/extdata/anticoagulantParameters.csv", row.names = FALSE)
message("Wrote ", nrow(registry), " registry rows and ", nrow(parameters),
        " parameter rows.")
