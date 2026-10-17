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
# full, source-checked model is in the library (R/drugs_<name>.R, a row in
# drugDefaults_global.csv, a unit test and a help page); everything else is a
# research placeholder with an explicit readiness and a named blocking issue,
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
      "Table 2 transcribed from a supplied specification, not re-read against the source; no ACT fit.",
      "https://doi.org/10.1186/s12967-015-0404-5"),
  reg("biva_zhang_2012_healthy", "direct thrombin inhibitor",
      "IV, young healthy volunteers", "bivalirudin plasma", "ACT (not plotted)",
      "IV", "research_ready", "bivalirudin",
      "Healthy volunteers only; no renal covariate, so PCI/CPB and renal impairment are out of range.",
      "https://doi.org/10.1038/aps.2012.37"),
  reg("apix_cirincione_2018_nvaf", "factor Xa inhibitor",
      "oral, nonvalvular atrial fibrillation", "apixaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "apixaban",
      "Indication/race/comedication/clock-time fixed to the NVAF morning-dose patient; CrCl>150 cap and F(dose) form inferred from the text (equation image not seen).",
      "https://doi.org/10.1002/psp4.12347"),
  reg("riva_willmann_2018", "factor Xa inhibitor",
      "oral, pooled indications (AF here)", "rivaroxaban plasma (ng/mL)", "none plotted",
      "PO", "research_ready", "rivaroxaban",
      "Covariate centring taken as the published means (supplement unread); F(dose) form recovered from later papers; indication fixed to AF; fed dosing.",
      "https://doi.org/10.1002/psp4.12288"),
  reg("warf_xue_2017_cardiac", "vitamin K antagonist",
      "oral, cardiac surgery", "S/R-warfarin (mg/L) -> INR", "INR (theory-based 1/PCA)",
      "PO", "research_ready", "warfarin",
      "Erratum text unread (Eq.2 form confirmed by the paper's worked example); FFM reference 56.1 vs 56.6 kg resolved to the formula-consistent 56.1; R antagonism study-specific.",
      "https://doi.org/10.1111/bcp.13157"),

  # --- Provisional (published, but a material gap) ---------------------------
  reg("ufh_delavenne_2017_cpb", "unfractionated heparin",
      "IV, cardiopulmonary bypass", "anti-Xa activity -> ACT", "Hemochron Jr ACT",
      "IV", "pending", NA,
      "Numeric PK and Emax/EC50 table not available from a verified publisher source; never combine with Jia anti-IIa.",
      "https://doi.org/10.1093/bja/aex044"),
  reg("enox_barras_2009", "low molecular weight heparin",
      "SC, adult therapeutic", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "Full-text parameter table (ka, CL/F renal+nonrenal coefficients, Q, V, LBW/CG form) not retrievable; needs an SC route and IU dosing.",
      "https://doi.org/10.1111/j.1365-2125.2009.03518.x"),
  reg("enox_berges_2007_elderly", "low molecular weight heparin",
      "SC, elderly prophylaxis", "anti-Xa activity (IU/mL)", "LMWH anti-Xa",
      "SC", "pending", NA,
      "PMC2048556 is Berges 2007 (PROPHRE.75), not Siguret; renal covariate is simplified MDRD, not CrCl; parameter table unread; needs an SC route.",
      "https://doi.org/10.1111/j.1365-2125.2007.02920.x"),
  reg("fonda_delavenne_2010", "factor Xa inhibitor (pentasaccharide)",
      "SC, orthopedic prophylaxis", "fondaparinux (mg/L)", "drug-calibrated anti-Xa",
      "SC", "pending", NA,
      "Numeric table is in an author thesis; publisher Table 2 unconfirmed; weakly identified peripheral volume; needs an SC route.",
      "https://doi.org/10.1160/TH10-02-0127"),
  reg("biva_label_pci", "direct thrombin inhibitor",
      "IV, PCI", "bivalirudin plasma", "ACT/ECT",
      "IV", "pending", NA,
      "Label CL (0.204 L/h/kg) differs from the healthy-volunteer fit; no full V/Q/PD vector for PCI patients.",
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=72c5413f-882a-4784-9be6-312c0a0a2a97"),
  reg("arg_label_pk", "direct thrombin inhibitor",
      "IV", "argatroban plasma", "aPTT/ACT",
      "IV", "pending", NA,
      "Label gives CL, half-life and Vss only; a one-compartment V must be derived; no portable Emax/EC50; published popPK full vectors (Cox 2004, Akimoto 2011) not retrievable.",
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=270dbb59-e080-4a64-91d9-a68bed4bd2e5"),
  reg("apix_byon_2017_vte", "factor Xa inhibitor",
      "oral, VTE", "apixaban plasma -> anti-Xa (IU/mL)", "Rotachrom on STA-Compact",
      "PO", "provisional", NA,
      "A separate VTE fit and assay map (0.0159 IU/mL per ng/mL); not the NVAF PK vector, and the assay slope is one reagent only.",
      "https://doi.org/10.1002/psp4.12184"),
  reg("edox_niebecker_2015", "factor Xa inhibitor",
      "oral, VTE", "edoxaban plasma (ng/mL)", "none portable",
      "PO", "pending", NA,
      "Renal slope-2 patient scaling is x3.74 (+274%), not the supplied x2.74; CrCl breakpoint, cap and weight exponents not confirmed against the paper's equations.",
      "https://doi.org/10.1111/bcp.12727"),
  reg("dabi_liesenfeld_2011_rely", "direct thrombin inhibitor",
      "oral, NVAF", "total active dabigatran (ng/mL)", "none portable",
      "PO", "pending", NA,
      "JTH full text not accessible; ka, Vc/F and the age/sex/weight/PPI coefficients conflict with the FDA review; CrCl<30 outside the fit.",
      "https://doi.org/10.1111/j.1538-7836.2011.04498.x"),
  reg("dabi_liesenfeld_2006_hip", "direct thrombin inhibitor",
      "oral, post-hip-surgery", "total active dabigatran -> aPTT/ECT", "aPTT/ECT",
      "PO", "pending", NA,
      "aPTT Emax verified from the abstract, but the post-surgery PK (Troconiz 2007) needs serum gastrin, not a routine covariate; full parameter set unread.",
      "https://doi.org/10.1111/j.1365-2125.2006.02667.x"),
  reg("warf_hamberg_2007", "vitamin K antagonist",
      "oral", "S/R-warfarin -> INR", "INR (transit chain)",
      "PO", "provisional", NA,
      "Numbers verified only against a source-traced reproducer (nlmixr2lib), not the typeset original; age functional form needs the appendix.",
      "https://doi.org/10.1038/sj.clpt.6100084"),

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
  reg("acenocoumarol", "vitamin K antagonist",
      "oral", "acenocoumarol -> INR", "INR",
      "PO", "pending", NA,
      "No complete dynamic parameter and observation set; R vs S PD driver unresolved.",
      "https://base-donnees-publique.medicaments.gouv.fr/medicament/61510352/extrait"),
  reg("phenprocoumon", "vitamin K antagonist",
      "oral", "phenprocoumon -> INR", "INR",
      "PO", "pending", NA,
      "Steady-state genotype CL only; no validated stop/restart dynamic model.",
      "https://pubmed.ncbi.nlm.nih.gov/23519598/"),
  reg("fluindione", "vitamin K antagonist",
      "oral", "fluindione -> INR", "INR",
      "PO", "pending", NA,
      "A two-compartment candidate exists but its full parameter/observation tables are inaccessible; France-specific.",
      "https://pubmed.ncbi.nlm.nih.gov/22149257/"),
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

JIA  <- "41 cardiac-bypass patients, Jia 2015"
ZH   <- "36 young healthy Chinese volunteers, Zhang 2012"
CIR  <- "4385 subjects (phase I-III), Cirincione 2018 NVAF reference"
WIL  <- "4918 patients, 7 trials, Willmann 2018 (population means)"
XUE  <- "264 cardiac-surgery patients, Xue 2017"

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

  par("rivaroxaban", "CL/F", 6.58, "L/h (x (CrCl/97.2)^0.406 x (WT/82.5)^-0.278 x0.849 AF)", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),
  par("rivaroxaban", "V/F", 62.5, "L (x (WT/82.5)^0.216 x (age/60.5)^-0.189 x0.889 female)", "estimated", WIL, "https://doi.org/10.1002/psp4.12288"),
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
  par("warfarin", "Emax", 1, "fixed", "fixed", XUE, "https://doi.org/10.1111/bcp.13157")
)

utils::write.csv(registry,
  "inst/extdata/anticoagulantRegistry.csv", row.names = FALSE)
utils::write.csv(parameters,
  "inst/extdata/anticoagulantParameters.csv", row.names = FALSE)
message("Wrote ", nrow(registry), " registry rows and ", nrow(parameters),
        " parameter rows.")
