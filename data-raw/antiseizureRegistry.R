# -----------------------------------------------------------------------------
# Build inst/extdata/antiseizureRegistry.csv and antiseizureParameters.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
#
# The registry is the machine-readable audit the antiseizure specification
# asks for: one row per FDA-approved, currently US-available antiseizure
# ingredient or prodrug (37 entries, valproate counted once and fosphenytoin
# as a prodrug route of phenytoin), each with its US product status, route and
# formulation, structural evidence tier, dose basis, analyte and a clickable
# source.  A drug is marked implemented only where a complete, validated
# population model is in the library (R/drugs_<name>.R, a row in
# drugDefaults_global.csv, a unit test and a help page); everything else is an
# implementation queue entry with an explicit status, never a plausible curve.
#
# The parameter audit lists every numeric field of the implemented models with
# its value, units, reference population, source URL and provenance (whether
# estimated, fixed, label-derived, guideline-derived or a calibration).
#
# Run with: Rscript data-raw/antiseizureRegistry.R
# Checked by tests/testthat/test-antiseizure-registry.R.
# -----------------------------------------------------------------------------

# Status vocabulary (specification):
#   US_SEIZURE_LABELED              an FDA seizure indication, US-marketed
#   US_MARKETED_SEIZURE_OFF_LABEL   US-marketed, but not for seizures
#   HISTORICAL_UNAVAILABLE          approved once, not currently US-available
#   PK_UNAVAILABLE                  label states PK inadequately characterised
# Evidence tier:
#   published_population_fit        a published population model is implemented
#   label_anchor                    only label/review anchors, no full fit
#   proposed_unestimated            a structure is proposed, constants missing
# PD status:
#   pk_only                         concentration only
#   pd_source_incomplete            a PD study exists, equation not transcribed
#   no_pk                           no PK at all (Acthar)

reg <- function(moiety, routes, status, tier, dose_basis, analyte,
                structural_model, pd_status, stanpumpr_drug, source_url, notes)
  data.frame(moiety, routes, us_status = status, evidence_tier = tier,
             dose_basis, analyte, structural_model, pd_status,
             stanpumpr_drug, source_url, notes, stringsAsFactors = FALSE)

L  <- "US_SEIZURE_LABELED"
OL <- "US_MARKETED_SEIZURE_OFF_LABEL"
HU <- "HISTORICAL_UNAVAILABLE"
PU <- "PK_UNAVAILABLE"
FIT <- "published_population_fit"
LAB <- "label_anchor"
PRO <- "proposed_unestimated"

registry <- rbind(
  reg("Acetazolamide", "oral IR, IV (ER off-label)", L, PRO,
      "acetazolamide base", "acetazolamide (total plasma)",
      "renal linear 1C (unestimated for epilepsy)", "pk_only", NA,
      "https://pubmed.ncbi.nlm.nih.gov/23683608/",
      "Yano 1998 oral popPK (intraocular-pressure PD) a candidate; epilepsy constants unestimated."),
  reg("Brivaracetam", "oral tablet/solution, IV", L, PRO,
      "brivaracetam base", "brivaracetam (apparent, oral)",
      "1C first-order, LBW allometric", "pd_source_incomplete", NA,
      "https://doi.org/10.1007/s00228-017-2230-6",
      "Schoemaker 2017 CL/F 3.63 L/h at LBW 50 kg; V/F and ka not retrievable. VPA lowers CL ~10% (handoff sign error)."),
  reg("Cannabidiol", "oral solution (Epidiolex)", L, PRO,
      "cannabidiol base", "cannabidiol (apparent, oral)",
      "1C transit absorption", "pk_only", NA,
      "https://doi.org/10.1111/epi.18255",
      "2025 paediatric CL/F 143.5 L/h, V/F 1892 L; transit counts, exponents, food F not retrievable."),
  reg("Carbamazepine", "oral IR/ER, suspension (no current IV)", L, FIT,
      "carbamazepine base", "carbamazepine (apparent, oral)",
      "1C first-order, chronic autoinduced state", "pk_only", "carbamazepine",
      "https://pubmed.ncbi.nlm.nih.gov/9545146/",
      "Graves 1998 maintenance model implemented; epoxide and single-dose naive state not modelled; Carnexiv IV withdrawn 2026."),
  reg("Cenobamate", "oral tablet", L, PRO,
      "cenobamate base", "cenobamate (apparent, oral)",
      "2C first-order with lag; dose-dependent (nonlinear) CL", "pk_only", NA,
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=565c2126-57ae-4e29-b443-723bbe7e2072",
      "Nonlinear CL (1.4 to 0.4 L/h with dose); handoff poster values unconfirmed; a single linear CL is only locally valid."),
  reg("Clobazam", "oral tablet/suspension/film", L, PRO,
      "clobazam + N-desmethylclobazam", "clobazam and N-desmethylclobazam",
      "tandem 1C parent + metabolite; CYP2C19 on metabolite", "pk_only", NA,
      "https://doi.org/10.3390/pharmaceutics17070813",
      "Tuo 2025 CL/F 5.66 L/h; metabolite V is model-conditional (1.84 L), CYP2C19 exponents in stripped table."),
  reg("Clonazepam", "oral tablet/ODT", L, FIT,
      "clonazepam base", "clonazepam (apparent, oral)",
      "2C first-order with lag", "pk_only", "clonazepam",
      "https://doi.org/10.1097/FTD.0b013e3181b9359b",
      "Already in the library (dos Santos 2009), oral only."),
  reg("Repository corticotropin (Acthar Gel)", "IM/SC depot (infantile spasms)", PU, PRO,
      "corticotropin (ACTH peptides)", "not characterised",
      "none (depot PK inadequately characterised)", "no_pk", NA,
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=7b48ddec-e815-45f4-9ca0-5c0daaf56f30",
      "PK_UNAVAILABLE: a peptide mixture dosed in units whose effect runs through cortisol; no small-molecule PK."),
  reg("Diazepam", "oral, IV/IM, rectal gel, intranasal (Valtoco)", L, FIT,
      "diazepam base", "diazepam (plasma)",
      "see library model", "pk_only", "diazepam",
      "https://doi.org/10.1111/epi.17249",
      "Already in the library (oral and IM). Intranasal Valtoco popPK typical values not retrievable; rectal has no engine route."),
  reg("Valproate (valproic acid/divalproex)", "oral IR/DR/ER, IV", L, FIT,
      "valproate base (valproic acid equivalents)", "valproate (total plasma)",
      "1C first-order, three oral inputs + IV", "pd_source_incomplete", "valproate",
      "https://doi.org/10.3390/pharmaceutics14040811",
      "Teixeira-da-Silva 2022 implemented, weight only; saturable binding and inducer/age terms not applied."),
  reg("Eslicarbazepine acetate", "oral tablet", L, FIT,
      "eslicarbazepine acetate (prodrug)", "eslicarbazepine (apparent, oral)",
      "1C first-order, active moiety", "pd_source_incomplete", "eslicarbazepine",
      "https://doi.org/10.2165/11596290-000000000-00000",
      "Falcao 2012 + poster implemented; acetate-to-moiety mass factor 0.858; inducer term not applied."),
  reg("Ethosuximide", "oral capsule/syrup", L, FIT,
      "ethosuximide base", "ethosuximide (apparent, oral)",
      "2C first-order", "pd_source_incomplete", "ethosuximide",
      "https://doi.org/10.1002/prp2.1032",
      "Diezi 2023 adult model implemented with FFM scaling; paediatric CAE model (Mizuno 2023) not retrievable."),
  reg("Everolimus", "tablets for oral suspension (TSC seizures)", L, PRO,
      "everolimus base", "everolimus (whole blood)",
      "2C first-order (Combes 2018)", "pd_source_incomplete", NA,
      "https://doi.org/10.1007/s10928-018-9600-2",
      "531-patient TSC model typical values not retrievable; whole-blood trough target 5-15 ng/mL; Kim 2023 is an implementable alternative."),
  reg("Felbamate", "oral tablet/suspension", L, PRO,
      "felbamate base", "felbamate (apparent, oral)",
      "1C first-order", "pk_only", NA,
      "https://doi.org/10.1002/j.1875-9114.1989.tb04151.x",
      "Graves 1989 CL 2.43 L/h, V 51 L, but no published absorption rate; ~30% renal."),
  reg("Fenfluramine", "oral solution (Fintepla)", L, PRO,
      "fenfluramine + norfenfluramine", "fenfluramine and norfenfluramine",
      "joint 2C+2C with presystemic metabolite", "pk_only", NA,
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=e88f360e-33ad-4cd6-b2de-5ef885857c5d&type=display",
      "Label CL/F 24.8 L/h, Vz/F 11.9 L/kg; joint model fixed effects not published. Paroxetine DDI parent AUC x1.81 is the CYP2D6 validation scenario, not a genotype coefficient."),
  reg("Fosphenytoin", "IV/IM prodrug (phenytoin sodium equivalents)", L, FIT,
      "fosphenytoin (prodrug, PE)", "phenytoin (total plasma)",
      "conversion compartment feeding phenytoin MM", "pk_only", "phenytoin",
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=d4c36fad-0ba2-4cd4-9c5e-dcf843f38a5a",
      "Implemented as a route of phenytoin (mg PE), 15-min conversion half-life, IM ka 2.47/h."),

  reg("Gabapentin", "oral IR (Neurontin); Gralise ER off-label", L, FIT,
      "gabapentin base", "gabapentin (plasma)",
      "1C renal, saturable absorption", "pk_only", "gabapentin",
      "https://doi.org/10.1007/s10928-017-9549-6",
      "Already in the library (Tran 2017, re-anchored); IR only."),
  reg("Ganaxolone", "oral suspension (Ztalmy)", L, PRO,
      "ganaxolone base", "ganaxolone (apparent, oral)",
      "2C first-order with lag, capped absorption", "pk_only", NA,
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2022/215904Orig1s000ClinPharmR.pdf",
      "FDA challenged the paediatric predictive checks; full model not release-ready; UK F~13% is non-US."),
  reg("Lacosamide", "oral tablet/solution, IV", L, FIT,
      "lacosamide base", "lacosamide (apparent, oral; IV 1:1)",
      "1C first-order, renal and sex covariates", "pd_source_incomplete", "lacosamide",
      "https://doi.org/10.1186/s40360-026-01114-2",
      "2026 model implemented; V and ka fixed from the literature; 12-h-AUC PD coefficients not retrievable."),
  reg("Lamotrigine", "oral IR/chewable/ODT; Lamictal XR", L, FIT,
      "lamotrigine base", "lamotrigine (apparent, oral)",
      "1C first-order (IR)", "pk_only", "lamotrigine",
      "https://doi.org/10.1111/bcp.12984",
      "Milosheska 2016 base model implemented; mixed co-medication not applied; XR not offered."),
  reg("Levetiracetam", "oral IR/ER, IV", L, FIT,
      "levetiracetam base", "levetiracetam (oral ~absolute; IV 1:1)",
      "1C first-order, renal/non-renal clearance split", "pk_only", "levetiracetam",
      "https://doi.org/10.1016/j.eplepsyres.2017.02.011",
      "Rhee 2017 structure with a 66/34 renal split (reduction); ER not offered."),
  reg("Lorazepam", "IV (status); oral off-label", L, FIT,
      "lorazepam base", "lorazepam (plasma)",
      "see library model", "pk_only", "lorazepam",
      "https://doi.org/10.1007/s40262-016-0486-0",
      "Already in the library (oral/IM adult). Paediatric status IV model (Gonzalez 2017) exponents not retrievable."),
  reg("Methsuximide", "oral capsule (Celontin)", L, PRO,
      "methsuximide + N-desmethylmethsuximide", "N-desmethylmethsuximide",
      "parent -> active metabolite (half-lives only)", "pk_only", NA,
      "https://dailymed.nlm.nih.gov/dailymed/lookup.cfm?setid=64a6ee88-c6b1-4e13-8208-b6772ef65a74",
      "No modern population parameter set; only half-lives known (parent ~1.4 h, metabolite 28-80 h)."),
  reg("Midazolam", "intranasal (Nayzilam), IM (Seizalam); IV off-label for status", L, FIT,
      "midazolam base", "midazolam (plasma)",
      "see library model", "pk_only", "midazolam",
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=2b29422e-54d5-4a49-8522-e9cf752368c3&type=display",
      "Already in the library (IV). Nayzilam F~0.44 (not the experimental 0.80); no nasal popPK ka retrievable."),
  reg("Oxcarbazepine", "oral IR (Trileptal); Oxtellar XR", L, PRO,
      "oxcarbazepine + MHD (eslicarbazepine/licarbazepine)", "MHD (monohydroxy derivative)",
      "parent 2C + MHD 1C with back-conversion", "pd_source_incomplete", NA,
      "https://doi.org/10.1111/bcp.13392",
      "Rodrigues 2017 parent/MHD; MHD is the active analyte; back-conversion the engine cannot represent."),

  reg("Pentobarbital", "IV/IM (status/coma)", L, FIT,
      "pentobarbital base", "pentobarbital (plasma)",
      "2C allometric, IV", "pk_only", "pentobarbital",
      "https://doi.org/10.1002/jcph.70204",
      "2026 paediatric IV model implemented; adults extrapolated."),
  reg("Perampanel", "oral tablet/suspension", L, PRO,
      "perampanel base", "perampanel (apparent, oral)",
      "1C first-order", "pd_source_incomplete", NA,
      "https://doi.org/10.1111/ane.12874",
      "Takenaka 2018 CL/F 0.668 L/h; V/F and ka not published (fix V/F ~43.5 L); inducer multipliers available."),
  reg("Phenobarbital", "oral IR, IV/IM", L, FIT,
      "phenobarbital base", "phenobarbital (plasma)",
      "1C first-order, ideal-body-weight allometric", "pk_only", "phenobarbital",
      "https://doi.org/10.1111/epi.18517",
      "Munich 2025 status model implemented; adults, children extrapolated below 18 y."),
  reg("Phenytoin", "oral ER/suspension/chewable, IV sodium", L, FIT,
      "phenytoin base (sodium 0.92)", "phenytoin (total plasma)",
      "1C Michaelis-Menten (numerical engine)", "pk_only", "phenytoin",
      "https://doi.org/10.1248/bpb.19.444",
      "Odani 1996 total-concentration MM implemented; CYP2C9 Vmax multipliers; free-drug not reported."),
  reg("Pregabalin", "oral IR (Lyrica); Lyrica CR off-label", L, FIT,
      "pregabalin base", "pregabalin (apparent, oral)",
      "1C renal, first-order with lag", "pd_source_incomplete", "pregabalin",
      "https://doi.org/10.1002/cpt.2132",
      "Already in the library (Chan 2021); IR only; focal-seizure Emax/EC50 exists but is not plotted."),
  reg("Primidone", "oral IR tablet", L, PRO,
      "primidone + phenobarbital + PEMA", "primidone and derived phenobarbital",
      "parent -> phenobarbital + PEMA (formation fractions unknown)", "pk_only", NA,
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=93b4b34c-6dba-4aec-b98e-b2b18feb86a9",
      "No identified modern joint model; ~15-25% to phenobarbital from secondary sources only."),
  reg("Rufinamide", "oral tablet/suspension", L, PRO,
      "rufinamide base", "rufinamide (apparent, oral)",
      "1C first-order, BSA-scaled, dose-dependent F", "pd_source_incomplete", NA,
      "https://doi.org/10.1007/s10928-009-9146-4",
      "Marchand 2010 popPK parameters not retrievable; Arzimanoglou 2016 CL/F 2.19 L/h (paediatric); VPA lowers CL."),
  reg("Stiripentol", "oral capsule/powder (Diacomit)", L, PRO,
      "stiripentol base", "stiripentol (apparent, oral)",
      "1C zero-order absorption, dose-dependent CL", "pk_only", NA,
      "https://doi.org/10.1007/s40262-017-0592-7",
      "Peigne 2017 CL/F 4.2 L/h at reference; zero-order absorption and auto-inhibition the linear engine cannot hold."),
  reg("Tiagabine", "oral IR tablet", L, FIT,
      "tiagabine base", "tiagabine (apparent, oral)",
      "1C first-order", "pk_only", "tiagabine",
      "https://doi.org/10.1016/s0928-0987(00)00109-3",
      "Ingwersen 2000 monotherapy implemented; height covariate replaced by FFM scaling."),
  reg("Topiramate", "oral IR/sprinkle; ER capsules", L, FIT,
      "topiramate base", "topiramate (absolute; oral F 1)",
      "3C linear, allometric", "pk_only", "topiramate",
      "https://doi.org/10.1002/jcph.70191",
      "Bamgboye 2026 IV-anchored model implemented; ER not offered; dose and inducer terms not applied."),
  reg("Vigabatrin", "oral tablet/powder", L, PRO,
      "vigabatrin (S(+) enantiomer active)", "vigabatrin S(+) (apparent, oral)",
      "racemate 2C + 5 transit; S(+) 1C zero-order", "pk_only", NA,
      "https://doi.org/10.1002/jcph.1309",
      "Ounissi 2019 S(+) CL/F 2.36 L/h; zero-order absorption and irreversible GABA-transaminase inhibition limit plasma modelling."),
  reg("Zonisamide", "oral capsule/suspension", L, FIT,
      "zonisamide base", "zonisamide (apparent, oral)",
      "1C first-order", "pk_only", "zonisamide",
      "https://doi.org/10.1016/j.ejps.2025.107023",
      "Silva 2025 implemented; refractory cohort, inducer load not applied; mild red-cell binding nonlinearity not modelled.")
)

outdir <- file.path("inst", "extdata")
utils::write.csv(registry, file.path(outdir, "antiseizureRegistry.csv"),
                 row.names = FALSE, na = "NA")
cat("Wrote", nrow(registry), "registry entries\n")

# ---------------------------------------------------------------------------
# Parameter audit for the implemented models
# ---------------------------------------------------------------------------
par <- function(drug, parameter, value, units, provenance, population, source_url)
  data.frame(drug, parameter, value, units, provenance, population, source_url,
             stringsAsFactors = FALSE)

E <- "estimated"; FX <- "fixed"; LD <- "label-derived"; GD <- "guideline-derived"
CA <- "calibration"; DR <- "derived"

odani <- "https://doi.org/10.1248/bpb.19.444"
parameters <- rbind(
  par("phenytoin", "V", "1.23", "L/kg (x weight^0.463)", E, "Odani 1996, 116 Japanese epilepsy patients", odani),
  par("phenytoin", "Vmax", "9.80", "mg/day/kg (x weight^0.463)", E, "Odani 1996", odani),
  par("phenytoin", "Km", "9.19", "mg/L total", E, "Odani 1996", odani),
  par("phenytoin", "ka (ER capsule)", "0.225", "1/h", FX, "Cheng 2020, 37 adults", "https://doi.org/10.1007/s40268-020-00323-2"),
  par("phenytoin", "ka (suspension/chewable)", "2.0", "1/h", CA, "label Tmax 1.5-3 h", "https://dailymed.nlm.nih.gov/dailymed/lookup.cfm?setid=db8c69b0-4697-433e-98c7-b0b2d2c52a83"),
  par("phenytoin", "salt factor (sodium)", "0.9199", "unitless (252.27/274.25)", LD, "molecular weights", odani),
  par("phenytoin", "conversion half-life (fosphenytoin)", "15", "min", LD, "Cerebyx label; Boucher 1989 8.0 min", "https://doi.org/10.1002/jps.2600781110"),
  par("phenytoin", "ka IM (fosphenytoin)", "2.47", "1/h", E, "Boucher 1989, 10 adults", "https://doi.org/10.1002/jps.2600781110"),
  par("phenytoin", "CYP2C9 Vmax (*1/*3, *2/*2)", "0.67", "x Vmax", E, "Odani 1997, *1/*3 heterozygotes", "https://doi.org/10.1016/S0009-9236(97)90031-X"),
  par("phenytoin", "CYP2C9 Vmax (*2/*3, *3/*3)", "0.50", "x Vmax", GD, "CPIC 2021 maintenance guidance", "https://doi.org/10.1002/cpt.2008"),

  par("valproate", "CL/F", "0.646", "L/h at 70 kg (x (WT/70)^0.75)", E, "Teixeira-da-Silva 2022, 836 patients", "https://doi.org/10.3390/pharmaceutics14040811"),
  par("valproate", "V/F", "14", "L at 70 kg (x WT/70)", FX, "Teixeira-da-Silva 2022", "https://doi.org/10.3390/pharmaceutics14040811"),
  par("valproate", "ka (DR)", "0.78", "1/h", FX, "Teixeira-da-Silva 2022", "https://doi.org/10.3390/pharmaceutics14040811"),
  par("valproate", "ka (syrup)", "2.64", "1/h", FX, "Teixeira-da-Silva 2022", "https://doi.org/10.3390/pharmaceutics14040811"),
  par("valproate", "ka (ER)", "0.38", "1/h", FX, "Teixeira-da-Silva 2022", "https://doi.org/10.3390/pharmaceutics14040811"),
  par("valproate", "relative F (ER/DR)", "0.89", "unitless", LD, "Dutta 2004 meta-analysis", "https://doi.org/10.1002/bdd.420"),

  par("phenobarbital", "F", "0.96", "unitless", E, "Epilepsia 2025, 37 adults", "https://doi.org/10.1111/epi.18517"),
  par("phenobarbital", "ka", "1.9", "1/h", FX, "Epilepsia 2025", "https://doi.org/10.1111/epi.18517"),
  par("phenobarbital", "CL", "0.38", "L/h at IBW 68.8 kg (x ^0.75)", E, "Epilepsia 2025", "https://doi.org/10.1111/epi.18517"),
  par("phenobarbital", "V", "34.3", "L at IBW 68.8 kg", E, "Epilepsia 2025", "https://doi.org/10.1111/epi.18517"),

  par("pentobarbital", "CL", "5.21", "L/h at 70 kg (x ^0.75)", E, "J Clin Pharmacol 2026, children", "https://doi.org/10.1002/jcph.70204"),
  par("pentobarbital", "V1", "37.4", "L at 70 kg", E, "J Clin Pharmacol 2026", "https://doi.org/10.1002/jcph.70204"),
  par("pentobarbital", "Q", "18.1", "L/h at 70 kg (x ^0.75)", E, "J Clin Pharmacol 2026", "https://doi.org/10.1002/jcph.70204"),
  par("pentobarbital", "V2", "63.9", "L at 70 kg", E, "J Clin Pharmacol 2026", "https://doi.org/10.1002/jcph.70204"),

  par("ethosuximide", "CL/F", "0.569", "L/h", E, "Diezi 2023, 12 healthy adults", "https://doi.org/10.1002/prp2.1032"),
  par("ethosuximide", "Vc/F", "31.3", "L", E, "Diezi 2023", "https://doi.org/10.1002/prp2.1032"),
  par("ethosuximide", "Q/F", "10.2", "L/h", E, "Diezi 2023", "https://doi.org/10.1002/prp2.1032"),
  par("ethosuximide", "Vp/F", "13.9", "L", E, "Diezi 2023", "https://doi.org/10.1002/prp2.1032"),
  par("ethosuximide", "ka (syrup)", "5.59", "1/h", E, "Diezi 2023", "https://doi.org/10.1002/prp2.1032"),

  par("topiramate", "CL", "1.31", "L/h at 70 kg (x ^0.75)", E, "Bamgboye 2026, 20 adults IV", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "V1", "9.84", "L at 70 kg", E, "Bamgboye 2026", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "Q2", "197", "L/h at 70 kg (x ^0.75)", E, "Bamgboye 2026", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "V2", "39.1", "L at 70 kg", E, "Bamgboye 2026", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "Q3", "0.6", "L/h at 70 kg (x ^0.75)", E, "Bamgboye 2026", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "V3", "9.01", "L at 70 kg", E, "Bamgboye 2026", "https://doi.org/10.1002/jcph.70191"),
  par("topiramate", "ka", "2.0", "1/h", CA, "label Tmax 1 h", "https://doi.org/10.1111/epi.12134"),
  par("topiramate", "F (oral)", "1", "unitless", LD, "Clark 2013 absolute F 109%", "https://doi.org/10.1111/epi.12134"),

  par("lacosamide", "CL/F", "1.86", "L/h (x 0.875 women, x (CrCL/119)^0.311)", E, "BMC Pharmacol Toxicol 2026, 180 adults", "https://doi.org/10.1186/s40360-026-01114-2"),
  par("lacosamide", "V", "0.6", "L/kg", FX, "BMC Pharmacol Toxicol 2026", "https://doi.org/10.1186/s40360-026-01114-2"),
  par("lacosamide", "ka", "6.47", "1/h", FX, "BMC Pharmacol Toxicol 2026", "https://doi.org/10.1186/s40360-026-01114-2"),

  par("lamotrigine", "CL/F", "2.32", "L/h", E, "Milosheska 2016, 100 adults (base)", "https://doi.org/10.1111/bcp.12984"),
  par("lamotrigine", "V/F", "77.6", "L", E, "Milosheska 2016", "https://doi.org/10.1111/bcp.12984"),
  par("lamotrigine", "ka", "1.96", "1/h", E, "Milosheska 2016", "https://doi.org/10.1111/bcp.12984"),

  par("zonisamide", "CL/F", "0.761", "L/h", E, "Silva 2025, 64 adults", "https://doi.org/10.1016/j.ejps.2025.107023"),
  par("zonisamide", "V/F", "48.10", "L", E, "Silva 2025", "https://doi.org/10.1016/j.ejps.2025.107023"),
  par("zonisamide", "ka", "0.671", "1/h", E, "Silva 2025", "https://doi.org/10.1016/j.ejps.2025.107023"),

  par("tiagabine", "CL/F", "6.10", "L/h at 170 cm", E, "Ingwersen 2000, 130 patients", "https://doi.org/10.1016/s0928-0987(00)00109-3"),
  par("tiagabine", "V/F", "62.0", "L at 170 cm", E, "Ingwersen 2000", "https://doi.org/10.1016/s0928-0987(00)00109-3"),
  par("tiagabine", "ka", "1.25", "1/h", E, "Ingwersen 2000", "https://doi.org/10.1016/s0928-0987(00)00109-3"),

  par("levetiracetam", "CL (total, reference)", "3.9", "L/h", E, "Rhee 2017, 425 adults", "https://doi.org/10.1016/j.eplepsyres.2017.02.011"),
  par("levetiracetam", "renal fraction of CL", "0.66", "unitless", LD, "label/mass balance (66% unchanged)", "https://doi.org/10.1016/j.eplepsyres.2017.02.011"),
  par("levetiracetam", "V/F", "65.3", "L at 70 kg", E, "Rhee 2017", "https://doi.org/10.1016/j.eplepsyres.2017.02.011"),
  par("levetiracetam", "ka", "2.44", "1/h", FX, "Rhee 2017", "https://doi.org/10.1016/j.eplepsyres.2017.02.011"),

  par("eslicarbazepine", "CL/F", "2.43", "L/h at 70 kg (x ^0.75)", E, "Falcao 2012 / poster, 641 patients", "https://doi.org/10.2165/11596290-000000000-00000"),
  par("eslicarbazepine", "V/F", "61.3", "L at 70 kg", E, "AAPS 2013 poster", "https://doi.org/10.2165/11596290-000000000-00000"),
  par("eslicarbazepine", "ka", "2.34", "1/h", E, "AAPS 2013 poster", "https://doi.org/10.2165/11596290-000000000-00000"),
  par("eslicarbazepine", "acetate-to-moiety factor", "0.858", "unitless (254.3/296.3)", LD, "molecular weights", "https://doi.org/10.2165/11596290-000000000-00000"),

  par("carbamazepine", "CL/F intercept", "3.58", "L/h (+ 0.0134 x weight)", E, "Graves 1998, 829 adults", "https://pubmed.ncbi.nlm.nih.gov/9545146/"),
  par("carbamazepine", "CL/F weight slope", "0.0134", "L/h/kg", E, "Graves 1998", "https://pubmed.ncbi.nlm.nih.gov/9545146/"),
  par("carbamazepine", "V/F", "1.97", "L/kg", E, "Graves 1998", "https://pubmed.ncbi.nlm.nih.gov/9545146/"),
  par("carbamazepine", "ka", "0.441", "1/h", E, "Graves 1998", "https://pubmed.ncbi.nlm.nih.gov/9545146/"),
  par("carbamazepine", "age>=70 CL factor", "0.749", "x CL/F", E, "Graves 1998", "https://pubmed.ncbi.nlm.nih.gov/9545146/"),
  par("carbamazepine", "relative F (ER)", "0.89", "unitless", LD, "Tegretol-XR label", "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=2a8c7366-ce89-47ce-8123-96f26812fca0")
)

utils::write.csv(parameters, file.path(outdir, "antiseizureParameters.csv"),
                 row.names = FALSE, na = "NA")
cat("Wrote", nrow(parameters), "parameter-audit rows\n")
