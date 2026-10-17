# -----------------------------------------------------------------------------
# Build inst/extdata/antidiabeticRegistry.csv and antidiabeticParameters.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# the antidiabetic implementation specification (scope date 2026-10-16) after
# checking each claimed source.
#
# The registry has one row per product key: ingredient x product/formulation x
# route, as the specification asks, each with two statuses:
#
#   spec_pk_status / spec_pd_status   what the specification claimed
#   pk_status / pd_status             what this review found, after reading the
#                                     source (PubMed Central full text or the
#                                     abstract; publishers' PDFs, FDA reviews
#                                     and supplements could not be reached)
#
# A product is implemented (stanpumpr_drug not NA) only where pk_status is
# PK-FIT and the model's structure is in the engine.  Every other row is an
# implementation queue entry: no curve is borrowed from a related product.
# pd_implemented is FALSE everywhere: no glucose, HbA1c, UGE, DPP-4 or GIR
# endpoint is plotted.  csii is the product's pump permission as the
# specification states it from the FDA insulin-product summary (TRUE, FALSE,
# or NA where the specification says the label must be checked), and only
# pump-permitted implemented products offer "U/hr SC".
#
# The parameter audit gives every number of the implemented models with its
# units, provenance (estimated, fixed, derived, assumed, library), population,
# source and locator.
#
# Run with: Rscript data-raw/antidiabeticRegistry.R
# Checked by tests/testthat/test-antidiabetic-registry.R.
# -----------------------------------------------------------------------------

reg <- function(key, ingredient, product, route, tier, jurisdiction,
                spec_pk, pk, spec_pd, pd, pd_endpoint, csii, drug, verified,
                source_url, notes)
  data.frame(key, ingredient, product, route, registry_tier = tier, jurisdiction,
             spec_pk_status = spec_pk, pk_status = pk,
             spec_pd_status = spec_pd, pd_status = pd, pd_endpoint,
             pd_implemented = FALSE, csii, stanpumpr_drug = drug,
             structure_verified = verified, source_url, notes,
             stringsAsFactors = FALSE)

FIT <- "PK-FIT"; AUD <- "PK-AUDIT"; CON <- "PK-CONSTRAINT"; TPL <- "PK-TEMPLATE"
DFIT <- "PD-FIT"; DAUD <- "PD-AUDIT"; DCON <- "PD-CONSTRAINT"; DTPL <- "PD-TEMPLATE"
SC <- "SC"; PO <- "PO"
fdaRoutes <- "https://www.fda.gov/media/195052/download"

registry <- rbind(
  # --- GLP-1 receptor agonists, tirzepatide, amylin -------------------------
  reg("semaglutide_sc_ozempic_petri2018", "semaglutide", "Ozempic injection", SC, "core", "US",
      FIT, FIT, "PD-AUDIT", DAUD, "HbA1c at week 26/30 (exposure-response)", NA, "semaglutide", TRUE,
      "https://doi.org/10.1007/s13300-018-0458-5",
      "Carlsson Petri 2018, 1C, ka fixed 0.0286/h, CL/F 0.0478 L/h, V/F 12.2 L at 85 kg; weight exponent 0.78 derived from the published 55/127 kg exposures (supplement not read). Reproduces Cavg 29.8 nmol/L at 1 mg."),
  reg("semaglutide_sc_overgaard2019", "semaglutide", "SC model version (1.34 mg/mL reference)", SC, "core", "study formulation",
      FIT, AUD, "PD-AUDIT", DAUD, "none", NA, NA, FALSE,
      "https://doi.org/10.1007/s13300-019-0581-y",
      "The specification's DOI (10.1007/s40262-019-00756-2) is an oral-semaglutide DDI study. CL 0.0348 L/h is the T2D value (reference profile healthy), so x1.12 for T2D would double count; ka, Q and weight exponents not readable."),
  reg("semaglutide_sc_wegovy", "semaglutide", "Wegovy injection", SC, "core", "US",
      FIT, AUD, "PD-AUDIT", DAUD, "body weight", NA, NA, FALSE,
      "https://doi.org/10.1007/s13300-018-0458-5",
      "2.4 mg in obesity: outside the doses and population of the implemented fit; product-specific analysis required."),
  reg("semaglutide_po_rybelsus", "semaglutide", "Rybelsus tablets", PO, "core", "US",
      AUD, AUD, "PD-AUDIT", DAUD, "HbA1c at week 26", NA, NA, FALSE,
      "https://doi.org/10.1007/s40262-021-01025-x",
      "Oral F about 0.8% with fasting/water/occasion effects; the specification's F and the FDA ka come from different analyses."),
  reg("semaglutide_po_ozempic_tablets", "semaglutide", "Ozempic tablets (2026)", PO, "core", "US",
      AUD, CON, "PD-AUDIT", DAUD, "HbA1c", NA, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=27f15fac-7d98-4114-a2ec-92494a91da98",
      "Not mg-for-mg substitutable with Rybelsus; may not inherit its F."),
  reg("tirzepatide_sc_mounjaro", "tirzepatide", "Mounjaro injection", SC, "core", "US",
      FIT, FIT, "PD-AUDIT", DAUD, "FPG/HbA1c (FDA linked model)", NA, "tirzepatide", TRUE,
      "https://doi.org/10.1002/psp4.13099",
      "Schneck 2024 Table 3 verified. F 0.8 x (1 - 0.181) = 0.655 derived: reproduces the paper's 0.061 L/h apparent CL in T2D."),
  reg("tirzepatide_sc_zepbound", "tirzepatide", "Zepbound injection", SC, "core", "US",
      FIT, AUD, "PD-AUDIT", DAUD, "body weight", NA, NA, FALSE,
      "https://doi.org/10.1002/psp4.13099",
      "Separate product alias; formulation and population evidence required before it shares the Mounjaro model."),
  reg("dulaglutide_sc", "dulaglutide", "Trulicity injection", SC, "core", "US",
      FIT, AUD, "PD-AUDIT", DAUD, "FPG/HbA1c (FDA)", NA, NA, FALSE,
      "https://doi.org/10.1007/s40262-015-0338-3",
      "FDA review Table 5 not reachable; Geiser 2016 abstract: 2C, CL 0.126 L/h, half-life 5 days (not 4.7)."),
  reg("liraglutide_sc_victoza_roge2015", "liraglutide", "Victoza injection", SC, "core", "US",
      FIT, FIT, "PD-FIT", DAUD, "T2D meal-test glucose/insulin (Roge 2015)", NA, "liraglutide", TRUE,
      "https://doi.org/10.1002/psp4.11",
      "1C, V/F 0.112 L/kg, CL/F 1.83e-4 L/min/kg, ka 9.41e-4/min; mantissas read, powers of ten from the specification (stripped in the available text)."),
  reg("liraglutide_sc_watson2010", "liraglutide", "SC model version (sequential zero/first order)", SC, "core", "study formulation",
      FIT, AUD, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://doi.org/10.1177/0091270009354996",
      "Abstract only; CL/F, V/F, durations and ka not readable. The engine supports the sequential input (zeroOrderSC into depot)."),
  reg("liraglutide_sc_saxenda", "liraglutide", "Saxenda injection", SC, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "body weight", NA, NA, FALSE,
      "https://doi.org/10.1002/psp4.11",
      "3 mg in obesity: outside the implemented fit."),
  reg("exenatide_ir_sc", "exenatide", "Byetta injection", SC, "core", "US",
      AUD, AUD, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC5306477/",
      "Saturable elimination and absorption: needs a nonlinear solver; parameters not re-read here."),
  reg("exenatide_er_bydureon", "exenatide", "Bydureon microspheres (studied)", SC, "core", "US",
      AUD, AUD, "PD-CONSTRAINT", DCON, "FPG/HbA1c", NA, NA, FALSE,
      "https://d-nb.info/1123334935/34",
      "Three release paths with noninteger gamma transit; needs a convolution kernel."),
  reg("exenatide_er_bcise", "exenatide", "Bydureon BCise", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "FPG/HbA1c", NA, NA, FALSE,
      "https://www.fda.gov/media/152135/download",
      "MCT vehicle; cannot inherit the microsphere release constants without comparability audit."),
  reg("lixisenatide_sc", "lixisenatide", "Adlyxin injection", SC, "core", "US",
      FIT, AUD, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://www.scitechnol.com/2325-9604/2325-9604-2-112.pdf",
      "Frank 2013 not retrievable or indexed; dose-dependent MAT would need per-dose ka."),
  reg("pramlintide_iv_research", "pramlintide", "IV study (research)", "IV", "core", "study",
      FIT, AUD, "PD-FIT", DAUD, "T1D meal EGP", NA, NA, FALSE,
      "https://doi.org/10.1208/s12248-012-9409-7",
      "Fang 2013 abstract only (2C IV); Table II not readable."),
  reg("pramlintide_sc", "pramlintide", "Symlin injection", SC, "core", "US",
      CON, CON, "PD-AUDIT", DAUD, "gastric emptying/meal", NA, NA, FALSE,
      "https://www.accessdata.fda.gov/drugsatfda_docs/label/2015/021332s025lbl.pdf",
      "Label F 30-40%, tmax 20 min, half-life 48 min: constraints only."),
  # --- Insulins ---------------------------------------------------------------
  reg("insulin_regular_u100_sc", "human insulin", "Regular U-100 (Humulin R, Novolin R)", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinRegular", TRUE,
      "https://doi.org/10.1002/jcph.899", "Tham 2017 Table 2 typical values; dose effects on ka and CL/F not applied."),
  reg("insulin_regular_iv", "human insulin", "Regular U-100 intravenous", "IV", "core", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://doi.org/10.1177/0091270010378520", "Kept separate from SC; Potocka 2011 has an IV arm (abstract only)."),
  reg("insulin_regular_u500_sc", "human insulin", "Regular U-500 (Humulin R U-500)", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinRegularU500", TRUE,
      "https://doi.org/10.1002/jcph.899", "Tham 2017 Table 2."),
  reg("insulin_human_inhaled_afrezza", "human insulin", "Afrezza (Technosphere)", "inhaled", "core", "US",
      FIT, AUD, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      "https://doi.org/10.1177/0091270010378520",
      "Potocka abstract verified (CL 43.4 L/h, Vc 5.0, Q 23.9, Vp 30.7, inhaled ka 2.35/h); no inhaled route in the engine, device-version F; inhaled U is not SC U."),
  reg("insulin_nph_sc", "human insulin", "NPH (Humulin N, Novolin N)", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinNPH", TRUE,
      "https://doi.org/10.1002/jcph.899", "Tham 2017 Table 2; lag 0.378 h."),
  reg("insulin_human_70_30", "human insulin", "70/30 premix (Humulin 70/30, Novolin 70/30)", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinHumanMix70", TRUE,
      "https://doi.org/10.1002/jcph.899", "Component sum, 70% NPH + 30% regular, as Tham."),
  reg("insulin_lispro_humalog_u100", "insulin lispro", "Humalog U-100", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", TRUE, "insulinLispro", FALSE,
      "https://doi.org/10.1002/jcph.899",
      "Zero-order routing (into the circulation, unlagged) and lag on the depot ASSUMED: Tham's supplement and Figure 1 not readable. Pump: U/hr SC."),
  reg("insulin_lispro_humalog_u200", "insulin lispro", "Humalog U-200", SC, "core", "US",
      FIT, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      fdaRoutes, "Tham's external SC-injection check only; must not be used in a pump."),
  reg("insulin_lispro_hovorka2004", "insulin lispro", "Hovorka T1D two-stage SC model", SC, "core", "study",
      FIT, AUD, "PD-FIT", DAUD, "T1D glucose host", NA, NA, FALSE,
      "https://doi.org/10.1088/0967-3334/25/4/010",
      "tau 55 min and ke 0.138/min confirmed from a secondary source only; VI 0.12 L/kg unconfirmed; equal-rate two-depot chain not in the engine."),
  reg("insulin_lispro_aabc_lyumjev_u100", "insulin lispro-aabc", "Lyumjev U-100", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", TRUE, NA, FALSE, fdaRoutes, "Separate formulation model required."),
  reg("insulin_lispro_aabc_lyumjev_u200", "insulin lispro-aabc", "Lyumjev U-200", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE, fdaRoutes, "Not for pumps."),
  reg("insulin_lispro_mix_75_25", "insulin lispro", "Humalog Mix75/25", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinLisproMix75", FALSE,
      "https://doi.org/10.1002/jcph.899", "Component sum, 75% lispro protamine + 25% lispro; lispro's assumed zero-order form."),
  reg("insulin_lispro_mix_50_50", "insulin lispro", "Humalog Mix50/50", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, "insulinLisproMix50", FALSE,
      "https://doi.org/10.1002/jcph.899", "Component sum, 50/50; lispro's assumed zero-order form."),
  reg("insulin_aspart_novolog", "insulin aspart", "NovoLog", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", TRUE, NA, FALSE,
      "https://pubmed.ncbi.nlm.nih.gov/12518253/", "No complete fit in hand."),
  reg("insulin_aspart_fiasp", "insulin aspart", "Fiasp (faster aspart)", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", TRUE, NA, FALSE,
      "https://pubmed.ncbi.nlm.nih.gov/25846340/", "No complete fit in hand."),
  reg("insulin_aspart_szjj_merilog", "insulin aspart-szjj", "Merilog (biosimilar)", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE, fdaRoutes, "SC injection only, not pump-approved."),
  reg("insulin_aspart_fsan_garzulys", "insulin aspart-fsan", "Garzulys (biosimilar)", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", NA, NA, FALSE, fdaRoutes, "Pump use pending current label."),
  reg("insulin_glulisine_apidra", "insulin glulisine", "Apidra", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", NA, NA, FALSE, fdaRoutes, "Pump use only where the label permits; not checked here."),
  reg("insulin_glargine_basaglar_u100", "insulin glargine", "Basaglar U-100", SC, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "clamp GIR (M1 active)", FALSE, "insulinGlargine", FALSE,
      "https://doi.org/10.1002/jcph.899",
      "Immunoreactive assay, C-peptide corrected; zero-order routing assumed as lispro's; dose effect on F not applied."),
  reg("insulin_glargine_lantus_u100", "insulin glargine", "Lantus U-100", SC, "core", "US",
      FIT, AUD, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      "https://doi.org/10.1002/jcph.899", "Tham external check only; product equivalence mapping required."),
  reg("insulin_glargine_semglee_u100", "insulin glargine-yfgn", "Semglee U-100", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE, fdaRoutes, "Biosimilar; no pump use."),
  reg("insulin_glargine_rezvoglar_u100", "insulin glargine-aglr", "Rezvoglar U-100", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE, fdaRoutes, "Biosimilar; no pump use."),
  reg("insulin_glargine_u300_toujeo", "insulin glargine", "Toujeo U-300", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR (U300/U100 AUC 0.73)", FALSE, NA, FALSE,
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC6298133/", "Precipitate depot; mechanistic constants an incomplete subset."),
  reg("insulin_detemir", "insulin detemir", "Levemir", SC, "core", "US discontinued; international",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", NA, NA, FALSE,
      "https://pubmed.ncbi.nlm.nih.gov/15855574/", "1 detemir U = 24 nmol; dose-dependent duration."),
  reg("insulin_degludec_u100_u200", "insulin degludec", "Tresiba U-100/U-200", SC, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", NA, NA, FALSE,
      "https://pubmed.ncbi.nlm.nih.gov/23749405/", "Strength equivalence (AUC ratio 0.99) but no complete fit."),
  reg("insulin_icodec_u700", "insulin icodec-abae", "Awiqli U-700 (weekly)", SC, "core", "US (2026, adult T2D)",
      CON, CON, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      "https://drc.bmj.com/content/9/1/e002301", "Median tmax 16 h, half-life 196 h: constraints only."),
  reg("insulin_aspart_mix_70_30", "insulin aspart", "NovoLog Mix 70/30", SC, "core", "US",
      TPL, TPL, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      "https://www.novo-pi.com/novologmix7030.pdf", "Aspart protamine release fit required."),
  reg("insulin_degludec_aspart_70_30", "insulin degludec/aspart", "Ryzodeg 70/30", SC, "core", "US",
      TPL, TPL, "PD-CONSTRAINT", DCON, "clamp GIR", FALSE, NA, FALSE,
      "https://link.springer.com/article/10.1007/s13300-014-0070-2", "Component release fits required."),
  reg("soliqua_100_33", "insulin glargine + lixisenatide", "Soliqua 100/33 (fixed-ratio pen)", SC, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "none", FALSE, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=4bba538b-cf7c-4310-ae8f-cb711ed21bcc",
      "n steps = n U glargine + 0.33n mcg lixisenatide; components not both implemented; no pump."),
  reg("xultophy_100_3_6", "insulin degludec + liraglutide", "Xultophy 100/3.6 (fixed-ratio pen)", SC, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "none", FALSE, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=21335fe4-d395-4501-ac2a-2f20d7520da9",
      "n steps = n U degludec + 0.036n mg liraglutide; degludec not implemented; no pump."),
  # --- Oral agents -------------------------------------------------------------
  reg("metformin_ir_chae2012", "metformin", "Immediate-release tablets", PO, "core", "US",
      FIT, FIT, "PD-FIT", DFIT, "single-dose glucose response, healthy adults", NA, "metformin", TRUE,
      "https://doi.org/10.1111/j.1365-2125.2012.04260.x", "Abstract verified; CrCl taken as Cockcroft-Gault (method not stated in the available text)."),
  reg("metformin_ir_alternative", "metformin", "Immediate release, 2C model version", PO, "core", "study",
      FIT, AUD, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC4171106/", "Not re-read; must not be spliced with Chae."),
  reg("metformin_er", "metformin", "Extended-release tablets", PO, "core", "US",
      AUD, AUD, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://www.page-meeting.org/Abstracts/a-population-pharmacokinetic-pk-model-of-metformin-regarding-immediate-and-extended-release-formulations-under-fasted-and-fed-conditions/",
      "Conference abstract; incomplete."),
  reg("empagliflozin_baron2016", "empagliflozin", "Jardiance", PO, "core", "US",
      FIT, FIT, "PD-FIT", DAUD, "FPG/HbA1c", NA, "empagliflozin", TRUE,
      "https://doi.org/10.1007/s13300-016-0174-y", "Fixed 0.5 h lag (missing from the specification); covariates in supplement Table S1 not applied."),
  reg("dapagliflozin_kobuchi2025", "dapagliflozin", "Farxiga", PO, "core", "US",
      FIT, AUD, "PD-FIT", DAUD, "HbA1c (Japanese T2D)", NA, NA, FALSE,
      "https://doi.org/10.7150/ijms.111519", "CL/F 229.3 L/day, weight exponent 0.41 verified; fixed V/F, ka, their source and the reference weight (mean, not the 77 kg median) not readable."),
  reg("canagliflozin_dewinter2017", "canagliflozin", "Invokana", PO, "core", "US",
      AUD, AUD, "PD-AUDIT", DAUD, "HbA1c", NA, NA, FALSE,
      "https://bpspubs.onlinelibrary.wiley.com/doi/pdfdirect/10.1111/bcp.13180", "Not re-read."),
  reg("sglt2_yao2023_class", "dapagliflozin, canagliflozin, empagliflozin", "Yao 2023 class reanalysis", PO, "core", "study",
      AUD, AUD, "PD-AUDIT", DAUD, "24-h UGE (AUC50)", NA, NA, FALSE,
      "https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.12934", "Transit absorption; not re-read; never spliced with Baron or Kobuchi."),
  reg("ertugliflozin_fediuk2021", "ertugliflozin", "Steglatro", PO, "core", "US",
      FIT, FIT, "PD-CONSTRAINT", DCON, "UGE", NA, "ertugliflozin", TRUE,
      "https://doi.org/10.1002/cpdd.970", "Data set 1; weight, MDRD eGFR and sex applied; T2D, race, age-on-Vc and food not."),
  reg("sitagliptin", "sitagliptin", "Januvia", PO, "core", "US",
      CON, CON, "PD-FIT", DAUD, "plasma DPP-4 inhibition", NA, NA, FALSE,
      "https://www.pmda.go.jp/files/000153174.pdf", "Label constraints only."),
  reg("saxagliptin", "saxagliptin", "Onglyza (with active metabolite)", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "DPP-4 inhibition", NA, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=6e8d8c4f-96eb-4b64-9c8c-4a5448a23a78&type=display", "Half-lives only."),
  reg("linagliptin", "linagliptin", "Tradjenta", PO, "core", "US",
      AUD, AUD, "PD-FIT", DAUD, "plasma DPP-4 inhibition", NA, NA, FALSE,
      "https://link.springer.com/article/10.1007/s40262-014-0232-4", "Target-mediated disposition: needs a binding solver."),
  reg("alogliptin", "alogliptin", "Nesina", PO, "core", "US",
      CON, CON, "PD-FIT", DAUD, "plasma DPP-4 inhibition", NA, NA, FALSE,
      "https://www.ema.europa.eu/en/documents/assessment-report/vipidia-epar-public-assessment-report_en.pdf", "No complete compatible table."),
  reg("vildagliptin", "vildagliptin", "Galvus", PO, "international", "EU and others",
      AUD, AUD, "PD-AUDIT", DAUD, "DPP-4 readout", NA, NA, FALSE,
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC3370343/", "Target-mediated."),
  reg("gliclazide_ir", "gliclazide", "Immediate release", PO, "international", "outside US",
      FIT, AUD, "PD-FIT", DAUD, "insulin (empirical)", NA, NA, FALSE,
      "https://doi.org/10.1111/bcpt.13868", "Abstract has no parameters; noninteger transit chain not in the engine."),
  reg("gliclazide_mr", "gliclazide", "Modified release", PO, "international", "outside US",
      AUD, AUD, "PD-AUDIT", DAUD, "insulin", NA, NA, FALSE, "https://doi.org/10.1111/bcpt.13868", "Distinct absorption."),
  reg("glyburide_rambiritch2016", "glyburide (glibenclamide)", "Tablets", PO, "core", "US",
      FIT, FIT, "PD-AUDIT", DAUD, "glucose (parent/M1/M2 effect sites)", NA, "glyburide", TRUE,
      "https://doi.org/10.2147/CPAA.S102676", "Verified; no covariates; metabolites not modelled."),
  reg("glipizide", "glipizide", "Glucotrol", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/6341263/", "IV parameters from six subjects."),
  reg("glimepiride", "glimepiride", "Amaryl", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://doi.org/10.1111/j.1365-2710.2006.00766.x", "Observed peak only."),
  reg("repaglinide", "repaglinide", "Prandin", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "meal-linked secretion", NA, NA, FALSE,
      "https://www.accessdata.fda.gov/drugsatfda_docs/label/2015/091517Orig1lbl.pdf", "IV Vss and CL only."),
  reg("nateglinide", "nateglinide", "Starlix", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "meal-linked secretion", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/11563410/", "Half-life only."),
  reg("pioglitazone_kadam2013", "pioglitazone", "Actos", PO, "core", "US",
      FIT, AUD, "PD-TEMPLATE", DTPL, "insulin sensitivity (delayed)", NA, NA, FALSE,
      "https://doi.org/10.1248/bpb.b12-00657", "Kadam 2013 abstract only; table not readable, and the claimed Q/F with its +1.03 effect gives +295%, not the abstract's 286%."),
  reg("rosiglitazone", "rosiglitazone", "Avandia", PO, "core", "US",
      CON, CON, "PD-TEMPLATE", DTPL, "insulin sensitivity", NA, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/getFile.cfm?setid=ef14122b-7fff-45fa-b13b-0ea9e48bd57d", "Constants from different analyses."),
  reg("acarbose", "acarbose", "Precose", PO, "core", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "meal carbohydrate hydrolysis", NA, NA, FALSE,
      "https://www.medicines.org.uk/emc/product/10000/smpc", "Local gut action; plasma PK is not the driver."),
  reg("miglitol", "miglitol", "Glyset", PO, "core", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "meal carbohydrate hydrolysis", NA, NA, FALSE,
      "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=b8da2015-d254-2425-e053-2995a90acf0e", "Local gut action."),
  reg("voglibose", "voglibose", "Basen", PO, "international", "Japan and others",
      TPL, TPL, "PD-TEMPLATE", DTPL, "meal carbohydrate hydrolysis", NA, NA, FALSE,
      "https://pubmed.ncbi.nlm.nih.gov/8536820/", "Local gut action."),
  # --- International extension -------------------------------------------------
  reg("ipragliflozin", "ipragliflozin", "Suglat", PO, "international", "Japan",
      AUD, AUD, "PD-AUDIT", DAUD, "UGE", NA, NA, FALSE, "https://pmc.ncbi.nlm.nih.gov/articles/PMC6624389/", "No borrowed parameters."),
  reg("luseogliflozin", "luseogliflozin", "Lusefi", PO, "international", "Japan",
      AUD, AUD, "PD-AUDIT", DAUD, "UGE", NA, NA, FALSE, "https://www.jstage.jst.go.jp/article/bpb/40/8/40_b16-00998/_pdf", "No borrowed parameters."),
  reg("tofogliflozin", "tofogliflozin", "Apleway / Deberza", PO, "international", "Japan",
      AUD, AUD, "PD-AUDIT", DAUD, "UGE", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/28427104/", "No borrowed parameters."),
  reg("bexagliflozin", "bexagliflozin", "Brenzavvy", PO, "core", "US",
      CON, CON, "PD-CONSTRAINT", DCON, "UGE", NA, NA, FALSE,
      "https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/214373s001lbl.pdf", "Pending a full fit."),
  reg("teneligliptin", "teneligliptin", "Tenelia", PO, "international", "Japan",
      CON, CON, "PD-CONSTRAINT", DCON, "DPP-4 inhibition", NA, NA, FALSE, "https://www.pmda.go.jp/files/000223901.pdf", ""),
  reg("anagliptin", "anagliptin", "Suiny", PO, "international", "Japan",
      CON, CON, "PD-CONSTRAINT", DCON, "DPP-4 inhibition", NA, NA, FALSE, "https://www.pmda.go.jp/files/000223901.pdf", ""),
  reg("omarigliptin", "omarigliptin", "Marizev (weekly)", PO, "international", "Japan",
      CON, CON, "PD-CONSTRAINT", DCON, "DPP-4 inhibition", NA, NA, FALSE, "https://www.pmda.go.jp/files/000223901.pdf", ""),
  reg("evogliptin", "evogliptin", "Suganon", PO, "international", "Korea and others",
      CON, CON, "PD-CONSTRAINT", DCON, "DPP-4 inhibition", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/25336915/", "Time-course data, no fit."),
  reg("mitiglinide", "mitiglinide", "Glufast", PO, "international", "Japan",
      TPL, TPL, "PD-TEMPLATE", DTPL, "meal-linked secretion", NA, NA, FALSE, "https://www.pmda.go.jp/files/000272006.pdf", ""),
  reg("lobeglitazone", "lobeglitazone", "Duvie", PO, "international", "Korea",
      CON, CON, "PD-TEMPLATE", DTPL, "insulin sensitivity", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/29932413/", ""),
  # --- Optional and out of scope ------------------------------------------------
  reg("albiglutide", "albiglutide", "Tanzeum (withdrawn)", SC, "optional", "historical",
      CON, CON, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://pubmed.ncbi.nlm.nih.gov/25387217/", "Not in the default list."),
  reg("retatrutide", "retatrutide", "Investigational", SC, "optional", "investigational",
      TPL, TPL, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://clinicaltrials.gov/study/NCT05929079", "Not in the default list."),
  reg("orforglipron", "orforglipron", "Oral GLP-1 (obesity)", PO, "optional", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE,
      "https://www.fda.gov/news-events/press-announcements/fda-approves-first-new-molecular-entity-under-national-priority-voucher-program", "Obesity-indicated; not in the antidiabetic default list."),
  reg("colesevelam", "colesevelam", "Welchol", PO, "out_of_scope", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://diabetesjournals.org/care/article/42/Supplement_1/S46/36544", "Lower-priority adjunct."),
  reg("bromocriptine_qr", "bromocriptine", "Cycloset (quick release)", PO, "out_of_scope", "US",
      TPL, TPL, "PD-TEMPLATE", DTPL, "none", NA, NA, FALSE, "https://diabetesjournals.org/care/article/42/Supplement_1/S46/36544", "Lower-priority adjunct.")
)

outdir <- file.path("inst", "extdata")
utils::write.csv(registry, file.path(outdir, "antidiabeticRegistry.csv"),
                 row.names = FALSE, na = "NA")
cat("Wrote", nrow(registry), "registry rows\n")

# --- Parameter audit -----------------------------------------------------------
par <- function(drug, parameter, value, units, provenance, population, source_url, locator)
  data.frame(drug, parameter, value, units, provenance, population, source_url, locator,
             stringsAsFactors = FALSE)
E <- "estimated"; FX <- "fixed"; DV <- "derived"; AS <- "assumed"; LB <- "library"
petri  <- "https://doi.org/10.1007/s13300-018-0458-5"
schneck <- "https://doi.org/10.1002/psp4.13099"
roge   <- "https://doi.org/10.1002/psp4.11"
tham   <- "https://doi.org/10.1002/jcph.899"
chae   <- "https://doi.org/10.1111/j.1365-2125.2012.04260.x"
baron  <- "https://doi.org/10.1007/s13300-016-0174-y"
fediuk <- "https://doi.org/10.1002/cpdd.970"
ramb   <- "https://doi.org/10.2147/CPAA.S102676"
thamPop <- "Tham 2017, 16 studies, mostly healthy volunteers"

thamRows <- function(drug, v, cl, absorption) rbind(
  par(drug, "V/F", v, "L (x library FFM factor)", E, thamPop, tham, "Table 2"),
  par(drug, "CL/F", cl, "L/h (x library FFM factor^0.75)", E, thamPop, tham, "Table 2"),
  absorption,
  par(drug, "dose and weight covariates", "not applied", "-", AS, thamPop, tham, "Table 2; equations in supplement (not read)"))
ab <- function(drug, name, ka, lag = NULL, f1 = NULL, d0 = NULL) rbind(
  par(drug, paste0("ka (", name, ")"), ka, "1/h", E, thamPop, tham, "Table 2"),
  if (!is.null(lag)) par(drug, paste0("lag (", name, ")"), lag, "h", E, thamPop, tham, "Table 2"),
  if (!is.null(f1)) par(drug, paste0("first-order fraction (", name, ")"), f1, "unitless", E, thamPop, tham, "Table 2"),
  if (!is.null(d0)) par(drug, paste0("zero-order duration (", name, ")"), d0, "h", E, thamPop, tham, "Table 2"),
  if (!is.null(d0)) par(drug, paste0("zero-order routing (", name, ")"), "into the circulation, unlagged", "-", AS, thamPop, tham, "Figure 1 and supplement not read"))
lispro <- function(drug) ab(drug, "lispro", "0.989", "0.265", "0.729", "1.06")

parameters <- rbind(
  par("semaglutide", "CL/F", "0.0478", "L/h at 85 kg", E, "Carlsson Petri 2018, phase 3 T2D", petri, "Results, Model Qualification (Table S3)"),
  par("semaglutide", "V/F", "12.2", "L (x library FFM factor)", E, "Carlsson Petri 2018", petri, "Results, Model Qualification"),
  par("semaglutide", "ka", "0.0286", "1/h", FX, "Carlsson Petri 2018 (rich clinical pharmacology data)", petri, "Methods"),
  par("semaglutide", "weight exponent on CL/F", "0.78", "unitless (x (WT/85)^0.78)", DV, "from the published 55 kg (+40%) and 127 kg (-27%) exposures", petri, "Results"),

  par("tirzepatide", "ka", "0.0373", "1/h", E, "Schneck 2024, 5802 participants", schneck, "Table 3"),
  par("tirzepatide", "CL", "0.0329", "L/h/70 kg (x (WT/70)^0.8)", E, "Schneck 2024", schneck, "Table 3, footnote d"),
  par("tirzepatide", "Q", "0.126", "L/h/70 kg (x (WT/70)^0.8)", E, "Schneck 2024", schneck, "Table 3, footnote d"),
  par("tirzepatide", "Vc", "2.47", "L/70 kg (x (FFM + 0.482 FM)/70)", E, "Schneck 2024", schneck, "Table 3, footnote e"),
  par("tirzepatide", "Vp", "3.98", "L/70 kg (x (FFM + 0.482 FM)/70)", E, "Schneck 2024", schneck, "Table 3, footnote e"),
  par("tirzepatide", "fat-mass fraction on V", "0.482", "unitless", E, "Schneck 2024", schneck, "Table 3"),
  par("tirzepatide", "F", "0.655", "unitless (0.8 x (1 - 0.181))", DV, "reproduces the reported 0.061 L/h apparent CL in T2D", schneck, "Table 3 (0.8 fixed; -0.181 between studies)"),

  par("liraglutide", "V/F", "0.112", "L/kg", E, "Roge 2015, 29 adults with T2D", roge, "Table 1"),
  par("liraglutide", "CL/F", "1.83e-4", "L/min/kg", E, "Roge 2015", roge, "Table 1 (power of ten from the specification)"),
  par("liraglutide", "ka", "9.41e-4", "1/min", E, "Roge 2015", roge, "Table 1 (power of ten from the specification)"),

  thamRows("insulinRegular", "178", "127", ab("insulinRegular", "regular U100", "0.67")),
  thamRows("insulinRegularU500", "178", "127", ab("insulinRegularU500", "regular U500", "0.185")),
  thamRows("insulinNPH", "178", "127", ab("insulinNPH", "NPH", "0.129", "0.378")),
  thamRows("insulinHumanMix70", "178", "127", rbind(ab("insulinHumanMix70", "NPH, 70%", "0.129", "0.378"),
                                                     ab("insulinHumanMix70", "regular, 30%", "0.67"))),
  thamRows("insulinLispro", "43.0", "30.5", lispro("insulinLispro")),
  thamRows("insulinLisproMix75", "43.0", "30.5", rbind(ab("insulinLisproMix75", "lispro protamine, 75%", "0.0365"),
                                                       lispro("insulinLisproMix75"))),
  thamRows("insulinLisproMix50", "43.0", "30.5", rbind(ab("insulinLisproMix50", "lispro protamine, 50%", "0.0365"),
                                                       lispro("insulinLisproMix50"))),
  thamRows("insulinGlargine", "768", "74.5", ab("insulinGlargine", "glargine", "0.0830", NULL, "0.822", "0.612")),

  par("metformin", "CL/F", "52.6", "L/h (x (CrCl/106.5)^0.782)", E, "Chae 2012, 42 healthy adults", chae, "Abstract"),
  par("metformin", "V/F", "113", "L (x library FFM factor)", E, "Chae 2012", chae, "Abstract"),
  par("metformin", "ka", "0.41", "1/h", E, "Chae 2012", chae, "Abstract"),
  par("metformin", "CrCl method", "Cockcroft-Gault", "mL/min", AS, "not stated in the available text", chae, "-"),

  par("empagliflozin", "CL/F", "10.6", "L/h", E, "Baron 2016, 2761 adults with T2D", baron, "Results"),
  par("empagliflozin", "Vc/F", "3.14", "L", E, "Baron 2016", baron, "Results"),
  par("empagliflozin", "Q/F", "6.34", "L/h", E, "Baron 2016", baron, "Results"),
  par("empagliflozin", "Vp/F", "70.6", "L", E, "Baron 2016", baron, "Results"),
  par("empagliflozin", "ka", "0.196", "1/h (printed L/h)", E, "Baron 2016", baron, "Results"),
  par("empagliflozin", "lag", "0.5", "h", FX, "Baron 2016", baron, "Methods"),
  par("empagliflozin", "covariates on CL/F", "not applied", "-", AS, "Baron 2016", baron, "Table S1 (not read)"),
  par("empagliflozin", "size scaling", "library FFM factors", "-", LB, "reference patient", baron, "-"),

  par("ertugliflozin", "CL/F", "11.9", "L/h at 85 kg (x (WT/85)^0.75)", E, "Fediuk 2021, data set 1", fediuk, "Table 2"),
  par("ertugliflozin", "eGFR exponent on CL/F", "0.458", "unitless (MDRD/90, capped 120)", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "female on CL/F", "0.960", "x CL/F", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "Vc/F", "6.51", "L at 85 kg (x WT/85)", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "female on Vc/F", "1.46", "x Vc/F", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "Q/F", "7.76", "L/h at 85 kg (x (WT/85)^0.75)", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "Vp/F", "107", "L at 85 kg (x WT/85)", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "ka (fasted)", "0.329", "1/h", E, "Fediuk 2021", fediuk, "Table 2"),
  par("ertugliflozin", "lag", "0.226", "h", E, "Fediuk 2021", fediuk, "Table 2 (ALAG1)"),
  par("ertugliflozin", "covariate forms", "power (continuous), factor (categorical)", "-", AS, "table labels; equations in supplement", fediuk, "Supplementary appendix (not read)"),

  par("glyburide", "ka", "0.53", "1/h", E, "Rambiritch 2016, 24 adults with T2D", ramb, "Results"),
  par("glyburide", "CL/F", "2.16", "L/h", E, "Rambiritch 2016", ramb, "Results"),
  par("glyburide", "Vc/F", "11.70", "L", E, "Rambiritch 2016", ramb, "Results"),
  par("glyburide", "Q/F", "3.84", "L/h", E, "Rambiritch 2016", ramb, "Results"),
  par("glyburide", "Vp/F", "68.1", "L", E, "Rambiritch 2016", ramb, "Results"),
  par("glyburide", "size scaling", "library FFM factors", "-", LB, "no covariate analysis in the source", ramb, "-")
)

utils::write.csv(parameters, file.path(outdir, "antidiabeticParameters.csv"),
                 row.names = FALSE, na = "NA")
cat("Wrote", nrow(parameters), "parameter-audit rows\n")
