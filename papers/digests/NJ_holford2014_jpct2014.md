---
file: NJ_holford2014_jpct2014.pdf
pmid: none (J Pharmacol Clin Toxicol, JSciMed Central, not PubMed-indexed)
doi: none given
citation: "Holford S, Allegaert K, Anderson BJ, Kukanich B, Sousa AB, Steinman A, Pypendop B, Mehvar R, Giorgi M, Holford N. Parent-Metabolite Pharmacokinetic Models for Tramadol – Tests of Assumptions and Predictions. J Pharmacol Clin Toxicol 2014;2(1):1023. https://www.jscimedcentral.com/public/assets/articles/pharmacology-spidpharmacokinetics-1023.pdf"
kind: pk-model
drugs: [tramadol, desmetramadol]
source_read: text layer, whole paper (12 pp.; Tables 1-6 readable; VPC figures not read)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# Holford 2014: joint tramadol / O-desmethyltramadol (M1) model across seven species

## Study
Pooled intravenous tramadol data (Table 1): humans 113 (57 healthy, 56 postoperative; studies 3 [n 41, 100 mg
bolus then 50 mg over 30 min; no M1 samples], 5 [n 16, 100 mg bolus], 7 [n 56, 3 mg/kg over 18 min, 3 samples
each]); dogs (studies 20, 21 [M1 1 mg/kg IV, n 3], 22), goat, horses, cat, rat, donkey. Human mean weight 71.1
(58-98) kg. Assays GCMS, LC/MS, HPLC; racemic tramadol and M1 (enantiomer data summed). Doses converted from
tramadol HCl to base (1 mg HCl = 0.8784 mg base); M1 concentrations converted to tramadol mg-equivalents (MW
249.38 vs 263.38, ratio 0.947). NONMEM VII (FOCE-I), bootstrap SE.

## Model and parameters
Tramadol two-compartment, zero-order (or bolus) input; M1 formed from the tramadol central compartment (CLPM),
two-compartment M1 disposition; tramadol also cleared by other routes (CLPO). Two identifiability assumptions
compared: Fm = 1 (OFV 17350.8) and VM1 = VM1(dog) (OFV 17343.5); final model VM1 = VM1dog with a separate total
CL for dog studies 20/21 (4.74x) (OFV 17305.7) and a two-class mixture on CLPM. Allometry (W/70)^0.75 on
clearances, ^1 on volumes. Table 2, human typical values at 70 kg (bootstrap RSE; PPV as SD of eta):

```yaml
parameters:
  - {name: CLPM_extensive, value: 10.5, unit: L/h/70 kg, source: "Table 2", note: "RSE 13%, PPV 0.525; tramadol -> M1"}
  - {name: CLPM_slow, value: 1.70, unit: L/h/70 kg, source: "Table 2", note: "RSE 27%, PPV 0.059; 16.2% of extensive (text p.7)"}
  - {name: CLPO, value: 18.4, unit: L/h/70 kg, source: "Table 2", note: "RSE 9%, PPV 0.762"}
  - {name: QP, value: 105, unit: L/h/70 kg, source: "Table 2", note: "RSE 29%, PPV 0.647"}
  - {name: VP1, value: 90, unit: L/70 kg, source: "Table 2", note: "RSE 16%, PPV 0.549"}
  - {name: VP2, value: 79, unit: L/70 kg, source: "Table 2", note: "RSE 17%, PPV 0.633"}
  - {name: CLMO, value: 84.2, unit: L/h/70 kg, source: "Table 2", note: "RSE 10%, PPV 0.154"}
  - {name: QM, value: 274, unit: L/h/70 kg, source: "Table 2", note: "RSE 112%, PPV 1.65"}
  - {name: VM1, value: 78.9, unit: L/70 kg, source: "Table 2", note: "FIXED to dog estimate (study 21, M1 IV), PPV 0.401"}
  - {name: VM2, value: 131, unit: L/70 kg, source: "Table 2", note: "RSE 24%, PPV 0.412"}
  - {name: slow_metabolizer_fraction, value: 0.060, unit: fraction, source: "text p.7", note: "49% RSE; all slow were human except 1 horse"}
```
Total tramadol CL 28.9 L/h (derived: 10.5 + 18.4). Residual error (Table 4): tramadol 12.3% + 0.901 mcg/L;
M1 22.3% + 0.580 mcg/L. Species ratios to human in Table 5 (e.g. dog study 22 CLPM 0.109).

## Pharmacodynamics and end points
Opioid audit:
- No ke0, t1/2ke0, tPeak or hysteresis; no PD model.
- MEAC-type value (Table 6 and its footnote): predicted average steady-state human M1 is 84 mcg/L at 2 mg/kg
  tramadol HCl every 6 h (70 kg), computed as dose rate / (CLPM + CLPO) x CLPM / CLMO. The footnote, citing
  Lehmann 1990 (postoperative PCA), says 1 mg/kg/6 h produced minimum effective M1 analgesic concentrations
  about 50% of that, i.e. about 42 mcg/L (derived). So 84 mcg/L is a typical-dose exposure, not a minimum
  effective concentration. Total racemic M1, plasma; not stated whether expressed as M1 mass or tramadol
  equivalents (derived: 2 mg/kg HCl x 0.8784 per 6 h gives 88 mcg/L tramadol-equivalents, x 0.947 = 84 mcg/L
  M1 mass, so the 84 appears to be M1 mass).
- Potency: M1 stated to be 6 times more potent than tramadol in non-human analgesia models (p.10, citing the
  Ryzolt product information).

## Covariates
Weight (allometry, theory-fixed exponents), species, and a latent slow/extensive metaboliser class on CLPM
(not genotype). Between-study difference in dogs.

## Caveats
Authors: VM1 is not identifiable without direct M1 dosing; fixing it to three dogs is the key assumption, and
they show it fails for cat and donkey; racemic concentrations obscure stereoselective kinetics. Noticed: human
M1 data come only from studies 5 and 7 (study 3 had none, study 7 three samples per subject); QM RSE 112%;
latent class is a phenotype mixture, not genotype.

## Used by stanpumpR
- R/drugs_tramadol.R:29-34, 150-172, 179-184 (and 96-97, 195-196) — parent disposition, CL split, allometry,
  formation constant, reference.
- R/drugs_tramadol.R:72-76 — Holford's slow class at 0.16 of fast supports the poor-metaboliser weight 0.10.
- R/drugs_tramadol.R:89-91 — caveat that Holford's classes are a latent mixture.
- R/drugs_tramadol.R:262 — mwRatio 249.38 / 263.38.
- R/drugs_desmetramadol.R:13-17, 23-24, 46-47, 162-184 — M1 disposition and the dog-fixed VM1.
- inst/help/drugs/tramadol.md:3, 17; inst/help/drugs/desmetramadol.md:3, 7, 19; tests/testthat/
  test-drugs-tramadol.R:72, 150, 171; inst/help/references.md:67 (context said :21); inst/help/
  investigators.md:61 (context said :52).

## Verification against stanpumpR
- R/drugs_tramadol.R:30-34, 162-171; help tramadol.md:3: VP1 90, VP2 79, QP 105, CLPO 18.4, CLPM 10.5 L/h,
  total 28.9; 57 healthy + 56 postoperative; (W/70)^0.75 and W/70. Table 2 and Methods identical. **matches**.
- R/drugs_desmetramadol.R:15-16, 172-176; help desmetramadol.md:3: CLMO 84.2, VM1 78.9, VM2 131, QM 274.
  **matches**.
- R/drugs_desmetramadol.R:23-24; help desmetramadol.md:7: no human IV metabolite data; VM1 fixed from three dogs.
  **matches** (study 21, n 3, Table 1).
- R/drugs_tramadol.R:73-74: slow class 0.16 of fast. Paper 16.2%. **matches**.
- R/drugs_tramadol.R:262: mwRatio 249.38/263.38. Paper uses the same MWs and ratio 0.947. **matches**.
- Dose salt: Holford's parameters were estimated with doses expressed as tramadol BASE (0.8784 mg per mg HCl).
  No HCl-to-base factor was found in R/drugs_tramadol.R (bioavailability_PO 0.70 is the only dose scaling), so a
  prescribed "100 mg" (tramadol HCl) is simulated as 100 mg base: concentrations about 14% high (derived
  1/0.8784 = 1.138) if doses are entered as the salt. **discrepancy (possible; depends on the app's dose
  convention)** at R/drugs_tramadol.R:170-176 and the IV test tests/testthat/test-drugs-tramadol.R:162-166.
- R/drugs_desmetramadol.R:125-143, inst/help/drugs/desmetramadol.md:15, drugDefaults_global.csv (desmetramadol
  MEAC 84): MEAC 84 ng/mL is attributed to Lee 2019's introduction. This paper's Table 6 is a likely origin of
  that number, and here 84 mcg/L is the predicted average M1 at a full 2 mg/kg/6 h dose, with minimum effective
  concentrations about half of it (~42 mcg/L). **discrepancy (provenance and meaning of MEAC 84; flag for the
  primary-source check the file already requests)**.
- inst/help/investigators.md:61: first author given as "Sophie Holford". The paper's first author is Sam
  Holford. **discrepancy** (name).
- inst/help/references.md:67: title truncated to "Parent-metabolite pharmacokinetic models for tramadol"; full
  title adds "– Tests of Assumptions and Predictions". Minor **discrepancy** (citation).
- tests/testthat/test-drugs-tramadol.R:72-80 (allometry), :150 (oral peak, Brvar), :171-175 (no M1 dose unit):
  consistent with the paper where it applies. **matches**.
