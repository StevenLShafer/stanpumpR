---
file: NJ_label-tinzaparin_020484_2010-10-12.pdf
pmid: null
doi: null
citation: "INNOHEP (tinzaparin sodium injection) for subcutaneous use. US prescribing information, LEO Pharma, NDA 020484 (supplement s014); label text dated May 2010, FDA action 2010-10-12. https://www.accessdata.fda.gov/drugsatfda_docs/label/2010/020484s014lbl.pdf"
kind: label
drugs: [tinzaparin]
source_read: "text layer, all 22 PDF pages (form-feed chunks = pages; old non-PLR format, no printed page numbers). Read Description (pp1-2), Clinical Pharmacology incl. Pharmacokinetics/Pharmacodynamics and Special Populations (pp3-7), Clinical Studies (pp8-9), Warnings and Precautions incl. Pregnancy, Pediatric and Geriatric Use (pp9-13), Overdosage and Dosage and Administration (pp19-22). Rendered pages 2 (Table 1), 4 (Figure 1, Table 2) and 21 (Table 8); all three confirmed from the images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# INNOHEP (tinzaparin) US label, 2010

## Study
This is the FDA-archived US label for Innohep (tinzaparin sodium), a low-molecular-weight heparin given SC
(LEO Pharma). It is the old non-PLR format, with the text dated May 2010 (p22). PDF page 1 is the Innohep
label as named, so there is no source file problem. It is not the DailyMed setid that the stanpumpR registry
links (see Verification).

**Product (Description, pp1-2; Table 1 rendered):**
- 2 mL multiple-dose vial at 20,000 anti-Xa IU/mL (40,000 IU per vial), with sodium metabisulfite 3.106 mg/mL
  and benzyl alcohol 10 mg/mL.
- Potency is about 100 anti-Xa IU per mg, against the first International LMWH Standard. Average MW is
  5,500-7,500 Da.

**Dosage (D&A, pp20-21; Table 8 rendered):**
- DVT with or without PE: 175 anti-Xa IU/kg SC once daily for at least 6 days, until INR is at least 2 on two
  consecutive days on warfarin.
- Volume = weight (kg) × 0.00875 mL/kg. Table 8 runs from 6,000 IU (0.3 mL) at 31-36 kg to 28,000 IU (1.4 mL)
  at 157-162 kg.
- Not for IM or IV use. No dose adjustment for pregnancy.

## Model and parameters
No population model is given. The label reports noncompartmental means as anti-Xa activity, because LMWH
concentrations cannot be measured directly (p3).
```yaml
parameters:
  - {name: Cmax after SC 4500 IU, value: 0.25, unit: IU/mL, source: "Table 2, p4", note: "mean (SD 0.05), N=23 healthy volunteers; mean dose 64.3 IU/kg"}
  - {name: Tmax after SC 4500 IU, value: 3.7, unit: h, source: "Table 2, p4", note: "SD 0.9"}
  - {name: AUC0-inf after SC 4500 IU, value: 2.0, unit: IU*h/mL, source: "Table 2, p4", note: "SD 0.5"}
  - {name: t1/2 after SC 4500 IU, value: 3.4, unit: h, source: "Table 2, p4", note: "SD 1.7"}
  - {name: "Cmax SC 175 IU/kg, day 1", value: 0.87, unit: IU/mL, source: "Table 2, p4", note: "SD 0.15, N=14; day 5 0.93 (0.15)"}
  - {name: "Tmax SC 175 IU/kg, day 1", value: 4.4, unit: h, source: "Table 2, p4", note: "SD 0.7; day 5 4.6 (1.0)"}
  - {name: "AUC0-inf SC 175 IU/kg, day 1", value: 9.0, unit: IU*h/mL, source: "Table 2, p4", note: "SD 1.1; day 5 9.7 (1.4)"}
  - {name: "t1/2 SC 175 IU/kg, day 1", value: 3.3, unit: h, source: "Table 2, p4", note: "SD 0.8; day 5 3.5 (0.6)"}
  - {name: F (SC, absolute), value: 0.867, unit: fraction, source: "Absorption, p5", note: "4500 IU SC vs IV, anti-Xa"}
  - {name: V, value: "3.1-5.0", unit: L, source: "Distribution, p5", note: "range; similar to blood volume"}
  - {name: CL (IV 4500 IU), value: 1.7, unit: L/h, source: "Elimination, p5", note: "approximately"}
  - {name: "t1/2 IV 75 IU/kg, healthy", value: 1.6, unit: h, source: "Hemodialysis studies, p6", note: "vs 5.2 h in 6 haemodialysis patients off-dialysis"}
  - {name: anti-Xa/anti-IIa ratio, value: 2.8, unit: ratio, source: "Absorption, pp4-5", note: "AUC-based, single SC dose; UFH about 1.2"}
```
- The SC elimination half-life is 3-4 h (p5). The anti-Xa AUC rose more than dose-proportionally from 4,500 to
  12,250 IU (p4).
- There was no accumulation at 175 IU/kg once daily for 5 days (p5).
- Arithmetic check (derived): ln 2 × 4 L / 1.7 L/h = 1.6 h, matching the 1.6 h IV half-life in healthy volunteers
  (p6). The range 3.1-5.0 L gives 1.3-2.0 h. The longer SC half-life is consistent with absorption-limited
  (flip-flop) kinetics, which the label does not state.
- Not given: no ka, no peripheral compartment, no IIV.

## Pharmacodynamics and end points
- **Activity:** measured as anti-Xa (and anti-IIa) activity, IU/mL. The label generally advises against
  anti-Xa-based monitoring (p3). aPTT is prolonged at treatment doses, and neither aPTT nor PT is used for
  monitoring (p3, p11).
- **Pregnancy (p7):** 175 IU/kg gave mean anti-Xa 0.3-1.0 IU/mL at 4 h, similar at 28 and 36 weeks.
- **Overdose (p19):** protamine 1 mg per 100 anti-Xa IU, which neutralises at most about 60% of anti-Xa activity.
- **Not given:** no therapeutic anti-Xa range is stated. Not an opioid: no MEAC or ke0.

## Covariates
- From the population PK analysis of about 180 DVT patients at 175 IU/kg and about 240 hip-replacement patients
  at about 65 IU/kg (p5): weight and Cockcroft-Gault CrCl affect CL; age and gender do not.
- Severe renal impairment (<30 mL/min): CL is 24% lower than with normal function (>80 mL/min). On
  haemodialysis, anti-Xa CL is 28% lower (n=12) (pp5-6).
- Elderly (n=30, mean 87 y, CrCl 40.6 mL/min): no accumulation of peak anti-Xa over 10 days (p6).
- Obesity (101-165 kg, BMI 26-61): weight-based dosing appropriate; PK independent of weight and BMI on a
  per-kg dose (p6, Figure 2 p7).
- No covariate equations or coefficients are printed.

## Caveats
- The label warns of increased mortality against UFH in elderly patients with renal insufficiency: 11.5% against
  6.3% in N = 269 against 268 (Warnings, p10).
- Paediatric safety and effectiveness are not established (p13).
- Healthy-volunteer noncompartmental values. Clearance and V are approximate, and the V range is unattributed.

## Used by stanpumpR
- `inst/extdata/anticoagulantRegistry.csv:21` (8baa733, found by content): `tinzaparin_label`, low molecular
  weight heparin, SC, analyte "anti-Xa activity (IU/mL)", assay "LMWH anti-Xa", readiness `pending`, no
  stanpumpR drug.
  - Blocking issue: "Barrett abstract gives CL/F and Vc/F but not ka, Q or Vp; no full population fit."
  - Source URL: DailyMed setid d3e5fbbf-4a1f-4aa2-8e80-9729f6cf04ee.
- The row is generated by `data-raw/anticoagulantRegistry.R:141-145`.
- `tests/testthat/test-anticoagulant-registry.R:58` requires `tinzaparin_label` to be catalogued. The context's
  line 57 is now line 58.
- `AGENTS.md:94` lists tinzaparin among the unimplemented, catalogued anticoagulants.

## Verification against stanpumpR
- **Analyte, route and assay** (registry row 21): the label doses and measures tinzaparin as anti-Xa IU, by SC
  injection only. **matches**
- **`pending`, no drug, and "no full population fit"**: this label gives no population model. It names a
  population PK analysis (weight and CrCl effects) but prints no parameters. It gives only approximate IV CL,
  a V range, F and SC noncompartmental means. That is consistent with pending. **matches**
- **The blocking issue's "Barrett abstract ... CL/F and Vc/F"** is about a different source, and this label
  cannot verify it. **cannot verify**
- **Source identity:** the registry links a DailyMed setid. The archived file is the FDA 2010 Innohep label
  (accessdata 020484s014). Both are the same product, but the version the registry points at cannot be confirmed
  from this file.
