---
file: NJ_dailymed-d3e5fbbf.pdf
pmid: null
doi: null
citation: "Celgene Corporation (manufactured by LEO Pharmaceutical Products). INNOHEP (tinzaparin sodium injection), 20,000 anti-Xa IU/mL, 2 mL multiple-dose vial, for subcutaneous use. DailyMed SPL setid d3e5fbbf-4a1f-4aa2-8e80-9729f6cf04ee; label dated April 2008 (SPL revised 4/2008); archived 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 17 PDF pages (old non-PLR format, no numbered sections): boxed warning and Description p1-2; Clinical Pharmacology incl. Pharmacokinetics/Pharmacodynamics, Table 2, Absorption, Distribution, Metabolism, Elimination p2-4; Special Populations (population PK, renal, hemodialysis, hepatic, elderly, obesity, pregnancy) p4-5; Clinical Studies p6-7; Indications, Contraindications, Warnings, Precautions incl. Pediatric and Geriatric Use p7-10; Adverse Reactions p10-13; Overdosage p13-14; Dosage and Administration incl. Table 8 p14-15; How Supplied and SPL data p15-17. PDF p3 (Figure 1, Table 2, Absorption, Distribution) and p17 (revision date) checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# INNOHEP (tinzaparin) US label, Celgene, April 2008

Tinzaparin is not a stanpumpR drug (registry-only), so `drugs` is empty.

## Study
Not a study: the 2008 US label for tinzaparin sodium, an LMWH from porcine intestinal mucosa (mean anti-Xa activity about
100 IU/mg; average MW 5,500-7,500 Da; PDF p1-2). Indication: treatment of acute symptomatic DVT with or without PE, with
warfarin, in hospitalized patients (PDF p7). Exposure is measured as plasma anti-Xa (and anti-IIa) activity, a surrogate,
because LMWH concentrations cannot be measured directly (PDF p2).

## Model and parameters
No model is given. Healthy-volunteer non-compartmental values from anti-Xa activity (Table 2, PDF p3; mean (SD)):

| | Single dose 4,500 IU (N = 23; mean 64.3 IU/kg) | 175 IU/kg once daily, day 1 (N = 14) | day 5 |
|---|---|---|---|
| Cmax, IU/mL | 0.25 (0.05) | 0.87 (0.15) | 0.93 (0.15) |
| Tmax, h | 3.7 (0.9) | 4.4 (0.7) | 4.6 (1.0) |
| AUC0-inf, IU*h/mL | 2.0 (0.5) | 9.0 (1.1) | 9.7 (1.4) |
| Half-life, h | 3.4 (1.7) | 3.3 (0.8) | 3.5 (0.6) |

```yaml
parameters:
  - {name: F_SC, value: 0.867, unit: fraction, source: "Absorption, PDF p3", note: "absolute, 4,500 IU SC vs IV, anti-Xa"}
  - {name: CL_IV, value: 1.7, unit: L/h, source: "Elimination, PDF p3", note: "approximate, after 4,500 IU IV, healthy volunteers"}
  - {name: V, value: "3.1-5.0", unit: L, source: "Distribution, PDF p3", note: "range; similar to blood volume"}
  - {name: t_half_SC, value: "3-4", unit: h, source: "Elimination, PDF p3; Table 2", note: "after 4,500 IU or 175 IU/kg SC"}
  - {name: t_half_IV_healthy, value: 1.6, unit: h, source: "Hemodialysis Studies, PDF p4", note: "comparator quoted for a 75 IU/kg IV dose"}
  - {name: t_half_IV_hemodialysis, value: 5.2, unit: h, source: "Hemodialysis Studies, PDF p4", note: "6 patients, off-dialysis day, 75 IU/kg IV"}
  - {name: anti-Xa_to_anti-IIa_ratio, value: 2.8, unit: ratio of AUCs, source: "Absorption, PDF p3", note: "UFH about 1.2"}
```
(derived: CL x t1/2(IV) / ln2 = 1.7 x 1.6 / 0.693 = 3.9 L, inside the quoted 3.1-5.0 L. The SC half-life of 3-4 h is
longer than the 1.6 h IV value, consistent with absorption-limited decline after SC dosing; my inference, not the
label's.) Anti-Xa rises over the first 2-3 h after SC injection and peaks at 4-5 h (PDF p3). Exposure is greater than
dose-proportional between 4,500 and 12,250 IU single doses (PDF p3). No accumulation over 5 days of 175 IU/kg (PDF p3).
No ka, Q or peripheral volume is given.

## Pharmacodynamics and end points
- Anti-Xa is a biomarker of exposure; the label says monitoring by anti-Xa is generally not advised and aPTT and PT are
  unsuitable for monitoring (PDF p2, p8, p14).
- Pregnancy (54 patients): 175 IU/kg gave mean anti-Xa 0.3-1.0 IU/mL at 4 h; similar curves at 28 (n = 17) and 36
  (n = 20) weeks (PDF p5).
- No target or toxic anti-Xa range, no concentration-effect relationship. Not an opioid.
- Protamine: 1 mg per 100 anti-Xa IU, then 0.5 mg per 100 IU if aPTT stays prolonged at 2-4 h; neutralizes at most about
  60% of anti-Xa activity (PDF p13-14).

Dosing (PDF p14-15): 175 anti-Xa IU/kg SC once daily for at least 6 days and until INR >= 2.0 on two consecutive days;
volume = weight (kg) x 0.00875 mL/kg (Table 8 and formula, PDF p14-15). Major bleeding 0.83% vs 2.73% for UFH (Table 4, N = 519 vs 524, PDF p10).

## Covariates
From a population PK analysis of about 180 DVT patients (175 IU/kg) and about 240 hip-replacement patients (~65 IU/kg),
neither PK parameters nor fit are printed (PDF p4):
- Age and sex: no significant effect on clearance.
- Renal function (Cockcroft-Gault CrCl): clearance reduced in moderate (30-50 mL/min) and severe (<30 mL/min)
  impairment; 24% lower in severe impairment than in normal (>80 mL/min). Hemodialysis (12 patients): anti-Xa clearance
  28% lower.
- Weight: an important predictor of clearance; PK parameters independent of weight and BMI when dosed per kg (obese
  study 101-165 kg, BMI 26-61; limited experience above BMI 40; PDF p5).
- Elderly (30 patients, 87.0 +/- 5.9 y, CrCl 40.6 +/- 15.3 mL/min): no accumulation of peak anti-Xa over 10 days (PDF p5).
- Contraindicated at 90 y or older with CrCl <= 60 mL/min (IRIS interim mortality; PDF p5, p7).

## Caveats
- 2008 label; no population-PK parameter vector is reported, only summary statistics and a qualitative covariate list.
- Anti-Xa activity is a surrogate measured by bioassay; values depend on the assay standard.
- Healthy-volunteer Table 2 values (N = 23 and 14) are non-compartmental.

## Used by stanpumpR
- `inst/extdata/anticoagulantRegistry.csv:21` (from `data-raw/anticoagulantRegistry.R:141-145`): row `tinzaparin_label`,
  LMWH, SC, anti-Xa activity (IU/mL), LMWH anti-Xa, `pending`, no drug; note "Barrett abstract gives CL/F and Vc/F but
  not ka, Q or Vp; no full population fit."

## Verification against stanpumpR
- Class, route (SC), analyte (anti-Xa IU/mL): **matches** (PDF p1-3).
- "No full population fit": **matches** for this label, which mentions a population PK analysis but prints none of its
  parameters, and gives no ka, Q or Vp.
- "Barrett abstract gives CL/F and Vc/F": refers to a different source (Barrett et al.), not to this label;
  **cannot verify** here.
- Readiness `pending` with no stanpumpR drug: consistent with the label's content.
</content>
</invoke>
