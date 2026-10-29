---
file: NJ_web-pmda-a1d7a5.pdf
pmid: null
doi: null
citation: "Pharmaceuticals and Medical Devices Agency (Japan). Report on the Deliberation Results and Review Report: Januvia Tablets / Glactiv Tablets 25 mg, 50 mg, 100 mg (sitagliptin phosphate hydrate). Applicants Banyu Pharmaceutical Co. and Ono Pharmaceutical Co.; application December 10, 2007; Review Report July 8, 2009; deliberation results August 25, 2009. English translation, https://www.pmda.go.jp/files/000153174.pdf"
kind: review
drugs: [sitagliptin]
source_read: "text layer of the 114-page PDF (114 form-feed chunks; printed page numbers equal PDF pages). Read: pp 1-4 (deliberation results, review results, approved indication and dosage), pp 36-61 (4.(i) biopharmaceutics, 4.(ii) clinical pharmacology including in vitro, healthy-subject PK, T2DM PK, population PK, renal, hepatic, elderly/sex/obesity, drug interactions, PMDA review of renal use and interactions), pp 96-103 (4.(iii).B.(4) special populations, 4.(iii).B.(6) dosage and administration), pp 106-107 and 111 (Review Report (2) renal use; overall evaluation). Tables 4, 5, 6 and 7 checked on rendered page images (PDF pp 42, 47, 49, 50); page 1 rendered for identity. Non-clinical (pp 9-35) and efficacy/safety (pp 61-95) skimmed only."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# PMDA 2009: Japanese review report for sitagliptin (Januvia / Glactiv)

## Study
A regulatory review, not a single study. It is the English translation of PMDA's Review Report (July 8, 2009) and
the MHLW deliberation results (August 25, 2009) for sitagliptin phosphate hydrate tablets, 25/50/100 mg, sold in
Japan as Januvia (Banyu) and Glactiv (Ono). The PDF's identity matches its URL and content: page 1 rendered.
Approved Japanese dosage: 50 mg orally once daily, which may be raised to 100 mg once daily (p 3, p 111). The
overseas approved dose of 100 mg once daily is cited for contrast (p 59). Salt MW 523.32 (p 2).

Clinical pharmacology data come from Japanese evaluation studies (P013, A111, A112, A201-A203, P046, P076) and
foreign reference studies (pp 41, 61). Assay: LC-MS/MS, LLOQ 0.5 ng/mL in plasma (p 36). Concentrations are
reported in nM and μM·h throughout.

## Model and parameters
No population PK parameter values are printed. A foreign population PK analysis (NONMEM V; 1059 healthy subjects
or T2DM patients aged 18-80; 10 119 samples from 16 studies) used a 2-compartment base model. Only renal function
was a clinically meaningful covariate (p 47-48). PMDA criticised the final model for including correlated covariates
(p 59).

Noncompartmental and descriptive values:
- **Absolute oral bioavailability 87%** (P029, foreign healthy adults, 100 mg; p 39).
- **IV (P029 Part I, 25-100 mg):** plasma CL 413-421 mL/min (geometric mean), renal CL 249-340 mL/min, volume of
  distribution 198-262 L (arithmetic mean), fe 0.654-0.828 (p 39).
- Protein binding about 38%; blood/plasma ratio 1.21 (in vitro, p 41).
- **Elimination route:** mainly renal, unchanged, with active tubular secretion: renal CL exceeded GFR (p 42). The
  drug is an OAT3 and P-gp substrate (p 41). In the 14C ADME study (P009, n = 6, 83.04 mg) about 87% of the dose
  was recovered in urine, about 79% of it unchanged, and about 13% in faeces; parent was 74% of plasma
  radioactivity (p 45). Metabolism is minor (CYP3A4, partly CYP2C8).
- **Table 4 (p 42; P013, Japanese healthy men, single fasted doses, n = 6 per dose):** geometric LS mean AUC, Cmax
  and CLR; median tmax; harmonic-mean t1/2.
- **Food:** in P029, fed/fasted AUC 1.03 [0.97, 1.11] and Cmax 0.94 [0.86, 1.03]. In P076 (Japanese men, 50 mg
  commercial tablet), AUC 0.98 [0.94, 1.02] but Cmax 1.37 [1.15, 1.62]. Fasted Cmax was 366 nM and AUC 4.08 μM·h;
  median tmax was 2.5 h fasted and 2 h fed (p 36, 39).
- **Multiple dosing (A111, Japanese healthy men, 25-200 mg once daily for 10 days):** steady state by day 2;
  AUC0-24 ratio day 10/day 1 was 1.03-1.19 (p 44).
- **Table 5 (p 47; A202, Japanese T2DM, geometric mean nM [95% CI]):** at week 12 the 50 mg arm was 51.0 nM pre-dose,
  443.1 at 1 h, 492.4 at 1.5 h and 446.0 at 2.5 h. The 100 mg arm was 81.9, 1098.8, 1149.9 and 980.8 nM at the same
  times.

```yaml
parameters:
  - {name: F_oral, value: 0.87, unit: fraction, source: "p 39 (P029)", note: "absolute bioavailability, 100 mg tablet, foreign healthy adults"}
  - {name: CL_plasma_IV, value: "413-421", unit: mL/min, source: "p 39 (P029 Part I)", note: "geometric mean range across 25-100 mg IV"}
  - {name: CL_renal_IV, value: "249-340", unit: mL/min, source: "p 39 (P029 Part I)", note: geometric mean range}
  - {name: Vd_IV, value: "198-262", unit: L, source: "p 39 (P029 Part I)", note: "arithmetic mean range; type of volume not stated"}
  - {name: fe_IV, value: "0.654-0.828", unit: fraction, source: "p 39 (P029 Part I)", note: "arithmetic mean range"}
  - {name: protein_binding, value: 0.38, unit: fraction, source: "p 41", note: "in vitro, 0.02-200 uM"}
  - {name: blood_plasma_ratio, value: 1.21, unit: ratio, source: "p 41", note: in vitro}
  - {name: Cmax_100mg_JP, value: 959, unit: nM, source: "Table 4 p 42", note: "P013 Japanese healthy men, n = 6, fasted, geometric LS mean"}
  - {name: AUCinf_100mg_JP, value: 8.65, unit: uM*h, source: "Table 4 p 42", note: "P013, geometric LS mean"}
  - {name: tmax_100mg_JP, value: 2, unit: h, source: "Table 4 p 42", note: median}
  - {name: t_half_JP, value: "9.07-13.8", unit: h, source: "Table 4 p 42", note: "harmonic mean, 400 mg to 5 mg; 9.56 h at 100 mg, 11.4 h at 50 mg"}
  - {name: CLR_JP, value: "299-513", unit: mL/min, source: "Table 4 p 42", note: "geometric LS mean across doses"}
  - {name: fe_JP, value: "0.730-1.00", unit: fraction, source: "Table 4 p 42", note: "arithmetic LS mean across doses"}
  - {name: Cmax_100mg_foreign_P032, value: 882.8, unit: nM, source: "p 46", note: "mean +/- SD 207.7; median tmax 3 h; foreign healthy men and women"}
  - {name: Cmax_50mg_JP_P076, value: 366, unit: nM, source: "p 36", note: "geometric mean, fasted, commercial 50 mg tablet; AUCinf 4.08 uM*h"}
  - {name: AUC_ratio_mild_renal, value: 1.61, unit: ratio, source: "Table 6 p 49 (P008)", note: "90% CI 1.43-1.81; CrCl 50 to <80 mL/min/1.73 m2"}
  - {name: AUC_ratio_moderate_renal, value: 2.26, unit: ratio, source: "Table 6 p 49 (P008)", note: "90% CI 2.02-2.53; 30 to <50"}
  - {name: AUC_ratio_severe_renal, value: 3.77, unit: ratio, source: "Table 6 p 49 (P008)", note: "90% CI 3.37-4.22; <30"}
  - {name: AUC_ratio_ESRD, value: 4.50, unit: ratio, source: "Table 6 p 49 (P008)", note: "90% CI 4.03-5.03; hemodialysis"}
  - {name: t_half_normal_renal, value: 13.1, unit: h, source: "Table 6 p 49", note: "harmonic mean, pooled historical controls (n = 82)"}
  - {name: EC50_DPP4, value: 26.2, unit: nM, source: "p 43 (P013)", note: "Emax model, plasma DPP-4 inhibition; Emax and slope not printed"}
```

## Pharmacodynamics and end points
- **Plasma DPP-4 inhibition, Emax model.** EC50 was 26.2 nM in P013, single dose (p 43), and about 25 nM on days 1
  and 10 of A111, with no difference between days (p 44). Emax, Hill coefficient and baseline are not reported.
- **Trough inhibition in Japanese T2DM (A202, week 12, geometric mean):** 67.3% [64.3, 70.1] at 50 mg and 77.3%
  [74.6, 79.7] at 100 mg. Doses of 50 mg and above inhibited DPP-4 almost completely by 1 h post-dose (p 47).
- **24-h inhibition in A203, week 4:** 79.7% with 100 mg once daily and 87.9% with 50 mg twice daily (p 47).
- **QTc (P032):** 100 mg had no effect. At 800 mg (Cmax 10 026 nM) the maximum placebo-corrected QTcF change was
  8.0 ms at 3 h (p 46).
- **HbA1c (A202):** the reductions were similar at 50 mg and above, about -1% against placebo (p 103).
- No glucose, insulin or GLP-1 exposure-response model is given. No ke0 or hysteresis is reported.

## Covariates
- **Renal function** (Table 6, P008, single 50 mg dose, foreign subjects, n = 6 per renal group). AUC, Cmax and
  C24h correlated with CrCl (ANCOVA, P < 0.05). Harmonic-mean t1/2 rose from 13.1 h (normal) through 16.1, 19.1 and
  22.5 h to 28.4 h (ESRD). Hemodialysis removed only a modest amount.
- **Moderate hepatic impairment** (P017, Child-Pugh 7-9, 100 mg, n = 10 + 10; Table 7, p 50): AUC ratio 1.21
  [1.01, 1.46] and Cmax ratio 1.13 [0.91, 1.42], judged not clinically meaningful.
- **Sex** (P003, 50 mg, p 51): female/male AUC 1.07 [0.97, 1.17] and Cmax 1.46 [1.23, 1.73].
- **Age** (P003, p 51): elderly/adult AUC 1.31 [1.19, 1.43], Cmax 1.23 [1.04, 1.46] and CLR 0.69, attributed to
  lower renal function.
- **BMI:** little effect (p 40).
- **Drug interactions** (sitagliptin as victim): cyclosporine raised AUC 1.29× and Cmax 1.68× (p 58); voglibose
  lowered AUC to 0.83× and Cmax to 0.66× (p 52).
- **Sitagliptin as perpetrator:** digoxin AUC 1.11-1.18× and Cmax 1.18-1.24× (p 55). Metformin, glibenclamide,
  simvastatin, rosiglitazone, warfarin and the oral contraceptive were not meaningfully affected.

## Caveats
- This is a regulatory review. Many study dates and formulation details are redacted (asterisks). The population PK
  model is described but none of its parameters are printed.
- Japanese-subject PK is from small panels (n = 6 per dose in P013; n = 8 per panel in A111).
- The renal ratios come from P008, with only 6 patients per renal group, against a pooled historical control of
  n = 82, dose-normalised to 50 mg (Table 6 footnotes).
- **Renal dosing in Japan at approval:** at first PMDA would have avoided use in moderate or severe insufficiency
  (p 59, p 98). After the Expert Discussion, moderate insufficiency was allowed at 25 mg with careful use, and severe
  insufficiency or dialysis was contraindicated (pp 106-107). This differs from the US label.
- The Cmax food effect differed between the Japanese and foreign studies (p 39-40).

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:59`, row key `sitagliptin`, built by `data-raw/antidiabeticRegistry.R:242-244`.
  The row gives product "Januvia", jurisdiction "US", pk_status PK-CONSTRAINT, spec_pd_status PD-FIT, pd_status
  PD-AUDIT, pd_endpoint "plasma DPP-4 inhibition" and stanpumpr_drug NA (registry-only). source_url is this PMDA
  file.
- The row's notes: "Label constraints only", Sangle 2018 and Zhou 2024 as noncompartmental constraints (t1/2 about
  10 h; fasted Cmax 492 ng/mL), no population model, and the Scheen 2015 AUC ratios 1.61/2.26/3.77/4.50 for
  mild/moderate/severe CKD and dialysis.

## Verification against stanpumpR
- **Renal AUC ratios 1.61/2.26/3.77/4.50: matches.** They equal Table 6 (p 49, Study P008) exactly. The row credits
  them to Scheen 2015; this document gives them from P008, the same underlying study.
- **"no population model": matches in substance.** The review says a NONMEM 2-compartment population analysis was
  done (p 47-48) but prints no parameters, so the source gives no usable model. The registry could say "no
  published parameters" instead of implying no model was fitted.
- **"t1/2 about 10 h" and "fasted Cmax 492 ng/mL": cannot verify here.** The row attributes both to Sangle 2018,
  which is not this document. This review's figures for comparison:
  - harmonic-mean t1/2 9.56 h at 100 mg and 9.07-13.8 h across doses in Japanese men (Table 4);
  - t1/2 13.1 h in pooled normal-renal controls (Table 6);
  - 100 mg Cmax 959 nM, about 391 ng/mL (derived with free-base MW 407.3 = 523.32 - H3PO4 - H2O), and 882.8 nM in
    P032.
  - Numeric coincidence only: Table 5 has 492.4 nM, the A202 50 mg week-12 value at 1.5 h. Its unit (nM) and cohort
    differ from the registry's ng/mL fasted single-dose Cmax, and nothing shows the two are linked.
- **PD endpoint "plasma DPP-4 inhibition" with pd_status PD-AUDIT: consistent.** The review reports an Emax-model
  EC50 of 26.2 nM (p 43) and about 25 nM (p 44) but no Emax or Hill coefficient, so it does not support a full
  PD-FIT from this source.
- **Source type and jurisdiction: discrepancy (labelling).** The notes call it "Label constraints only" and the row
  says jurisdiction "US". The linked source is a Japanese PMDA review report, not a label. The dosing it approves is
  Japanese: 50 mg daily, up to 100 mg, with moderate renal impairment at 25 mg and severe impairment contraindicated
  (pp 3, 106-107, 111), not the US 100 mg. No numeric field in the row depends on this. Location
  `inst/extdata/antidiabeticRegistry.csv:59` / `data-raw/antidiabeticRegistry.R:243-244`.
- **Absolute F 87%, IV CL about 415 mL/min and V 198-262 L (p 39):** present in this source but not used in the
  registry row. Arithmetic check: CL 0.415 L/min × 60 = 24.9 L/h. With t1/2 of about 12 h, Vz is about
  24.9 × 12 / 0.693, roughly 430 L (derived). That is larger than the printed 198-262 L, which may be a different
  volume term (not stated); unresolved, and it does not affect the repo.
