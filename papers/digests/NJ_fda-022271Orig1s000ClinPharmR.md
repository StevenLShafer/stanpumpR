---
file: NJ_fda-022271Orig1s000ClinPharmR.pdf
pmid: ""
doi: ""
citation: "FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 022271 (Nesina, alogliptin benzoate, Takeda), file 022271Orig1s000ClinPharmR: original review (submission 27 Dec 2007; Chung, Bi; pharmacometric review Earp) plus resubmission review (submission 25 Jul 2011; Chung), its 7 Mar 2012 amendment, and Biopharmaceutics memos of 4 Apr 2012 and 13 Dec 2012."
kind: review
drugs: [alogliptin]
source_read: "partial. Text layer read for the cover memos, both executive summaries, the original review's question-based review (PDF pp. 53-71), the study synopses for the mass-balance, single-dose and renal-impairment studies, the pharmacometric review (population PK, PDF pp. 189-198) and the resubmission's renal-impairment section; the drug-interaction, food-effect, QT and in-vitro study sections (about PDF pp. 105-185) were not read. Page images rendered and read for PDF pp. 16, 25, 61, 77, 97, 99, 100, 191, 193, 196. The sponsor's population-PK tables 8.5, 8.6 and 8.9 are not in this PDF."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 78681f75
verification: discrepancy
open_questions:
  - "[source] R/drugs_alogliptin.R:61-66 and inst/app-help/drugs/alogliptin.md:10 say about three quarters of a dose is excreted unchanged in urine. This review (original, PDF 54, 60 and 76-77, Study 014, 25 mg 14C solution, n=8) gives about 68% of the dose as alogliptin in urine by 120 h; 75.6% (76%) is the urinary share of total radioactivity, with 12.9% in feces (88.5% total recovery). The sponsor popPK text says 60% to 70% unchanged (PDF 192). Which figure the repo intends, and whether 'three quarters' should be about two thirds, needs Shafer's call."
  - "[source] R/drugs_alogliptin.R:37-48, inst/app-help/drugs/alogliptin.md:16 and inst/extdata/antidiabeticParameters.csv:262 attribute to this review (a) Cockcroft-Gault on 'total body weight' in the renal-impairment study, (b) a sponsor popPK whose median creatinine clearance is 72.95 mL/min, computed from serum creatinine, weight, age and sex. The review names Cockcroft-Gault for Study 006 (PDF 97) but says only 'Weight(kg)', not total or ideal body weight. The popPK section prints 72.95 as the reference in the CL/F equation (PDF 196) without calling it a median or naming the formula; Table 8.5 (demographics) is missing from the PDF ('page ##', PDF 191). The only statement that creatinine clearance was computed from serum creatinine, weight, age and gender is for the renal-safety analysis of Study PLC-010 (pharmacometric review), not the popPK. The sponsor's own popPK report (or Naik 2016 supplement) would settle both."
  - "[source] R/drugs_alogliptin.R:58-59 sets the dose basis as mg of alogliptin. This review gives MW 461.51 for the benzoate and 339.39 for the free base (PDF 55) but, in the pages read, does not say whether the 6.25/12.5/25 mg strengths are base-equivalent. The Nesina label (papers/digests/NJ_label-nesina_022271_2025-03-24.md) would settle it; the ratio 461.51/339.39 = 1.36 is the size of the error if it is wrong."
---

# FDA 2008-2012: Nesina (alogliptin) clinical pharmacology review

## Study
This PDF holds five documents for NDA 22-271. (1) Biopharmaceutics memo, 13 Dec 2012, and assessment, 4 Apr 2012: dissolution only (0.01 N HCl, 900 mL, paddle 50 rpm; high solubility, 47.0 to 21.9 mg/mL across pH 1-6.8); NDAs 22271 and 22426 (Oseni) recommended for approval. (2) Amendment of 7 Mar 2012 to the resubmission review. (3) Resubmission clinical pharmacology review (submission 25 Jul 2011): absolute bioavailability study SYR-322-103 (25 mg oral vs 12.5 mg IV, 2-period crossover, 24 planned, 21 analyzed; the F value is in a table not reproduced in the text layer or the pages read), once vs twice daily, voglibose interaction (voglibose cut AUC(0-72) 23.2% and Cmax 10.3%), and the mild-renal-impairment question. (4) The original review (Dec 2007 submission), the substantive PK source, with 24 phase 1 studies, one phase 2 and five phase 3 studies. (5) Its pharmacometric review (exposure-response, exposure-safety, and the sponsor's popPK of phase 3 Study 008, 375 subjects, 788 concentrations, 1.07-347 ng/mL, median age 56 y, weight mean 88 kg, range 45.5-141.6). Assay: plasma LC/MS/MS, LLOQ 1.00 ng/mL, linear to 1000 ng/mL (PDF 71).

## Model and parameters
Typical values, free of the sponsor's table (Table 8.9 absent); sources are printed review pages.

```yaml
parameters:
  - {name: CL/F, value: 16.88, unit: L/h, source: "Table 14, PDF 77 (printed p. 33)", note: "25 mg oral solution, 8 healthy men, mean (CV 14.5%)"}
  - {name: Vz/F, value: 609.61, unit: L, source: "Table 14, PDF 77", note: "mean (CV 40.3%); Vz, not Vss"}
  - {name: terminal t1/2, value: 25.61, unit: h, source: "Table 14, PDF 77", note: "mean (CV 43.9%); Study 027 tablets 19.5-20.2 h (PDF 75)"}
  - {name: tmax, value: 2.0, unit: h, source: "Table 14, PDF 77", note: "median, range 1.0-3.0; tablets median 1.00 (0.5-6.0), Table 2, PDF 61"}
  - {name: Cmax 25 mg, value: 100.1, unit: ng/mL, source: "Table 14, PDF 77", note: "solution; Table 2 PDF 61: tablets 125.8 (commercial) and 118.6 (phase 3), n=36"}
  - {name: AUC0-inf 25 mg, value: 1505.97, unit: ng.h/mL, source: "Table 14, PDF 77", note: "solution; tablets 1530.5 and 1525.4, Table 2"}
  - {name: CL/F, value: 16.72, unit: L/h, source: "Table 2, PDF 61", note: "25 mg commercial tablet, mean (CV 15.7%); phase 3 tablet 16.83"}
  - {name: renal excretion Fe(0-24), value: 35.23, unit: "% of dose", source: "Table 2, PDF 61", note: "25 mg tablet; CLr 8.41 L/h"}
  - {name: urinary recovery as alogliptin, value: 68, unit: "% of dose by 120 h", source: "text, PDF 77 (printed p. 32)", note: "75.59% of radioactivity in urine, 12.94% feces, total 88.53%"}
  - {name: plasma protein binding, value: "28-38", unit: "%", source: "text, PDF 60 (p. 11)", note: "concentration dependent over 0.01-10 ug/mL"}
  - {name: M1 and M2 exposure, value: "<1 and <4", unit: "% of alogliptin AUC", source: "text, PDF 54 and 61", note: "M1 active on DPP-4 like the parent, M2 not"}
  - {name: dose proportionality slope, value: 1.052, unit: power model, source: "Table 4, PDF 63", note: "AUC0-t single dose, 90% CI 1.010-1.095; Cmax 1.214 (1.150-1.278); multiple dose AUC 0.952 (0.91-1.00), Cmax 1.008 (0.94-1.07)"}
  - {name: accumulation ratio day 14, value: 1.34, unit: AUC0-24, source: "text, PDF 63", note: "Cmax 1.09; 25 mg QD in T2D"}
  - {name: popPK CL/F, value: "17.8 x (CrCL/72.95)^0.375 + 0.086 x (WT - 85.15)", unit: L/h, source: "Eq. 9, PDF 196 (printed p. 152)", note: "sponsor Study 008 final model; CrCL in mL/min, WT in kg; additive weight term"}
  - {name: popPK Vc/F, value: "187 x (WT/85.15)^1.5", unit: L, source: "Eq. 10, PDF 196", note: "sponsor Study 008 final model"}
  - {name: popPK Vp, value: 191, unit: L, source: "text, PDF 192, 195-196 (p. 148, 151-152)", note: "fixed, from Study 002 (T2D, full profiles); not estimable from sparse Study 008; Q and ka values are in Table 8.9, absent"}
  - {name: popPK terminal t1/2, value: 20.9, unit: h, source: "text, PDF 197-198 (p. 153-154)", note: "from typical predicted values"}
  - {name: popPK IIV, value: "ka and CL/F; Vc/F shares the CL/F eta", unit: "", source: "PDF 196", note: "RV 32.25 %CV; intra-occasion CV from Study 002 re-fit: CL 44.0%, Vc 65.8%, IIV 29.9% (pharmacometric review p. 139, PDF 183)"}
```

Renal-impairment study SYR-322-006 (50 mg tablet, single dose; groups by Cockcroft-Gault CrCl: healthy > 80, mild 51-80, moderate 30-50, severe < 30 not on dialysis, ESRD on hemodialysis; 6 per impaired group, 24 healthy; PDF 97). Tables 34 and 7 (PDF 99, 100, 25; arithmetic means, CV in brackets in the source):

```yaml
parameters:
  - {name: mild CL/F, value: 9.53, unit: L/h, source: "Table 34, PDF 99", note: "healthy match 15.48; AUC0-t 5554 vs 3199 ng.h/mL, ratio 169% (134-213); t1/2 40.4 h"}
  - {name: moderate CL/F, value: 8.21, unit: L/h, source: "Table 34, PDF 99", note: "healthy match 16.19; AUC0-t ratio 208% (168-258); t1/2 40.0 h"}
  - {name: severe CL/F, value: 4.30, unit: L/h, source: "Table 34, PDF 100", note: "healthy match 14.43; AUC0-t ratio 319% (235-432); t1/2 60.9 h"}
  - {name: ESRD CL/F, value: 3.42, unit: L/h, source: "Table 34, PDF 100", note: "healthy match 16.15; AUC0-t ratio 381% (309-469); t1/2 80.0 h"}
  - {name: renal clearance CLr, value: "5.74 / 4.54 / 0.99", unit: L/h, source: "Table 34, PDF 100", note: "mild / moderate / severe; healthy matches 10.14, 9.71, 8.80"}
```

Mild-group AUC increase of 69% (original) is driven by one subject: the 2012 amendment (the second document in the file) and the resubmission review (PDF 25) give mean AUC0-t 4738.9 ng.h/mL (n=5, without that subject) vs 3258.1 (n=24 pooled controls), +45%; +76% with all six against the matched controls (3261.24, AUC0-inf). The original's "ng/ml*min" is a unit typo for ng.h/mL.
Other intrinsic factors (original summary, PDF 54 and 64): AUC0-t +28% in the elderly, +19% in women, +28% in white vs black subjects; moderate hepatic impairment and food had no significant effect; T2D exposure at 400 mg QD was slightly lower than in healthy volunteers (Table 5: day-1 AUC0-24 15823 vs 20162 ng.h/mL).

## Pharmacodynamics and end points
- Efficacy surrogate is HbA1c; end point of exposure-response is ex vivo plasma DPP-4 inhibition. Over 80% inhibition for 24 h is described as the goal for chronic glucose lowering, and 25 mg was the minimum dose reaching it (PDF 55, section 2.2.1); minimum effective dose on HbA1c was 12.5 mg (PDF 53).
- No concentration-HbA1c relationship was found across the phase 3 studies, no 25 mg benefit over 12.5 mg by exposure-response (pharmacometric review, PDF 177-178 and 184). No MEAC, ke0, t1/2ke0 or tPeak is reported. No QTc effect at 50 or 400 mg.
- Concentration range at 25 mg: Cmax 100-126 ng/mL (single dose); popPK observations 1.07-347 ng/mL. No therapeutic range is stated.
- Not an opioid; no equianalgesic ratios.

## Covariates
Sponsor popPK (Study 008): creatinine clearance (power 0.375) and weight (linear) on CL/F, weight (power 1.5) on Vc/F; age on CL/F was significant but imprecise (%SEM > 150) and dropped; sex, race and time-varying covariates were not retained (PDF 193-196). The FDA pharmacometric reviewer judged weight on CL/F unnecessary given creatinine clearance (reduces IIV 0.84 points, PDF 181, 184, 193-194). The renal-impairment study calculates Cockcroft-Gault with c = 0.85 for women; the printed formula has misplaced parentheses (PDF 97), the standard formula being meant.

## Caveats
The popPK uses sparse samples (1 trough, 1 non-trough) from a metformin add-on study at steady state; Vp is fixed at 191 L and the MVOF kept falling to 100 times that value, so the distribution phase is not determined by these data (PDF 195-196). Renal-impairment groups are 6 subjects each. Several tables (absolute bioavailability, Table 8.5/8.6/8.9, DDI tables) are pictures not carried by the text layer or absent. Review dates run 2008-2012; labelling later changed.

## Used by stanpumpR
The context file lists no locations. The checkout cites this file in `R/drugs_alogliptin.R:37-48, 72-73` (Cockcroft-Gault as the renal estimator, 72.95 normalisation), `inst/app-help/drugs/alogliptin.md:10, 16`, `inst/extdata/antidiabeticParameters.csv:262` and the registry `data-raw/antidiabeticRegistry.R:685` (CrCl method "Cockcroft-Gault, total body weight"), `inst/extdata/therapeuticRanges.csv:120` (no range established). The PK model itself is Naik 2016, not this review.

## Verification against stanpumpR
- **Cockcroft-Gault as the estimator (alogliptin.R:37-48):** the renal-impairment study grouped subjects by Cockcroft-Gault with 0.85 for women (PDF 97) and the PK study used CG (resubmission, PDF 25). **Matches**, with the weight type unstated: 'total body weight' (antidiabeticParameters.csv:262, alogliptin.md:16) is **not supported by this review** (it prints 'Weight(kg)'). Open question 2.
- **72.95 mL/min (alogliptin.R:40-45, alogliptin.md:16):** present as the reference in the sponsor's CL/F equation, but the review does not call it a median, and its popPK does not state the formula used for creatinine clearance. **Cannot verify** as written.
- **'About three quarters excreted unchanged' (alogliptin.R:61-66, alogliptin.md:10):** this review says about 68% of the dose is recovered in urine as alogliptin; 76% is total radioactivity. **Discrepancy** (open question 1).
- **Dose basis (alogliptin.R:58-59):** cannot verify here (open question 3).
- **Independent comparison of the Naik model (R/drugs_alogliptin.R:21-24, 76-87) with this review:** Naik's typical CL/F is 17.1 L/h x (CLcr/72.95)^0.534 x (WT/84.90)^0.345 and Vc/F 194 L x (WT/84.90)^1.11, Vp/F 149 L; the sponsor's Study 008 model reviewed here is CL/F 17.8 x (CrCL/72.95)^0.375 + 0.086(WT-85.15), Vc/F 187 x (WT/85.15)^1.5, Vp fixed 191 L. At the reference (73 mL/min, 85 kg) CL/F is 17.1 (Naik) vs 17.8 L/h, Vc/F 194 vs 187 L; Naik's creatinine exponent is steeper (0.534 vs 0.375) and its Vp smaller (149 vs 191 L). Healthy-volunteer non-compartmental CL/F is 16.7-16.9 L/h (Tables 2 and 14), within 2% of the Naik value at the reference. Observed 50 mg CL/F falls to 9.53, 8.21, 4.30 and 3.42 L/h in mild, moderate, severe and ESRD (Table 34); with illustrative midpoint CrCl of 65, 40 and 20 mL/min (my assumption; the study's group CrCl values were not read) Naik's formula gives about 16.1, 12.4 and 4.8 L/h at 85 kg (derived), so Naik is well above the observed means in mild and moderate impairment (the mild mean is inflated by one subject; the 2012 amendment gives AUC +45% without that subject, which is closer to Naik's direction but still more clearance than observed). Observed group means are against this study's own healthy controls (CL/F 14.4-16.2 L/h), not against Naik.
- Terminal half-life: Naik's model is not tested here; the review's t1/2 of 19.5-25.6 h (healthy) and 20.9 h popPK (typical) are for comparison with the library's two-compartment curve, which was not run in this digest.
- Drugs that appear here only as interaction substrates (metformin, pioglitazone, digoxin, midazolam, warfarin, etc.) were not compared with their library models.
- Audit: no opioids in this document.
