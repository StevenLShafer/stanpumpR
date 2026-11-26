---
file: NJ_fda-nda207145_207145_207145Orig1s000ClinPharmR.pdf
pmid: ""
doi: ""
citation: "FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 207145, Xadago (safinamide mesylate) tablets 50 and 100 mg, Newron Pharmaceuticals; submission 29 Dec 2014; reviewer signatures dated 3-4 Dec 2015. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/207145Orig1s000ClinPharmR.pdf"
kind: review
drugs: [safinamide]
source_read: "text layer of the question-based review (PDF pp1-57; QBR page references below are printed page numbers, which are the PDF page minus 1), the Pharmacometrics Review (PDF pp 262-271, text layer) and the platelet MAO-B pharmacodynamic passages of the individual-study reviews (PDF pp 69, 160, text layer); page images read for PDF pp 16, 17 (absolute bioavailability tables), 47 (caffeine/midazolam DDI), 262 (popPK Table 1) and 263 (popPK conclusions). The 271-page file's individual-study reviews (PDF pp 64-250) were not read in full; the hepatic and renal impairment tables and the BE and midazolam ANOVA tables are images that were not rendered."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 8c1ea1bd
verification: partly-verified
open_questions:
  - "[source] The review prints Model 136's coefficients (Table 1, PDF p262) but not the equations; R/drugs_safinamide.R:27-28 uses CL/F = 4.54 (WT/70)^0.75 (AGE/60)^-0.199 / Frel and V/F = 168 (WT/70) / Frel. The power forms agree with the review's own statements (41% lower to 63% higher CL/F over 35-134 kg; 50% lower to 91% higher Vd/F; 18% fall in CL/F from 30 to 80 y; PDF p263), but the age reference of 60 y and whether Frel divides both apparent parameters are inferred. The sponsor's report safinamide-pop-pk-pd.pdf (Table 11-15 and the control stream) would settle both."
  - "[source] The review does not say whether the phase 3 doses and the 50/100 mg tablets are expressed as safinamide base or as the mesylate (it calls them '50 mg safinamide' and gives molecular masses 302.34 base and 398.45 mesylate, QBR printed p8). The library assumes base (R/drugs_safinamide.R:46-52); the label or the sponsor's report settles it."
---

# FDA CDER 2015: Clinical Pharmacology and Biopharmaceutics Review, Xadago (safinamide), NDA 207145

## Study
FDA review (Office of Clinical Pharmacology) of a standard NDA for safinamide as add-on therapy in Parkinson's disease with levodopa (the agency would not recommend it in early PD). Tablets 50 and 100 mg once daily, with or without food; start at 50 mg, increase to 100 mg (QBR printed p9-10). The review covers 37 clinical trials (20 phase I, 8 phase II, 9 phase III; 3184 subjects) and 41 in vitro studies (QBR printed p4). Assay: LC-MS/MS, LLOQ 20 ng/mL originally, later 15 ng/mL; free-fraction assay by equilibrium dialysis 5-1000 ng/mL (QBR printed p56). Plasma assay, total (not free) concentrations. Steady-state maximum concentrations at 100 mg/day: 700-2200 ng/mL safinamide, 60-200 NW-1153, 1800-13000 NW-1689, 300-5000 NW-1689 AG (QBR printed p56).

## Model and parameters
Population PK (Pharmacometrics Review, PDF pp262-263, page images read): sponsor's final Model 136, linear one-compartment with first-order absorption and elimination, 3 phase III trials in patients with PD (study 016 n=446, MOTION n=452, SETTLE n=273; 1620 + 2913 + 1754 samples). Table 1 (Table 11-15 of the sponsor's report): reference patient is 70 kg (the age and race reference are given in the text, PDF p263: 60 y, Asian).

```yaml
parameters:
  - {name: CL/F, value: 4.54, unit: L/h, source: "PopPK Table 1, PDF p262", note: "typical, 70 kg, 60 y, Asian; RSE 1.22%, 95% CI 4.43-4.65"}
  - {name: Vd/F, value: 168, unit: L, source: "PopPK Table 1, PDF p262", note: "RSE 1.17%, 95% CI 164-172"}
  - {name: KA, value: 1.19, unit: 1/h, source: "PopPK Table 1, PDF p262", note: "RSE 8.03%, 95% CI 1.00-1.38"}
  - {name: CL/F weight exponent, value: 0.75, unit: none, source: "PopPK Table 1, PDF p262", note: fixed}
  - {name: Vd/F weight exponent, value: 1, unit: none, source: "PopPK Table 1, PDF p262", note: fixed}
  - {name: CL/F age coefficient, value: -0.199, unit: none, source: "PopPK Table 1, PDF p262", note: "RSE 24.0%, 95% CI -0.293 to -0.105; functional form not printed"}
  - {name: F Whites, value: 1.04, unit: ratio to Asian, source: "PopPK Table 1, PDF p262", note: "95% CI 1.01-1.07"}
  - {name: F Others, value: 1.16, unit: ratio to Asian, source: "PopPK Table 1, PDF p262", note: "95% CI 1.10-1.22"}
  - {name: omega2 CL, value: 0.0506, unit: variance, source: "PopPK Table 1, PDF p262", note: CV 22.5%}
  - {name: omega2 Vd, value: 0.0329, unit: variance, source: "PopPK Table 1, PDF p262", note: "CV 18.1%; covariance 0.0232, R 0.569"}
  - {name: sigma2 proportional, value: 0.0648, unit: variance, source: "PopPK Table 1, PDF p262", note: CV 25.5%}
  - {name: Cmax fasted oral 50 mg, value: 322.3, unit: ng/mL, source: "p15 table (PDF p16)", note: "geometric mean (CV 17.9%), n=14 healthy, study EMR701165-022"}
  - {name: tmax fasted oral, value: 2.5, unit: h, source: "p15 table (PDF p16)", note: "median, range 2.0-5.0"}
  - {name: AUC0-inf oral, value: 10205, unit: ng.h/mL, source: "p15 table (PDF p16)", note: "geometric mean (CV 26%)"}
  - {name: t1/2, value: 26.2, unit: h, source: "p15 table (PDF p16)", note: "oral; IV 26.0 h"}
  - {name: CL/f, value: 4.9, unit: L/h, source: "p15 table (PDF p16)", note: "oral (CV 26%); IV CL 4.6 L/h"}
  - {name: Vz/f, value: 185.0, unit: L, source: "p15 table (PDF p16)", note: "oral; IV 174.1 L"}
  - {name: absolute bioavailability, value: 0.9498, unit: fraction, source: "p15 table (PDF p16)", note: "geometric mean 94.98% (CV 9.25%), range 83.93-119.33%; AUC ratio oral/IV 94.62% (90% CI 90.09-99.39)"}
  - {name: Vss, value: 165, unit: L, source: "QBR printed p16", note: "about 2.5-fold body volume"}
```

Other facts (text layer): bound fraction in plasma 89% (safinamide), 71% NW-1153, over 99.5% NW-1689, 97% NW-1689 AG, mainly to albumin, concentration-independent over 0.5-50 uM; blood-cell/plasma coefficient 0.4-0.6 (QBR printed p16-17). Metabolised mainly by cytosolic amidases, MAO-A and ALDH; CYP3A4 minor; ketoconazole raised safinamide AUC 13% (study 28778). About 5-7% excreted unchanged; 76% of radioactivity in urine, 1.5% in faeces over 192 h. Metabolites NW-1153, NW-1689, NW-1689 AG are pharmacologically inactive; NW-1689 reaches 161% of parent exposure; their t1/2 are 19.5, 25.3 and 26.5 h (QBR printed p19). Fasted tmax 1.8-2.8 h; food delays tmax 0.75 h (90% CI 0.50-1.25) with AUC and Cmax ratios 100% and 95% (QBR printed pp7, 52). PK is dose-proportional (50 mg to 10 mg/kg single dose; 100 vs 350 mg trough ratio 3.5-fold) and time-independent; steady state in 5-6 days. Hepatic impairment: AUC about +30% (Child-Pugh A) and +80% (Child-Pugh B, so 50 mg maximum); not studied in severe impairment. Renal impairment does not change safinamide PK (metabolite AG exposure is 4-5-fold higher). No difference between healthy subjects and PD patients (QBR printed p6).

## Pharmacodynamics and end points
No ke0, hysteresis, effect-compartment model or tPeak is reported; no concentration range is stated; the review says the exposure-effectiveness analysis (average concentration vs ON time, studies 016 and SETTLE) showed no difference between 50 and 100 mg and no exposure-safety analysis was done (QBR printed p13-14). Platelet MAO-B (the pharmacological target, IC50 79 nM in human brain mitochondrial fraction and 9.3 nM in platelet-rich plasma, over 1000-fold selective over MAO-A, QBR printed p9): in healthy men, single 300 and 600 ug/kg doses inhibited platelet MAO-B by about 84% and 92%, with maximal inhibition at 3-12 h and statistically significant inhibition still present at 120 h, activity restored after 168 h at 600 ug/kg (PDF p160, text layer); 75% and 69% inhibition still seen 24 h after 600 and 300 ug/kg (QBR printed p10). In the first-in-human study, inhibition was still high 48 h after 2.5-10 mg/kg (PDF p69). Effect therefore outlasts plasma concentration. QTc: no effect at 100 mg and 350 mg (QBR printed p14). Tyramine sensitivity factor ratios vs placebo 1.6 (100 mg), 1.8 (350 mg), 2.2 (selegiline 10 mg), 6 (phenelzine 30 mg) (QBR printed p39). Not an opioid; no MEAC or potency ratio.

## Covariates
PopPK (PDF p263, image): weight allometric (CL/F 0.75 fixed, Vd/F 1 fixed; over 35-134 kg CL/F 41% lower to 63% higher and Vd/F 50% lower to 91% higher than 70 kg); age on CL/F (18% decrease over 30-80 y); relative bioavailability 4% higher in Whites and 16% higher in Others than Asians (comparator: Asian patients of the same trials). Sex, renal function and levodopa had no effect.

## Caveats
The sponsor's popPK model leaves unexplained misfit in some individuals (reviewer's comment, PDF p264, text layer). Study 015 PK data were excluded because of a roughly 30% lower exposure in its follow-up study 017 and measurable safinamide in 26% of placebo samples (QBR printed p52). Race effect compares against Asian patients, not against healthy volunteers. Doses and the sponsor's report equation forms are not in the review. Draft labelling pages are withheld.

## Used by stanpumpR
The context lists no repo location that cites this file (`cited_in_repo: false`), but the checkout does: R/drugs_safinamide.R (header, lines 11-38, and constants at 83-91), inst/extdata/parkinsonsParameters.csv:23-28, and inst/extdata/therapeuticRanges.csv:232 ("no therapeutic range is stated").

## Verification against stanpumpR
- Constants (R/drugs_safinamide.R:84-91): CL 4.54 L/h, V 168 L, KA 1.19 /h, CL weight exponent 0.75, V weight exponent 1, age exponent -0.199, age reference 60 y, F White 1.04. **Matches** PopPK Table 1 and the text of PDF p262-263 (page images read). Units: the code divides CL/F and KA by 60 (R/drugs_safinamide.R:112-113), turning L/h and 1/h into L/min and 1/min; V stays in L. **Matches**.
- Equation forms (lines 106-108): CL = 4.54 (W/70)^0.75 (age/60)^-0.199 / 1.04 and V = 168 (W/70) / 1.04. The review prints only coefficients, so the forms are inferred; the header's arithmetic checks the review's quoted ranges: (35/70)^0.75 = 0.59 (41% lower), (134/70)^0.75 = 1.63, 0.50 and 1.91 for V, (80/30)^-0.199 = 0.82 (18% lower). **Consistent; the equation itself cannot be verified from this document** (open question).
- Race: the library uses Whites (1.04, relative to Asian), so its typical concentrations are 4% below the Asian reference's; the review's comparator for F is Asian patients. **Matches** the header's own statement (lines 42-44).
- Sanity checks in the header: label half-life 20-26 h (review printed p15, PDF p16: 26.2 h oral, 26.0 h IV in healthy volunteers; terminal 20-26 h, QBR printed p6), and tmax 2-3 h; the code's 3.2 h after one tablet (header line 62) is slightly later than the review's median fasted tmax 2.5 h (range 2.0-5.0) and 1.8-2.8 h across studies. Healthy-volunteer apparent CL/f is 4.9 L/h (oral, PDF p16) and Vz/f 185 L, against the PD-patient Model 136 values the code uses (4.54 L/h, 168 L, Asian, 70 kg, 60 y); the code's White 70 kg 60 y CL/F 4.37 L/h is 11% below the healthy-volunteer 4.9. The two populations differ; this is not a discrepancy.
- "No effect site, no band" (R/drugs_safinamide.R:66-72, tPeak 0, MEAC 0): the review supports the premise: no ke0 or concentration range, platelet MAO-B inhibition persists to 120 h (PDF p160) and no exposure-response was found for ON time. The header's "over 90% inhibition above 20 mg" is not in the pages read; **cannot verify** here.
- IC50 9.3 nM in platelet-rich plasma (header line 68): **matches** (QBR printed p9).
- Midazolam: safinamide lowered midazolam AUC about 20% after 14 days at 100 mg (a weak CYP3A4 induction; PDF p47, image read); caffeine AUC +30% after one dose, +13% after 14 days (weak CYP1A2 inhibition). Midazolam is a library drug; the library does not model a safinamide-midazolam interaction and none is required by this review.
- Therapeutic ranges file (inst/extdata/therapeuticRanges.csv:232, "Xadago (FDA): no therapeutic range is stated"): **matches** (no range in the pages read; 100 mg/day steady-state Cmax 700-2200 ng/mL is an observed exposure, not a target).
