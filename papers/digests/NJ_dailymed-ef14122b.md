---
file: NJ_dailymed-ef14122b.pdf
pmid: null
doi: null
citation: "Physicians Total Care (repackager of GlaxoSmithKline). AVANDIA (rosiglitazone maleate) film-coated tablets 2, 4, 8 mg, NDA 021071. DailyMed SPL setid ef14122b-7fff-45fa-b13b-0ea9e48bd57d; SPL revised 1/2010; archived 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 36 PDF pages (PLR format): boxed warning, sections 1-3 Indications, Dosage and Administration incl. 2.4 specific populations, Dosage forms p1-2; 4-5 Contraindications and Warnings (headings and 5.6 hepatic skimmed) p2-10; 7 Drug Interactions p13; 8 Use in Specific Populations p13-15; 10 Overdosage, 11 Description p15-16; 12.1-12.4 Clinical Pharmacology incl. Table 8 p16-20; 15-16 References, How Supplied p27; SPL product data p32-36. Table 8 and the 12.3 text (PDF p17-18) checked on rendered pages."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# AVANDIA (rosiglitazone) label, repackager SPL, January 2010

Rosiglitazone is not a stanpumpR drug (registry-only), so `drugs` is empty.

## Study
Not a study: a 2010 repackager SPL (Physicians Total Care; NDCs 54868-5249, -4198, -4221; PDF p27, p33-36) of the GSK
Avandia label, NDA021071. Tablets contain rosiglitazone maleate equivalent to 2, 4 or 8 mg rosiglitazone base (MW 473.52
maleate, 357.44 base; PDF p15-16), so doses are as base. It carries the 2010 boxed warning on heart failure and myocardial
ischaemia (PDF p1); it is not the current Avandia label.

## Model and parameters
Table 8 (PDF p17-18, rendered): mean (SD), single oral doses, N = 32:

| | 1 mg fasting | 2 mg fasting | 8 mg fasting | 8 mg fed |
|---|---|---|---|---|
| AUC0-inf, ng h/mL | 358 (112) | 733 (184) | 2,971 (730) | 2,890 (795) |
| Cmax, ng/mL | 76 (13) | 156 (42) | 598 (117) | 432 (92) |
| Half-life, h | 3.16 (0.72) | 3.15 (0.39) | 3.37 (0.63) | 3.59 (0.70) |
| CL/F, L/h | 3.03 (0.87) | 2.89 (0.71) | 2.85 (0.69) | 2.97 (0.81) |

```yaml
parameters:
  - {name: CL_F, value: "2.85-3.03", unit: L/h, source: "Table 8, PDF p18", note: "mean by dose, single dose, N = 32"}
  - {name: t_half, value: "3-4", unit: h, source: "12.3, PDF p17; Table 8", note: "dose-independent; Table 8 means 3.15-3.59 h"}
  - {name: Vss_F, value: 17.6, unit: L, source: "12.3 Distribution, PDF p18", note: "mean, CV 30%, population PK analysis"}
  - {name: F, value: 0.99, unit: fraction, source: "12.3 Absorption, PDF p18", note: absolute}
  - {name: Tmax, value: 1, unit: h, source: "12.3 Absorption, PDF p18", note: "about 1 h; food delays to 1.75 h and lowers Cmax about 28%, AUC unchanged"}
  - {name: protein_binding, value: 0.998, unit: fraction, source: "12.3 Distribution, PDF p18", note: "mainly albumin"}
  - {name: CL_F_pediatric, value: 3.15, unit: L/h, source: "12.3 Pediatric, PDF p19", note: "population mean, 96 patients 10-17 y, 35-178.3 kg"}
  - {name: V_F_pediatric, value: 13.5, unit: L, source: "12.3 Pediatric, PDF p19", note: "population mean"}
```
Derived (mine): dose/AUC is 2.79, 2.73, 2.69 and 2.77 L/h for the four columns, slightly below the printed CL/F means
(means of individual ratios); CL/F x t1/2 / ln 2 at 8 mg fasting = 2.85 x 3.37 / 0.693 = 13.9 L, below the population
Vss/F of 17.6 L (different analyses and populations). Metabolism mainly CYP2C8, minor CYP2C9; no unchanged drug in urine;
[14C] 64% urine, 23% faeces, total-radioactivity half-life 103-158 h (PDF p18).

## Pharmacodynamics and end points
No concentration-effect model. Lipid changes (Table 7, PDF p16-17) and glycaemic efficacy (section 14). Dosing (PDF p1-2):
start 4 mg/day once or in two doses; may increase to 8 mg/day after 8-12 weeks by FPG response; maximum 8 mg/day; with or
without food. Not an opioid.

## Covariates
Population PK in 642 men and 405 women with type 2 diabetes, 35-80 y (PDF p18): no effect of age, race, smoking, alcohol;
CL/F and Vss/F rise with weight (over 50-150 kg, less than 1.7-fold and 2.3-fold); CL/F about 15% lower in women (12.3 text),
while the Gender subsection says about 6% lower than in men of the same body weight (PDF p18-19). Hepatic impairment
(Child-Pugh B/C): unbound Cmax and AUC 2- and 3-fold higher, half-life about 2 h longer (PDF p19). Renal impairment and
haemodialysis: no relevant change (PDF p19). Gemfibrozil raises AUC 127%; rifampin lowers it 66% (PDF p19-20).

## Caveats
- Old (2010) repackager label, not the current Avandia prescribing information.
- The population model's structure and parameter vector are not printed; only Vss/F and qualitative covariate effects.
- Two different female-clearance statements (15% unadjusted for weight, 6% at the same weight), both from the same analysis.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:73` (built by `data-raw/antidiabeticRegistry.R:283-285`): key `rosiglitazone`,
  Avandia, PO, core, US; spec and review PK status `PK-CONSTRAINT`, PD `PD-TEMPLATE`, endpoint "insulin sensitivity",
  no stanpumpR drug, structure not verified; source_url this SPL; note "Constants from different analyses."

## Verification against stanpumpR
- Product, route and US status: **matches** (Avandia oral tablets, NDA021071).
- `PK-CONSTRAINT` with "Constants from different analyses": **matches** the label, which gives CL/F and half-life from a
  single-dose study (Table 8, N = 32) but Vss/F from a separate population analysis, and no complete model.
- Endpoint "insulin sensitivity" as a template: consistent with the label's mechanism (12.1); no fitted PD in the label.
- Not implemented (stanpumpr_drug NA): consistent; nothing in the code uses the numbers.
