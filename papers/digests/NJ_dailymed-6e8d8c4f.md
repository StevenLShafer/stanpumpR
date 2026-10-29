---
file: NJ_dailymed-6e8d8c4f.pdf
pmid: null
doi: null
citation: "Saxagliptin tablets, film coated, 2.5 mg and 5 mg (generic; Mylan Pharmaceuticals Inc., ANDA205980). Prescribing information and Medication Guide, Revised 2/2025. DailyMed SPL setid 6e8d8c4f-96eb-4b64-9c8c-4a5448a23a78."
kind: label
drugs: [saxagliptin]   # registry-only; not a stanpumpR library drug at 8baa733
source_read: "text layer, 46 PDF pages: Highlights and contents p1-2, Dosage and Administration p3, Use in Specific Populations p10-12, Overdosage and Description p12-13, Clinical Pharmacology 12.1-12.3 p13-18 (Table 2 p16-18, Table 3 p18), Clinical Studies 14.2 p32, How Supplied p35, SPL product data p43-46; page images of p14, p15 and p17 (12.2-12.3, excretion, Table 2)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Saxagliptin tablets (Mylan generic) label, February 2025

## Study
PLR-format US prescribing information for a **generic** saxagliptin tablet (Mylan Pharmaceuticals; ANDA205980,
marketing start 08/04/2023; NDC 0378-4706; "Revised: 2/2025", p1 and p46), not AstraZeneca's Onglyza. Initial US
approval 2009. Tablets contain 3.075 or 6.149 mg saxagliptin hydrochloride dihydrate (MW 387.92), equivalent to 2.5
or 5 mg saxagliptin. DPP-4 inhibitor for type 2 diabetes in adults.

## Model and parameters
No compartmental model. Section 12.3 (PDF p14-16; p14-15 rendered):
- 5 mg single dose, healthy subjects: mean AUC 78 (saxagliptin) and 214 ng·h/mL (5-hydroxy saxagliptin); Cmax 24 and
  47 ng/mL; %CV of AUC and Cmax under 25%. Dose-proportional over 2.5-400 mg; no appreciable accumulation once daily;
  no dose- or time-dependence in clearance over 14 days.
- Median Tmax after 5 mg once daily: 2 h (parent), 4 h (metabolite). High-fat meal: Tmax +about 20 min, AUC +27%.
- Protein binding negligible in vitro.
- Metabolism mainly CYP3A4/5; the major metabolite is a DPP-4 inhibitor half as potent as the parent. P-gp
  substrate.
- 50 mg 14C dose: urine 24% as saxagliptin, 36% as the metabolite, 75% total radioactivity; faeces 22%. Renal
  clearance about 230 mL/min against estimated GFR about 120 mL/min (active secretion).
- **Terminal half-life after 5 mg: 2.5 h (saxagliptin) and 3.1 h (5-hydroxy saxagliptin), mean** (p15).

```yaml
parameters:
  - {name: t_half_saxagliptin, value: 2.5, unit: h, source: "12.3 Excretion, PDF p15", note: "mean, single 5 mg, healthy"}
  - {name: t_half_5_hydroxy, value: 3.1, unit: h, source: "12.3 Excretion, PDF p15", note: "active metabolite, mean"}
  - {name: AUC_5mg, value: 78, unit: ng*h/mL, source: "12.3, PDF p14", note: "saxagliptin, single 5 mg"}
  - {name: AUC_5mg_metabolite, value: 214, unit: ng*h/mL, source: "12.3, PDF p14", note: "5-hydroxy saxagliptin"}
  - {name: Cmax_5mg, value: 24, unit: ng/mL, source: "12.3, PDF p14", note: saxagliptin}
  - {name: Cmax_5mg_metabolite, value: 47, unit: ng/mL, source: "12.3, PDF p14", note: "5-hydroxy saxagliptin"}
  - {name: Tmax, value: 2, unit: h, source: "12.3 Absorption, PDF p14", note: "median, parent; metabolite 4 h"}
  - {name: CL_renal, value: 230, unit: mL/min, source: "12.3 Excretion, PDF p15", note: "approximately, saxagliptin"}
  - {name: metabolite_potency_ratio, value: 0.5, unit: ratio, source: "12.3 Metabolism, PDF p15", note: "DPP-4 inhibition relative to parent"}
```

Drug interactions (Table 2, PDF p16-18; p17 rendered), AUC and Cmax ratios for saxagliptin: diltiazem 360 mg LA
2.09 / 1.63 (metabolite 0.66 / 0.57); rifampin 600 mg 0.24 / 0.47 (metabolite 1.03 / 1.39; DPP-4 inhibition over 24
h unchanged); ketoconazole 200 mg BID with 100 mg saxagliptin 2.45 / 1.62 (metabolite 0.12 / 0.05) and with 20 mg
3.67 / 2.44 (p18, from the text layer, not rendered). Metformin, glyburide, pioglitazone, digoxin, simvastatin,
omeprazole, antacid and famotidine about 1.

## Pharmacodynamics and end points
DPP-4 activity inhibited for 24 h; after a glucose load or meal, active GLP-1 and GIP rise 2- to 3-fold, glucagon
falls, glucose-dependent insulin secretion rises (12.2, p14). No concentration-inhibition relation, EC50 or
time course given. No QTc effect up to 40 mg.

## Dosing (Section 2, PDF p3)
2.5 or 5 mg once daily, with or without food; 2.5 mg once daily if eGFR < 45 mL/min/1.73 m² (including ESRD on
haemodialysis, dosed after dialysis) or with strong CYP3A4/5 inhibitors.

## Covariates
Renal (10 mg single dose): eGFR 30-45, 15-30 and haemodialysis gave saxagliptin or metabolite AUC more than 2-fold
normal; Cmax unchanged (p16). Hepatic (Child-Pugh A-C, 10 mg): parent Cmax up to +8%, AUC up to +77%; metabolite
Cmax up to -59%, AUC -33%. Elderly 65-80 vs 18-40 y: parent Cmax +23%, AUC +59% (geometric means); age, sex, race and
BMI not significant covariates on apparent clearance in the population analysis (not printed). Females about 25%
higher metabolite exposure. Haemodialysis removes 23% of the dose over 4 h (p12).

## Caveats
A generic label; its clinical pharmacology normally follows the reference product (Onglyza), but no Onglyza label
was compared here. Summary means only; the population PK analysis it mentions is not reported.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:61` (from `data-raw/antidiabeticRegistry.R:249-251`): row `saxagliptin`,
  product "Onglyza (with active metabolite)", PO, tier core, US; spec_pk_status and pk_status PK-CONSTRAINT, PD
  PD-TEMPLATE, pd_endpoint "DPP-4 inhibition", `stanpumpr_drug` NA, `structure_verified` FALSE, source_url this
  DailyMed setid, note "Half-lives only."

## Verification against stanpumpR
- **Product identity: discrepancy (minor).** The row names Onglyza; the linked SPL is Mylan's generic saxagliptin
  tablets (ANDA205980). Ingredient, route, US status and the active metabolite all match.
- **"Half-lives only."**: the half-lives (2.5 h parent, 3.1 h metabolite) are in the label, p15. The note
  understates the source, which also gives AUC, Cmax, Tmax, renal clearance, the metabolite's potency and
  interaction ratios; it remains true that there is no fitted model, so PK-CONSTRAINT and no library drug are
  consistent with it.
- **PD-TEMPLATE, DPP-4 inhibition**: the label describes 24-h DPP-4 inhibition without a concentration-response.
  **Matches.**
