---
file: NJ_dailymed-21335fe4.pdf
pmid: null
doi: null
citation: "Novo Nordisk. XULTOPHY 100/3.6 (insulin degludec and liraglutide) injection, for subcutaneous use; 100 units/mL insulin degludec and 3.6 mg/mL liraglutide, 3 mL pen. US prescribing information, revised 10/2025; DailyMed SPL setid 21335fe4-d395-4501-ac2a-2f20d7520da9; archived 2026-10-21."
kind: label
drugs: [liraglutide]
source_read: "text layer, PDF p1-21 of 48: Highlights p1-2; contents p2-3; sections 1-2 Dosage and Administration incl. Table 1 and Table 2 p3-6; 3-5 Warnings p6-9 (skimmed); 7 Drug Interactions p13-14; 8 Use in Specific Populations p14-16; 10 Overdosage and 11 Description p16-18; 12.1-12.3 Clinical Pharmacology and 12.6 Immunogenicity p18-21. Section 14 Clinical Studies (p23-30), 16-17 and the Medication Guide and Instructions for Use (p31-48) not read. PDF p4 (Table 1) and p18 checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# XULTOPHY 100/3.6 (insulin degludec + liraglutide) US label, 2025

Insulin degludec is not a stanpumpR drug; liraglutide is (`liraglutide`, `liraglutideWatson`), so `drugs` lists only
liraglutide. The fixed-ratio product itself is registry-only.

## Study
Not a study: the PLR-format US label (initial approval 2016, revised 10/2025) for a once-daily SC fixed-ratio
combination of a long-acting basal insulin analogue and a GLP-1 receptor agonist, for adults with type 2 diabetes
(PDF p1, p3).

## Model and parameters
Dosing (section 2, PDF p4-5; Table 1 rendered p4): each dosage unit is 1 unit insulin degludec + 0.036 mg liraglutide;
pen delivers 10-50 units per injection; maximum 50 units (50 U + 1.8 mg); starting dose 10 units (10 U + 0.36 mg) if
naive to basal insulin or GLP-1 RA, 16 units (16 U + 0.58 mg) if on either; titrate by 2 units (2 U + 0.072 mg) once or
twice weekly (Table 2, PDF p5). Not IV, not in an insulin pump, not diluted or mixed, dose not split (PDF p6).

Section 12.3 (PDF p18-19), population PK in T2D (mean weight 87.5 kg) at 50 units/1.8 mg daily:

```yaml
parameters:
  - {name: AUC0-24_ss_degludec, value: 113, unit: h*nmol/L, source: "12.3 Absorption, PDF p18", note: "estimated mean, popPK, at 50 units"}
  - {name: Cmax_ss_degludec, value: 5196, unit: pmol/L, source: "12.3, PDF p18", note: "estimated mean"}
  - {name: AUC0-24_ss_liraglutide, value: 1227, unit: h*ng/mL, source: "12.3 Absorption, PDF p18", note: "estimated mean, popPK, at 1.8 mg"}
  - {name: Cmax_ss_liraglutide, value: 55, unit: ng/mL, source: "12.3, PDF p18", note: "estimated mean"}
  - {name: thalf_degludec, value: 25, unit: h, source: "12.3 Elimination, PDF p18", note: "approximately"}
  - {name: thalf_liraglutide, value: 13, unit: h, source: "12.3 Elimination, PDF p18-19", note: "approximately"}
  - {name: protein_binding, value: ">0.99 degludec, >0.98 liraglutide", unit: fraction, source: "12.3 Distribution, PDF p18", note: ""}
```
Steady state after 2-3 days (PDF p18). (derived: liraglutide CL/F at steady state = 1.8 mg / 1227 ng h/mL = 1.47 L/h;
degludec CL/F not derivable without converting units to moles, which the label does not give here.) No ka, V/F or
covariate equations. Exposure falls with baseline body weight for both components; no clinically relevant effect of
age (to 83 y), sex or race (PDF p19). Liraglutide AUC 35%, 19%, 29% and 30% lower in mild, moderate, severe renal
impairment and ESRD, and 11%, 14%, 42% lower in mild, moderate, severe hepatic impairment; degludec PK unchanged by
renal or hepatic impairment (0.4 U/kg single dose, n = 6/group) (PDF p19-20). Liraglutide Cmax at 8-12 h (DDI section,
PDF p20).

## Pharmacodynamics and end points
Glucodynamic action reflects the two components' profiles; lowers fasting and postprandial glucose (12.2, PDF p18).
No clamp GIR or concentration-effect values. Liraglutide up to 1.8 mg did not prolong QTc. Gastric emptying delay: DDI
Cmax reductions of 27-38% for lisinopril, atorvastatin, digoxin, acetaminophen (PDF p20).

## Covariates
See above; no equations. Pregnancy: no human data (8.1, PDF p14). Lactation: both components in rat milk (8.2, PDF
p15). Geriatric: 19.9% of 1,881 trial subjects were >= 65 y (8.5, PDF p16).

## Caveats
Population-PK summary statistics only; no parameter vector. Sections 14 and later not read.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:48` (`data-raw/antidiabeticRegistry.R:206-209`): `xultophy_100_3_6`, SC, core,
  US, `PK-CONSTRAINT` / `PD-TEMPLATE`, PD endpoint none, not implemented, csii FALSE, no stanpumpR drug; note "n steps =
  n U degludec + 0.036n mg liraglutide; degludec not implemented; no pump."

## Verification against stanpumpR
- Fixed ratio, 1 unit = 1 U degludec + 0.036 mg liraglutide: **matches** (2.1 and Table 1, PDF p1, p4-5).
- "no pump" / csii FALSE: **matches** (do not use in an insulin infusion pump, PDF p1, p6).
- "degludec not implemented": consistent with the repo at 8baa733 (no degludec drug file; liraglutide has two).
- SC route, US jurisdiction, PK constraint only (no full fit in the label), no PD endpoint: **matches**.
