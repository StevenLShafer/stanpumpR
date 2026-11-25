---
file: NJ_label-basaglar_205692_2026-08-10.pdf
pmid: null
doi: null
citation: "BASAGLAR KwikPen / BASAGLAR Tempo Pen (insulin glargine) injection 100 units/mL, US prescribing information, Eli Lilly and Company; NDA 205692; file dated 2026-08-10; highlights say Revised 7/2021, end of document says Revised 6/2026."
kind: label
drugs: [insulinGlargine]
source_read: "text layer only: Highlights, Dosage and Administration (2.1-2.4), 5.2-5.3, 8.1-8.7, 10, 11, 12.1-12.3 read; adverse-reaction tables, section 14 clinical studies, patient information not read. No page images rendered."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 94262c07
verification: partly-verified
open_questions:
  - "[check] Label PK numbers (Cmax 103 pmol/L, AUC0-24 1720 pmol*h/L, median tmax 12 h, 0.5 U/kg, n=91; section 12.3) were read from the text layer only; confirm on the rendered page before they are used to audit R/insulinTham.R."
  - "[decision] The label gives no subject weight, so the model-vs-label comparison below assumes a 70 kg subject (35 U); the label's conversion basis from pmol/L to mU/L is also not stated. Whether to treat the label as a check on the Tham glargine fit is Shafer's call."
---

# Basaglar label (NDA 205692): insulin glargine U-100

## Study
FDA-approved prescribing information. Indication: glycaemic control in adults and children with type 1 and adults with type 2 diabetes. Not for DKA; not intravenous; not for insulin pumps (2.1). Dose forms: 100 units/mL, 3 mL KwikPen / Tempo Pen (3). Description: 100 units = 3.6378 mg insulin glargine per mL; MW 6.063 kDa (11).

## Model and parameters
No population PK model. Single-dose PK, section 12.3: 0.5 U/kg SC in a euglycaemic clamp study of 91 healthy subjects: median tmax 12 h; AUC0-24 mean 1720 pmol*h/L; Cmax mean 103 pmol/L; concentrations back to baseline at about 24 h; no pronounced peak. Effect of age, race, sex, BMI not evaluated; renal and hepatic impairment not studied (8.6, 8.7). Metabolites M1 (21A-Gly-insulin) and M2 (21A-Gly-des-30B-Thr-insulin) form at the B-chain carboxyl terminus (reported for another glargine product; in vitro activity similar to insulin). Half-life, clearance, volume and bioavailability are not given.
```yaml
parameters:
  - {name: tmax, value: 12, unit: h, source: "12.3", note: "median; 0.5 U/kg SC, 91 healthy subjects"}
  - {name: Cmax, value: 103, unit: "pmol/L", source: "12.3", note: "mean observed, serum insulin (assay not stated)"}
  - {name: AUC0-24, value: 1720, unit: "pmol*h/L", source: "12.3", note: "mean observed"}
  - {name: dose in PK study, value: 0.5, unit: "U/kg", source: "12.3"}
  - {name: starting dose T2DM, value: 0.2, unit: "U/kg or up to 10 U once daily", source: "2.3"}
  - {name: max single injection, value: 80, unit: U, source: "2.2"}
```

## Pharmacodynamics and end points
12.2: same 0.5 U/kg euglycaemic clamp, 91 healthy subjects: median time to maximum effect (peak glucose infusion rate) about 12.0 h; AUC of glucose infusion rate 1670 mg/kg; maximum GIR 2.12 mg/kg/min; sustained glucose lowering over 24 h, no pronounced peak. In 20 type 1 patients, 0.3 U/kg gave a similar 24 h profile. Injection site (abdomen, deltoid, thigh) gave a similar duration (other glargine product). No MEAC, ke0 or tPeak is stated; the 12 h figure is a time-to-peak of the clamp GIR, not a ke0.

## Covariates
None quantified. Conversions (2.4): from glargine U-300 or twice-daily NPH, start at 80% of the prior dose; from other glargine 100 units/mL, same dose.

## Caveats
Single-dose healthy-volunteer data; the label does not give the assay, weight, or whether baseline endogenous insulin was removed. Two revision dates appear in the file.

## Used by stanpumpR
Context lists no repo locations (`cited_in_repo: false`). The library does model glargine: R/drugs_insulinGlargine.R, "fitted to Basaglar", from R/insulinTham.R (Tham 2017, J Clin Pharmacol 57:1126; registry row insulin_glargine_basaglar_u100).

## Verification against stanpumpR
Independent check of the library's Basaglar fit (R/insulinTham.R:33, 59-60: V = 768 L x (1 + 0.00728 (BW - 71.4)), CL/F 74.5 L/h, ka 0.0830 /h, first-order fraction 0.822, zero-order duration 0.612 h, F = (Dose/35)^-0.300). Derived by me, simple Euler integration of that structure for a 70 kg subject at 0.5 U/kg (35 U, so F = 1; exogenous insulin only, assumed 6 pmol/L per mU/L): Cmax about 96 pmol/L at about 8.4 h; AUC0-24 about 1920 pmol*h/L. Label: Cmax 103 pmol/L (model 7% lower), AUC0-24 1720 (model 12% higher), tmax 12 h median (model about 8 h, earlier). Direction: the model peaks earlier and shows a flat profile, consistent with the label's "no pronounced peak"; magnitudes agree within about 15% under assumptions the label does not let me test (weight, pmol/L conversion). No discrepancy found in the library's number transcription because the label does not report those parameters. Verdict: partly-verified (see open questions).
