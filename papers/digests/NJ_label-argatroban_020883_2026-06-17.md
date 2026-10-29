---
file: NJ_label-argatroban_020883_2026-06-17.pdf
pmid: null
doi: null
citation: "ARGATROBAN INJECTION, for intravenous infusion only, 250 mg/2.5 mL. US prescribing information, NDA 020883 (distributed by Novartis Pharmaceuticals Corporation), Revised 06/2026, Reference ID 5817283. https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/020883s021lbl.pdf"
kind: label
drugs: [argatroban]
source_read: "text layer, 18 PDF pages (18 form-feed chunks = pages). Read Highlights and contents (p1), 1 Indications and 2 Dosage and Administration incl. Tables 1-3 and 2.4 hepatic impairment (pp2-5), 5 Warnings incl. 5.2 hepatic impairment (p5), 8 Use in Specific Populations (pp10-11), 10 Overdosage, 11 Description, 12.1-12.3 Clinical Pharmacology (pp12-15), 14 Clinical Studies headings (pp15-18). Rendered page 14 (12.3) and confirmed its numbers from the image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Argatroban injection (NDA 020883), US label revised 06/2026

## Study
This is the reference US label for argatroban, a synthetic direct thrombin inhibitor given by IV infusion. It
covers NDA 020883, distributed by Novartis, initial US approval 2000, revised 06/2026. Page 1 is the argatroban
label, as named. The drug is a 65:35 mixture of 21-(R) and 21-(S) diastereoisomers (MW 526.66).

**Dosage (section 2, pp2-5):**
- Dilute 100-fold to 1 mg/mL.
- HIT without hepatic impairment: 2 mcg/kg/min continuous infusion (Table 1: 140 mcg/min = 8 mL/h at 70 kg). Target
  an aPTT of 1.5-3 times baseline (not over 100 s), checked 2 h after starting or changing the dose. The maximum is
  10 mcg/kg/min.
- PCI: 350 mcg/kg bolus over 3-5 min plus 25 mcg/kg/min, with an ACT check at 5-10 min.
  - ACT under 300 s: give a 150 mcg/kg bolus and increase to 30 mcg/kg/min.
  - ACT over 450 s: reduce to 15 mcg/kg/min.
  - The target ACT is 300-450 s, and up to 40 mcg/kg/min may be used (Tables 2-3).
- Moderate or severe hepatic impairment (Child-Pugh) in HIT: start at 0.5 mcg/kg/min, "based on the approximately
  4-fold decrease" in clearance (2.4, p4). Avoid high doses in PCI with significant hepatic disease or AST/ALT at
  least 3 times the upper limit of normal.

## Model and parameters
Section 12.3 (PDF p14, confirmed from the image) is descriptive. It has no compartmental model.
```yaml
parameters:
  - {name: Vss, value: 174, unit: mL/kg, source: "12.3, p14", note: "apparent; 12.18 L at 70 kg"}
  - {name: protein binding, value: 54, unit: "%", source: "12.3, p14", note: "albumin 20%, alpha1-acid glycoprotein 34%"}
  - {name: CL, value: 5.1, unit: mL/kg/min, source: "12.3, p14", note: "0.31 L/kg/h, for infusions up to 40 mcg/kg/min"}
  - {name: terminal t1/2, value: "39-51", unit: min, source: "12.3, p14"}
  - {name: CL (hepatic impairment, Child-Pugh > 6), value: 1.9, unit: mL/kg/min, source: "12.3, p14", note: "at 2.5 mcg/kg/min; t1/2 181 min"}
  - {name: unchanged drug in urine, value: 16, unit: "% of dose", source: "12.3, p14", note: "faeces at least 14%; radioactivity 65% faeces, 22% urine"}
  - {name: dialysis removal, value: 20, unit: "% over 4 h", source: "12.3, p15; 10, p12", note: "2 mcg/kg/min during haemodialysis"}
  - {name: CL (seriously ill children), value: 0.16, unit: L/h/kg, source: "8.4, p11", note: "population PK in 15 patients; 0.03 with elevated bilirubin (n=4)"}
```
- Metabolism: by CYP3A4/5 in vitro (M1 is 3-5-fold weaker, at 0-20% of parent), but erythromycin had no effect, so
  CYP3A is not an important pathway in vivo.
- Steady-state concentrations are proportional to dose up to 40 mcg/kg/min (12.2).
- Renal impairment (CLcr down to a mean of 5 mL/min): PK and PD are not significantly affected, and no adjustment is
  needed.
- Age and gender have no clinically significant effect.
- Derived checks:
  - From Vss and CL: 174 / 5.1 = 34 min MRT. A one-compartment drug with t1/2 39-51 min would have an MRT of
    56-74 min. The label gives no compartmental information to reconcile the two.
  - Compared with Cox 2004 (`PMID_15317826.md`) at 87 kg: 0.31 L/kg/h is 27 L/h against 22 L/h, and 174 mL/kg is
    15 L against Cox's V1 + V2 of 24 L.
  - Hepatic: 5.1 / 1.9 = 2.7-fold, which is less than the 4-fold the D&A states.

## Pharmacodynamics and end points
- aPTT, ACT, PT/INR and TT rise dose-dependently. Steady state of drug and effect is reached in 1-3 h (12.2, p13).
- Effects return to baseline within 2-4 h of stopping (section 10), and take longer in hepatic impairment.
- The Ki for thrombin is 0.04 µM.
- INR correction on warfarin co-therapy at 2 mcg/kg/min: INR(warfarin alone) = 0.19 + 0.57 × INR(combined), ±0.4,
  for one thromboplastin (p14).
- Pediatric PK/PD modelling suggested starting at 0.75 mcg/kg/min, or 0.2 with hepatic impairment (8.4); safety and
  efficacy are not established.
- The label gives no concentration-effect model parameters.

## Covariates
Hepatic impairment (Child-Pugh > 6): CL 1.9 mL/kg/min. Weight-based dosing throughout. Renal function, age and sex
have no effect.

## Caveats
- The contents list (p1) numbers Clinical Pharmacology as 12.4, 12.5 and 12.6, while the body uses 12.1-12.3. This is
  a source typo in the contents.
- The 4-fold hepatic clearance decrease (2.4) does not match the 12.3 figures (5.1 against 1.9 mL/kg/min, 2.7-fold).
  The label does not say where the 4-fold figure comes from, so this is unresolved.
- The PK values are descriptive, mostly from healthy volunteers.

## Used by stanpumpR
- `R/drugs_argatroban.R:31-36` (8baa733): the Cox 2004 PCI model has no hepatic covariate. Argatroban is cleared
  hepatically, and "the label reports markedly reduced clearance in moderate hepatic impairment", so the model will
  under-predict concentrations in liver disease. There is no renal covariate, because argatroban "is not renally
  cleared".
- `inst/help/drugs/argatroban.md:15`: the same statements in the help page.
- The repo does not name the label version. This archived copy is the 06/2026 revision.

## Verification against stanpumpR
- Markedly reduced clearance in moderate hepatic impairment: the label gives a 4-fold decrease in moderate or severe
  impairment (2.4) and CL 1.9 against 5.1 mL/kg/min with t1/2 181 min at Child-Pugh > 6 (12.3). The repo's
  qualitative statement, and its conclusion that the model under-predicts, are supported. **matches**
- Hepatically cleared: the label says argatroban is mainly hepatic and excreted primarily in faeces via bile.
  **matches**
- "Not renally cleared", so no renal covariate:
  - The label shows no significant renal effect on PK or PD and needs no renal dose adjustment, so leaving out a
    renal covariate is supported.
  - Strictly, 16% of the dose appears unchanged in urine (22% of radioactivity), so "not renally cleared" overstates
    it slightly.
  - **matches** (wording approximate)
