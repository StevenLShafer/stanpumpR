---
file: NJ_label-angiomax_020873_2019-07-10.pdf
pmid: null
doi: null
citation: "ANGIOMAX (bivalirudin) for injection, for intravenous use. Full prescribing information, NDA 020873 (supplement 039 per the file URL), revised 06/2019, FDA Reference ID 4457301; marketed by Sandoz Inc. https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/020873s039lbl.pdf (approval date 2019-07-10 per archive metadata)."
kind: label
drugs: [bivalirudin]
source_read: "text layer, all 10 PDF pages: Highlights and contents (p. 1), 1 Indications and 2 Dosage and Administration incl. 2.2 renal dose adjustment and Table 1 (pp. 1-3), 3-7 (pp. 4-5), 8 Use in Specific Populations incl. 8.5-8.6 (pp. 5-6), 10 Overdosage, 11 Description, 12.1 Mechanism (p. 7), 12.2 Pharmacodynamics and 12.3 Pharmacokinetics (p. 8), 13-14 Clinical Studies (pp. 9-10), 16-17 (p. 10). Section 12.3 is clean prose and was not rendered; the Table 1 dosing grid is scrambled in the text layer and is not transcribed here."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Angiomax (bivalirudin) label, FDA 2019 (NDA 020873)

PLR-format label, initial US approval 2000, revised 06/2019. Indication: anticoagulant in PCI, including patients with HIT/HITTS (p. 1).

## Study
Not a study. Section 12.3 cites PK in PTCA patients. Section 14 summarises the BAT trials (4312 patients with unstable angina undergoing PCI; median age 63, median weight 80 kg, 39-120 kg) and AT-BAT (51 HIT/HITTS patients) (pp. 9-10).

## Model and parameters
No compartmental model. Section 12.3 (p. 8) and 8.6 (p. 6):

```yaml
parameters:
  - {name: t_half_normal_renal, value: 25, unit: min, source: "12.3 p. 8", note: "PTCA patients, normal renal function; not called terminal"}
  - {name: CL_normal_renal, value: 3.4, unit: mL/min/kg, source: "12.3 p. 8", note: "PTCA patients; = 0.204 L/h/kg (derived)"}
  - {name: Css_1mgkg_2.5mgkgh, value: 12.3, unit: mcg/mL, source: "12.3 p. 8", note: "+/- 1.7; after 1 mg/kg bolus + 4-h 2.5 mg/kg/h, PTCA patients"}
  - {name: CL_reduction_moderate_severe, value: 21, unit: "%", source: "12.3 p. 8; 8.6 p. 6", note: "t1/2 34 min (moderate), 57 min (severe)"}
  - {name: CL_reduction_dialysis, value: 70, unit: "%", source: "12.3 p. 8; 8.6 p. 6", note: "t1/2 3.5 h"}
  - {name: fraction_removed_hemodialysis, value: 25, unit: "%", source: "12.3 p. 8", note: approximately}
  - {name: molecular_weight, value: 2180, unit: Da, source: "11 p. 7", note: "anhydrous free-base peptide"}
```

No binding to plasma proteins other than thrombin, or to red cells. Eliminated by proteolytic cleavage and glomerular filtration, with tubular secretion and reabsorption "implicated" to an unknown extent (p. 8). The label gives **no fraction renally excreted**. Clearance in mild renal impairment is similar to normal. Linear PK (p. 8).

## Pharmacodynamics and end points
Dose- and concentration-dependent prolongation of ACT, aPTT, PT and TT. Immediate onset; coagulation times return to baseline about 1 h after stopping (12.2, p. 8). In 291 PTCA patients, 1 mg/kg + 2.5 mg/kg/h for 4 h (then 0.2 mg/kg/h) brought every patient to a maximal ACT > 300 s (p. 8). AT-BAT median ACT at device activation was 379 s (high dose) and 317 s (lower dose) (p. 10). No target concentration.

## Covariates
- Renal function: dose adjustment in 2.2 (p. 2). No bolus change at any degree of impairment. Infusion 1 mg/kg/h if CrCl < 30 mL/min (Cockcroft-Gault); 0.25 mg/kg/h on haemodialysis.
- Elderly: more bleeding (8.5, p. 6).

## Caveats
- Dosage (2.1, p. 1-2): 0.75 mg/kg bolus, then 1.75 mg/kg/h for the procedure. Check the ACT 5 min after the bolus, and give a further 0.3 mg/kg if needed. Consider up to 4 h of post-procedure infusion in STEMI. Studied only with concomitant aspirin.
- The label's half-life and clearance come from PTCA patients, not healthy volunteers.
- Overdoses have followed failure to reduce the infusion in renal dysfunction or dialysis. There is no antidote; bivalirudin is dialysable (p. 7).

## Used by stanpumpR
- R/drugs_bivalirudin.R:23-26 (8baa733): the label's terminal half-life in normal renal function is 25 min, slower than the model's 35 min terminal phase.
- R/drugs_bivalirudin.R:29-37: about 20% renally cleared; the label (Angiomax) reports lower clearance in moderate and severe renal impairment and on dialysis, and 3.4 mL/min/kg (0.204 L/h/kg) in PCI patients against the study's 0.323; not spliced in.
- inst/help/drugs/bivalirudin.md:7 and :19 repeat the 25 min and 0.204 L/h/kg. tests/testthat/test-drugs-bivalirudin.R:55 cites the 25 min in a comment.
- Related: inst/extdata/anticoagulantRegistry.csv:17 (biva_label_pci, "Label CL (0.204 L/h/kg)", status pending). It links DailyMed setid 72c5413f-882a-4784-9be6-312c0a0a2a97, not this FDA PDF. The repo names no edition, so this 2019 FDA copy stands in for "the label".

## Verification against stanpumpR
- Half-life 25 min in normal renal function: **matches** 12.3 (p. 8). The label says "half-life", not "terminal half-life", and gives it for PTCA patients. The repo's comparison with the model's slower 35-min terminal phase is fair.
- CL 3.4 mL/min/kg = 0.204 L/h/kg in PCI (label: PTCA) patients: **matches** (3.4 x 60 / 1000 = 0.204, derived).
- Lower clearance in moderate and severe renal impairment and on dialysis: **matches** (21% and 70%; pp. 6, 8).
- "Bivalirudin is about 20% renally cleared" (drugs_bivalirudin.R:30-31): **cannot verify**. The sentence does not attribute it to the label, and this label gives no renally excreted fraction (it calls the extent of tubular handling unknown). Zhang 2012 does not give one either.
- Product identity (Angiomax, bivalirudin, IV): **matches**. Whether the repo's "the label" is this 06/2019 edition or the DailyMed SPL in anticoagulantRegistry.csv:17 cannot be told from the repo. The numbers quoted agree with this edition.
