---
file: NJ_dailymed-3bb7bc69.pdf
pmid: null
doi: null
citation: "Boehringer Ingelheim Pharmaceuticals (relabelled by Physicians Total Care, Inc.). MICARDIS (telmisartan) tablets 20, 40 and 80 mg, NDA020850. DailyMed SPL setid 3bb7bc69-7752-4c16-9c60-e61eb5355a4d; label text revised January 2012, SPL revised 2/2012; archived 2026-10-21."
kind: label
drugs: [telmisartan]
source_read: "text layer, all 23 PDF pages (PLR format; form-feed chunks equal PDF pages): Highlights p1-2; contents p2-3; Dosage and Administration 2.1-2.2 p3-4; Warnings 5.1-5.6 p4-5; Adverse Reactions p5-7; Drug Interactions 7 p7-8; Use in Specific Populations 8.1-8.6 p8-9; Overdosage, Description p9-10; Clinical Pharmacology 12.1-12.3 p10-11; Clinical Studies 14.1-14.2 p12-14; How Supplied p14; patient leaflet, carton and SPL data p15-23. PDF p1 and p10 checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Telmisartan (MICARDIS) US label, 2012 relabel

## Study
Not a study: the US prescribing information for MICARDIS (telmisartan) tablets, an AT1 angiotensin II receptor blocker,
as relabelled by Physicians Total Care (NDA020850, marketing start 03/25/2002 for this relabeller; PDF p22-23). Initial US
approval 1998 (PDF p1). An old label (revised 2/2012); the current Boehringer label may differ. Indications: hypertension;
cardiovascular risk reduction in patients 55 or older unable to take an ACE inhibitor (PDF p3). MW 514.63 (PDF p9).

## Model and parameters
No model. Section 12.3 (PDF p10-11):

```yaml
parameters:
  - {name: tmax, value: "0.5-1", unit: h, source: "12.3, PDF p10", note: "after oral dosing"}
  - {name: F_40mg, value: 0.42, unit: fraction, source: "12.3, PDF p10", note: "absolute bioavailability at 40 mg"}
  - {name: F_160mg, value: 0.58, unit: fraction, source: "12.3, PDF p10", note: "absolute bioavailability at 160 mg; F is dose dependent"}
  - {name: thalf_terminal, value: 24, unit: h, source: "12.3, PDF p11", note: "approximately; bi-exponential decay"}
  - {name: Vd, value: 500, unit: L, source: "12.3 Distribution, PDF p11", note: "approximately; kind of volume not stated"}
  - {name: CL_total, value: ">800", unit: mL/min, source: "12.3 Metabolism and Elimination, PDF p11", note: "total plasma clearance; independent of dose"}
  - {name: protein_binding, value: ">0.995", unit: fraction, source: "12.3 Distribution, PDF p11", note: "albumin and alpha1-acid glycoprotein"}
  - {name: trough_to_peak_conc, value: "0.10-0.25", unit: ratio, source: "12.3, PDF p11", note: "once-daily dosing"}
  - {name: accumulation_index, value: "1.5-2.0", unit: ratio, source: "12.3, PDF p11", note: "once-daily repeated dosing"}
```

Other PK facts (PDF p10-11): nonlinear oral PK over 20-160 mg, with greater than proportional rises in Cmax and AUC; food
lowers AUC about 6% (40 mg) and about 20% (160 mg); over 97% of an IV or oral 14C dose is excreted unchanged in faeces by
biliary excretion, urine 0.91% (IV) and 0.49% (oral) of radioactivity; one metabolite, the inactive acyl glucuronide
(about 11% of plasma radioactivity after a single dose); no CYP450 metabolism; terminal half-life and clearance
independent of dose. Not removed by haemodialysis (PDF p9) or haemofiltration (PDF p11).
(derived: 500 L x ln2 / 48 L/h = 7.2 h at CL 800 mL/min, much shorter than the 24 h terminal half-life, so the
approximately 500 L is not the terminal volume; the label does not say which volume it is.)

## Pharmacodynamics and end points
- 80 mg inhibited the pressor response to IV angiotensin II by about 90% at peak plasma concentration, about 40%
  persisting at 24 h (12.2, PDF p10).
- Placebo-subtracted trough BP falls (14.1, PDF p12): 6-8/6 mmHg at 20 mg, 9-13/6-8 at 40 mg, 12-13/7-8 at 80 mg;
  no further fall up to 160 mg; onset within 3 h of a single dose; maximal reduction by about 4 weeks (2.1 says most
  effect within 2 weeks, maximal by 4); 24-h trough-to-peak ratio 70-100% at 40-80 mg; BP returns to baseline over
  several days to a week after stopping. Less response in Black patients. No heart-rate change.
- No concentration-effect relation, EC50 or therapeutic range given.

## Covariates
- Sex: plasma concentrations 2-3 times higher in women, without a larger BP response (12.3, PDF p11).
- Hepatic insufficiency: concentrations up, absolute F approaches 100%; start low (5.4, 8.6, 12.3).
- Renal impairment: no dose adjustment; elderly: PK not different from under 65; paediatric PK not studied.
- Interactions: digoxin peak +49%, trough +20%; ramipril Cmax/AUC up 2.3/2.1-fold while telmisartan Cmax/AUC fall 31%/16%
  (7, PDF p7-8).

Dosing (2.1-2.2, PDF p3-4): hypertension 40 mg once daily, range 40-80 mg; CV risk reduction 80 mg once daily.

## Caveats
Summary label values with no n or study identified; an old (2012) relabel of the innovator text. The volume figure is
not defined and is inconsistent with a 24 h terminal half-life at the stated clearance (derived above).

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:117` (built by `data-raw/antihypertensiveRegistry.R:200-204`):
  `telmisartan__tablet__oral__catalog__none__label`, plasma, `official_label_descriptive`, `proposed_unfitted`,
  `descriptive_only`, BP simulation off. Limitations: label F dose dependent (about 42% at 40 mg, about 58% at 160 mg),
  so F must not be held constant; Tmax 0.5-1 h; terminal half-life about 24 h; the fitted research record is separate.

## Verification against stanpumpR
- F 42% at 40 mg and 58% at 160 mg, dose-dependent: **matches** (12.3, PDF p10, rendered).
- Tmax 0.5-1 h: **matches** (PDF p10). Terminal half-life about 24 h: **matches** (PDF p11).
- "Descriptive only", no BP coefficients: **matches** (the label gives trial mean BP falls, no concentration-BP model).
- The separate fitted record is the PAGE 2015 poster (see `NJ_web-page-meeting-70bcef.md`).
