---
file: NJ_label-catapresstts_018891_2026-06-23.pdf
pmid: null
doi: null
citation: "Catapres-TTS-1/-2/-3 (clonidine transdermal system) prescribing information, NDA 018891 (supplement s034), Technomed Inc., manufactured by Corium Innovations; label text revised November 2025, FDA Reference ID 5706317, signed 12/04/2025; posted by FDA 2026 (018891Orig1s034lbl.pdf)."
kind: label
drugs: [clonidine]
source_read: "text layer, all 22 pages: Description and Release Rate Concept (PDF p1-2), Clinical Pharmacology incl. Pharmacokinetics (p2-3), Warnings/Precautions (p3-5), Pediatric Use (p6), Overdosage (p7-8), Dosage and Administration, Renal Impairment, How Supplied (p8-9, p9 rendered), patient instructions (p10-12), cover/pouch art (p13-21, text layer garbage on p16-18), signature page (p22)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Catapres-TTS FDA label, NDA 018891 s034 (Corium-made system, revised Nov 2025)

## Study
This is a US label (non-PLR format), not a study. It is the same product line as the DailyMed SPL `NJ_dailymed-d4a55825`, but a reformulated system:
- Areas are 4.33, 8.67 and 13 cm2 for 0.1, 0.2 and 0.3 mg/day over 7 days (Description, p1).
- How Supplied (p9, rendered): clonidine content 3.09, 6.19 and 9.28 mg; NDC 82089-201/202/203-34; debossed "Clonidine 0.x mg/day".
- Manufacturer is Corium Innovations, Grand Rapids. The older Alza system was 3.5/7.0/10.5 cm2 with 2.5/5.0/7.5 mg.

## Model and parameters
No model. The Pharmacokinetics text (p2-3) is the same as the DailyMed SPL, apart from the patch areas:

```yaml
parameters:
  - {name: F_transdermal, value: 0.60, unit: fraction, source: "Pharmacokinetics p2", note: "absolute, approximately"}
  - {name: time_to_steady_state, value: 3, unit: days, source: "Pharmacokinetics p2", note: "within 3 days"}
  - {name: Css_0.1mg_day, value: 0.4, unit: ng/mL, source: "Pharmacokinetics p2", note: "4.33 cm2"}
  - {name: Css_0.2mg_day, value: 0.8, unit: ng/mL, source: "Pharmacokinetics p2", note: "8.67 cm2"}
  - {name: Css_0.3mg_day, value: 1.1, unit: ng/mL, source: "Pharmacokinetics p2", note: "13 cm2"}
  - {name: t_half_distribution_IV, value: 20, unit: min, source: "Pharmacokinetics p2"}
  - {name: t_half_elimination_IV, value: "12-16", unit: h, source: "Pharmacokinetics p2", note: "up to 41 h, severe renal impairment"}
  - {name: CL_total, value: 177, unit: mL/min, source: "Pharmacokinetics p2"}
  - {name: CL_renal, value: 102, unit: mL/min, source: "Pharmacokinetics p2"}
  - {name: Vz, value: 197, unit: L, source: "Pharmacokinetics p2", note: "2.9 L/kg"}
  - {name: t_half_after_removal, value: 20, unit: h, source: "Pharmacokinetics p3", note: approximately}
```

- After removal without replacement, therapeutic levels persist about 8 h and then decline over several days (Release Rate Concept, p2).

## Pharmacodynamics and end points
- Blood pressure, qualitative: the effect may not begin until 2-3 days after the first application. Going above two TTS-3 systems usually adds no efficacy (Dosage and Administration, p8).
- No concentration-effect relation is given.

## Covariates
Renal impairment: lower initial dose, hemodialysis removes little (p8). Paediatric safety and effectiveness are not established (p6).

## Caveats
- The PK paragraph was carried over unchanged from the Alza system's label. The label gives no new bioequivalence data for the Corium system.
- Pages 16-18 are pouch or carton art with no usable text layer. They carry no PK content.

## Used by stanpumpR
`tests/testthat/test-antihypertensive-registry.R:184-185` (context gives line 75): the pair `clonidine__ir_tablet__oral__catalog__none__label` / `clonidine__transdermal_patch__transdermal__catalog__none__label` must both exist as separate records. The repo nowhere cites this FDA PDF by URL. The registry record (`antihypertensiveRegistry.csv:40`) links the DailyMed SPL instead.

## Verification against stanpumpR
- **test:184-185**: the label establishes a transdermal product, a distinct route and formulation from the oral tablet. That supports keeping the records separate. **Matches.**
- **Registry:40** (checked against the DailyMed card, but true of this label too): F about 60%, steady state within 3 days, 7-day programmed delivery, 0.1/0.2/0.3 mg/day. All match p1-2.
- Point for the registry text: the current approved system is the 4.33/8.67/13 cm2 Corium patch, not the 3.5/7.0/10.5 cm2 system in the linked DailyMed SPL. The registry gives no area, so it states nothing wrong.
