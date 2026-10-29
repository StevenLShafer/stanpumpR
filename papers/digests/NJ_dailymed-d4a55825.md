---
file: NJ_dailymed-d4a55825.pdf
pmid: null
doi: null
citation: "Catapres-TTS-1/-2/-3 (clonidine transdermal system) prescribing information, Technomed Inc. (manufactured by Alza), NDA018891; DailyMed SPL setid d4a55825-7041-42f4-b3b2-dd7a25dbe793, label text revised March 2023, SPL revised 10/2026."
kind: label
drugs: [clonidine]
source_read: "text layer, all 26 pages: Description and Release Rate Concept (PDF p1-2), Clinical Pharmacology incl. Pharmacokinetics (p2-3), Warnings and Precautions incl. perioperative use (p3-5), Pediatric Use (p7), Overdosage (p9-10), Dosage and Administration and Renal Impairment (p10), How Supplied (p10-11, rendered), patient instructions and carton panels (p11-22), SPL product data (p23-26)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Catapres-TTS label (DailyMed, Alza-made systems): clonidine transdermal

## Study
This is a US label (old non-PLR format), not a study. The product is a membrane-controlled reservoir patch.
- Programmed delivery is 0.1, 0.2 or 0.3 mg/day for 7 days, with delivery proportional to area (p1-2).
- How Supplied table (p10, rendered):

| System | Area | Clonidine content | Delivery | NDC |
|---|---|---|---|---|
| TTS-1 | 3.5 cm2 | 2.5 mg | 0.1 mg/day | 82089-101-34 |
| TTS-2 | 7.0 cm2 | 5.0 mg | 0.2 mg/day | 82089-102-34 |
| TTS-3 | 10.5 cm2 | 7.5 mg | 0.3 mg/day | 82089-103-34 |

- The current FDA label for the same NDA (`NJ_label-catapresstts_018891_2026-06-23`) describes a different, Corium-made system: 4.33/8.67/13 cm2, 3.09/6.19/9.28 mg, NDC 82089-201/202/203. Its PK text is the same.

## Model and parameters
No model. These are label statements, all from Pharmacokinetics, p3, unless noted:

```yaml
parameters:
  - {name: F_transdermal, value: 0.60, unit: fraction, source: "Pharmacokinetics p3", note: "absolute bioavailability, approximately"}
  - {name: time_to_steady_state, value: 3, unit: days, source: "Pharmacokinetics p3", note: "within 3 days; therapeutic levels 2-3 days (Release Rate Concept p2)"}
  - {name: Css_TTS1, value: 0.4, unit: ng/mL, source: "Pharmacokinetics p3", note: "mean steady state, 3.5 cm2"}
  - {name: Css_TTS2, value: 0.8, unit: ng/mL, source: "Pharmacokinetics p3", note: "7.0 cm2"}
  - {name: Css_TTS3, value: 1.1, unit: ng/mL, source: "Pharmacokinetics p3", note: "10.5 cm2"}
  - {name: t_half_distribution_IV, value: 20, unit: min, source: "Pharmacokinetics p3", note: about}
  - {name: t_half_elimination_IV, value: "12-16", unit: h, source: "Pharmacokinetics p3", note: "up to 41 h in severe renal impairment"}
  - {name: CL_total, value: 177, unit: mL/min, source: "Pharmacokinetics p3"}
  - {name: CL_renal, value: 102, unit: mL/min, source: "Pharmacokinetics p3"}
  - {name: Vz, value: 197, unit: L, source: "Pharmacokinetics p3", note: "2.9 L/kg"}
  - {name: t_half_after_patch_removal, value: 20, unit: h, source: "Pharmacokinetics p3", note: approximately}
```

- Arithmetic checks (derived): 197 L / 2.9 L/kg implies about 68 kg. ln2 x 197 L / 10.6 L/h gives about 12.9 h, consistent with the stated 12-16 h.
- After oral dosing, 40-60% of the absorbed dose appears unchanged in urine in 24 h and about 50% is metabolised in the liver.
- Removal: therapeutic levels persist about 8 h and then decline slowly over several days (Release Rate Concept, p2; Overdosage, p10).

## Pharmacodynamics and end points
- Blood pressure, qualitative only. The antihypertensive effect may not begin until 2-3 days after the first application (Dosage and Administration, p10).
- Going above two TTS-3 systems usually adds no efficacy (p10).
- No concentration-effect relation is given in this label. The 0.2-2.0 ng/mL range is in the oral tablet label.
- Overdose case, oral powder (p10): plasma clonidine reached 370 ng/mL at 2 h.

## Covariates
Renal impairment: half-life up to 41 h; a lower initial dose may help; dialysis removes little (p3, p10). Paediatric safety is not established (p7). Hepatic function, age and sex are not addressed quantitatively.

## Caveats
- The SPL is a Technomed relabel of the older Alza-made system, while the current FDA approval is the Corium system. The two differ in area and drug content, but the delivery rates and PK text are the same.
- There is no absorption-rate or lag parameter. The skin-site saturation is described only qualitatively (p2).

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:40`: the `source_url` of `clonidine__transdermal_patch__transdermal__catalog__none__label` (catalog only, `descriptive_only`).

## Verification against stanpumpR
- **Formulation** "transdermal system 0.1/0.2/0.3 mg per day": **matches** (p1, p10).
- **"A programmed 7-day delivery"**: matches.
- **"F about 60%"**: matches (p3).
- **"Steady state within about 3 days"**: matches (p3).
- **"A residual skin depot that keeps delivering after removal"**: consistent with the label but worded as the repo's interpretation. The label says the adhesive's clonidine saturates the skin under the system (p2), and that levels persist about 8 h after removal and then fall with a half-life of about 20 h (p2-3). It does not use the word depot. **Matches in substance.**
- **"Its coefficients and BP fit are missing"**: true. The label gives no fitted model.
