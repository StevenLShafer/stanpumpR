---
file: NJ_dailymed-c9d8edb5.pdf
pmid: none
doi: none
citation: "Nebivolol tablets (nebivolol hydrochloride), for oral use. Generic label (ANDA203659), manufactured by ANI Pharmaceuticals, marketed by Golden State Medical Supply (GSMS), revised 10/2025. DailyMed setid c9d8edb5-5312-ad38-e053-2995a90a7154, printed 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=c9d8edb5-5312-ad38-e053-2995a90a7154"
kind: label
drugs: [nebivolol]
source_read: "text layer: Highlights p. 1, Contents p. 2, sections 1-2 pp. 3-4, 4-5 pp. 4-6, 7-8 pp. 8-11, 10-12.5 pp. 11-15; SPL product tables pp. 23-27 (ANDA, NDC) skimmed. Clinical studies, patient information not used"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Nebivolol label (DailyMed, GSMS/ANI generic, 2025)

The PDF is what its URL names: a DailyMed SPL print of a repackager's (GSMS) generic nebivolol tablet label, ANI
Pharmaceuticals ANDA203659, 2.5/5/10/20 mg, revised 10/2025, 27 pages (one page per text chunk). Not the Bystolic
NDA label; its clinical pharmacology text is the reference product's. PDF page = printed page.

## Study
Label, not a study. Indication: hypertension. Start 5 mg once daily, titrate at 2-week intervals to 40 mg; 2.5 mg
start in severe renal impairment (CrCl <30 mL/min) and moderate hepatic impairment; contraindicated in severe
hepatic impairment (Child-Pugh >B); no adjustment for age or CYP2D6 poor metabolisers (section 2, p. 4).

## Model and parameters
No compartmental model, no clearance or volume in absolute units. Section 12.3-12.5 (pp. 13-15):

```yaml
parameters:
  - {name: d-nebivolol effective half-life EM, value: 12, unit: h, source: "12.3 p. 13", note: "about; CYP2D6 extensive metabolisers"}
  - {name: d-nebivolol effective half-life PM, value: 19, unit: h, source: "12.3 p. 13", note: "poor metabolisers"}
  - {name: PM/EM d-nebivolol Cmax ratio, value: 5, unit: fold, source: "12.3 p. 13", note: "same dose"}
  - {name: PM/EM d-nebivolol AUC ratio, value: 10, unit: fold, source: "12.3 p. 13", note: "same dose"}
  - {name: accumulation once daily EM, value: 1.5, unit: fold, source: "12.3 p. 13", note: "d-nebivolol"}
  - {name: tmax, value: "1.5-4", unit: h, source: "12.3 p. 13", note: "EMs and PMs"}
  - {name: protein binding, value: 98, unit: "%", source: "12.3 p. 14", note: "mostly albumin"}
  - {name: absolute bioavailability, value: null, unit: "", source: "12.3 p. 13", note: "not determined"}
  - {name: half-life low doses (overdose section), value: "12-19", unit: h, source: "10 p. 12", note: ""}
```

Dose proportional for d-nebivolol up to 20 mg in EMs and PMs; l-nebivolol exposure is higher, but d-nebivolol's
beta-receptor affinity is more than 1000-fold greater. Food has no effect. Mass balance (14C): EMs 38% urine, 44%
faeces; PMs 67% urine, 13% faeces; essentially all as oxidative metabolites and glucuronides (p. 14). Moderate
hepatic impairment: d-nebivolol Cmax ×3, AUC ×10, apparent CL −86%. Renal: apparent CL unchanged in mild (n = 7),
negligibly reduced in moderate (n = 9), −53% in severe impairment (n = 5), single 5 mg dose. Fluoxetine 20 mg/day
raised d-nebivolol AUC 8-fold and Cmax 3-fold; cimetidine +23% (pp. 14-15).

## Pharmacodynamics and end points
β1-selective in EMs at ≤10 mg; in PMs and at higher doses blocks β1 and β2; metabolites (hydroxyl, glucuronides)
contribute to β-blockade (12 and 12.3, p. 13). Mechanism of the antihypertensive effect not established (12.1). No
concentration-effect relationship is given.

## Covariates
CYP2D6 phenotype (large PK effect, small clinical one per the label), hepatic and severe renal impairment.
Pregnancy (8.1, p. 9): human data insufficient; rat embryofetal and perinatal lethality near the MRHD; third-
trimester β-blocker neonatal risks. Lactation: not recommended (8.2, p. 10).

## Caveats
Repackager's generic label; absolute bioavailability, CL and V are not stated; half-lives are "effective"
half-lives of the active d-isomer.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv` line 92 (built by `data-raw/antihypertensiveRegistry.R` lines
  286-290): a descriptive-only, unfitted catalogue record for oral nebivolol, no BP simulation; nebivolol is not a
  library drug (not in `drugDefaults_global.csv`).

## Verification against stanpumpR
- "Effective d-nebivolol half-life about 12 h in CYP2D6 extensive and about 19 h in poor metabolisers, whose
  d-isomer exposure is substantially higher": **matches** 12.3 (p. 13).
- Active moiety "d-nebivolol and active metabolites": **matches** (metabolites contribute to β-blockade, p. 13).
- "A racemic-parent-only Emax is unsupported; the active-moiety split and BP are missing": an inference consistent
  with the label, which gives no concentration-BP relationship and says the d-isomer and metabolites carry the
  activity. **matches.**
- Record fields route oral, immediate-release tablet, plasma, source_type official_label: **matches.**
