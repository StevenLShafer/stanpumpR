---
file: NJ_dailymed-9e22e1d8.pdf
pmid: none
doi: none
citation: "METHYLDOPA tablets USP 250 mg and 500 mg, Rising Pharma Holdings, Inc.; ANDA070076 (marketing start 04/04/2025); prescribing information Issued 08/2024, SPL Revised 3/2025, DailyMed SPL setid 9e22e1d8-04ec-4b67-92ea-b09c706f2703, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=9e22e1d8-04ec-4b67-92ea-b09c706f2703"
kind: label
drugs: [methyldopa]
source_read: "text layer, all 13 pages (one form-feed chunk per page). Read: Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism p.1-2; Indications, Contraindications, Warnings p.2-4; Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p.4-6; Adverse Reactions p.6-7; Overdosage p.7-8; Dosage and Administration p.8-9; How Supplied p.9; package and SPL data p.9-13. Old-format label without numbered sections. Page image: p.1 (identity)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Methyldopa tablets (Rising) prescribing information, DailyMed

## Study
Not a study: a generic methyldopa 250/500 mg film-coated tablet label (Rising Pharma, ANDA070076). The file is what
its name says (image p.1).

## Model and parameters
No model. Clinical Pharmacology, Pharmacokinetics and Metabolism (p.2):
- Maximum fall in blood pressure 4-6 h after an oral dose; once an effective dose is reached, a smooth response
  in 12-24 h in most patients; after withdrawal, BP returns to baseline within 24-48 h.
- Extensively metabolised (mono-O-sulfate, 3-O-methyl-alpha-methyldopa, 3,4-dihydroxyphenylacetone,
  alpha-methyldopamine, 3-O-methyl-alpha-methyldopamine and conjugates).
- About 70% of the absorbed drug is excreted in urine as methyldopa and its mono-O-sulfate. Renal clearance about
  130 mL/min in normal subjects, reduced in renal insufficiency.
- Plasma half-life 105 min; excretion essentially complete 36 h after oral doses.
- Crosses the placenta, appears in cord blood and breast milk; dialysable.
- No oral bioavailability, volume or total clearance is given. Molecular weight 238.24 (sesquihydrate) (p.1).
Mechanism (p.1): the antihypertensive effect is probably due to its metabolite alpha-methylnorepinephrine
(central alpha-adrenergic stimulation, false transmitter, and/or lower renin); only the L-isomer is active.
Dosage (p.8-9): adults 250 mg two to three times daily for 48 h, then adjusted at intervals of at least 2 days;
usual 500 mg-2 g/day in two to four doses, maximum 3 g/day; children 10 mg/kg/day in two to four doses, maximum
65 mg/kg or 3 g/day. Ferrous sulfate or gluconate reduces bioavailability (p.4-5).

```yaml
parameters:
  - {name: t_half_methyldopa, value: 105, unit: min, source: "Pharmacokinetics p.2", note: plasma, parent}
  - {name: CL_renal, value: 130, unit: mL/min, source: "Pharmacokinetics p.2", note: "about, normal subjects"}
  - {name: fe_urine_absorbed, value: 0.70, unit: fraction of absorbed dose, source: "Pharmacokinetics p.2", note: "as methyldopa plus mono-O-sulfate"}
  - {name: t_max_BP_effect, value: "4-6", unit: h, source: "Pharmacokinetics p.2", note: maximum BP decrease after oral dose}
  - {name: MW_methyldopa_sesquihydrate, value: 238.24, unit: g/mol, source: "Description p.1"}
```

## Pharmacodynamics and end points
Peak BP effect 4-6 h after an oral dose; effect offset within 24-48 h of stopping (p.2). Pregnancy (p.6): five
studies, 332 hypertensive pregnant women, improved fetal outcome. No therapeutic plasma range. Not an opioid.

## Covariates
Renal function (renal clearance falls; elderly dosing caution); iron salts lower bioavailability.

## Caveats
Old descriptive label; no n, assay or study design behind the numbers; no metabolite kinetics.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:82: catalog record `methyldopa__ir_tablet__oral__catalog__none__label`,
  active moiety alpha-methylnorepinephrine (central), analyte methyldopa (parent), plasma, official_label,
  descriptive only; limitations "Parent half-life about 105 min but the oral BP maximum is at 4-6 h, because the
  effector is a central metabolite: parent plasma is a poor driver and the metabolite's kinetics are missing."
  Built at data-raw/antihypertensiveRegistry.R:369-374.

## Verification against stanpumpR
- **Parent half-life about 105 min**: p.2. **matches**.
- **Oral BP maximum at 4-6 h**: p.2. **matches**.
- **Effector is a central metabolite (alpha-methylnorepinephrine)**: p.1 says the effect "probably" comes from
  that metabolite; the registry states it as fact, a fair reading of the label. **matches**.
- **Metabolite kinetics missing**: the label gives none. **matches**.
