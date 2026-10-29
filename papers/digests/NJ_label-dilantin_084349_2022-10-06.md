---
file: NJ_label-dilantin_084349_2022-10-06.pdf
pmid: none
doi: none
citation: "DILANTIN (extended phenytoin sodium capsules) US prescribing information, Pfizer/Parke-Davis, ANDA 084349 (supplement s088), Drugs@FDA label dated 2022-10-06 (highlights \"Revised: 3/2022\"). https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/084349s088lbl.pdf"
kind: label
drugs: [phenytoin]
source_read: text layer; read highlights, sections 2 (2.1-2.7), 8.4-8.7, 10, 11, 12.1, 12.3, 12.5
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Dilantin extended capsules label (2022)

## Study
Not a study: US label for phenytoin sodium extended capsules, 30 mg and 100 mg (Kapseals). Each capsule
contains the stated mg of phenytoin SODIUM (section 11).

## Model and parameters
No compartmental model. Label facts:
- Absorption (12.3, and 11): extended capsule, peak serum levels 4 to 12 h after a dose. Section 11 contrasts
  Prompt Phenytoin Sodium Capsules, peak 1.5 to 3 h.
- Steady state at least 7 to 10 days (5-7 half-lives) at 300 mg/day (12.3; 2.3).
- Elimination (12.3): plasma half-life after oral dosing averages 22 h, range 7 to 42 h.
- Metabolism: CYP2C9 mainly, CYP2C19 less; saturable hydroxylation, so a dose increase of 10% or more can
  raise the steady state disproportionately (12.3).
- Salt/acid (2.4): suspension and Infatabs are free acid; capsules and injection are sodium salt; the acid
  forms carry about 8% more drug.
- Dosing (2.1, 2.2): adult start 100 mg three times daily, maintenance usually 300-400 mg/day, max 600 mg/day;
  once-daily 300 mg allowed for this product only; oral load 1 g as 400/300/300 mg at 2 h intervals.
  Pediatric 5 mg/kg/day start, 4-8 mg/kg/day maintenance, max 300 mg/day.
- No volume, clearance, Vmax or Km values are given.

```yaml
parameters:
  - {name: tmax_ER_capsule, value: "4-12", unit: h, source: "12.3 and 11", note: range}
  - {name: tmax_prompt_capsule, value: "1.5-3", unit: h, source: "11", note: comparator product}
  - {name: t_half_oral, value: 22, unit: h, source: "12.3", note: "mean, range 7-42"}
  - {name: acid_vs_sodium_content, value: 8, unit: "% more drug in acid forms", source: "2.4"}
  - {name: CL_elderly, value: -20, unit: "% (over 70 y vs 20-30 y)", source: "12.3 Specific Populations"}
```

## Pharmacodynamics and end points
- Therapeutic range (2.3): total serum 10-20 mcg/mL (unbound 1-2 mcg/mL); some mild cases controlled lower.
- Toxicity (10): nystagmus on lateral gaze usually at 20 mcg/mL, ataxia at 30, dysarthria and lethargy above 40;
  50 mcg/mL reported without toxicity. Adult lethal dose estimated 2-5 g.
- No effect-site, ke0 or MEAC data (not an opioid).

## Covariates
Age (clearance 20% lower over 70 y), CYP2C9 genotype (intermediate/poor metabolisers have lower clearance;
prevalence in 12.5), pregnancy (clearance rises, peak in third trimester), albumin/renal/hepatic disease
(higher unbound fraction). Sex and race: no significant effect.

## Caveats
Ranges are label summaries without population or study details. Half-life of a saturable drug depends on
concentration; the 22 h figure is a single average.

## Used by stanpumpR
- R/drugs_phenytoin.R:70 — "mg PO ER" is phenytoin sodium extended capsule, x 0.92.
- R/drugs_phenytoin.R:85 — ER capsule ka 0.225/h (Cheng) gives a 300 mg peak at about 11 h, inside the
  label's 4 to 12 h.
- R/drugs_phenytoin.R:160 — combined label reference line.

## Verification against stanpumpR
- :68-70, :236 (saltFactor default = 252.27/274.25 for the ER sodium capsule): label says capsules are
  phenytoin sodium and acid forms carry ~8% more drug; 1/0.9199 = 1.087 (derived). **matches**.
- :84-86 peak about 11 h for 300 mg vs label 4-12 h: within the range (at its upper end). The 11 h value is a
  simulation result not re-run here. **matches** (range check).
- :86 bioavailability 1 for the ER capsule: label (injection and Cerebyx sections) states capsules are about
  90% bioavailable; the code says F = 1 on Odani's apparent scale, which is internally consistent for oral
  dosing, but it makes IV and oral doses equal per mg where the label expects IV levels to be modestly
  higher. Not claimed from this label; noted only.
- :139 band 10-20 mcg/mL total (unbound 1-2): label 2.3 gives the same. **matches**.
- :160-161 reference line names no revision or date. Documentation note only.
- Derived context (not a repo claim): the model's low-concentration half-life is 0.693 x 65.4 L x 9.19 / 21.7
  mg/h = 19 h, rising to about 45 h at 12.4 mg/L (derived), against the label's mean 22 h (7-42).
