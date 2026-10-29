---
file: NJ_label-dilantininfatabs_084427_2019-03-21.pdf
pmid: none
doi: none
citation: "DILANTIN INFATABS (phenytoin chewable tablets) US prescribing information, Pfizer/Parke-Davis, ANDA 084427 (supplement s042), Drugs@FDA label dated 2019-03-21 (highlights \"Revised: 10/2018\"). https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/084427s042lbl.pdf"
kind: label
drugs: [phenytoin]
source_read: text layer; read sections 2 (2.1-2.8), 3, 8.4-8.6, 11, 12.1, 12.3
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Dilantin Infatabs chewable tablet label (2018/2019)

## Study
Not a study: US label for 50 mg phenytoin (free ACID) chewable tablets (sections 3, 11). Not for once-daily
dosing (2.1).

## Model and parameters
- Absorption (12.3): peak levels 1.5 to 3 h after a dose. Chewed and unchewed tablets are bioequivalent and
  absorbed faster than 100 mg Dilantin extended capsules.
- Steady state at least 7-10 days (5-7 half-lives) at 300 mg/day.
- Elimination (12.3): studies with Infatabs gave an average plasma half-life of 14 h, range 7-29 h (note:
  shorter than the 22 h, 7-42 h in the capsule and suspension labels).
- Metabolism CYP2C9 and CYP2C19; saturable.
- Salt/acid (2.5): Infatabs are free acid, ~8% more drug than sodium salt.
- Dosing (2.2, 2.3): adult start two 50 mg tablets three times daily, max 12 tablets/day, usual 6-8/day;
  pediatric 5 mg/kg/day start, 4-8 mg/kg/day maintenance, max 300 mg/day.

```yaml
parameters:
  - {name: tmax_chewable, value: "1.5-3", unit: h, source: "12.3", note: range}
  - {name: t_half_oral_infatabs, value: 14, unit: h, source: "12.3", note: "mean, range 7-29"}
  - {name: strength, value: 50, unit: "mg phenytoin acid per tablet", source: "3, 11"}
```

## Pharmacodynamics and end points
Therapeutic range total 10-20 mcg/mL (unbound 1-2) (2.4). No ke0/MEAC (not an opioid).

## Covariates
Clearance 20% lower over 70 y; pregnancy raises clearance; higher unbound fraction in renal/hepatic disease
and hypoalbuminaemia; sex and race no effect.

## Caveats
No study details for the tmax or half-life figures.

## Used by stanpumpR
- R/drugs_phenytoin.R:73 — "mg PO tablet" = phenytoin acid chewable tablet (Infatabs), x 1.
- R/drugs_phenytoin.R:88, :90 — label peak 1.5-3 h; ka 2.0/h label-anchored.
- R/drugs_phenytoin.R:160 and inst/help/drugs/phenytoin.md:11.

## Verification against stanpumpR
- :72-73, :238 (tablet saltFactor 1): label says Infatabs are free acid. **matches**.
- :88-90, :211 (shared fast ka 2.0/h; peak 2.4 h for 300 mg): label 1.5-3 h; inside. **matches**.
- help phenytoin.md:11: **matches**.
- Not used by the repo: the label's shorter Infatabs half-life (14 h). The model gives one elimination for all
  forms, which is appropriate since half-life is a disposition property; noted only.
