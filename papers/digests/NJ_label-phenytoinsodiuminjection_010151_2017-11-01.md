---
file: NJ_label-phenytoinsodiuminjection_010151_2017-11-01.pdf
pmid: none
doi: none
citation: "DILANTIN (phenytoin sodium) injection US prescribing information, Pfizer/Parke-Davis, NDA 010151 (supplements s047/s048), Drugs@FDA label dated 2017-11-01 (highlights \"Revised: 10/2017\"). Archived as the reference for generic phenytoin sodium injection (the listed ANDA 084307 has no Drugs@FDA label). https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/010151s047s048lbl.pdf"
kind: label
drugs: [phenytoin]
source_read: text layer; read highlights, sections 2 (2.1-2.8), 3, 8.4-8.6, 10, 11, 12.1, 12.3
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Dilantin (phenytoin sodium) injection label (2017)

## Study
Not a study: label for 50 mg phenytoin SODIUM per mL injection (40% propylene glycol, 10% alcohol, pH 12),
IV or IM (sections 3, 11).

## Model and parameters
- Rate limits (2.1): IV no faster than 50 mg/min in adults; children 1-3 mg/kg/min or 50 mg/min, whichever is
  slower. Infusion diluted in saline at no less than 5 mg/mL, completed within 1-4 h, in-line filter.
- Status epilepticus (2.2): adult load 10-15 mg/kg IV at up to 50 mg/min (about 20 min in a 70 kg patient),
  then 100 mg orally or IV every 6-8 h; pediatric load 15-20 mg/kg.
- Non-emergent (2.3): adult load 10-15 mg/kg slowly, then maintenance every 6-8 h. IM dose must be 50% above
  the oral dose; on return to oral, halve the oral dose for one week.
- Substitution (2.4): capsules about 90% bioavailable orally; phenytoin 100% bioavailable IV, so levels may rise
  modestly on IV substitution. Acid forms carry ~8% more drug than sodium salt.
- PK (12.3): IM gives peak levels 50-60% of stable oral levels at an equal dose (slow absorption); IM peak may
  take up to 24 h (2.2). Serum half-life after IV 10 to 15 h. Extensively protein bound. CYP2C9 and CYP2C19.
- No V, CL, Vmax or Km.

```yaml
parameters:
  - {name: t_half_IV, value: "10-15", unit: h, source: "12.3", note: range}
  - {name: F_oral_capsule, value: 0.90, unit: fraction, source: "2.4", note: approximate}
  - {name: F_IV, value: 1.0, unit: fraction, source: "2.4"}
  - {name: max_rate_adult, value: 50, unit: mg/min, source: "2.1"}
  - {name: load_adult, value: "10-15", unit: mg/kg, source: "2.2, 2.3"}
  - {name: load_pediatric, value: "15-20", unit: mg/kg, source: "2.2, 2.8"}
```

## Pharmacodynamics and end points
Therapeutic range total 10-20 mcg/mL (unbound 1-2) (2.1, 12.3). Overdose: nystagmus ~20, ataxia ~30,
dysarthria/lethargy >40 mcg/mL; lethal adult dose 2-5 g (10). No ke0/MEAC (not an opioid).

## Covariates
Elderly clearance 20% lower (>70 vs 20-30 y); pregnancy; renal/hepatic/hypoalbuminaemia raise unbound fraction.

## Caveats
The label never states the 0.92 molecular-weight factor; it gives the ~8% content difference.

## Used by stanpumpR
- R/drugs_phenytoin.R:160 — reference line; context: IV phenytoin sodium dose basis (x 0.92 acid).

## Verification against stanpumpR
- :68, :234 (units "mg", "mg/kg", "mg/min" are phenytoin sodium injection, x 252.27/274.25 = 0.9199): label
  confirms the injection is the sodium salt; 0.92 is a molecular-weight ratio (code :77), consistent with the
  label's ~8% (1/0.92 = 1.087, derived). **matches**.
- :60-61 check "18 mg/kg of phenytoin sodium ... 1159 mg x 0.92 / 65.5 L": arithmetic for a 64.4 kg patient;
  the 18 mg/kg load is slightly above this label's adult 10-15 mg/kg but within the pediatric 15-20 mg/kg and
  common guideline use; not a label claim. Noted only.
- Not represented: F 0.90 oral vs 1.0 IV (the model gives both 1, so IV-for-oral substitution shows no rise).
  The code states F = 1 is Odani's assumption; noted only.
- Phenytoin IM (sodium) route is not offered in the code; only fosphenytoin IM. Consistent with this label's
  advice against IM phenytoin.
