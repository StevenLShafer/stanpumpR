---
file: NJ_label-tramadol_022370_2026-01-06.pdf
pmid: none
doi: none
citation: CONZIP (tramadol hydrochloride) extended-release capsules, US prescribing information, NDA 022370, revised 12/2025, FDA posting 2026-01-06 (Reference ID 5715909). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/022370s027lbl.pdf
kind: label
drugs: [tramadol]
source_read: text layer; read Highlights, contents, 2 Dosage and Administration (2.1-2.4), 8.4-8.7, 12.1-12.3 in full (Table 3 re-extracted with pypdf because the layout text scrambled it); skimmed boxed warning and section 5
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: not-applicable
---

# CONZIP label (NDA 022370, 2025/2026): once-daily extended-release tramadol capsules

## Study
FDA label for CONZIP extended-release capsules (racemic tramadol HCl; 100, 200, 300 mg), chronic pain. PK from
healthy-subject studies; Table 3 is a steady-state bioequivalence study (N = 38) against an unnamed reference
extended-release tramadol product.

## Model and parameters
Section 12.3:
- Single dose: Tmax about 10-12 h; 300 mg fasting mean Cmax 308 ng/mL, AUC 6777 ng·h/mL; bioequivalent to a
  reference ER tramadol product (300 mg single dose, fasting). Dose-proportional Cmax and AUC 100-300 mg.
- **No absolute bioavailability and no bioavailability relative to IR are stated.**

Table 3, steady state 200 mg, mean (%CV), N = 38:

| Parameter | Tramadol CONZIP | Tramadol reference ER | M1 CONZIP | M1 reference ER |
|---|---|---|---|---|
| AUC0-24 (ng·h/mL) | 5678 (27%) | 5563 (32%) | 1319 (34%) | 1302 (40%) |
| Cmax (ng/mL) | 332 (25%) | 350 (31%) | 70 (34%) | 74 (41%) |
| Cmin (ng/mL) | 128 (39%) | 125 (45%) | 35 (34%) | 33 (42%) |
| Tmax (h) | 5.9 (66%) | 10 (30%) | 11 (37%) | 13 (29%) |
| % fluctuation | 88 (19%) | 101 (30%) | 64 (22%) | 76 (30%) |

Other: food has no meaningful effect (300 mg). Vd 2.6 / 2.9 L/kg (male / female) after 100 mg IV; protein binding
about 20%. Plasma t½ after CONZIP: tramadol about 10 h, M1 about 11 h. N-demethylation by CYP3A4 and CYP2B6;
O-demethylation (M1) by CYP2D6. 30% excreted unchanged, 60% as metabolites. Same elderly, CYP2D6 poor-metaboliser,
quinidine, hepatic (M1 -50%) and renal (M1 +20-40%) statements as the ULTRAM ER label (studies of "an extended-release
tramadol product").

Dosing (section 2): start 100 mg once daily; IR patients convert to the 24-h IR total rounded down to the next 100 mg;
titrate by 100 mg every 5 days; maximum 300 mg/day. Not for severe hepatic or renal impairment (8.6, 8.7).

```yaml
parameters:
  - {name: Cmax_single_300mg, value: 308, unit: ng/mL, source: "12.3 Absorption", note: "fasting"}
  - {name: AUC_single_300mg, value: 6777, unit: ng*h/mL, source: "12.3 Absorption"}
  - {name: Tmax_single, value: "10-12", unit: h, source: "12.3 Absorption"}
  - {name: t_half_tramadol, value: 10, unit: h, source: "12.3 Elimination"}
  - {name: t_half_M1, value: 11, unit: h, source: "12.3 Elimination"}
  - {name: Vd, value: 2.6, unit: L/kg, source: "12.3 Distribution", note: "male; female 2.9; 100 mg IV"}
```

## Pharmacodynamics and end points
- **MEAC**: 12.2 states the minimum effective analgesic concentration varies widely and can rise with tolerance; **no
  value given**. 12.1: the exposure-efficacy relationship of tramadol and M1 has not been evaluated clinically.
- **Potency**: in animal models M1 is up to 6 times more potent than tramadol for analgesia and 200 times in mu
  binding (12.1). No equianalgesic ratio; no established conversion ratios from other opioids (2.3).
- No ke0, t½ke0, tPeak or hysteresis.

## Covariates
Descriptive only (age > 75, sex, hepatic/renal impairment, CYP2D6, CYP3A4/2D6 interactions, carbamazepine).

## Caveats
- ER capsule, not the IR capsule stanpumpR models; reference ER comparator not named.
- Section 8.7 again labels severe renal impairment "Child-Pugh Class C" (label error).
- In the Table 3 layout text the rows are shifted; values above are from the pypdf re-extraction.

## Used by stanpumpR
- R/drugs_tramadol.R:55-58 — header: oral bioavailability 0.70 "the regulatory figure for capsules", 0.75 for a
  100 mg IR tablet, and 85-90% for a 24-hour ER product relative to IR. Context notes say it is uncertain which
  label(s) the comment refers to.

## Verification against stanpumpR
- R/drugs_tramadol.R:55 (0.70 for capsules): CONZIP is a capsule, but its label gives no absolute or IR-relative
  bioavailability. **cannot verify**; this label does not support the 0.70.
- R/drugs_tramadol.R:55-56 (0.75 for a 100 mg IR tablet): not in this label. **cannot verify**.
- R/drugs_tramadol.R:56-57 (ER at 85-90% of IR): not in this label (that figure is in the ULTRAM ER label, NDA 021692).
  **cannot verify** from this document.
- Overall the archived CONZIP label supplies none of the numbers the comment uses; it is not the evidently intended
  source. Its ER parameters are not represented in stanpumpR, consistent with the comment.
