---
file: NJ_label-calciumgluconate_208418_2022-10-19.pdf
pmid: none
doi: none
citation: "Calcium Gluconate in Sodium Chloride Injection (20 mg/mL premixed, 1,000 mg/50 mL and 2,000 mg/100 mL freeflex bags), US prescribing information, NDA 208418 supplement 013 (Fresenius Kabi USA, Lake Zurich IL), revised 10/2022, Drugs@FDA posting 2022-10-19. https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/208418Orig1s013lbl.pdf"
kind: label
drugs: [calcium]
source_read: "text layer, all 13 pages: Highlights, 2 Dosage and Administration (Tables 1 and 2), 3, 5.4, 8 Use in Specific Populations, 10, 11 Description, 12 Clinical Pharmacology; page image of PDF p. 3 (Table 2 and rate limits) and p. 11 (container label)"
digest_by: Claude Opus 5.5, 2026-10-14
checked_against: stanpumpR master 0c9788e
verification: matches
---

# Calcium gluconate in sodium chloride injection label (NDA 208418), 2022

PDF pages 1-10 = the label's printed footer pages 1-10; PDF p. 11-12 are container labels; PDF p. 13 is the FDA
signature page.

## Study
Not a study: the US label for **Calcium Gluconate in Sodium Chloride Injection**, a ready-to-use premixed bag
(Fresenius Kabi, made in Norway). Initial US approval 1941. Indication: acute symptomatic hypocalcemia in pediatric
and adult patients; long-term safety not established (§1, PDF p. 2). Note: this is **not** the 10% (100 mg/mL)
calcium gluconate vial. NDA 208418's original 2017 label (s000), which Ansari 2025 cites as its ref. 2, was the 10%
vial; this archived supplement (s013) is the premixed 20 mg/mL product. No clinical PK study is described.

**Composition (salt vs elemental calcium vs mEq):**
- **20 mg/mL of calcium gluconate**, which contains **1.86 mg (0.093 mEq) of elemental calcium per mL** (§2.1,
  PDF p. 2; §3, PDF p. 4; §11, PDF p. 8; Highlights, PDF p. 1).
- Table 1 (§2.1, PDF p. 2): 1,000 mg per 50 mL = **93 mg = 4.65 mEq elemental calcium**; 2,000 mg per 100 mL = 186 mg
  = 9.3 mEq. Container label (PDF p. 11): total elemental calcium in bag 93 mg (4.65 mEq).
- §11 Description (PDF p. 8): calcium gluconate is calcium D-gluconate (1:2) monohydrate, C12H22CaO14.H2O,
  **molecular weight 448.39**; solubility 3.5 g/100 mL at 25 C. Each mL's 20 mg of calcium gluconate is
  **equivalent to 18.8 mg of calcium gluconate and 0.9 mg of calcium saccharate tetrahydrate**; NaCl 6.75 mg/mL;
  pH 6.0-8.2. (The same section also lists the bags as 1,000 mg per 50 mL "(18.8 mg per mL)", an internal
  inconsistency with the 20 mg/mL elsewhere.) Aluminum up to 100 mcg/L (§5.5).
- Derived: pure monohydrate would give 20/448.39 x 40.08 = 1.79 mg Ca/mL (0.089 mEq); the label's 1.86 mg (0.093 mEq)
  reflects the saccharate. Per gram of labelled calcium gluconate: 93 mg Ca = 4.65 mEq = **2.32 mmol** (derived),
  vs 2.23 mmol for pure monohydrate (derived: 1000/448.39).

## Model and parameters
No PK model or numeric PK parameters. §12.3 Pharmacokinetics (PDF p. 9):
- Absorption: 100% bioavailable after IV injection.
- Metabolism: calcium not metabolized; release of ionized calcium is direct and not affected by first pass through the
  liver. §8.7 (PDF p. 8): hepatic function does not affect availability of ionized calcium; no dose adjustment.
- Distribution: 99% of body calcium in skeleton, 1% in ECF and soft tissue; about **50% of total serum calcium is
  ionized**; 8-10% bound to organic and inorganic acids; ~40% protein bound (mainly albumin).
- Elimination: urinary calcium excretion rises significantly after IV calcium gluconate. No half-life, clearance or
  volume.
- §12.1 (PDF p. 8-9): IV calcium gluconate raises serum ionized calcium; it dissociates into ionized calcium in plasma.

**Dosing, Table 2 (§2.2, PDF p. 3; confirmed from the page image), in mg of calcium gluconate:**

| Population | Initial dose | Subsequent bolus (if needed) | Continuous infusion |
|---|---|---|---|
| Neonate (<= 1 month) | 100-200 mg/kg | 100-200 mg/kg every 6 h | initiate 17-33 mg/kg/hour |
| Pediatric (> 1 month to < 17 yr) | 29-60 mg/kg | 29-60 mg/kg every 6 h | initiate 8-13 mg/kg/hour |
| Adult | 1000-2000 mg | 1000-2000 mg every 6 h | initiate 5.4-21.5 mg/kg/hour |

- **Bolus rate limit: do not exceed 200 mg/minute in adults, 100 mg/minute in pediatric patients** (Table 2 footnote,
  PDF p. 3; repeated in §5.4, PDF p. 5, with ECG monitoring recommended). Continuous infusion adjusted to serum calcium.
- Do not dilute; secure IV line; serum calcium every 4-6 h during intermittent and every 1-4 h during continuous
  infusion (§2.3, PDF p. 4). Renal impairment: lowest dose, serum calcium every 4 h (§2.4, §8.6). Geriatric: start at
  the low end (§8.5, PDF p. 7). Incompatible with ceftriaxone (contraindicated in neonates <= 28 days), phosphate,
  bicarbonate, minocycline (§2.5).

```yaml
parameters:
  - {name: concentration_salt, value: 20, unit: "mg/mL calcium gluconate", source: "§2.1, §3, PDF p. 2, 4", note: "premixed in NaCl; 18.8 mg gluconate + 0.9 mg saccharate per §11"}
  - {name: elemental_calcium, value: 1.86, unit: "mg/mL", source: "§2.1 and §11, PDF p. 2, 8", note: "0.093 mEq/mL"}
  - {name: elemental_calcium_per_g_salt, value: 93, unit: "mg per 1,000 mg (4.65 mEq)", source: "Table 1, §2.1, PDF p. 2", note: "derived 2.32 mmol/g"}
  - {name: MW_calcium_gluconate_monohydrate, value: 448.39, unit: g/mol, source: "§11, PDF p. 8", note: "C12H22CaO14.H2O"}
  - {name: max_bolus_rate_adult, value: 200, unit: "mg/min calcium gluconate", source: "Table 2 footnote, PDF p. 3; §5.4, PDF p. 5", note: ""}
  - {name: max_bolus_rate_pediatric, value: 100, unit: "mg/min calcium gluconate", source: "Table 2 footnote, PDF p. 3; §5.4, PDF p. 5", note: ""}
  - {name: adult_dose, value: "1000-2000", unit: mg, source: "Table 2, PDF p. 3", note: "initial and every 6 h"}
  - {name: adult_infusion, value: "5.4-21.5", unit: "mg/kg/hour", source: "Table 2, PDF p. 3", note: "initial rate"}
  - {name: bioavailability_IV, value: 100, unit: "%", source: "§12.3, PDF p. 9", note: ""}
  - {name: hypercalcemia_symptom_threshold, value: 12, unit: "mg/dL total serum calcium", source: "§10, PDF p. 8", note: ">= 12"}
```

## Pharmacodynamics and end points
No PD parameters. §10 Overdosage (PDF p. 8): hypercalcemia symptoms typically when **total** serum calcium >= 12 mg/dL.
Contraindicated in hypercalcemia (§4). No ionized-calcium target range. Not an opioid label: no MEAC, ke0 or potency
ratio, and no statement of equivalence to calcium chloride.

## Covariates
None quantified. Renal impairment, geriatric and hepatic statements as above. Pregnancy (§8.1, PDF p. 6): data
insufficient to inform drug-associated risk; maternal hypocalcemia carries risks. Pediatric dosing (§8.4) is based on
literature and clinical experience, not controlled trials.

## Caveats
- Doses and rates are in mg of the **calcium gluconate product**, whose 20 mg/mL includes calcium saccharate, so the
  elemental calcium per gram (93 mg, 2.32 mmol) is about 4% above pure-monohydrate stoichiometry (2.23 mmol).
- A premixed 20 mg/mL bag label, not the 10% vial label that Ansari 2025 cited; the 200 mg/min adult limit is the
  same quantity they quote.
- No PK numbers.

## Used by stanpumpR
- inst/help/drugs/calcium.md:23 and inst/help/scenarios/calcium-salt-substitution.md:10: the calcium gluconate
  insert's maximal rate, 200 mg/min (as cited by Ansari 2025). Related, not listed in the context: calcium.md:7
  (calcium gluconate monohydrate 448 g/mol; a 10% ampoule 1 g in 10 mL, 2.2 mmol of calcium).

## Verification against stanpumpR
Checked at master 0c9788e (worktree stanpumpr-digests-review-490f93).
- **inst/help/drugs/calcium.md:23** "the calcium gluconate package insert's maximal rate of 200 mg/min": Table 2
  footnote (PDF p. 3) and §5.4 (PDF p. 5): 200 mg/min for bolus administration in adults (100 mg/min in children).
  The help's comparison (200 mg/min gluconate is two-thirds of 100 mg/min chloride in calcium terms; 300 mg/min is
  the equivalent) uses the fitted ratio 3.11 from Ansari 2025, not this label. **matches**.
- **inst/help/scenarios/calcium-salt-substitution.md:10** "The calcium gluconate insert allows 200 mg/min": **matches**
  (adult bolus limit).
- **inst/help/drugs/calcium.md:7** (related) monohydrate, 448 g/mol: §11 gives C12H22CaO14.H2O, 448.39. **matches**.
  "A 10% ampoule is 1 g in 10 mL, 2.2 mmol of calcium": this label covers only the 20 mg/mL premixed bag, so the 10%
  presentation **cannot be verified** here. Per gram of labelled calcium gluconate this label gives 93 mg = 4.65 mEq
  elemental calcium = 2.32 mmol (derived), not 2.2; the help's 2.2 is pure-monohydrate stoichiometry (2.23). The model
  itself does not use either: R/drugs_calcium.R:102 converts a gram of gluconate as 1/3.11 g of calcium chloride
  (2.19 mmol chloride equivalent), the ratio fitted in vivo by Ansari 2025, which absorbs product composition.
  Not counted as a discrepancy.
