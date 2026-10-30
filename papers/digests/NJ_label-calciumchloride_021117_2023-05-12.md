---
file: NJ_label-calciumchloride_021117_2023-05-12.pdf
pmid: none
doi: none
citation: "10% Calcium Chloride Injection, USP (calcium chloride dihydrate, 100 mg/mL; Ansyr plastic and LifeShield Abboject glass syringes), US prescribing information, NDA 021117 (distributed by Hospira, Inc., Lake Forest IL; adverse-reaction contact Pfizer), revised 05/2023, Drugs@FDA posting 2023-05-12 (Reference ID 5172753). https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/021117s025lbl.pdf"
kind: label
drugs: [calcium]
source_read: "text layer, all 9 pages: Highlights, 1 Indications, 2 Dosage and Administration, 3 Dosage Forms, 4-5, 8 Use in Specific Populations, 10 Overdosage, 11 Description, 12 Clinical Pharmacology, 16 How Supplied"
digest_by: Claude Opus 5.5, 2026-10-14
checked_against: stanpumpR master 0c9788e
verification: matches
---

# Calcium chloride 10% injection label (NDA 021117), 2023

PDF pages = the label's printed footer page numbers (1-9).

## Study
Not a study: the US label for 10% Calcium Chloride Injection, USP, single-dose 10 mL prefilled syringes. Initial US
approval 1938. Indication: acute symptomatic hypocalcemia in adults and children; long-term safety and effectiveness
not established (§1, PDF p. 3). No clinical PK study is described.

**Composition (salt vs elemental calcium vs mEq):**
- 10% = 1,000 mg/10 mL = **100 mg/mL of calcium chloride dihydrate** (§3, PDF p. 4; Highlights, PDF p. 1; §16, PDF p. 9).
- 100 mg/mL = **27 mg or 1.4 mEq of elemental calcium per mL** (§3, PDF p. 4; Highlights; §16).
- §11 Description (PDF p. 7-8): each mL contains 100 mg (1.4 mEq/mL) of calcium chloride dihydrate, i.e. 1.4 mEq each
  of Ca++ and Cl-, in water for injection; chemically CaCl2.2H2O (dihydrate). Molecular weight not printed. Osmolarity
  2.04 mOsmol/mL (calc.); pH 5.5-7.5 (diluted to 5%); oxygen sensitive; aluminum <= 1,000 mcg/L.
- Derived: 100 mg / 147.01 g/mol = 0.680 mmol/mL = 1.36 mEq/mL and 27.3 mg Ca/mL; the full syringe (1 g) = 6.80 mmol
  = 13.6 mEq = 273 mg elemental calcium (derived: stoichiometry; the label rounds to 1.4 mEq/mL, i.e. 14 mEq per syringe).

## Model and parameters
No PK model or numeric PK parameters. §12.3 Pharmacokinetics (PDF p. 8):
- Absorption: 100% bioavailable after IV injection.
- Distribution: ~99% of body calcium in skeleton, 1% in extracellular fluid and soft tissue. About **50% of total serum
  calcium is ionized** (the biologically active part); 8-10% bound to organic and inorganic acids; ~40% protein bound
  (mainly albumin).
- Metabolism: none. Excretion: renal (glomerular filtration plus tubular reabsorption); urinary calcium rises during
  and after IV calcium chloride. No half-life, clearance or volume given.
- Specific populations: effect of age, sex, race, ethnicity, renal or hepatic impairment on calcium PK not evaluated.
- §12.1 (PDF p. 8): IV calcium chloride raises serum ionized calcium; it dissociates into ionized calcium in plasma.
- §12.2: exposure-response and time course of PD response not fully characterized.

**Dosing (§2, PDF p. 3; Highlights, PDF p. 1):**
- Slow IV infusion into a central or deep vein; **do not give by bolus**. **Maximum recommended rate 1 mL/minute
  (100 mg/minute)** of the 10% solution (§2.1). §5.2 (PDF p. 4-5): injection faster than 1 mL/min may cause
  hypotension and cardiac syncope; rapid injection may cause vasodilation, bradycardia, arrhythmias, cardiac arrest.
- Adults: **200 mg to 1,000 mg**. Pediatric: **2.7 to 5 mg/kg of calcium chloride** (§2.2). Doses < 200 mg cannot be
  given with this syringe. Individualize by serum ionized calcium, symptom severity and acuity. Repeat injections may be
  needed because calcium is excreted rapidly.
- Renal impairment (§2.3): start at 200 mg (adults), 2.7 mg/kg (children); §8.6 (PDF p. 7): lowest dose, monitor
  serum calcium e.g. every 4 hours (calcium-phosphorus product).
- Not IM or SC. Not with ceftriaxone (contraindicated in neonates <= 28 days needing ceftriaxone).

```yaml
parameters:
  - {name: concentration_salt, value: 100, unit: "mg/mL CaCl2.2H2O", source: "§3 and §11, PDF p. 4, 7", note: "10%, 1,000 mg/10 mL"}
  - {name: elemental_calcium, value: 27, unit: "mg/mL", source: "§3, PDF p. 4; §16, PDF p. 9", note: "label value"}
  - {name: elemental_calcium_mEq, value: 1.4, unit: "mEq/mL", source: "§3 and §11, PDF p. 4, 7", note: "label value (stoichiometry 1.36)"}
  - {name: max_rate, value: 100, unit: "mg/min (1 mL/min)", source: "§2.1, PDF p. 3", note: "slow infusion; no bolus"}
  - {name: adult_dose, value: "200-1000", unit: mg, source: "§2.2, PDF p. 3", note: "calcium chloride"}
  - {name: pediatric_dose, value: "2.7-5", unit: "mg/kg", source: "§2.2, PDF p. 3", note: "calcium chloride"}
  - {name: bioavailability_IV, value: 100, unit: "%", source: "§12.3, PDF p. 8", note: ""}
  - {name: ionized_fraction_total_serum_Ca, value: 50, unit: "%", source: "§12.3, PDF p. 8", note: "about; ~40% protein bound, 8-10% complexed"}
  - {name: hypercalcemia_symptom_threshold, value: 12, unit: "mg/dL total serum calcium", source: "§10, PDF p. 7", note: ">= 12"}
```

## Pharmacodynamics and end points
No PD parameters. §10 Overdosage (PDF p. 7): symptoms of hypercalcemia typically when **total** serum calcium is
>= 12 mg/dL (QT shortening, bradycardia, hypertension, GI and neurological effects, diuresis). Contraindicated in
ventricular fibrillation and in asystole/electromechanical dissociation (§4). No therapeutic range given in ionized
calcium units. Not an opioid label: no MEAC, ke0 or potency ratio.

## Covariates
None quantified. Renal impairment: lower starting dose and monitoring (§2.3, §8.6). Geriatric: insufficient numbers
studied (§8.5). Pregnancy (§8.1, PDF p. 6): IV calcium chloride for acute hypocalcemia not expected to cause birth
defects or adverse outcomes; no animal reproduction studies.

## Caveats
- Labelled dose and rate are in mg of the **dihydrate salt**, not elemental calcium or mmol.
- No PK numbers (half-life, volume, clearance); the label's own §12.2 says the PD time course is uncharacterized.
- This is the 2023 revision (s025); Ansari 2025 cites the 2017/2018 Hospira label (021117s015). The 100 mg/min
  maximum is the same quantity they quote.
- Product is a prefilled syringe; stanpumpR's help calls it an ampoule.

## Used by stanpumpR
- inst/help/drugs/calcium.md:23 and :38, inst/help/scenarios/calcium-salt-substitution.md:10: the calcium chloride
  insert's maximal rate of 100 mg/min (as cited by Ansari 2025). Related, not listed in the context: calcium.md:7
  (10% = 1 g in 10 mL, 6.8 mmol of calcium; dihydrate, 147 g/mol).

## Verification against stanpumpR
Checked at master 0c9788e (worktree stanpumpr-digests-review-490f93).
- **inst/help/drugs/calcium.md:23** "the calcium chloride insert's 100 mg/min": §2.1, PDF p. 3, maximum 1 mL/minute
  (100 mg/minute). **matches**.
- **inst/help/drugs/calcium.md:38** trials infused 1 g over 10 min, "within the package insert's maximal rate of
  100 mg/min": 1 g/10 min = 100 mg/min, exactly at the label maximum. The help also calls a 1-minute push outside the
  data; the label goes further and says not to give it by bolus (§2.1). **matches**.
- **inst/help/scenarios/calcium-salt-substitution.md:10** "the calcium chloride insert 100 mg/min": **matches**.
- **inst/help/drugs/calcium.md:7** (related) calcium chloride as the dihydrate (§11: CaCl2.2H2O), 1 g in 10 mL (§3),
  6.8 mmol of calcium (label: 27 mg = 1.4 mEq elemental Ca per mL, so 270 mg = 14 mEq = 6.7-7.0 mmol per 10 mL;
  stoichiometry 6.80 mmol). **matches**. "Ampoule" vs the label's syringe is immaterial.
- R/drugs_calcium.R:91 uses 147.01 g/mol for CaCl2.2H2O; the label prints no molecular weight but confirms the
  dihydrate and its 1.4 mEq/mL is consistent with 147 g/mol (derived 1.36). **matches**.
