---
file: NJ_label-cleviprex_022156_2021-04-19.pdf
pmid: null
doi: null
citation: "CLEVIPREX (clevidipine) injectable emulsion, for intravenous use. FDA prescribing information, NDA 022156 (s007), Chiesi USA; revised 04/2021 (Reference ID 4780628). https://www.accessdata.fda.gov/drugsatfda_docs/label/2021/022156s007lbl.pdf"
kind: label
drugs: [clevidipine]
source_read: "text layer, whole label (16 PDF pages); rendered page images of PDF p2 (sections 1-2.2) and p9 (12.2 Pharmacodynamics, 12.3 Pharmacokinetics start) read to confirm numbers"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Cleviprex label 2021: clevidipine prescribing information (NDA 022156)

## Study
FDA label, not a study. Revised 04/2021; initial US approval 2008. Dihydropyridine L-type calcium channel blocker,
0.5 mg/mL clevidipine in a lipid emulsion (soybean oil 200 mg/mL; ~0.2 g lipid/mL, 2.0 kcal/mL; section 5.3, 11),
racemic, MW 456.3 g/mol, enantiomers equipotent (section 11). Indicated to lower blood pressure when oral therapy is
not feasible or not desirable.

Clinical database (section 6.1, PDF p4-5): 19 studies, 99 healthy subjects and 1307 hypertensive patients
(1406 exposures); 1099 perioperative, 126 severe, 82 essential hypertension. Pivotal trials (section 14): ESCAPE-1
(n=105, pre-op) and ESCAPE-2 (n=110, post-op), placebo-controlled; ECLIPSE (three open-label trials, 1512 randomized,
752 on clevidipine, mean 8 h at 4.5 mg/h) vs nitroglycerin, nitroprusside, nicardipine; VELOCITY (n=126 severe
hypertension, open-label, mean rate 9.5 mg/h, mean exposure 21 h); a 72-h essential hypertension study (n=61,
placebo or 2, 4, 8, 16 mg/h).

### Dosing (section 2.2, PDF p2; confirmed on rendered page)
- Initial: 1-2 mg/h IV infusion.
- Titration: may double at 90-s intervals initially; near goal, less than doubling, every 5-10 min. ~1-2 mg/h
  increase gives an additional 2-4 mmHg fall in systolic pressure.
- Maintenance: most patients respond at 4-6 mg/h; severe hypertension may need up to 32 mg/h (limited experience).
- Maximum: most patients 16 mg/h or less; limited short-term experience to 32 mg/h. Lipid load: no more than
  1000 mL or an average of 21 mg/h per 24 h. Little experience beyond 72 h.
- Highest dose given in trials: 106 mg/h, or 1153 mg total (section 10, text layer).
- Monitor for rebound hypertension at least 8 h after stopping a prolonged infusion (2.1, 5.6).

## Model and parameters
No compartmental model is given. Label PK statements (section 12.3, PDF p9-10; p9 confirmed on rendered page,
p10 text layer only):
```yaml
parameters:
  - {name: t_half_initial, value: 1, unit: min, source: "12.3, PDF p9", note: "approximate; arterial blood; accounts for 85-90% of elimination"}
  - {name: t_half_terminal, value: 15, unit: min, source: "12.3, PDF p9", note: approximate}
  - {name: Vss, value: 0.17, unit: L/kg, source: "12.3, PDF p9", note: "steady-state volume, arterial blood"}
  - {name: protein_binding, value: ">99.5", unit: "%", source: "12.3, PDF p9", note: "plasma, 37 C"}
  - {name: CL_metabolite, value: 0.03, unit: L/h/kg, source: "12.3, PDF p10 (text layer)", note: "primary dihydropyridine (carboxylic acid) metabolite, inactive"}
  - {name: t_half_metabolite, value: 9, unit: h, source: "12.3, PDF p10 (text layer)", note: "terminal, approximate"}
```
No clearance of the parent drug is stated. Metabolism by blood and tissue esterases (ester hydrolysis) to the inactive
carboxylic acid and formaldehyde; elimination unlikely to depend on hepatic or renal function. Radiolabel: 83%
recovered, 63-74% urine, 7-22% feces, >90% of recovered within 72 h. No CYP inhibition/induction in vitro.

## Pharmacodynamics and end points
End point: systolic blood pressure (section 12.2, PDF p9, confirmed; also 14.3).
- Emax: effect plateaus at about 25% of baseline systolic pressure.
- ED50 (infusion rate, not a concentration): about 10 mg/h gives half the maximal effect.
- Onset: 4-5% systolic fall within 2-4 min of a 0.4 mcg/kg/min infusion (stated as ~1-2 mg/h), perioperative patients.
- Offset: full BP recovery in 5-15 min in most patients after stopping.
- No evidence of tolerance or hysteresis in infusions up to 72 h. No ke0, t½ke0 or tPeak reported.
- No therapeutic or toxic blood concentration is stated anywhere in the label. Section 12.2 mentions
  "therapeutic and supratherapeutic concentrations (approximately 2.8 times steady-state)" for a QT study without
  giving any number.
- Not an opioid; no MEAC or potency ratio applies.

## Covariates
None quantified. Geriatric (8.5): 620 of 1406 subjects ≥65 y, 232 ≥75 y; no overall differences; start low.
Pediatric (<18 y) not established. Hepatic/renal impairment: not expected to matter (esterase metabolism), no data given.

## Caveats
- Section 3 lists only 50 mL and 100 mL vials, while the Highlights list 50, 100 and 250 mL; dosing is unaffected.
- The dose-response (Emax 25%, ED50 10 mg/h) is in infusion-rate terms from a 61-patient study; the label gives no
  EC50 in concentration units.
- PK half-lives and Vss are in arterial whole blood, not plasma.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:118` — registry source for the clevidipine row (cited as "CLEVIPREX label
  (rev. 04/2021) gives none"), supporting `band_status = none_established`.

## Verification against stanpumpR
- `inst/extdata/therapeuticRanges.csv:118`: row has analyte clevidipine, matrix whole blood, lower/upper/typical 0,
  units ng/mL, `band_status` none_established, and source text saying the drug is dosed to blood pressure and the
  label gives no range. Label: titrated to blood pressure (2.2); no therapeutic, target or toxic concentration
  appears in any section (read whole text layer; 12.2/12.3 confirmed on PDF p9). Whole-blood matrix is consistent
  with the label's arterial-blood PK. Revision date 04/2021 matches the archived label. **matches**.
- The zeros in lower/upper/typical are placeholders for "none established", not label values; no unit conversion
  is involved.
