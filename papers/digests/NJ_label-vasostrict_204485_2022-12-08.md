---
file: NJ_label-vasostrict_204485_2022-12-08.pdf
pmid: null
doi: null
citation: "VASOSTRICT (vasopressin injection) for intravenous use, full prescribing information. Par Pharmaceutical. NDA 204485, revised 12/2022 (FDA label 204485Orig1s027lbl.pdf, 2022-12-08). https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/204485Orig1s027lbl.pdf"
kind: label
drugs: [vasopressin]
source_read: "text layer (all 10 PDF pages: Highlights, sections 1-16); page images rendered and read for PDF pages 2 (2.1-2.2 dosing), 6 (11 Description, 12.1-12.3) and 7 (12.3 elimination, specific populations, interactions)"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: partly-verified
revised: 2026-10-28 (both [decision] questions answered by Shafer and removed; answers under Verification)
---

# Vasostrict label (NDA 204485), rev. 12/2022: vasopressin injection

## Study
Not a study: US prescribing information for synthetic arginine vasopressin (Par Pharmaceutical), Initial US approval 2014, revised 12/2022. Indication: raising blood pressure in adults with vasodilatory shock who stay hypotensive despite fluids and catecholamines (section 1, PDF p2). Efficacy rests on the literature: rises in systolic and mean pressure in 7 septic-shock and 8 post-cardiotomy vasodilatory-shock studies (section 14, PDF p8). No study population, assay or PK model is described.

Dosing (section 2.2, PDF p2, confirmed on page image):
- Starting dose: post-cardiotomy shock 0.03 units/min; septic shock 0.01 units/min.
- Titrate up by 0.005 units/min every 10 to 15 min to target BP. Data are limited above 0.1 units/min (post-cardiotomy) and 0.07 units/min (septic). Highlights give ranges of 0.03-0.1 and 0.01-0.07 units/min.
- Taper: once target BP has held for 8 h without catecholamines, reduce by 0.005 units/min each hour as tolerated.
- Dilution (Table 1, PDF p2): 0.1 units/mL (2.5 mL = 50 units in 500 mL) or 1 unit/mL (5 mL = 100 units in 100 mL) if fluid restricted. Premixed 0.2, 0.4 and 0.6 units/mL in 100 mL.
- Doses are absolute (units/min), not per kg.

Description (section 11, PDF p6): C46H65N15O12S2, MW 1084.23; **one mg is equivalent to 530 units**.

## Model and parameters
No compartmental model. Section 12.3 (PDF p6-7, confirmed on page images) gives:
```yaml
parameters:
  - {name: CL, value: "9 to 25", unit: mL/min/kg, source: "12.3 Elimination, PDF p7", note: "patients with vasodilatory shock at 0.01 to 0.1 units/min; range, no central value"}
  - {name: Vd, value: 140, unit: mL/kg, source: "12.3 Distribution, PDF p6", note: "type of volume (Vc / Vss) not stated; population not stated"}
  - {name: t_half_apparent, value: "<=10", unit: min, source: "12.3 Elimination, PDF p7", note: "at vasodilatory-shock infusion rates"}
  - {name: time_to_steady_state, value: 30, unit: min, source: "12.3, PDF p6", note: "continuous IV infusion"}
  - {name: linearity_range, value: "10 to 200", unit: microU/kg/min, source: "12.3, PDF p6", note: "plasma concentration linear in infusion rate"}
  - {name: fraction_excreted_unchanged_urine, value: 0.06, unit: fraction, source: "12.3 Excretion, PDF p7", note: about 6%}
  - {name: protein_binding, value: none, unit: "", source: "12.3 Distribution, PDF p6", note: "does not appear to bind plasma protein"}
  - {name: potency, value: 530, unit: units/mg, source: "11 Description, PDF p6"}
```
Derived (mine): 9-25 mL/min/kg at 70 kg is 0.63-1.75 L/min; 140 mL/kg at 70 kg is 9.8 L.
Metabolism: serine protease, carboxypeptidase and disulfide oxido-reductase; metabolites not expected to be active.

## Pharmacodynamics and end points
End point: arterial blood pressure (pressor effect), in vasodilatory shock (12.2, PDF p6, confirmed on page image).
- Pressor effect proportional to infusion rate; peak within 15 min; fades within 20 min after the infusion stops. These are stated for infusion; no bolus time course, no ke0, t½ke0, tPeak after a bolus, or EC50.
- No tachyphylaxis or tolerance to the pressor effect.
- Tends to lower heart rate and cardiac output; reduces norepinephrine requirement.
- No therapeutic or toxic plasma concentration range is given; dose is titrated to the lowest one that gives a clinically acceptable response (2.2).
- Overdose: direct effects resolve within minutes of stopping (section 10, PDF p5).
- Interactions (7, 12.3): catecholamines additive; tetraethylammonium raises the pressor effect by 20% in healthy subjects; indomethacin more than doubles time to offset; halothane, morphine, fentanyl, alfentanil, sufentanil do not change endogenous vasopressin exposure.
(Not an opioid: the opioid audit does not apply.)

## Covariates
- Pregnancy (12.3, PDF p7, page image): clearance rises over pregnancy from placental vasopressinase: slightly in the first trimester, about 4-fold by the third trimester, up to 5-fold at term; back to baseline within 2 weeks of delivery. 8.1 says the dose may need increasing.
- Pediatrics: safety and effectiveness not established. Geriatrics: no differences identified; start low.
- No weight, renal or hepatic covariate model.

## Caveats
- A label: PK statements carry no study size, assay, variability or citation.
- The clearance is a range in shock patients, normalised to kg; the 140 mL/kg volume has no stated population or type.
- The 15-minute peak is an infusion statement and cannot give a bolus tPeak.
- The potency of 530 units/mg is this product's; older literature (and stanpumpR) uses about 400 U/mg for pure AVP.

## Used by stanpumpR
- R/drugs_vasopressin.R:35 — corroborates the Fabian clearance (9-25 mL/min/kg in vasodilatory shock).
- R/drugs_vasopressin.R:79 (the passage is at :81-87, the label sentence at :84) — the label's peak within 15 min refers to an infusion, which is why the 3-min tPeak is provisional.
- inst/help/drugs/vasopressin.md:5 — the same clearance corroboration.
- inst/extdata/therapeuticRanges.csv:198 (the vasopressin row is at line 199 in the snapshot) — registry source: no therapeutic concentration range.

## Verification against stanpumpR
Checked against `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- **R/drugs_vasopressin.R:35-38**: the code says the label gives 9 to 25 mL/min/kg in vasodilatory shock, bracketing Fabian's 1.0 L/min (14 mL/min/kg at 70 kg). The label (PDF p7, image) says 9 to 25 mL/min/kg at 0.01-0.1 units/min in vasodilatory shock. 1.0 L/min / 70 kg = 14.3 mL/min/kg (derived), inside the range. Units are correct (per kg, per min, mL). **matches**.
- **R/drugs_vasopressin.R:81-87** (context pointer :79): the code says no published bolus tPeak was found, and that the label's 15 minutes is for an infusion. In the label (12.2, PDF p6, image), the peak-within-15-min sentence sits in the paragraph about infusion (effect proportional to infusion rate; fades after stopping the infusion). There is no bolus data. The 3-min tPeak (`VASOPRESSIN_TPEAK`, :92) is not from the label, and the code says so. **matches**.
- **inst/help/drugs/vasopressin.md:5**: 9 to 25 mL/min/kg, 14 mL/min/kg at 70 kg. **matches**.
- **inst/extdata/therapeuticRanges.csv:199** (context said :198): lower/upper/typical 0, units mcU/mL, band_status none_established, "no therapeutic concentration range; titrated to effect (label read: VASOSTRICT, rev. 12/2022)". The label gives no concentration range and says to titrate to the lowest dose with an acceptable response (2.2). Its revision date is 12/2022 (Highlights; "R12/2022" PDF p9). **matches**.
- Cross-checks outside the cited lines:
  - Pregnancy factor `VASOPRESSIN_PREGNANCY_CL` = 3.4/0.67 = 5.07 (:94, from Davison 1993). The label says about 4-fold in the third trimester and up to 5-fold at term, so it is consistent at the top of the label's range.
  - The label's 140 mL/kg (9.8 L at 70 kg) and t½ ≤10 min are close to Fabian's V 8.5 L and t½ 5.6 min.
  - **Potency conflict, unresolved**: the label states 530 units/mg (PDF p6). stanpumpR's pg/mL conversion uses 400 U/mg (R/drugs_vasopressin.R:41, :50-51, :130; help :5, :9). At 530 U/mg, 1 mcU/mL = 1.89 pg/mL (derived). The model itself works in units, so the simulation is unaffected; only the displayed conversion and the Landry comparison change. See open_questions.
  - **Binding**: the label says no apparent plasma protein binding. The code header (:52) says about 30% is bound, citing Fabian. The plotted total is unaffected. See open_questions.

Overall **partly-verified**: every claim the repo draws from the label matches. Two side statements in the same code (the potency conversion and binding) conflict with the label and are left to Shafer.
- **Decisions (Shafer, 2026-10-28).** (1) The potency is the label's 530 units per mg (section 11), not Davison's 400 U/mg standard. In R/drugs_vasopressin.R, its help page and test, 1 mcU/mL becomes 1.89 pg/mL. The plotted curve is in mcU/mL and does not change, but the Landry 1997 comparison does: 22 pg/mL with the baseline against about 30, 27% low, a value Davison's 0.67 L/min clearance would reproduce (31). (2) Vasopressin does not appear to bind plasma protein (section 12.3; also the HealthFirst relabel of NDA 204485, revised 2/2022, that Shafer supplied). The header's 'about 30% bound (Fabian)' is replaced. On branch `claude/digest-review-decisions`, not yet merged.
