---
file: NJ_label-acetaminophen_022450_2018-10-18.pdf
pmid: none
doi: none
citation: OFIRMEV (acetaminophen) injection, US prescribing information, NDA 022450 (Mallinckrodt Hospital Products), revised 03/2018, Drugs@FDA posting 2018-10-18 (Reference ID 4245258). https://www.accessdata.fda.gov/drugsatfda_docs/label/2018/022450s011lbl.pdf
kind: label
drugs: [acetaminophen]
source_read: text layer, 16 pages; read Highlights, Boxed Warning, 2 Dosage and Administration, 5.1, 8.4-8.7, 10 Overdosage, 12 Clinical Pharmacology (Table 5 re-read with pypdf because the layout text scrambles its CL/Vss columns)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# OFIRMEV label 2018: intravenous acetaminophen

## Study
Product label for IV acetaminophen 10 mg/mL (1000 mg/100 mL), given as a 15-minute infusion. PK section: patients
and healthy subjects up to 60 years; dose proportional over single doses of 500, 650 and 1000 mg (12.3).

## Model and parameters
No compartmental model. Section 2 dosing (Tables 1-3):
- Adults and adolescents >= 50 kg: 1000 mg q6h or 650 mg q4h; max single dose 1000 mg; minimum interval 4 h;
  **maximum daily dose of acetaminophen 4000 mg per day, all routes and all products** (2.2, Table 1).
- Adults/adolescents < 50 kg and children 2-12 y: 15 mg/kg q6h or 12.5 mg/kg q4h; max single 15 mg/kg (up to
  750 mg); max 75 mg/kg/day (up to 3750 mg) (Tables 1, 2).
- Fever, neonates (to 28 d, >= 32 wk GA): 12.5 mg/kg q6h, max 50 mg/kg/day; infants 29 d-2 y: 15 mg/kg q6h,
  max 60 mg/kg/day (Table 3).
- No conversion needed between oral and IV in adults >= 50 kg (2.1). Severe renal impairment (CrCl <= 30 mL/min):
  longer intervals and lower daily dose "may be warranted" (8.7).

Table 5 (single IV dose: 15 mg/kg children/adolescents, 1000 mg adults; mean (SD)):
```yaml
parameters:
  - {name: AUC0-6h, value: [38, 41, 43], unit: ug*h/mL, source: "Table 5", note: "children, adolescents, adults; SD 8, 7, 11"}
  - {name: Cmax, value: [29, 31, 28], unit: ug/mL, source: "Table 5", note: "SD 7, 9, 21; at end of 15-min infusion"}
  - {name: t_half, value: [3.0, 2.9, 2.4], unit: h, source: "Table 5", note: "SD 1.5, 0.7, 0.6"}
  - {name: CL, value: [0.34, 0.29, 0.27], unit: L/h/kg, source: "Table 5", note: "SD 0.10, 0.08, 0.08"}
  - {name: Vss, value: [1.2, 1.1, 0.8], unit: L/kg, source: "Table 5", note: "SD 0.3, 0.3, 0.2"}
  - {name: protein_binding, value: "10-25", unit: "%", source: "12.3"}
```
IV Cmax is up to 70% higher than the same oral dose, AUC very similar (12.3). Under 5% excreted unchanged in
urine; over 90% of the dose excreted in 24 h. Metabolism: glucuronidation, sulfation, CYP2E1 to NAPQI.
Derived: adult CL 0.27 L/h/kg x 70 kg = 18.9 L/h.

## Pharmacodynamics and end points
No concentration-effect data, no ke0, no therapeutic range. Toxic concentrations (10 Overdosage): plasma > 300
mcg/mL at 4 h after oral ingestion associated with hepatic damage in 90%; minimal damage expected if < 150 mcg/mL at
4 h or < 37.5 mcg/mL at 12 h; Rumack-Matthew lower treatment line 150 mcg/mL at 4 h, 37.5 mcg/mL at 12 h. Not an
opioid; no MEAC or potency ratio.

## Covariates
Age band and weight (>= 50 kg) define dosing; hepatic impairment, alcoholism, malnutrition, hypovolemia and severe
renal impairment are cautions.

## Caveats
IV product label; oral bioavailability is not given. "This label may not be the latest approved by FDA" on every
page. PK studied only up to 60 years.

## Used by stanpumpR
- R/help-scenarios.R:315 (now :362 at 7bc2719): 'acetaminophen-arthritis-qid' scenario, "The labelled maximum of
  4 g a day"; 1000 mg PO qid.
- (also) inst/help/scenarios/acetaminophen-arthritis-qid.md:3: "Four grams a day is the labelled maximum for adults."

## Verification against stanpumpR
- R/help-scenarios.R:362 and the scenario page: 1000 mg q6h = 4 g/day as the adult labelled maximum. Label 2.2 and
  Table 1: 1000 mg q6h, max 4000 mg/day for adults >= 50 kg, by all routes. **matches** (the label is the IV
  product, but its 4 g ceiling explicitly covers oral dosing; the repo names no label, so this one is a fair
  source). The scenario's "for adults" omits the >= 50 kg condition (75 mg/kg/day below 50 kg); minor.
- The page's tid "lower ceiling often used for older patients" (3 g/day) is not in this label: **cannot verify**
  from it.
- Not a cited location, for context: the page's 2.3 h half-life is close to the label's adult 2.4 h; its 23.9 L/h
  clearance is above the label's 0.27 L/h/kg (18.9 L/h at 70 kg, derived) and comes from the drug's own model, not
  this label.
