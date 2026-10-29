---
file: NJ_label-naloxone10mg_215457_2022-02.pdf
pmid: none
doi: none
citation: "kaleo, Inc. NALOXONE HYDROCHLORIDE injection 10 mg/0.4 mL, single-dose pre-filled auto-injector, for intramuscular or subcutaneous use: full prescribing information and Instructions for Use. NDA 215457, original approval (FDA file 215457s000lbl.pdf), Revised 02/2022, FDA Reference ID 4944757. Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [naloxone]
source_read: "text layer, all 16 PDF pages (printed pages 1-10 of the prescribing information are PDF pp1-10; Instructions for Use printed 1-6 are PDF pp11-16): Highlights and contents p1; 1-2 Indications and Dosage and Administration pp2-3; 3-4 p3; 5-6 pp4-5; 8 Use in Specific Populations pp5-6; 11 Description pp6-7; 12.1-12.3 pp7-9 (Table 1 and Figure 1 pp8-9); 13 and 16 p10; Instructions for Use pp11-16. Table 1 and Figure 1 read on page images of pp8-9 (the text layer scrambles the table)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: not-applicable
---

# FDA 2022: naloxone 10 mg auto-injector prescribing information (NDA 215457)

## Study
Not a study. A 10 mg/0.4 mL naloxone HCl auto-injector (equivalent to 9.0 mg naloxone base; 25 mg/mL) for
intramuscular or subcutaneous injection into the anterolateral thigh, through clothing or MOPP4 protective equipment
if needed (2.1-2.2, 3, 11; pp2-3, 7). Indication (1, p2): **military personnel and chemical incident responders**, for
emergency treatment of patients 12 years and older where a high-potency opioid such as a fentanyl analogue is
suspected as a chemical weapon, and for temporary prophylaxis before entering a contaminated area. Further naloxone may
be given for relapse; the device is single-use.

Pharmacokinetic study (12.3, pp7-8): crossover, **24 fasted healthy subjects**, a single 2 mg (0.4 mL of 5 mg/mL)
and a single 10 mg (0.4 mL of 25 mg/mL) auto-injector dose into the anterolateral thigh; plasma naloxone sampled to
12 h. Assay not stated. Adverse-reaction base (6.1, p4): three PK studies, 78 healthy adults, given 0.4 mg, two 0.4 mg,
2 mg or 10 mg auto-injectors.

## Model and parameters
No compartmental model. Table 1 (p8, page image), mean (%CV); Tmax median (minimum, maximum):

| Parameter | 2 mg (N = 24) | 10 mg (N = 24) |
|---|---|---|
| Tmax (h) | 0.17 (0.08, 0.67) | 0.26 (0.09, 0.67) |
| Cmax (ng/mL) | 6.95 (44.8) | 42.0 (49.9) |
| AUC0-t (ng.h/mL) | 9.22 (14.6) | 51.1 (17.9) |
| AUC0-inf (ng.h/mL) | 9.25 (14.6) | 51.3 (17.8) |
| T1/2 (h) | 1.46 (11.1) | 1.46 (14.1) |

```yaml
parameters:
  - {name: Cmax_10mg_IM_SC, value: 42.0, unit: ng/mL, source: "Table 1, p8", note: "mean (CV 49.9%), N = 24"}
  - {name: AUCinf_10mg_IM_SC, value: 51.3, unit: ng.h/mL, source: "Table 1, p8", note: "mean (CV 17.8%)"}
  - {name: Tmax_10mg_IM_SC, value: 15.6, unit: min, source: "Absorption, p9; Table 1 p8", note: "median, range 5.4-40.2 min (0.26 h, 0.09-0.67 h)"}
  - {name: t_half_10mg, value: 1.46, unit: h, source: "Table 1 p8; Elimination p9", note: "CV 14.1%"}
  - {name: Cmax_2mg_IM_SC, value: 6.95, unit: ng/mL, source: "Table 1, p8", note: "mean (CV 44.8%)"}
  - {name: AUCinf_2mg_IM_SC, value: 9.25, unit: ng.h/mL, source: "Table 1, p8", note: "mean (CV 14.6%)"}
  - {name: t_half_2mg, value: 1.46, unit: h, source: "Table 1, p8", note: "CV 11.1%"}
  - {name: CL_F_10mg, value: 195, unit: L/h, source: "derived", note: "(derived: 10 mg HCl / 51.3 ng.h/mL; 2 mg / 9.25 = 216 L/h)"}
```
The label calls the rise from 2 to 10 mg slightly greater than 5-fold for Cmax and AUC (p7) (derived: AUC0-inf ratio
5.55, Cmax ratio 6.04). Figure 1 (pp8-9): mean (SD) concentrations to 8 h; the 10 mg mean curve peaks near 35 ng/mL at
about 15 min (read from the figure; the tabulated mean of individual Cmax is 42.0). No absolute bioavailability (no
intravenous arm), and the device may deliver IM or SC. Distribution, metabolism and excretion text (p9) is the class
wording also in the NARCAN label: weak protein binding, glucuronidation, 25-40% urinary metabolites in 6 h.

## Pharmacodynamics and end points
No MEAC, ke0, tPeak or potency ratio (an antagonist). Section 12.2 (p7): IV onset generally within 2 minutes; duration
depends on dose and route. The dose was chosen by modelling: simulations from healthy-volunteer data showed a single
10 mg dose could improve ventilation depressed by morphine, buprenorphine, fentanyl and carfentanil, with reversal
depending on the opioid, its amount, the naloxone amount and timing. No model, parameters or end-point values are
printed.

## Covariates
- Pediatric (8.4, p6): established from 12 years by adult bioavailability and PK/PD simulation; no pediatric studies;
  not established below 12. IM or SC absorption in children may be erratic or delayed.
- Geriatric (8.5, p6): exposure can be higher with organ impairment; no numbers.
- Pregnancy (8.1, p5): do not withhold for overdose; animal margins 4 and 8 times a 10 mg dose in a 50 kg human
  (mg/m2). Lactation (8.2): minimally orally available.
- Warnings (5.1, p4): safety in opioid-dependent adults not established; a lower-dose product may be preferable in
  known dependence.

## Caveats
- Healthy fasted volunteers only; route mixes IM and SC; no assay or sampling schedule printed.
- Cmax CV near 50% at both doses.

## Used by stanpumpR
Not cited. The context file lists no locations; a search of origin/master (R, inst, docs, tests) for
auto-injector, 215457, 10 mg/0.4 mL, Zimhi, kaleo and chemical-incident wording found none.

## Verification against stanpumpR
**Not applicable**: nothing in the repo relies on this document. It could serve as an intramuscular exposure check for
the naloxone model at 2 and 10 mg (healthy adults; AUC0-inf 9.25 and 51.3 ng.h/mL, dose as the hydrochloride), but no
such check exists on master.
