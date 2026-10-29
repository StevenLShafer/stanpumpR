---
file: NJ_label-hydromorphone_019034_2026-01-07.pdf
pmid: none
doi: none
citation: Fresenius Kabi USA. DILAUDID INJECTION and Hydromorphone Hydrochloride Injection / Injection (HPF), for intravenous, intramuscular or subcutaneous use, CII. US prescribing information, NDA 019034, revised 12/2025 (FDA Reference ID 5715763; accessdata.fda.gov 019034s048lbl.pdf, posted 2026-01-07). Two labels in one PDF.
kind: label
drugs: [hydromorphone]
source_read: text layer; read Highlights, sections 2.1-2.5 and 12.2-12.3 of both labels (DILAUDID INJECTION, pp. 1-27; Hydromorphone HCl Injection / HPF, pp. 28-54); skimmed the rest
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# DILAUDID INJECTION label 2025: parenteral hydromorphone dosing and IV disposition

## Study
Prescribing information, not a study. Products: DILAUDID INJECTION (1, 2, 4 mg/mL) and Hydromorphone HCl Injection, including the high-potency formulation (HPF, 10 mg/mL, opioid-tolerant patients only). No PK study design, n or assay given.

Dosing (2.2, both labels identical): subcutaneous or intramuscular, usual starting dose **1 mg to 2 mg every 2 to 3 h** as necessary (may be lowered in opioid-naive patients); intravenous, initial **0.2 mg to 1 mg every 2 to 3 h**, given slowly over at least 2-3 min, reduced to 0.2 mg in elderly or debilitated patients. Conversion from other opioids: convert to an equivalent total daily dose "using publicly available data", then reduce by one-half for incomplete cross-tolerance; no ratio given. Hepatic or renal impairment: start at one-fourth to one-half the usual dose (2.3, 2.4).

## Model and parameters
No compartmental model; no IM or SC pharmacokinetics (no IM tmax, Cmax or bioavailability). Section 12.3 (same text in both labels):
```yaml
parameters:
  - {name: Vss_IV, value: 302.9, unit: L, source: "12.3 Distribution", note: "mean (CV 32%), after IV bolus"}
  - {name: CL_IV, value: 1.96, unit: L/min, source: "12.3 Elimination", note: "mean (CV 20%), 'approximately'"}
  - {name: t_half_IV, value: 2.3, unit: h, source: "12.3 Elimination", note: "terminal, 'about'"}
  - {name: protein_binding, value: "8-19", unit: "%", source: "12.3 Distribution"}
```
>95% of the dose metabolised by hepatic glucuronidation to hydromorphone-3-glucuronide; minor 6-hydroxy reduction metabolites.

## Pharmacodynamics and end points
- MEAC: no value; 12.2 says the minimum effective analgesic concentration varies widely and may rise with tolerance (12.2 of both labels).
- No ke0, t1/2ke0, tPeak or hysteresis. No onset or duration figures for IV versus IM.
- No equianalgesic or potency ratio (conversion advice defers to "publicly available data").

## Covariates
Hepatic (from oral 4 mg study): Cmax and AUCinf about 4-fold higher in moderate (Child-Pugh B) impairment. Renal (oral 4 mg): Cmax and AUC0-inf 2-fold higher in moderate (CLcr 40-60 mL/min) and **4-fold** in severe (CLcr < 30 mL/min) impairment; t1/2 40 h versus 15 h. Geriatric: no effect of age. Sex: females about 25% higher Cmax, comparable AUC0-24.

## Caveats
All special-population data are from oral dosing. The severe-renal exposure ratio is 4-fold here but 3-fold in the DILAUDID oral label (NDA 019892) for what appears to be the same study. No IM/SC kinetics at all.

## Used by stanpumpR
- R/drugs_hydromorphone.R:121 (context :81): IM bioavailability set to 1.0 partly "because the product labelling gives the same dose by either route while oral needs several times more".
- tests/testthat/test-drugs-hydromorphone.R:71: test comment, "the labelling gives the same dose by either route".

## Verification against stanpumpR
- "Same dose by either route" (drugs_hydromorphone.R:121; test-drugs-hydromorphone.R:71): **discrepancy**. The label gives different starting doses: IM/SC 1-2 mg, IV 0.2-1 mg (2.2). The IM/SC range sits at or above the top of the IV range, which neither establishes nor contradicts F_IM = 1; the label reports no IM bioavailability. "Oral needs several times more": the DILAUDID oral label starts tablets at 2-4 mg, about twice the IM/SC dose, not several times (cross-label, derived). The code's own caveat that IM has no direct citation (line 117) is accurate; the label is weak support for F_IM 1.0 and the wording should be softened.
- The model's IM parameters (ka 0.0128/min, 30 min peak, F 1.0): **cannot verify** from this document (no IM PK).
- Context: the label's IV CL 1.96 L/min and Vss 302.9 L differ from the Drover disposition used by the model (CL 1.30 L/min); the code does not cite this label for disposition.
