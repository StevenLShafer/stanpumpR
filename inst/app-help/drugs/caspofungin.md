### The model

stanpumpR draws caspofungin with Xu and colleagues' model of critically ill children (Xu 2026, *Antimicrob Agents Chemother* 70:e01277-25). They studied 29 children in a paediatric intensive care unit, aged 4 months to 16 years and weighing 4.9 to 74 kg. Four of the children were on ECMO. All were given CANCIDAS as a one-hour infusion each day. The model has two compartments, and every parameter is scaled by body-surface area (Mosteller) about a child of 0.79 m². Clearance and intercompartmental clearance scale with BSA to the power 0.66, and both volumes in proportion to BSA. The paper fixes those exponents for "clearance" and "volume" but does not say whether Q and V2 take them. stanpumpR scales Q as a clearance and V2 as a volume, which is an assumption. Steven Shafer chose this model on 28 November 2026, over Yang 2022's step covariates.

Doses are in mg of caspofungin, as CANCIDAS is labelled: a 50 mg vial holds caspofungin acetate equivalent to 50 mg of caspofungin. The label forbids an intravenous bolus, so a dose is entered as a rate and stopped after an hour. Children are dosed per m² (for example 50 mg/m²/hr from 0 to 60 minutes) and adults as a total (50 mg/hr from 0 to 60 minutes).

**Not applied.** Xu found ECMO enlarged the central volume (a coefficient of 18.2, in a form the paper does not state, from four children). The Patient Profile has no ECMO field, so the curve is that of a child not on ECMO.

### Comparison with the label

The model agrees with Xu's own estimates of its patients' exposure: their median AUC over a day without ECMO was 194 mg·h/L, and the model gives 204 at 50 mg/m² in a 6-year-old of 0.82 m². It does **not** agree with the CANCIDAS label's Table 8, whose patients were not in intensive care:

| | Label AUC (mg·h/L) | Model AUC | Label clearance (mL/min) | Model clearance |
|---|---|---|---|---|
| 12-17 years, 50 mg/m² | 124.9 | 250 | 12.6 | 5.0 |
| 2-11 years, 50 mg/m² | 120.0 | 204 | 6.4 | 3.4 |
| 3-23 months, 50 mg/m² | 131.2 | 167 | 3.2 | 2.3 |
| Adults, 50 mg | 87.3 | 147 | 10.6 | 5.7 |

The model's patients cleared caspofungin about half as fast as the label's, so the curve runs 1.3 to 2 times higher than the label's patients would. Xu found its BSA-normalised clearance higher than three earlier paediatric models (0.248 against a median of 0.208 L/h/m²). The gap is therefore between the label's studies and the population models, not a misreading of this one.

**Adults.** Xu studied no adults. Because this is the library's only caspofungin model, an adult gets it by the nearest-population rule. For adults it is an extrapolation of a paediatric ICU model, and it predicts about 1.7 times the label's adult exposure. Whether adults should keep this model or get an adult one is a question open for Steven Shafer.

### Target concentrations

No band is drawn. Xu judges exposure by target attainment, using the ratio of the daily total AUC to the organism's MIC and a trough concentration, not by a concentration range. The label gives no therapeutic range.

### Pregnancy

No pharmacokinetic study of caspofungin in pregnancy was found. Caspofungin is slowly metabolised by hydrolysis and N-acetylation, and about 1.4% of a dose is excreted unchanged in the urine, so the rise in filtration of pregnancy does not reach it. Nothing is applied.
