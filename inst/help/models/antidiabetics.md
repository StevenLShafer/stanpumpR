## What is here

The **Antidiabetic** drugs in the startup menu are named, source-specific population pharmacokinetic models of glucose-lowering drugs. Each reproduces one published analysis of one product, by one route, in the population it was fitted in. **Every one is plotted as a concentration only.** None predicts blood glucose, and none is a dosing algorithm.

| Drug | Product and route | Source | Population | What the curve is |
|---|---|---|---|---|
| [semaglutide](help:drugs/semaglutide) | Ozempic, SC once weekly | Carlsson Petri and colleagues, 2018 | Phase 3 trials, type 2 diabetes | One compartment, apparent; weight, sex and age on clearance |
| [tirzepatide](help:drugs/tirzepatide) | Mounjaro, SC once weekly | Schneck and Urva, 2024 | 5802 participants, 19 studies | Two compartments, true scale; weight and fat mass |
| [dulaglutide](help:drugs/dulaglutide) | Trulicity, SC once weekly | Geiser and colleagues, 2016 | 1067 adults with type 2 diabetes (phase 3) | Two compartments, true scale; weight on bioavailability |
| [exenatide](help:drugs/exenatide) | Byetta, SC twice daily | Cirincione and Mager, 2017 | 8 studies, healthy and type 2 diabetes, normal renal function to end-stage | Two compartments, linear plus saturable elimination, saturable absorption; eGFR and weight. Run by numerical integration |
| [liraglutide](help:drugs/liraglutide) | Victoza, SC once daily | Røge and colleagues, 2015 | 29 adults with type 2 diabetes | One compartment, apparent, per kilogram |
| [liraglutide (Watson)](help:drugs/liraglutideWatson) | Victoza, SC once daily | Watson and colleagues, 2010 | Healthy volunteers and type 2 diabetes, phase 1 | One compartment, apparent, per kilogram; zero-order then first-order absorption |
| [insulin regular](help:drugs/insulinRegular), [regular U-500](help:drugs/insulinRegularU500), [NPH](help:drugs/insulinNPH), [human 70/30](help:drugs/insulinHumanMix70) | Human insulins, SC | Tham and colleagues, 2017 | Clamp and PK studies, mostly healthy | One compartment, apparent, per formulation |
| [insulin lispro](help:drugs/insulinLispro), [lispro Mix75/25](help:drugs/insulinLisproMix75), [lispro Mix50/50](help:drugs/insulinLisproMix50) | Humalog U-100 (also by pump), Humalog Mix, SC | Tham and colleagues, 2017 | Healthy volunteers | One compartment; lispro with parallel zero-order input |
| [insulin glargine](help:drugs/insulinGlargine) | Glargine U-100 (Basaglar fit), SC | Tham and colleagues, 2017 | 209 healthy volunteers | One compartment, parallel zero-order input, immunoreactive assay |
| [metformin](help:drugs/metformin) | Immediate release, oral | Chae and colleagues, 2012 | 42 healthy adults, 500 mg | One compartment, apparent; creatinine clearance |
| [empagliflozin](help:drugs/empagliflozin) | Oral | Baron and colleagues, 2016 | 2761 adults with type 2 diabetes | Two compartments, apparent, lagged; renal function |
| [dapagliflozin](help:drugs/dapagliflozin) | Oral | Kobuchi and colleagues, 2025 | 85 Japanese adults with type 2 diabetes, troughs only | One compartment, apparent; weight. Only the steady-state trough is fitted |
| [ertugliflozin](help:drugs/ertugliflozin) | Oral | Fediuk and colleagues, 2021 | 2276 subjects, 15 studies | Two compartments, apparent, lagged; weight, eGFR, age, sex, type 2 diabetes |
| [pioglitazone](help:drugs/pioglitazone) | Oral | Kadam and colleagues, 2013 | 31 healthy Caucasian adults, 15 mg | Two compartments, apparent; CYP2C8\*1/\*1 (no CYP2C8 field) |
| [pramlintide (intravenous)](help:drugs/pramlintideIV) | Intravenous research route; the product is SC | Fang and colleagues, 2013 | 25 men with type 1 diabetes | Two compartments, true scale |
| [linagliptin](help:drugs/linagliptin) | Tradjenta, oral | Retlich and colleagues, 2015 | 462 adults with type 2 diabetes | Two compartments, apparent, with equilibrium binding to DPP-4; total concentration. Run by numerical integration |
| [vildagliptin](help:drugs/vildagliptin) | Galvus (outside the US), oral | Landersdorfer, He and Jusko, 2012 | 13 adults with type 2 diabetes | Two compartments with slow binding to DPP-4 and hydrolysis by it. Run by numerical integration |
| [glyburide](help:drugs/glyburide) | Glibenclamide, oral | Rambiritch and colleagues, 2016 | 24 adults with type 2 diabetes | Two compartments, apparent |

The weekly peptides (semaglutide, dulaglutide, tirzepatide) offer `mg SC qwk`, and exenatide `mcg SC bid`, plotted in pg/mL; the insulins are dosed in units (`U SC`, `U/kg SC`) and plotted in mcU/mL; insulin lispro alone also offers an insulin pump (`U/hr SC`). See [subcutaneous doses](help:models/absorption).

## A product is not a molecule

The key of each model is the **product, formulation and route**, not the ingredient. Exenatide is the immediate-release injection (Byetta); the extended-release microspheres (Bydureon) and BCise suspension release it over weeks and are not modelled. The oral semaglutide tablets (Rybelsus, and the newer tablets the 2026 label says are not interchangeable mg for mg with them) are not the injection. Insulin U-100 and U-500 are absorbed at different rates. The glargine model was fitted to Basaglar on an immunoreactive assay and is not a model of Lantus, Semglee, Rezvoglar or glargine U-300 (Toujeo), whose depot behaves differently. Humalog U-200 was a validation set only and is not offered, and neither is its pump use, which its label forbids. Biosimilars, other insulins (aspart, glulisine, degludec, detemir, icodec), the fixed-ratio pens (Soliqua, Xultophy) and every product not listed above are kept in a registry with their evidence status, not offered as a curve borrowed from a related product.

## Concentration, not glucose

Glucose lowering is not a function of the plasma concentration with a delay that a ke0 could describe, so there is no effect site, no band and no *Time until threshold*. The published pharmacodynamic models belong to endpoints of their own, each with its own population and time scale, and none is drawn:

- **HbA1c and fasting glucose** over weeks to months: semaglutide (exposure-response at week 26 or 30), tirzepatide and dulaglutide (FDA disease models), empagliflozin, dapagliflozin and canagliflozin.
- **The meal glucose-insulin system** in type 2 diabetes for liraglutide (Røge 2015), and a published type 1 diabetes glucose model for lispro (Hovorka 2004).
- **The single-dose glucose response** to metformin in healthy adults (Chae 2012).
- **Urinary glucose excretion** for the SGLT2 inhibitors, **DPP-4 inhibition** for the gliptins, and **the glucose infusion rate of a euglycaemic clamp** for the insulins. A clamp's glucose infusion rate is what it takes to hold glucose constant, not the patient's glucose.

Adding the effects of two drugs, or of exogenous and endogenous insulin, is not supported by any of them.

## What each curve leaves out

- **Covariates not applied.** Tham's endogenous-insulin baseline, empagliflozin's smoking, race, protein and liver-enzyme terms, and ertugliflozin's race and food effects. Each drug's page lists what is applied and what is not.
- **Insulin is the exogenous insulin alone.** Endogenous secretion, the baseline Tham estimated, and the C-peptide correction are not added, and an insulin unit is never converted into another insulin's.

## The registry

`inst/extdata/antidiabeticRegistry.csv` lists every product the specification named, implemented or not, with its pharmacokinetic and pharmacodynamic evidence status (PK-FIT, PK-AUDIT, PK-CONSTRAINT, PK-TEMPLATE and the PD equivalents), its route permissions where the product's label decides them, and its source. `inst/extdata/antidiabeticParameters.csv` gives every number of the implemented models with its units, whether it was estimated, fixed or derived, the population and the source. Both are rebuilt by `data-raw/antidiabeticRegistry.R`. A product stays registry-only until a complete source model has been read and checked; borrowing a related product's parameters is not allowed.
