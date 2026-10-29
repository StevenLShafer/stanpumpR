---
file: NJ_web-ema-827952.pdf
pmid: none
doi: none
citation: "European Medicines Agency, CHMP. CHMP assessment report: Vipidia (alogliptin). Procedure No. EMEA/H/C/002182/0000. EMA/CHMP/207780/2013, 25 July 2013. https://www.ema.europa.eu/en/documents/assessment-report/vipidia-epar-public-assessment-report_en.pdf"
kind: review
drugs: [alogliptin]
source_read: "text layer, partial: cover and contents (PDF pp 1-4), non-clinical pharmacokinetics in pregnant animals, placental transfer and milk (pp 18-19) and non-clinical discussion (p 28), clinical introduction and study overview Tables 1-2 (pp 30-32), 2.4.2 Pharmacokinetics (pp 32-34), 2.4.3 Pharmacodynamics (pp 35-36), 2.4.4-2.4.5 discussion and conclusions on clinical pharmacology (pp 36-38), dose-response study 003 (p 38-39), RMP missing-information table on pregnancy (pp 88-89), renal-dose passages in the efficacy discussion (p 71) and benefit-risk (pp 90-95, renal p 91). Prose pages; no PK table to render. Text matches page 1 (Vipidia, 96 pages)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# EMA 2013: Vipidia (alogliptin) CHMP assessment report

## Study
The European public assessment report for alogliptin (as the benzoate), tablets 6.25, 12.5 and 25 mg once daily, for
type 2 diabetes. It is the EU document. The US product, Nesina, has the same ingredient but is not covered here.
The clinical pharmacology programme (Table 1, PDF p 31) covers ADME (014), absolute bioavailability (103),
bioequivalence (027), single and multiple ascending dose studies in the US and Japan (001, CPH-001, CPH-002, 002 in
T2DM), age/race/sex (022, CPH-003), renal (006) and hepatic (023) impairment, many interaction studies, and a
population PK report (008, phase III T2DM). The report gives only summary results from these studies, with no
parameter tables.

## Model and parameters
All values are from 2.4.2 (PDF pp 32-33) unless noted. They are summary statements, not a fitted model:
```yaml
parameters:
  - {name: tmax, value: "1-2", unit: h, source: "p 32", note: "median, single and multiple doses"}
  - {name: F, value: "close to 100%", unit: "", source: "p 32", note: "absolute bioavailability (study 103); food has no effect"}
  - {name: protein binding, value: 20, unit: "%", source: "p 32", note: "approximate; unaffected by renal impairment; M-I 12-32%"}
  - {name: Vz, value: 417, unit: L, source: "p 32", note: "after 12.5 mg IV"}
  - {name: Vz/F, value: 300, unit: L, source: "p 32", note: "steady state, 25 mg qd x 14 d, T2DM"}
  - {name: CL, value: 14, unit: L/h, source: "p 33", note: "after 12.5 mg IV"}
  - {name: CL/F, value: "15-20", unit: L/h, source: "p 33", note: range}
  - {name: urinary recovery, value: 76, unit: "% of oral radioactivity", source: "p 33", note: "total urine + faeces 88.5%; 95% of urinary radioactivity unchanged"}
  - {name: accumulation, value: 1.4, unit: ratio, source: "p 33", note: "steady state after 7 days"}
  - {name: dose proportionality, value: "6.25-800", unit: mg, source: "p 33", note: ""}
  - {name: between-subject variability, value: "17-31", unit: "% (Cmax, AUC)", source: "p 33", note: "within-subject <23%"}
  - {name: AUC increase in renal impairment, value: "71, 112, 251, 377", unit: "% vs healthy", source: "p 33", note: "mild, moderate, severe, ESRD; Cmax +13, 42, 27, 32%"}
  - {name: AUC increase with age or race, value: 30, unit: "%", source: "p 33", note: "about, after multiple doses; not clinically relevant"}
```
No terminal half-life is given in the clinical section. The IV values give Vz/CL x ln 2 = 417/14 x 0.693 = 20.6 h
(derived, not stated in the report). The metabolites are M-I (N-demethylated, CYP2D6, active, <1% of parent
exposure) and M-II (N-acetylated, inactive, <6%) (p 33). The report says a two-compartment, first-order absorption
NONMEM model was fitted (p 36) but gives none of its parameters. The CHMP asked for an updated population PK analysis
(pooled 002, 006, 008, with CrCl and weight). It accepted that body weight has no clinically relevant effect, but the
report prints no parameter values from that analysis either (p 37).

## Pharmacodynamics and end points
The end point is plasma DPP-4 inhibition (pp 35-36):
- PK/Emax model: Emax 96.2% and EC50 3.73 ng/mL in healthy subjects (study 001). Emax 98.9% and EC50 6.55 ng/mL in
  T2DM (study 002). EC80 is about 30 ng/mL in T2DM, which the report says fits the 25 mg dose (p 36).
- The report cites the literature for the view that 80% or more inhibition is needed for the best glucose lowering
  (p 35).
- After single doses in healthy subjects, Emax was above 93% at 25-800 mg (001) and above 88% at 6.25-200 mg
  (CPH-001). E24/E72 were 74.3/47.5% at 25 mg and 97.0/83.0% at 800 mg (001); in CPH-001 they were 64.7/27.8% at
  6.25 mg and 94.2/74.4% at 200 mg.
- After multiple doses in T2DM (002), E24 was 81.8-96.7% on day 14.
- Study 022: 24 h inhibition was 76-80% across age, sex and race groups.
- Dose-response study 003 (12 weeks, n = 265): HbA1c fell significantly at 12.5 mg and above, with no extra benefit
  above 25 mg (Table 3, p 38).
- QTc was unaffected at 50 and 400 mg (p 36).

## Covariates
The only covariate that changes dosing is renal function. The doses are 25 mg for normal function or mild
impairment, 12.5 mg for moderate impairment, and 6.25 mg for severe impairment or ESRD. These are proportional to
the exposure increases in study 006 (pp 38-39; efficacy discussion p 71; benefit-risk p 91). Moderate hepatic impairment needs no change;
severe hepatic impairment was not studied (p 33). Sex and weight have no clinically relevant effect.

## Caveats
- Every clinical PK number is a summary statement. No population model parameters, fixed or random, are printed, so
  no curve can be built from this document alone.
- Pregnancy: there are no human data. Pregnant or breastfeeding women are listed as missing information in the RMP
  (pp 88-89), and the SmPC states the absence of data.
- Animal data: in pregnant rats and rabbits, exposure differed between early and late gestation, which the report
  attributes to a larger distribution volume and to elimination changes (p 18). In rats, maternal-to-fetal transfer
  happened but fetal tissue Cmax was below maternal plasma, and the drug was excreted in milk (pp 18-19).

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:63` (row written by `data-raw/antidiabeticRegistry.R:256-258`): the
  `source_url` of the `alogliptin` row (product Nesina, PO, tier core, jurisdiction US). Spec and review PK status are
  both PK-CONSTRAINT; spec PD is PD-FIT and review PD is PD-AUDIT ("plasma DPP-4 inhibition"); `stanpumpr_drug` NA,
  `structure_verified` FALSE; note "No complete compatible table."

## Verification against stanpumpR
- `antidiabeticRegistry.csv:63`. **matches.** The EPAR has only summary PK values (IV CL 14 L/h, Vz 417 L, CL/F
  15-20 L/h, Vz/F 300 L, F about 100%, tmax 1-2 h). The structural PK model it mentions comes with no parameters, so
  "No complete compatible table" and PK-CONSTRAINT are correct, and not implementing the drug follows the registry's
  rule. PD-AUDIT rather than PD-FIT is also right: Emax and EC50 are printed (96.2%, 3.73 ng/mL healthy; 98.9%,
  6.55 ng/mL T2DM), but the PK model they were fitted with is not.
- One note, not an error: the row names the US product, Nesina, with jurisdiction US, but its source is the EU
  assessment report for Vipidia. The PK is the same ingredient's, but the document is not the US label.
