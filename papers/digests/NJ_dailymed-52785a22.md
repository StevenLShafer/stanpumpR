---
file: NJ_dailymed-52785a22.pdf
pmid: null
doi: null
citation: "Carvedilol tablets USP, 25 mg, film coated (generic). DailyMed SPL setid 52785a22-ddca-4f5b-8579-5974df2a9a4e, labeler Proficient Rx LP (repackager); made by Piramal Pharma for Teva Pharmaceuticals USA, ANDA076373; prescribing information revised 9/2024, SPL revised 12/2025; downloaded 2026-10-21."
kind: label
drugs: [carvedilol]
source_read: "text layer, 34 pages: Highlights and contents (PDF p1-3), 2 Dosage and Administration (p4-5), 8 Use in Specific Populations (p15-17), 10 Overdosage and 11 Description (p17-18), 12.1-12.5 Clinical Pharmacology (p18-22), 16 How Supplied and 17 (p28-29), package label and SPL product data (p33-34). Page 1 rendered to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Carvedilol tablets (generic, Teva/Proficient Rx) DailyMed label

The document is a generic carvedilol immediate-release tablet label, not the Coreg reference label: 25 mg tablets
only (debossed TV/7296), repackaged by Proficient Rx LP (NDC 82804-138), ANDA076373 (marketing start 09/06/2007),
"Revised: 9/2024" in the Highlights and "Revised: 12/2025" on the SPL. Its clinical pharmacology text is the
Coreg text (compare NJ_label-coreg_020297_2023-10-30), with "COREG" replaced by "carvedilol".

## Study
Not a study. Regulatory label; indications heart failure, post-MI LV dysfunction, hypertension.

## Model and parameters
No model and no PK table. Section 12.3 (PDF p20-21):
- Absolute bioavailability about 25% to 35% (first-pass metabolism).
- Apparent mean terminal elimination half-life generally 7 to 10 hours after oral dosing.
- Dose-proportional; food delays the time to peak without changing extent.
- Vss about 115 L; plasma clearance 500 to 700 mL/min; over 98% bound, mainly to albumin.
- R(+) plasma levels 2 to 3 times S(-); half-lives R(+) 5 to 9 h, S(-) 7 to 11 h.
- Parent about 7% of plasma radioactivity AUC; under 2% unchanged in urine.
- No Cmax, tmax or concentration in ng/mL is given.

```yaml
parameters:
  - {name: oral bioavailability, value: "25-35", unit: "%", source: "12.3 p20"}
  - {name: terminal half-life, value: "7-10", unit: h, source: "12.3 p20", note: "apparent mean, oral, racemate"}
  - {name: terminal half-life R(+), value: "5-9", unit: h, source: "12.3 p20"}
  - {name: terminal half-life S(-), value: "7-11", unit: h, source: "12.3 p20"}
  - {name: Vss, value: 115, unit: L, source: "12.3 p21", note: approximately}
  - {name: plasma clearance, value: "500-700", unit: mL/min, source: "12.3 p21"}
```

## Pharmacodynamics and end points
Beta-blockade (S(-) only) usually within 1 hour; alpha1-blockade (both enantiomers) within 30 minutes (12.2). The
4'-hydroxyphenyl metabolite is about 13 times more potent than parent (preclinical); the 3 active metabolites
reach about one-tenth of parent concentrations (12.3, p20). No therapeutic or toxic concentration range.

## Covariates
As in the Coreg label (12.3-12.4, p20-21): CYP2D6 poor metabolisers 2- to 3-fold higher R(+), 20% to 25% higher
S(-); heart failure raises AUC and Cmax (up to 50% to 100% in NYHA IV); elderly about 50% higher; severe cirrhosis
4- to 7-fold; moderate to severe renal impairment AUC 40% to 50% higher.

## Dosing (section 2, p4-5; Highlights p1)
Take with food. Heart failure 3.125 mg twice daily, doubling at intervals of at least 2 weeks to 25 mg twice
daily; post-MI 6.25 mg twice daily, up-titrated after 3 to 10 days; hypertension 6.25 mg twice daily up to 25 mg
twice daily over 1 to 2 week intervals.

## Caveats
- A repackager's SPL of a generic; one strength only. Section 17 carries a broken cross-reference ("Error!
  Hyperlink reference not valid", p29), a formatting fault with no bearing on PK.
- Half-life is a summary range after oral dosing; study details are not given.

## Used by stanpumpR
Registry source: `inst/extdata/antihypertensiveRegistry.csv` row `carvedilol__ir_tablet__oral__catalog__none__label`
(and its generator `data-raw/antihypertensiveRegistry.R`).

## Verification against stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:23` (8baa733; same text at `data-raw/antihypertensiveRegistry.R:271-276`):
  active moiety "carvedilol (racemic)" **matches** (section 11 calls it a racemic mixture); route oral,
  immediate-release tablet **matches**; "Label parent half-life 7-10 h" **matches** 12.3 (p20); the PD statement
  (fitted record's end point is inhibition of isoproterenol tachycardia) refers to Hwang 2023, not this label, but
  the label does list reduction of isoproterenol-induced tachycardia as a beta-blocking effect (12.2). Status
  fields (`official_label_descriptive`, `descriptive_only`, no BP simulation) are consistent with a label that
  carries no model. **Matches.**
