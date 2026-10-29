---
file: NJ_dailymed-2a8c7366.pdf
pmid: none
doi: none
citation: "Major Pharmaceuticals (manufactured by Torrent Pharmaceuticals). Carbamazepine Tablets USP 200 mg and Carbamazepine Tablets USP (Chewable) 100 mg, prescribing information and Medication Guide; ANDA077272 (200 mg) and ANDA075712 (100 mg chewable); PI 8113104 revised July 2026, SPL revised 9/2026; DailyMed setid 2a8c7366-ce89-47ce-8123-96f26812fca0, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=2a8c7366-ce89-47ce-8123-96f26812fca0"
kind: label
drugs: [carbamazepine]
source_read: "text layer, all 35 PDF pages (one form-feed chunk per page; an empty 36th chunk after the last form feed). Old-format label with no numbered sections. Read: Boxed warning pp.1-2; Description and Clinical Pharmacology incl. Pharmacokinetics pp.3-4; Indications and Contraindications p.5; Warnings incl. Usage in Pregnancy pp.5-11; Precautions incl. Laboratory Tests, Drug Interactions, Pregnancy, Labor and Delivery, Nursing Mothers, Pediatric and Geriatric Use pp.11-16; Adverse Reactions pp.16-18; Overdosage pp.18-19; Dosage and Administration with the dosage table pp.19-21; How Supplied p.21; Medication Guide pp.22-27; carton panels and SPL product data pp.27-35. Page images: p.1 and p.4 (Pharmacokinetics)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Major Pharmaceuticals carbamazepine IR and chewable tablets label (DailyMed)

## Study
This is not a study. It is the US prescribing information, Medication Guide and SPL data for **generic carbamazepine
immediate-release tablets, 200 mg, and chewable tablets, 100 mg**:
- Manufactured by Torrent; packaged and distributed in unit-dose packs by Major Pharmaceuticals.
- ANDA077272 for the 200 mg tablet, marketed from 09/20/2010; ANDA075712 for the 100 mg chewable, from 03/22/2010
  (pp.33-35).
- The document is what its DailyMed setid is (p.1 image). It is **not** the Tegretol-XR label; it sells no
  extended-release product.
- Its Clinical Pharmacology section carries the reference listed drug's (Tegretol) text, with its formulation
  comparisons.
- Carbamazepine MW 236.27 g/mol (p.3).

## Model and parameters
No model. Pharmacokinetics (p.4, image-confirmed):
- **Formulations:** in clinical studies, Tegretol suspension, conventional tablets and extended-release tablets
  delivered equivalent amounts of drug to the systemic circulation. The suspension was absorbed somewhat faster,
  and the XR tablet slightly slower, than the conventional tablet.
- **Bioavailability of the XR tablet:** 89% compared with the suspension.
- **Dosing equivalences at the same total daily dose:**
  - Twice-daily suspension gives higher peaks and lower troughs than twice-daily conventional tablets.
  - Suspension three times a day matches tablets twice a day.
  - Tegretol-XR twice a day matches conventional tablets four times a day.
- **Binding and levels:**
  - 76% bound to plasma protein; 24% unbound, which matches the CSF/serum ratio of 0.22.
  - Plasma levels range from 0.5 to 25 mcg/mL, with no apparent relation to daily intake.
  - Usual adult therapeutic range 4-12 mcg/mL.
- **Tmax with chronic dosing:** about 1.5 h for the suspension, 4-5 h for conventional tablets and 3-12 h for XR
  tablets.
- **Half-life and autoinduction:** autoinduction is complete after 3-5 weeks. The half-life falls from 25-65 h
  initially to 12-17 h on repeated doses.
- **Metabolism and excretion:**
  - CYP3A4 forms the 10,11-epoxide; microsomal epoxide hydrolase forms the trans-diol.
  - Of a 14C dose, 72% is recovered in urine and 28% in faeces; only 3% of urinary radioactivity is unchanged drug.
- **Children:**
  - Disposition is similar to adults, but concentration correlates poorly with dose.
  - Epoxide:parent ratio 0.44 below 1 year against 0.18 at 10-15 years.
  - The 4-12 mcg/mL range applies to children too (Pediatric Use, p.16).
- **Breast milk (p.15):** milk:plasma about 0.4 for the parent and 0.5 for the epoxide. Estimated infant dose 2-5
  mg/day of parent and 1-2 mg/day of epoxide.
- **Dosage (pp.19-21):**
  - Epilepsy, over 12 y: start 200 mg twice daily, increase by up to 200 mg/day weekly. Maintenance usually
    800-1200 mg/day. Maximum 1000 mg at 12-15 y and 1200 mg above 15 y; 1600 mg in rare adults.
  - Epilepsy, 6-12 y: start 100 mg twice daily, maintenance 400-800 mg.
  - Epilepsy, under 6 y: 10-20 mg/kg/day, keeping below 35 mg/kg/day.
  - Trigeminal neuralgia: start 100 mg twice daily, maximum 1200 mg/day.
- **Overdose (p.18):** the lowest known lethal adult dose is 3.2 g.

```yaml
parameters:
  - {name: relative_F_XR_vs_suspension, value: 0.89, unit: fraction, source: "Pharmacokinetics p.4", note: "Tegretol XR tablet against Tegretol suspension; same paragraph says all three forms deliver equivalent amounts"}
  - {name: protein_binding, value: 0.76, unit: fraction, source: "p.4"}
  - {name: therapeutic_range, value: "4-12", unit: mcg/mL, source: "p.4, p.16", note: "usual adult total plasma level; same in children"}
  - {name: t_half_initial, value: "25-65", unit: h, source: "p.4", note: "single dose, before autoinduction"}
  - {name: t_half_repeated, value: "12-17", unit: h, source: "p.4", note: "after autoinduction (3-5 weeks)"}
  - {name: tmax_conventional_tablet, value: "4-5", unit: h, source: "p.4", note: "chronic dosing"}
  - {name: tmax_XR, value: "3-12", unit: h, source: "p.4", note: chronic dosing}
  - {name: tmax_suspension, value: 1.5, unit: h, source: "p.4", note: approximately}
  - {name: MW, value: 236.27, unit: g/mol, source: "Description p.3"}
```

## Pharmacodynamics and end points
- **Therapeutic range:** 4-12 mcg/mL, total plasma, in adults and children. Monitoring of levels is recommended
  (pp.13, 19).
- **Epoxide:** described as equipotent to carbamazepine in animal screens, with its clinical significance not
  established.
- No concentration-effect model. Not an opioid.

## Covariates
- **Autoinduction:** time on drug.
- **Age:** in children, the epoxide ratio and the poor dose-concentration relation.
- **Interactions:** CYP3A4 inhibitors and inducers.
- **Race and sex:** effects not systematically evaluated.
- **HLA-B\*1502:** screening is advised for the SJS/TEN risk (boxed warning).

## Caveats
- All values are descriptive ranges, with no n or estimator.
- The label contradicts itself on formulations. It says equivalent systemic delivery from all three formulations,
  and, in the next sentences, 89% for the XR tablet against the suspension. It does not compare XR with the
  conventional tablet directly.
- XR statements in a generic IR label are inherited from the Tegretol RLD text; they are not data on this
  manufacturer's product.

## Used by stanpumpR
- `inst/extdata/antiseizureParameters.csv:64`, built by `data-raw/antiseizureRegistry.R:334`:
  - Fields: carbamazepine, "relative F (ER)", 0.89, unitless, provenance label-derived.
  - population "Tegretol-XR label"; `source_url` this DailyMed setid.
- The same value is applied in `R/drugs_carbamazepine.R:104` (`oralFormulations$ER$bioavailability_PO = 0.89`,
  with a comment at :42-44: "the XR label, against the suspension"). It is described in
  `inst/help/drugs/carbamazepine.md:11` and pinned in `tests/testthat/test-drugs-carbamazepine.R:7`.

## Verification against stanpumpR
- **Value 0.89:** p.4, "89% compared to suspension". **matches**. The code comment's "against the suspension" is
  exact. The model gives the suspension and the IR tablets the same parameters, which agrees with the label's
  statement that they deliver equivalent amounts.
- **Source description: discrepancy (citation only).** The CSV's population field calls the source the
  "Tegretol-XR label". The linked setid is a Major Pharmaceuticals/Torrent generic label for IR 200 mg and chewable
  100 mg tablets (ANDA077272, ANDA075712), with no XR product. The 89% sentence is present because the generic
  repeats the Tegretol text, so the number is supported, but a reader who opens the link will not find a
  Tegretol-XR label. The Novartis Tegretol/Tegretol-XR label is the matching source.
- **Typical 8, range 4-12 mg/L** (drugs_carbamazepine.R:95; not a context location): agrees with the label's 4-12
  mcg/mL.
