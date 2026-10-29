---
file: NJ_dailymed-a3e33add.pdf
pmid: null
doi: null
citation: "CARDENE IV (nicardipine hydrochloride injection, solution), Baxter Healthcare Corporation. DailyMed SPL setid a3e33add-0d09-423a-9109-42e5ba7148d5, NDA 019734, NDC 43066-026 and 43066-028 (PREMIERProRx), revised 4/2026; retrieved 2026-10-21."
kind: label
drugs: [nicardipine]
source_read: "text layer, all 27 pages: Highlights (PDF p1-p2), contents (p2-p3), 1-2 Indications and Dosage and Administration (p3-p5), 5-6 Warnings and Adverse Reactions (p5-p7), 8 Use in Specific Populations (p8-p10), 12.1-12.3 Clinical Pharmacology (p12-p14), 14 Clinical Studies (p15), 16 and container/carton labels (p15-p25), SPL product data (p26-p27); p1 confirmed on rendered page image"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Cardene IV DailyMed SPL (Baxter PREMIERProRx, revised 4/2026)

## Study
DailyMed structured product label for Baxter's Cardene IV premixed bags marketed under NDC 43066-026-10 (20 mg in
200 mL, 0.1 mg/mL, NaCl 0.86%) and 43066-028-10 (40 mg in 200 mL, 0.2 mg/mL, NaCl 0.83%), marketing start
08/22/2020, NDA019734 (p26-p27). The prescribing-information text is the same as the FDA accessdata label of 3/2026
(NJ_label-cardeneiv_019734_2026-03-30); differences are the NDC numbers, a PREMIERProRx trademark line, the
container/carton label art (p16-p25) and the revision date 4/2026. The highlights still name the dextrose
presentation, though this SPL lists only the two sodium chloride products.

Dosing (2.1): start 5 mg/h, +2.5 mg/h every 5-15 min to a maximum 15 mg/h, then 3 mg/h after rapid titration;
oral 20/30/40 mg q8h = 0.5/1.2/2.2 mg/h IV; 50% offset about 30 min after stopping; 50% of the ultimate BP fall in
about 45 min (2.2).

## Model and parameters
Section 12.3 (PDF p14):

```yaml
parameters:
  - {name: t_half_alpha, value: 2.7, unit: min, source: "12.3"}
  - {name: t_half_beta, value: 44.8, unit: min, source: "12.3"}
  - {name: t_half_gamma, value: 14.4, unit: h, source: "12.3", note: "only after long-term infusions"}
  - {name: CL, value: 0.4, unit: L/h/kg, source: "12.3", note: "total plasma clearance"}
  - {name: Vd, value: 8.3, unit: L/kg, source: "12.3", note: non-compartmental}
  - {name: protein_binding, value: ">95", unit: "%", source: "12.3"}
```

Steady state approached at 24-48 h; linear 0.5-40 mg/h; CYP2C8/2D6/3A4 metabolism, no unchanged drug excreted
(12.3). No compartmental volumes or study n.

## Pharmacodynamics and end points
As in the accessdata label: normotensive volunteers < 5 / < 3 mmHg change at 0.25-3 mg/h; HR +7 to +8 bpm; 48-h 4
mg/h infusion SBP -26 mmHg, DBP -20.7 mmHg; time to therapeutic response 77 +/- 5.2 min (severe) and 11.5 +/- 0.8
min (postoperative) (12.2, 14). Adverse events (6.1, p6): headache 15%, hypotension 6%, tachycardia 4%,
nausea/vomiting 5%. No ke0, no therapeutic range.

## Covariates
Hepatic and renal impairment raise exposure (5.3, 5.4, 12.2), unquantified; elderly similar steady-state PK (8.5);
pregnancy and lactation sections as in the accessdata label (8.1, 8.2).

## Caveats
Summary values only; Vd non-compartmental; no source studies named.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:94 (record nicardipine__injection__iv__catalog__none__label,
  official_label_descriptive, descriptive_only).

## Verification against stanpumpR
- **antihypertensiveRegistry.csv:94**: "2.7 min, 44.8 min and 14.4 h phases, clearance about 0.4 L/h/kg and a
  non-compartmental Vd about 8.3 L/kg. Three half-lives plus one clearance do not identify V1, V2, V3, Q2 and Q3."
  All values are in 12.3 (p14); no V1 is given, so the identifiability point holds. Route iv, "injection / premixed
  infusion", URL matches the SPL setid. **matches**.
