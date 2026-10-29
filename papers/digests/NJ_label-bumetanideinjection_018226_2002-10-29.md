---
file: NJ_label-bumetanideinjection_018226_2002-10-29.pdf
pmid: null
doi: null
citation: "BUMEX (bumetanide) TABLETS and INJECTION. US prescribing information, Roche Laboratories (NDA 018226, injection; combined label also covering the tablets); revised July 2002 (HLR 7/10/02); FDA label 018226s025lbl.pdf, 2002-10-29."
kind: label
drugs: [bumetanide]
source_read: "page images: the PDF (10 pages) is a scan whose text layer holds only the FDA banner, so every page was rendered. Read Description and Clinical Pharmacology with Pediatric and Geriatric Pharmacology (p1-3), Indications and Contraindications (p3), Pediatric and Geriatric Use (p7), Adverse Reactions and Overdosage (p8), Dosage and Administration, Miscibility and How Supplied (p9-10). Pages 4-6 (Warnings, Precautions, Pregnancy) were rendered but not read."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Bumex (bumetanide) tablets and injection label, Roche 2002

## Study
This is the Roche BUMEX label for both tablets and injection. Its header is HLR 7/10/02 and the last page says
"Revised: July 2002" (code 27898162-0702). The file is filed under the injection NDA 018226, and it does cover
the injection, so there is no source file problem. Note, though, that it is a combined tablets and injection
label, not an injection-only one.

The context file says `scan: false`, but the text layer is empty apart from the FDA banner. Every number below is
from the rendered pages.

- **Description (p1):**
  - Tablets 0.5, 1 and 2 mg.
  - Injection: 2, 4 and 10 mL vials of 0.25 mg/mL for IV or IM use. Each 2 mL holds 0.5 mg, in 0.85% NaCl and
    0.4% ammonium acetate, with 0.01% edetate and 1% benzyl alcohol, pH about 7.
  - MW 364.41.
- **Dosage (p9):**
  - Oral: as for the current tablet label, 0.5-2 mg/day, repeat at 4-5 h, at most 10 mg/day, 1:40 for furosemide.
  - Parenteral: start at 0.5-1 mg IV or IM, with IV given over 1-2 min. Repeat at 2-3 h intervals, at most
    10 mg/day. Switch to oral as soon as possible.
- **Miscibility (p9):** compatible with D5W, 0.9% NaCl and lactated Ringer's in glass and PVC. Use within 24 h.

## Model and parameters
Descriptive values only (p2-3). They match the 2024 tablet label except where noted.
```yaml
parameters:
  - {name: elimination t1/2 (oral or parenteral), value: "1-1.5", unit: h, source: "Clinical Pharmacology, p2"}
  - {name: plasma protein binding, value: "94-96", unit: "%", source: "p2"}
  - {name: urinary recovery of 14C oral dose, value: 81, unit: "%", source: "p2", note: "45% of it unchanged; biliary 2%"}
  - {name: "neonatal t1/2 (IV)", value: 6, unit: h, source: "Pediatric Pharmacology, p2", note: "approximately; up to 15 h; CL 0.2-1.1 mL/min/kg"}
  - {name: "infant serum CL, <2 mo / 2-6 mo", value: "2.17 / 3.8", unit: mL/min/kg, source: "p2", note: "the 2024 tablet label rounds to 2.2"}
  - {name: "V, neonates and infants", value: "0.26-0.39", unit: L/kg, source: "p3"}
  - {name: "total CL, oral 0.5 mg, elderly / younger", value: "1.8 / 2.9", unit: mL/min/kg, source: "Geriatric Pharmacology, p3", note: "± 0.3 / ± 0.2"}
  - {name: "Cmax, oral 0.5 mg, elderly / younger", value: "16.9 / 10.3", unit: ng/mL, source: "p3"}
```

## Pharmacodynamics and end points
- **Time course (p2):** oral diuresis begins in 30-60 min and peaks at 1-2 h. IV diuresis starts within minutes
  and peaks at 15-30 min.
- **Potency:** 1 mg ≈ 40 mg furosemide (p1).
- **Infants (p3):** maximal effect at a bumetanide excretion rate of about 7 mcg/kg/h, at doses of 0.035-0.040 mg/kg.
- **Indications (p3):** oral and parenteral routes give almost equal diuretic response.

## Covariates
Age (neonates, infants and the elderly; quantified above). Renal and hepatic impairment are qualitative only.

## Caveats
- A 2002 label superseded by later revisions.
- It gives no adult IV clearance, no volume and no oral bioavailability.

## Used by stanpumpR
Lines at 8baa733, found by content:
- `tests/testthat/test-antihypertensive-registry.R:173-174`: the tablet and injection records must be separate.
- `docs/antihypertensives.md:29-34`: product and route identity.
- This FDA label's URL is not in the repo at 8baa733. The injection registry row
  (`inst/extdata/antihypertensiveRegistry.csv:17`) cites the Sagent generic SPL (`NJ_dailymed-c38a404d`).

## Verification against stanpumpR
- **Injection identity:** the label describes a 0.25 mg/mL injection for IV or IM use, as the registry's injection
  record ("iv", "0.25 mg/mL injection") does. **matches**
- **Separate from the tablet:** this one document covers both products, but with separate route-specific dosing,
  and it states that parenteral use is for patients with impaired GI absorption. That supports separate records.
  The registry's "tablet absorption must not be imported" is a modelling rule, not a label statement. **matches**
- **"IV disposition ... missing":** consistent. The label gives only the 1-1.5 h half-life and the binding for
  adults.
