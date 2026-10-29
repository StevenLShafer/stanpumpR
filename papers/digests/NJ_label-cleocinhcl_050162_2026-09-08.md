---
file: NJ_label-cleocinhcl_050162_2026-09-08.pdf
pmid: none
doi: none
citation: "Pfizer (Pharmacia & Upjohn). CLEOCIN HCl (clindamycin hydrochloride capsules, USP) prescribing information, NDA 050162, LAB-0040-31.2, Revised 9/2026. https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/050162s108lbl.pdf"
kind: label
drugs: [clindamycin]
source_read: "text layer, all 12 PDF pages (old-format label with no numbered sections). Read: Boxed warning and Description pp.1-2; Clinical Pharmacology (absorption, distribution, metabolism, excretion, specific populations) pp.2-3; Microbiology pp.3-4; Indications p.5; Warnings and Precautions incl. Pregnancy, Nursing Mothers, Pediatric and Geriatric Use pp.6-10; Adverse Reactions p.10; Overdosage and Dosage and Administration p.11; How Supplied and revision p.12. Page images: p.1 (identity) and p.12 (revision block). No PK table in the label."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Cleocin HCl (clindamycin hydrochloride capsules) label, revised 9/2026

## Study
This is not a study. It is the US prescribing information for CLEOCIN HCl capsules, 75, 150 and 300 mg of
clindamycin as the hydrated hydrochloride, distributed by Pharmacia & Upjohn (Pfizer). It is revised 9/2026
(LAB-0040-31.2, p.12; image-confirmed) and is what its name says. Unlike the Phosphate label, the archived PDF
carries no FDA "Reference ID" footer. stanpumpR's help cites the HCl label as Reference ID 4572239, which cannot be
checked against this file.

## Model and parameters
No model. The PK statements (Clinical Pharmacology, pp.2-3):
- **Single 150 mg oral dose in 24 healthy adults:** mean peak serum concentration 2.50 mcg/mL at 45 min; 1.51
  mcg/mL at 3 h and 0.70 mcg/mL at 6 h.
- **Absorption:** virtually complete, 90%; food does not appreciably change serum concentrations.
- **Repeat dosing:** no accumulation or altered metabolism with dosing for up to 14 days. Up to 2 g/day for 14 days
  was well tolerated, though with more GI effects.
- **Distribution:** serum concentrations rise linearly with dose. Serum stays above the MIC for most indicated
  organisms for at least 6 h after usual doses. Wide tissue distribution, including bone; no significant CSF
  levels.
- **Elimination:** average biological half-life 2.4 h. About 10% of bioactivity appears in urine and 3.6% in faeces;
  the rest is excreted as inactive metabolites.
- **Metabolism:** mainly CYP3A4, with minor CYP3A5, to the sulfoxide and N-desmethyl metabolites.
- **Renal or hepatic impairment:** half-life slightly prolonged; dialysis does not remove the drug; no renal dose
  change.
- **Elderly:** oral half-life about 4.0 h (3.4-5.1) against 3.2 h (2.1-4.2) in young adults, with absorption
  unchanged.
- **Obesity (ages 2-20):** clearance and volume per kg of total body weight are comparable regardless of obesity.
- **Dosage (p.11):**
  - Adults: 150-300 mg every 6 h for serious infections; 300-450 mg every 6 h for more severe ones.
  - Children who can swallow capsules: 8-16 mg/kg/day for serious infections, 16-20 mg/kg/day for more severe ones,
    in 3-4 doses, by total body weight.
  - Take with a full glass of water, at least 30 min before lying down.
  - Serious anaerobic infections are usually treated with CLEOCIN PHOSPHATE.
- **Breast milk (p.9):** <0.5 to 3.8 mcg/mL.

```yaml
parameters:
  - {name: Cmax_150mg_oral, value: 2.50, unit: mcg/mL, source: "Clinical Pharmacology p.2", note: "mean of 24 healthy adults, at 45 min"}
  - {name: C_3h_150mg_oral, value: 1.51, unit: mcg/mL, source: "p.2", note: mean}
  - {name: C_6h_150mg_oral, value: 0.70, unit: mcg/mL, source: "p.2", note: mean}
  - {name: F_oral, value: 0.90, unit: fraction, source: "p.2", note: "absorption described as virtually complete"}
  - {name: t_half_biological, value: 2.4, unit: h, source: "Excretion p.3", note: average}
  - {name: urinary_bioactivity, value: 0.10, unit: fraction, source: "p.3", note: "faeces 3.6%"}
```

## Pharmacodynamics and end points
- Microbiology (p.3): the label calls clindamycin bacteriostatic.
- The active-against and in-vitro-only organism lists (p.4) are the same as in the Phosphate label. Neither list
  contains *Bacteroides*.
- No MIC, MBC or PK/PD target; breakpoints are at fda.gov/STIC. Not an opioid.

## Covariates
- Total body weight for paediatric dosing.
- CYP3A4/5 interactions.
- No renal dose change.
- Age prolongs the oral half-life without a dose change.

## Caveats
- The values are descriptive means from old studies, with only the 150 mg study's n given.
- The label's 90% absorption is the extent of absorption, not a formally measured absolute bioavailability.

## Used by stanpumpR
- `R/antibioticMbc.R:16`: the header lists the Cleocin HCl and Phosphate labels as read in full.
- `R/antibioticMbc.R:47`: band rule; clindamycin is bacteriostatic against *S. aureus* ("the FDA label").
- `R/antibioticMbc.R:280`: *S. aureus* excluded row, sourced to the Phosphate and HCl labels, Microbiology:
  "Clindamycin is bacteriostatic."
- `inst/help/models/antibiotics.md:18`: the label calls clindamycin bacteriostatic.
- `inst/help/models/antibiotics.md:45`: "Cleocin HCl, Reference ID 4572239" among the labels read.

## Verification against stanpumpR
- **"Clindamycin is bacteriostatic"** (antibioticMbc.R:47, :280; antibiotics.md:18): Microbiology, p.3, verbatim.
  **matches**.
- **Reference ID 4572239** (antibiotics.md:45): the archived 9/2026 file carries no Reference ID, so the ID cannot
  be confirmed. It presumably belongs to an earlier FDA-posted revision. This is a citation detail only.
- Context, not cited by the repo: the label's 90% oral absorption agrees with the 0.876 bioavailability of
  clindamycin's Bouazza 2012 model (`inst/help/drugs/clindamycin.md`). The label's 2.4-3.2 h oral half-life agrees
  with the model's 3.0 h at 70 kg.
