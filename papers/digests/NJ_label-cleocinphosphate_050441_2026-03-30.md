---
file: NJ_label-cleocinphosphate_050441_2026-03-30.pdf
pmid: none
doi: none
citation: "Pfizer. CLEOCIN PHOSPHATE (clindamycin injection, USP; clindamycin injection in 5% dextrose) prescribing information, NDA 050441, LAB-0044-31.1, Revised 3/2026, Reference ID 5770229. https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/050441s091lbl.pdf"
kind: label
drugs: [clindamycin]
source_read: "text layer, all 20 PDF pages (old-format label with no numbered sections). Read: Boxed warning and Description pp.1-2; Clinical Pharmacology (distribution, metabolism, excretion, specific populations, Table 1) pp.2-4; Microbiology pp.4-5; Indications p.5-6; Warnings and Precautions incl. Pregnancy, Nursing Mothers, Pediatric and Geriatric Use pp.7-11; Adverse Reactions and Overdosage pp.11-12; Dosage and Administration incl. Tables 2-3 and infusion rates pp.12-14; How Supplied p.16; References and revision p.17; ADD-Vantage instructions pp.18-20. Page images: p.4 (Table 1, Microbiology) and p.13 (Table 2, scrambled in the text layer)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Cleocin Phosphate (clindamycin injection) label, revised 3/2026

## Study
This is not a study. It is Pfizer's US prescribing information for CLEOCIN PHOSPHATE: vials (150 mg/mL with benzyl
alcohol), ADD-Vantage vials and GALAXY premix in 5% dextrose (300, 600 and 900 mg). It is revised 3/2026
(LAB-0044-31.1, Reference ID 5770229, p.17), which matches the file name. stanpumpR cites the **12/2021** revision
(Reference ID 4899100); see Verification. Clindamycin phosphate MW is 504.96 (p.2).

## Model and parameters
No model. The PK statements (Clinical Pharmacology, pp.2-4):
- Clindamycin phosphate is a biologically inactive prodrug converted to active clindamycin.
  - Its average serum half-life is 6 min.
  - Active clindamycin's average serum elimination half-life is about 3 h in adults and 2.5 h in children.
- Peak active concentration after IM injection: within 3 h in adults and 1 h in children. After a short IV
  infusion it peaks at the end of the infusion.
- Equilibrium is reached by the third dose. Dosing every 8-12 h (adults) or every 6-8 h (children), or continuous
  infusion, keeps serum above the MIC for most indicated organisms.
- No significant CSF concentration, even with inflamed meninges.
- Metabolism is mainly by CYP3A4, with a minor CYP3A5 contribution, to clindamycin sulfoxide and
  N-desmethylclindamycin.
- Renal or hepatic impairment slightly prolongs the half-life. Haemodialysis and peritoneal dialysis do not remove
  the drug, and no dose change is needed.
- Elderly (61-79 y) against young (18-39 y): IV PK is unchanged by age. After oral HCl the half-life is about
  4.0 h (3.4-5.1) against 3.2 h (2.1-4.2), with absorption unchanged.
- Obesity (ages 2-20): clearance and volume normalised to total body weight are comparable regardless of obesity.
- Premature infants: CL rises with weight and post-menstrual age (PMA).

**Table 1 (p.4, image-confirmed): predicted exposure, mean (SD), every 8 h**

| | Adult (70 kg) | PMA <=32 wk | PMA >32 to <=40 wk |
|---|---|---|---|
| Dose | 600 mg | 5 mg/kg | 7 mg/kg |
| AUCss,0-8h, mcg.h/mL | 50.5 (30.95) | 52.5 (17.0) | 55.9 (23.55) |
| Cmax,ss, mcg/mL | 12.0 (3.49) | 9.0 (2.02) | 10.5 (2.79) |
| Cmin,ss, mcg/mL | 3.1 (3.34) | 4.6 (2.00) | 4.4 (2.77) |

The values are **predicted**, not measured. The label does not say whether they are total or free (serum).

**Table 2 (p.13, image-confirmed): rapid infusion followed by maintenance infusion**

| To maintain serum above | Rapid infusion | Maintenance rate |
|---|---|---|
| 4 mcg/mL | 10 mg/min for 30 min | 0.75 mg/min |
| 5 mcg/mL | 15 mg/min for 30 min | 1.00 mg/min |
| 6 mcg/mL | 20 mg/min for 30 min | 1.25 mg/min |

**Dosage (pp.12-14):**
- Adults, IM or IV, aerobic Gram-positive cocci and the more susceptible anaerobes (the label says this is NOT
  generally *B. fragilis*, *Peptococcus* or clostridia other than *C. perfringens*): 600-1200 mg/day in 2-4 doses.
- More severe infections, particularly proven or suspected *Bacteroides fragilis*: 1200-2700 mg/day.
- Up to 4800 mg/day has been given IV. Single IM doses above 600 mg are not recommended.
- Children 1 month to 16 y: 20-40 mg/kg/day in 3-4 doses (or 350-450 mg/m2/day), by total body weight.
- Under 1 month: 15-20 mg/kg/day. Table 3: 5 mg/kg every 8 h at PMA <=32 wk, 7 mg/kg every 8 h at >32-40 wk.
- Infusion: at most 18 mg/mL and 30 mg/min. Usual schedule 300 mg over 10 min up to 1200 mg over 40 min; more than
  1200 mg in a single 1-h infusion is not recommended.

**Breast milk (p.10):** <0.5 to 3.8 mcg/mL at doses of 150 mg orally to 600 mg IV.

```yaml
parameters:
  - {name: t_half_clindamycin_adult, value: 3, unit: h, source: "Clinical Pharmacology p.3", note: "average serum elimination half-life of active drug"}
  - {name: t_half_clindamycin_phosphate, value: 6, unit: min, source: "p.3", note: "prodrug disappearance"}
  - {name: Cmax_ss_600q8h_adult, value: 12.0, unit: mcg/mL, source: "Table 1 p.4", note: "predicted mean, SD 3.49, 70 kg"}
  - {name: Cmin_ss_600q8h_adult, value: 3.1, unit: mcg/mL, source: "Table 1 p.4", note: "predicted mean, SD 3.34"}
  - {name: AUCss_0_8_600q8h_adult, value: 50.5, unit: mcg.h/mL, source: "Table 1 p.4", note: "predicted mean, SD 30.95"}
  - {name: maintenance_rate_above_4, value: 0.75, unit: mg/min, source: "Table 2 p.13", note: "after 10 mg/min for 30 min"}
  - {name: maintenance_rate_above_6, value: 1.25, unit: mg/min, source: "Table 2 p.13", note: "after 20 mg/min for 30 min"}
  - {name: MW_clindamycin_phosphate, value: 504.96, unit: g/mol, source: "Description p.2"}
```

## Pharmacodynamics and end points
- Microbiology (p.4): clindamycin binds the 23S RNA of the 50S ribosomal subunit and calls the drug bacteriostatic.
- Organisms against which it is active in vitro and in clinical infections (pp.4-5): MSSA, penicillin-susceptible
  *S. pneumoniae*, *S. pyogenes*, *C. perfringens*, *Fusobacterium necrophorum* and *nucleatum*,
  *Peptostreptococcus anaerobius*, *Prevotella melaninogenica*.
- *Bacteroides* are on neither the clinical list nor the in-vitro-only list. Breakpoints are deferred to
  fda.gov/STIC.
- No MIC or MBC values and no PK/PD target. Not an opioid.

## Covariates
- Body weight: dose by total body weight, regardless of obesity.
- Post-menstrual age in neonates.
- CYP3A4/5 inhibitors and inducers.
- No dose adjustment for renal or hepatic disease, or for age.

## Caveats
- Table 1 is model-predicted; the label does not name the model or its source data. The Cmin SD (3.34) exceeds
  the mean.
- The half-lives are "average" values with no n.
- This is the 3/2026 revision, not the 12/2021 revision that stanpumpR cites.

## Used by stanpumpR
- `R/antibioticMbc.R:16`: the header lists the Cleocin HCl and Phosphate labels as read in full.
- `R/antibioticMbc.R:47`: band rule; clindamycin is bacteriostatic against *S. aureus* ("the FDA label").
- `R/antibioticMbc.R:260`: Bacteroides row note. The label no longer lists *B. fragilis* among the organisms it is
  active against but still doses for it at 1200-2700 mg/day.
- `R/antibioticMbc.R:267`: the Bacteroides source string, "FDA Cleocin Phosphate label, revised 12/2021, Dosage and
  Administration ('proven or suspected Bacteroides fragilis')".
- `R/antibioticMbc.R:280`: *S. aureus* excluded row. "FDA Cleocin Phosphate (revised 12/2021) and Cleocin HCl
  labels, Microbiology: 'Clindamycin is bacteriostatic.'"
- `inst/help/models/antibiotics.md:18` and `:45`: the bacteriostatic statement; ":45" lists the label as "revised
  12/2021, Reference ID 4899100".
- `tests/testthat/test-antibiotic-mbc.R:134-138`: the band must lie between Cmin,ss 3.1 and Cmax,ss 12.0 mcg/mL
  total (Table 1, 600 mg every 8 h, 70 kg).
- `inst/help/drugs/clindamycin.md:17` uses four facts from the label:
  - bacteriostatic "in those words";
  - Table 1 peak 12.0 and trough 3.1;
  - continuous infusion holds serum above 4, 5 or 6 mcg/mL;
  - 1200-2700 mg/day for proven or suspected *B. fragilis*.

## Verification against stanpumpR
- **"Clindamycin is bacteriostatic"** (antibioticMbc.R:47, :280; antibiotics.md:18; clindamycin.md:17): p.4,
  verbatim. **matches**. The label says it without naming an organism; restricting it to *S. aureus* is
  stanpumpR's framing, supported by other cited sources.
- **B. fragilis** (antibioticMbc.R:260, :267; clindamycin.md:17):
  - Absent from the active-against lists (pp.4-5): **matches**.
  - 1200-2700 mg/day for more severe infections, particularly proven or suspected *B. fragilis* (p.12):
    **matches**.
- **Table 1 values 12.0 / 3.1 mcg/mL at 600 mg every 8 h, 70 kg** (test :135; clindamycin.md:17): p.4.
  **matches**. Two notes:
  - These are predicted means, not observations.
  - The label does not say "total", although the test comment does. Serum total is the natural reading.
- **Continuous infusion above 4, 5, 6 mcg/mL** (clindamycin.md:17): Table 2, p.13. **matches**.
- **Revision cited.** stanpumpR cites "revised 12/2021, Reference ID 4899100" (antibioticMbc.R:267, :280;
  antibiotics.md:45; test :135). The archived file is revised 3/2026, Reference ID 5770229. Every fact the repo
  takes from the label is in the 3/2026 text, so the substance is verified. The 12/2021 document itself is not in
  the archive, and the citation strings name a revision other than the one archived.
