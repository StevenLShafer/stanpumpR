---
file: NJ_eucast-breakpoints_v16-1.pdf
pmid: none
doi: none
citation: The European Committee on Antimicrobial Susceptibility Testing (EUCAST). Breakpoint tables for interpretation of MICs and zone diameters. Version 16.1, valid from 2026-06-24. https://www.eucast.org/fileadmin/eucast/pdf/breakpoints/v_16.1_Breakpoint_Tables.pdf
kind: guideline
drugs: [cefalexin, cefazolin, ceftriaxone, clindamycin, gentamicin, metronidazole]
source_read: partial; contents, notes, dosage tables (PDF pp 10-12), Enterobacterales cephalosporins and aminoglycosides (pp 18, 20), Staphylococcus spp. cephalosporins, aminoglycosides, macrolides/lincosamides (pp 36, 38, 39), Streptococcus groups A/B/C/G (pp 49-50), Gram-negative anaerobes (pp 81-82). The pdftotext layout scrambles the tables, so these pages were re-read with pypdf extract_text (row order intact; superscript note numbers run into the values, e.g. "(2)1" = (2) with note 1)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# EUCAST 2026: clinical breakpoint tables v16.1

## Study
Consensus clinical MIC and zone-diameter breakpoints by organism group, each tied to the dosage in the "Dosages used
to define breakpoints" table. Breakpoints are S <= x and R > y mg/L; I ("susceptible, increased exposure") is the
interval between. Values in brackets are ECOFF-based and for use with other active therapy or specific indications
(Notes 11-12).

## Model and parameters
Breakpoints relevant to stanpumpR (mg/L; PDF page):
```yaml
parameters:
  - {name: ceftriaxone_Enterobacterales, value: "S<=1, R>2", unit: mg/L, source: "p18", note: "indications other than meningitis; meningitis S<=1, R>1"}
  - {name: cefazolin_Enterobacterales, value: "S<=0.001, R>4", unit: mg/L, source: "p18", note: "only infections originating from the urinary tract, E. coli and Klebsiella (not K. aerogenes); 0.001 = 'I only' off-scale breakpoint"}
  - {name: cefalexin_Enterobacterales, value: "S<=16, R>16", unit: mg/L, source: "p18", note: "uncomplicated UTI only"}
  - {name: gentamicin_Enterobacterales_systemic, value: "(S<=2, R>2)", unit: mg/L, source: "p20", note: "in brackets, note 1: see guidance on bracketed breakpoints"}
  - {name: gentamicin_Enterobacterales_urinary, value: "S<=2, R>2", unit: mg/L, source: "p20", note: "infections originating from the urinary tract; not bracketed"}
  - {name: staph_cephalosporins, value: "Note 1", source: "p36", note: "inferred from cefoxitin susceptibility (cefalexin, cefazolin, ceftriaxone all 'Note1'); MSSA reported I for cefazolin, cefepime, cefotaxime, ceftriaxone, cefuroxime iv"}
  - {name: cefoxitin_screen_S_aureus, value: ">4 = methicillin resistant", unit: mg/L, source: "p36 note 4"}
  - {name: gentamicin_S_aureus, value: "(S<=2, R>2)", unit: mg/L, source: "p38", note: "bracketed"}
  - {name: clindamycin_Staphylococcus, value: "S<=0.25, R>0.25", unit: mg/L, source: "p39"}
  - {name: clindamycin_Strep_ABCG, value: "S<=0.5, R>0.5", unit: mg/L, source: "p49"}
  - {name: metronidazole_Bacteroides, value: "S<=4, R>4", unit: mg/L, source: "p81", note: "also Parabacteroides, Phocaeicola dorei/vulgatus"}
  - {name: clindamycin_Bacteroides, value: "(S<=4, R>4)", unit: mg/L, source: "p81", note: "bracketed"}
  - {name: metronidazole_Prevotella, value: "S<=4, R>4", unit: mg/L, source: "p82", note: "Fusobacterium necrophorum/nucleatum S<=1, R>1"}
```
Dosages the breakpoints assume (pp 10-12): cefalexin 0.25-1 g x 2-3 oral; cefazolin 1 g x 3 iv standard, 2 g x 3 iv
high (S. aureus: high dose only); ceftriaxone 2 g x 1 iv standard, 2 g x 2 or 4 g x 1 iv high (and meningitis);
gentamicin 6-7 mg/kg x 1 iv; clindamycin 0.3 g x 2 oral or 0.6 g x 3 iv; metronidazole 0.4 g x 3 oral or iv.

## Pharmacodynamics and end points
MIC breakpoints only; no PK/PD target attainment values are given in these tables (PK-PD cut-offs are on a separate
sheet, p 121, not read). Not opioid-related; no MEAC, ke0 or potency ratio.

## Covariates
None.

## Caveats
Breakpoints change yearly; values are tied to the stated dosages. Bracketed breakpoints are not ordinary S/R
categories. Text extraction of these tables is unreliable; values above were read row by row with pypdf.

## Used by stanpumpR
- R/drugs_ceftriaxone.R:27 and :64; inst/help/drugs/ceftriaxone.md:15; R/antibioticThresholds.R:72 - ceftriaxone
  Enterobacterales S <= 1, R > 2 (v16.1, 2026), MIC threshold 1 mg/L.
- R/drugs_gentamicin.R:54-59; inst/help/drugs/gentamicin.md:11; R/antibioticThresholds.R:79 - gentamicin
  Enterobacterales S <= 2 (EUCAST and CLSI share), MIC threshold 2 mg/L.
- R/drugs_clindamycin.R:43, :45; R/antibioticThresholds.R:75 - EUCAST streptococci A, B, C, G S <= 0.5; EUCAST
  staphylococcal breakpoint 0.25 mg/L.
- R/drugs_metronidazole.R:53-54 (now :64-66); R/antibioticThresholds.R:82 - Bacteroides spp. S <= 4, R > 4 (v16.1).
- R/drugs_cefazolin.R:62-63 - EUCAST infers staphylococcal susceptibility from cefoxitin.
- R/drugs_cefalexin.R:44 and R/antibioticThresholds.R:69 - no EUCAST cefalexin breakpoint for staphylococci.

## Verification against stanpumpR
- Ceftriaxone, Enterobacterales, S <= 1, R > 2 (non-meningitis): p18. **matches** (version and year right).
- Gentamicin, Enterobacterales, S <= 2: p20 gives S <= 2 for infections originating from the urinary tract and
  **(2)** in brackets for systemic infections. The value **matches**; the repo does not say that EUCAST brackets the
  systemic breakpoint (ECOFF-based, for combination therapy), and "EUCAST" is cited without a version
  (drugs_gentamicin.R:59, gentamicin.md:11, antibioticThresholds.R:79). Minor wording omission, not a numeric error.
- Clindamycin, streptococci A/B/C/G S <= 0.5 (p49) and staphylococci 0.25 (p39). **matches**.
- Metronidazole, Bacteroides spp. S <= 4, R > 4 (p81). **matches**.
- Cefazolin, staphylococcal susceptibility inferred from cefoxitin (p36 note 1/A). **matches**. The table adds that
  MSSA should be reported I (increased exposure, 2 g x 3), which the repo does not mention; not a discrepancy.
- Cefalexin, no staphylococcal breakpoint: p36 lists cefalexin as "Note1" (inferred from cefoxitin), so no numeric
  breakpoint. **matches**; the repo's "(inferred from oxacillin or cefoxitin)" combines CLSI (oxacillin) and EUCAST
  (cefoxitin) correctly.
