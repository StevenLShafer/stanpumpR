---
file: NJ_label-gentamicin_062366_2014-03-21.pdf
pmid: null
doi: null
citation: "Fresenius Kabi USA. Gentamicin Injection, USP (40 mg/mL) and Gentamicin Injection, USP (Pediatric, 10 mg/mL) prescribing information, revised October 2013 (inserts 45855H and 45814J). ANDA 062366, FDA label posted 2014-03-21 (accessdata.fda.gov 062366s033lbl.pdf)."
kind: label
drugs: [gentamicin]
source_read: "text layer (all 9 PDF pages: carton/vial labels pp1-5, adult insert pp6-7, pediatric insert pp8-9); page images rendered and read for p6 (Clinical Pharmacology, Microbiology, Table 2), p7 (Dosage and Administration, Tables 3 and 4), p8 (pediatric Clinical Pharmacology), p9 (pediatric Dosage and Administration, left half)"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# FDA label 2013: Gentamicin Injection USP (Fresenius Kabi, adult 40 mg/mL and pediatric 10 mg/mL)

## Study
Not a study: an old-format (pre-PLR) US package insert. The PDF holds five carton/vial label images (PDF pp1-5:
20 mg/2 mL pediatric single-dose vial, 80 mg/2 mL and 800 mg/20 mL multiple-dose vials), the adult insert for the
40 mg/mL product (PDF pp6-7, code 45855H, revised October 2013) and the pediatric insert for the preservative-free
10 mg/mL product (PDF pp8-9, code 45814J, revised October 2013). The 40 mg/mL product contains methylparaben,
propylparaben and sodium metabisulfite; the pediatric product is preservative-free. Sections: Boxed Warnings,
Description, Clinical Pharmacology (with Microbiology), Indications, Contraindications, Warnings, Precautions,
Adverse Reactions, Overdosage, Dosage and Administration, How Supplied, References (CLSI M07-A9, M02-A11, M100-S23).
Assays named for monitoring: microbiologic, enzymatic and radioimmunoassay (p7).

## Model and parameters
No compartmental model. Pharmacokinetic statements (adult insert p6 unless stated; all confirmed on rendered pages):
- IM: peak serum concentration usually 30 to 60 min after injection; serum levels measurable 6 to 8 h. IV infusion
  over 2 h gives serum concentrations similar to IM.
- Adults, normal renal function: peak serum concentration (mcg/mL) is usually up to four times the single IM dose
  (mg/kg); 1 mg/kg may give a peak up to 4 mcg/mL, 1.5 mg/kg up to 6 mcg/mL. 1 mg/kg every 8 h for 7-10 days does not
  accumulate; 4 mg/kg/day or more for 7-10 days may give a slight progressive rise in peak and trough.
- Distribution in extracellular fluid; lower peaks with a large ECF volume; febrile patients lower; burns shorter
  half-life and lower concentrations.
- Protein binding low, 0 to 30% depending on method.
- 70% or more of the dose recovered in urine in 24 h (normal renal function); urine concentrations above 100 mcg/mL;
  little or no metabolism; excretion mainly by glomerular filtration; renal clearance similar to endogenous creatinine.
  Renal cortex concentrations sometimes eight times serum. CSF concentrations often low.
- Serum creatinine and creatinine clearance correlate highly with gentamicin half-life. No adult half-life value is
  stated.
- Pediatric insert (p8): 2.5 mg/kg in infants usually gives a peak of 3 to 5 mcg/mL; 1 mg/kg gave mean peaks of 1.58,
  2.03 and 2.81 mcg/mL at 6 months-5 years, 5-10 years and over 10 years ("in one report", unreferenced). Half-life
  3 to 3.5 h (infants 1 week-6 months), 5.5 h (full-term and large premature infants under 1 week), 11.5 h (premature
  under 1,500 g), 8 h (1,500-2,000 g), about 5 h (over 2,000 g). Neonates under 3 days excrete about 10% of the dose in
  12 h; infants 5-40 days about 40%.
- Eight-hour hemodialysis may reduce serum concentrations by about 50% (p7).

```yaml
parameters:
  - {name: tmax_IM, value: "30 to 60", unit: min, source: "adult insert, Clinical Pharmacology, PDF p6", note: label statement}
  - {name: peak_per_dose_adult, value: "up to 4 x dose", unit: "mcg/mL per mg/kg", source: "PDF p6", note: "1 mg/kg -> up to 4 mcg/mL; 1.5 mg/kg -> up to 6 mcg/mL"}
  - {name: protein_binding, value: "0 to 30", unit: "%", source: "PDF p6", note: method-dependent}
  - {name: urinary_recovery_24h, value: ">= 70", unit: "% of dose", source: "PDF p6", note: normal renal function}
  - {name: peak_expected_adult, value: "4 to 6", unit: mcg/mL, source: "Dosage and Administration, PDF p7", note: "30-60 min after IM"}
  - {name: peak_expected_pediatric, value: "3 to 5", unit: mcg/mL, source: "pediatric Dosage and Administration, PDF p9", note: "30-60 min after IM"}
  - {name: peak_avoid, value: 12, unit: mcg/mL, source: "Boxed Warnings PDF p6; Dosage PDF p7 and p9", note: "avoid prolonged levels above"}
  - {name: trough_avoid, value: 2, unit: mcg/mL, source: "Boxed Warnings PDF p6; Dosage PDF p7 and p9", note: "avoid levels above (just before next dose)"}
  - {name: t_half_infant_1wk_6mo, value: "3 to 3.5", unit: h, source: "pediatric insert, PDF p8"}
  - {name: t_half_neonate_lt_1wk_fullterm, value: 5.5, unit: h, source: "PDF p8"}
  - {name: t_half_premature_lt_1500g, value: 11.5, unit: h, source: "PDF p8"}
  - {name: t_half_premature_1500_2000g, value: 8, unit: h, source: "PDF p8"}
  - {name: t_half_gt_2000g, value: 5, unit: h, source: "PDF p8", note: approximately}
  - {name: QC_MIC_E_faecalis_ATCC29212, value: "4 to 16", unit: mcg/mL, source: "Table 2, adult insert PDF p6", note: "CLSI quality-control range, not a clinical MIC distribution"}
  - {name: QC_MIC_E_coli_ATCC25922, value: "0.25 to 1", unit: mcg/mL, source: "Table 2, PDF p6"}
  - {name: QC_MIC_P_aeruginosa_ATCC27853, value: "0.5 to 2", unit: mcg/mL, source: "Table 2, PDF p6"}
  - {name: QC_MIC_S_aureus_ATCC29213, value: "0.12 to 1", unit: mcg/mL, source: "Table 2, PDF p6"}
  - {name: breakpoint_S_I_R, value: "<=4 / 8 / >=16", unit: mcg/mL, source: "Table 1, PDF p6", note: "Enterobacteriaceae, P. aeruginosa, Staphylococcus spp.; disk >=15 / 13-14 / <=12 mm"}
```

## Pharmacodynamics and end points
- Microbiology (p6): binds the prokaryotic ribosome and inhibits protein synthesis; bactericidal in vitro against
  Gram-positive and Gram-negative bacteria. No MBC, no MIC50/MIC90, no PK/PD index (peak/MIC or AUC/MIC) stated.
- Usually resistant: most streptococci (including S. pneumoniae and group D), most enterococci (E. faecalis,
  E. faecium, E. durans), anaerobes. Synergy in vitro: with penicillin G (synergistic bactericidal against enterococci),
  ampicillin, carbenicillin, nafcillin, oxacillin; carbenicillin against many P. aeruginosa; cephalosporins against
  other Gram-negatives. Not effective clinically against Salmonella and Shigella.
- Table 1 footnote b: for susceptible staphylococci, aminoglycosides are used only in combination with other active
  agents.
- Toxicity: nephrotoxicity and (usually irreversible) oto-/vestibular toxicity; avoid prolonged peaks above 12 mcg/mL
  and troughs above 2 mcg/mL. Neuromuscular blockade and respiratory paralysis possible, especially with anesthetics or
  neuromuscular blockers. No therapeutic range is given as such; the expected peaks (adult 4-6, pediatric 3-5 mcg/mL)
  are orientation values, not targets.
- Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Dose per kg of pretreatment body weight; lean body mass in obese patients. Renal function: adult interval (h) about
serum creatinine (mg/100 mL) x 8; or dose reduction at 8-h intervals by dividing the usual dose by serum creatinine.
Table 4 (adult, image-confirmed, p7), percent of usual dose by serum creatinine (mg%) / approx. CrCl
(mL/min/1.73 m2): <=1 / >100 / 100; 1.1-1.3 / 70-100 / 80; 1.4-1.6 / 55-70 / 65; 1.7-1.9 / 45-55 / 55; 2-2.2 / 40-45
/ 50; 2.3-2.5 / 35-40 / 40; 2.6-3 / 30-35 / 35; 3.1-3.5 / 25-30 / 30; 3.6-4 / 20-25 / 25; 4.1-5.1 / 15-20 / 20;
5.2-6.6 / 10-15 / 15; 6.7-8 / <10 / 10. The pediatric insert prints the same guide as its Table 3 (text layer only;
its digits are scrambled there and were not checked on the image). Age (neonatal maturation, see half-lives), fever,
anemia, burns, ECF volume.

Dosing (p7, image-confirmed): adults 3 mg/kg/day in three doses q8h; up to 5 mg/kg/day in three or four doses for
life-threatening infection, reduced to 3 mg/kg/day when indicated (Table 3: 1 mg/kg q8h and 1.7 mg/kg q8h by weight
40-100 kg); children 6-7.5 mg/kg/day (2-2.5 mg/kg q8h); infants and neonates 7.5 mg/kg/day (2.5 mg/kg q8h); premature
or full-term neonates one week or less 5 mg/kg/day (2.5 mg/kg q12h). After hemodialysis 1-1.7 mg/kg (adults), 2 mg/kg
(children, adult insert) or 2-2.5 mg/kg (pediatric insert). IV: dilute, infuse over 0.5-2 h; IM and IV doses identical.
Usual duration 7-10 days.

## Caveats
- Old-format generic label; no clinical PK study is cited, and the pediatric age-peak figures come from an
  unreferenced "one report".
- Table 1 breakpoints are the CLSI M100-S23 (2013) values (S <=4 mcg/mL), superseded by the CLSI 2023 aminoglycoside
  revision; the label still lists P. aeruginosa as an indication and gives it a breakpoint.
- Table 2 MIC ranges are quality-control strain ranges for the test, not clinical isolate distributions.
- Text layer scrambles Tables 3 and 4 (digits dropped/merged); numbers above are from the rendered p7.
- Conventional (multiple-daily) dosing only; extended-interval dosing is not addressed.

## Used by stanpumpR
From the context file (registry source therapeuticRanges.csv; "bactericidal in vitro"; E. faecalis control-strain
MIC; synergy use):
- `R/antibioticMbc.R:296` (gentamicin Enterobacterales row, Source quotes the label's bactericidal statement)
- `R/antibioticMbc.R:313` (gentamicin "Enterococci and staphylococci" excluded row)
- `inst/help/models/antibiotics.md:45` (list of FDA labels read; none gives an MBC)
- `inst/extdata/therapeuticRanges.csv:143` (gentamicin row: source, label_statement, notes)

## Verification against stanpumpR
Checked in `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- `R/antibioticMbc.R:293-296`: Source quotes the FDA label as "bactericidal in vitro against Gram-positive and
  Gram-negative bacteria". The label (PDF p6, Microbiology, image-confirmed) says exactly that. The MIC50/MIC90/MBC
  values in the row (0.5/2) come from Sader 2023, not from this label; the label gives no MBC, so "MBC taken as the
  MIC" is the repo's assumption, correctly not attributed to the label. **matches**.
- `R/antibioticMbc.R:311-313`: Note says gentamicin is given against enterococci and staphylococci only for synergy
  with a cell-wall agent, and that the E. faecalis control strain's MIC is 4 to 16 mg/L; Source "FDA gentamicin
  injection label, 2013". Table 2 (p6): E. faecalis ATCC 29212, 4 to 16 mcg/mL (= mg/L), image-confirmed; insert
  revised October 2013. The label supports the synergy reading for enterococci (usually resistant; synergistic with
  penicillin G and other cell-wall agents) and for staphylococci through Table 1 footnote b (combination only). The
  label also lists Staphylococcus species among indications and says gentamicin may be considered for serious
  staphylococcal infections when less toxic drugs are contraindicated, which is a little broader than "only for
  synergy" but consistent with footnote b. **matches**.
- `inst/help/models/antibiotics.md:45`: lists gentamicin among the FDA labels read and says none gives an MBC. The
  label gives no MBC. **matches**. (Line 35's "peak 8 to 10 times the MIC" is not from this label and not checked here.)
- `inst/extdata/therapeuticRanges.csv:143`: source "Gentamicin Injection USP label (Fresenius Kabi, rev. 10/2013)";
  matrix serum; units mg/L; label_statement "Peak (30-60 min after IM) expected 4 to 6 mcg/mL in adults; avoid
  prolonged peaks above 12 and troughs above 2 mcg/mL"; notes add 3 to 5 in children. Label: adult 4 to 6 mcg/mL
  (p7), pediatric 3 to 5 mcg/mL (p9), 12 and 2 mcg/mL (p6 boxed warning, p7, p9), all image-confirmed; mcg/mL = mg/L.
  The band itself (free drug at the expected MBC) and the 8-10 x MIC target are from the repo's MBC table and
  Abdul-Aziz 2020, not this label. Protein binding 0-30% (p6) is consistent with treating free drug as close to total.
  **matches**.

Overall: matches.
