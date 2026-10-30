---
file: NJ_label-ceftriaxone_050585_2018-07-13.pdf
pmid: null
doi: null
citation: "Genentech / Hoffmann-La Roche. ROCEPHIN (ceftriaxone sodium) for injection, prescribing information. NDA 050585 (label PDF 050585s068lbl.pdf), revised July 2018, Reference ID 4290275; Drugs@FDA date 2018-07-13."
kind: label
drugs: [ceftriaxone]
source_read: "text layer, whole label (24 PDF pages, old non-PLR format): Description p. 1; Clinical Pharmacology p. 1-4 (Tables 1-2 p. 2, Tables 3-4 p. 3, middle ear and calcium interaction p. 3-4); Microbiology p. 4-8 (Table 5 interpretive criteria p. 6-7, Table 6 QC p. 8); Indications p. 8-10; Contraindications p. 10-11; Warnings p. 11-12; Precautions incl. renal/hepatic, Pregnancy, Nursing, Pediatric and Geriatric Use p. 12-16; Adverse Reactions p. 16-18; Overdosage and Dosage and Administration p. 18-20; Compatibility p. 20-22; Clinical Studies p. 22-23; References p. 23-24. Tables 1-4 (p. 2-3) and Table 5 (p. 6-7) read from rendered page images, since the text layer scrambles Tables 1, 3 and 4"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: not-applicable
---

# Roche/Genentech 2018: Rocephin (ceftriaxone) label

## Study
The US innovator label for Rocephin powder for IV or IM injection (500 mg and 1 g vials listed under How Supplied), revised July 2018. The file is what its name says (NDA 050585, Reference ID 4290275 on every page). Sodium about 83 mg (3.6 mEq) per gram of ceftriaxone activity.

Dosage (p. 19-20): adults 1-2 g once daily (or divided twice daily), not over 4 g/day; uncomplicated gonorrhoea 250 mg IM once; surgical prophylaxis 1 g IV once, 1/2 to 2 h before surgery. Children: 50-75 mg/kg/day (max 2 g) for skin and serious non-meningeal infection; otitis media 50 mg/kg IM once (max 1 g); meningitis 100 mg/kg loading then 100 mg/kg/day (max 4 g). IV infusion over 30 min (60 min in neonates). No adjustment for renal or hepatic impairment alone; with both, not over 2 g/day (p. 13). Contraindicated in premature neonates to 41 weeks postmenstrual age, hyperbilirubinaemic neonates, and neonates (<= 28 days) needing calcium-containing IV solutions (p. 10).

## Model and parameters
No compartmental model. Clinical Pharmacology (p. 1-3; healthy adults unless stated):

```yaml
parameters:
  - {name: t1/2 healthy, value: "5.8-8.7", unit: h, source: "p. 2 text; Table 4, p. 3", note: "range over 0.15-3 g doses"}
  - {name: V healthy, value: "5.78-13.5", unit: L, source: "p. 2 text (5.78); Table 4 prints 5.8-13.5", note: "apparent volume of distribution"}
  - {name: CL healthy, value: "0.58-1.45", unit: L/h, source: "p. 2; Table 4", note: "plasma (total) clearance"}
  - {name: CLrenal healthy, value: "0.32-0.73", unit: L/h, source: "p. 2"}
  - {name: urinary unchanged, value: "0.33-0.67", unit: fraction of dose, source: "p. 2", note: "remainder biliary, to faeces as inactive compounds"}
  - {name: protein binding, value: "0.95 at < 25 ug/mL; 0.85 at 300 ug/mL", unit: fraction bound, source: "p. 2", note: "reversible, concentration-dependent"}
  - {name: Cp 1 g IV 0.5 h, value: 151, unit: ug/mL, source: "Table 1, p. 2", note: "average; 30-min infusion; 111 at 1 h, 67 at 4 h, 43 at 8 h, 28 at 12 h, 18 at 16 h, 9 at 24 h"}
  - {name: Cp 2 g IV 0.5 h, value: 257, unit: ug/mL, source: "Table 1, p. 2", note: "average; 192 at 1 h, 117 at 4 h, 74 at 8 h, 46 at 12 h, 31 at 16 h, 15 at 24 h"}
  - {name: Cp 0.5 g IV 0.5 h, value: 82, unit: ug/mL, source: "Table 1, p. 2", note: "average; 5 at 24 h"}
  - {name: Cp 1 g IM peak, value: 76, unit: ug/mL, source: "Table 1, p. 2", note: "at 2 h; IM completely absorbed, Tmax 2-3 h"}
  - {name: accumulation, value: "0.15-0.36", unit: fraction above single dose, source: "p. 2", note: "0.5-2 g every 12-24 h, IV or IM"}
  - {name: t1/2 elderly, value: 8.9, unit: h, source: "Table 4, p. 3", note: "mean age 70.5 y; CL 0.83 L/h, V 10.7 L"}
  - {name: t1/2 haemodialysis, value: 14.7, unit: h, source: "Table 4, p. 3", note: "CLcr 0-5 mL/min; CL 0.65 L/h, V 13.7 L"}
  - {name: t1/2 severe renal, value: 15.7, unit: h, source: "Table 4, p. 3", note: "5-15 mL/min; CL 0.56, V 12.5"}
  - {name: t1/2 moderate renal, value: 11.4, unit: h, source: "Table 4, p. 3", note: "16-30 mL/min; CL 0.72, V 11.8"}
  - {name: t1/2 mild renal, value: 12.4, unit: h, source: "Table 4, p. 3", note: "31-60 mL/min; CL 0.70, V 13.3"}
  - {name: t1/2 liver disease, value: 8.8, unit: h, source: "Table 4, p. 3", note: "CL 1.1 L/h, V 13.6 L"}
  - {name: paediatric meningitis Cmax, value: "216 (50 mg/kg); 275 (75 mg/kg)", unit: ug/mL, source: "Table 3, p. 3", note: "t1/2 4.6 / 4.3 h; CL 49 / 60 mL/h/kg; V 338 / 373 mL/kg; CSF 5.6 / 6.4 ug/mL"}
```
The text layer scrambles Tables 1, 3 and 4 (it puts the 24-h column of Table 1 and the rows of Tables 3-4 out of place); the values above are from the rendered pages. Bile after 1 g IV, 1-3 h: gallbladder 581, common duct 788, cystic duct 898 ug/mL, gallbladder wall 78.2 ug/g, against plasma 62.1 ug/mL. Middle-ear fluid after 50 mg/kg IM in 42 children: peak 35 (SD 12) ug/mL at 24 h, 19 (7) at 48 h, half-life 25 h. Not removed by haemodialysis; elimination markedly reduced in 6 of 26 dialysis patients; probenecid does not alter elimination. Derived consistency check: healthy-range midpoints CL 1.0 L/h and V 9.6 L give t1/2 = 0.693 x 9.6 / 1.0 = 6.7 h, inside 5.8-8.7 h (derived).

## Pharmacodynamics and end points
- Microbiology (p. 4): ceftriaxone is "a bactericidal agent" acting on cell-wall synthesis. No MBC is given. S. aureus and S. epidermidis are in the list of organisms active in vitro and in clinical infection (p. 5); staphylococcal susceptibility is to be deduced from penicillin and cefoxitin or oxacillin (p. 7).
- Table 5 (p. 6-7, rendered), MIC interpretive criteria, mcg/mL, S / I / R: Enterobacteriaceae <= 1 / 2 / >= 4 (based on 1 g IV q24h; intermediate isolates 2 g q24h); Haemophilus influenzae <= 2 (2 g q24h); N. gonorrhoeae <= 0.25; N. meningitidis <= 0.12; S. pneumoniae meningitis <= 0.5 / 1 / >= 2, non-meningitis <= 1 / 2 / >= 4; beta-haemolytic streptococci <= 0.5; viridans streptococci <= 1 / 2 / >= 4; anaerobes (agar) <= 1 / 2 / >= 4. Disk zone criteria (30 mcg disk) also given.
- No PK/PD target (time above MIC) is stated.

## Covariates
Age (elderly t1/2 8.9 h), renal impairment and dialysis (t1/2 11.4-15.7 h, no dose change up to 2 g/day), liver disease, paediatric meningitis (per-kg values in Table 3). Pregnancy (p. 15): Category B; crosses the placenta (p. 2); no pregnancy PK. Low concentrations in milk.

## Caveats
A 2018 label in the old format, with PK data from the original 1980s studies and no SDs or n for the healthy-subject ranges. All concentrations are total; binding is saturable, so the free fraction rises at high concentrations. The susceptibility criteria are FDA's 2018 recognition and predate the current CLSI and EUCAST tables (the label's MSSA listing in particular). The "ND" cells in Table 1 are not determined, not zero.

## Used by stanpumpR
- inst/help/models/antibiotics.md:46 (at 8baa733): "FDA, still to read. The ceftriaxone label, and the interpretive criteria (STIC) page for each drug". The context file's used_for is "To read". No number in the repo is drawn from this label.

## Verification against stanpumpR
- antibiotics.md:46: the line records that the label had not been read; the label is now archived and digested here, so the line is out of date but makes no factual claim to check. **not applicable**. (For the band-building statement in antibiotics.md:45, this label agrees with the other cephalosporin labels: no MBC, "bactericidal" only.)
- Cross-checks the repo does not cite: R/drugs_ceftriaxone.R uses Sanz-Codina 2023 (six healthy men, CL 1.21 L/h, Vc 5.76 + Vp 2.97 = 8.73 L). Both lie inside the label's healthy ranges (CL 0.58-1.45 L/h, V 5.78-13.5 L). The repo's threshold rests on the Enterobacterales susceptible breakpoint of 1 mg/L from CLSI 2026 / EUCAST v16.1; the label's Table 5 gives the same <= 1 mcg/mL. The label's 95% binding below 25 ug/mL is consistent with the header's free fraction of about 0.065 at a free concentration of 1 mg/L. The label lists S. aureus among susceptible organisms, whereas R/antibioticMbc.R:217-227 excludes MSSA from the band on EUCAST 2026 evidence of bacteriostasis only; this is a difference between a 2018 label and later guidance, not a transcription question.
