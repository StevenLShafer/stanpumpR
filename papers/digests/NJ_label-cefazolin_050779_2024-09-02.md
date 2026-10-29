---
file: NJ_label-cefazolin_050779_2024-09-02.pdf
pmid: null
doi: null
citation: "B. Braun Medical Inc. Cefazolin for Injection USP and Dextrose Injection USP in the DUPLEX container, for intravenous use, prescribing information. NDA 050779 (supplement 034, label PDF 050779s034lbl.pdf), revised 8/2024, Reference ID 5435584; Drugs@FDA date 2024-09-02."
kind: label
drugs: [cefazolin]
source_read: "text layer, whole label (21 PDF pages): Highlights p. 1; contents p. 2; 1 Indications p. 3-4; 2 Dosage and Administration p. 4-10 (Tables 1-6, p. 5-7); 3-5 Dosage forms, Contraindications, Warnings p. 10-12; 6 Adverse Reactions p. 12-14; 7 Drug Interactions and 8 Use in Specific Populations p. 14-16; 10-11 Overdosage, Description p. 16-17; 12 Clinical Pharmacology p. 17-19 (Tables 7 and 8 on p. 18, from the rendered page image; 12.4 Microbiology p. 19); 13, 15-17 p. 20-21"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# B. Braun 2024: Cefazolin for Injection and Dextrose Injection (DUPLEX) label

## Study
A PLR-format US label for a premixed dual-chamber product: lyophilized cefazolin sodium 1, 2 or 3 g with 50 mL of 4%, 3% or 2% dextrose diluent, iso-osmotic (about 290 mOsmol/kg) after activation. Intravenous only, over about 30 min. Not to be used when a dose other than 1, 2 or 3 g is needed (2.1). Initial US approval of cefazolin 1973. Recent major changes 8/2024: 2.3 perioperative dosing and 5.5 prothrombin activity.

Note on identity: the file is what its name says (cefazolin, NDA 050779, revised 8/2024), but it is not the revision stanpumpR's help names ("cefazolin injection, revised 1/2021, Reference ID 4739968"); this file is Reference ID 5435584 and its product is "Cefazolin for Injection and Dextrose Injection". Whether 4739968 is an earlier revision of this NDA cannot be told from the file.

Dosage (section 2):
- Adults, CLcr >= 55 mL/min, treatment (Table 1, p. 5): moderate to severe 500 mg to 1 g q6-8h; mild Gram-positive 250-500 mg q8h; uncomplicated UTI 1 g q12h; pneumococcal pneumonia 500 mg q12h; severe (endocarditis, septicaemia) 1-1.5 g q6h; up to 12 g/day used rarely.
- Children, CLcr >= 70 mL/min (Table 2): 25-50 mg/kg/day in 3-4 doses, up to 100 mg/kg/day.
- Adult perioperative prophylaxis (Table 3, p. 6): 1-2 g if < 120 kg, 3 g if >= 120 kg, 1/2 to 1 h before incision; 500 mg-1 g during procedures of 2 h or more; 500 mg-1 g q6-8h for 24 h; stop within 24 h (3-5 days for open-heart surgery or arthroplasty). Children 10-17 y (Table 4): 1 g < 50 kg, 2 g 50 to < 120 kg.
- Renal impairment (Table 5, p. 7), adults: CLcr 35-54 full dose q8h or longer; 11-34 half dose q12h; <= 10 half dose q18-24h. Children (Table 6): 40-70 mL/min 60% of daily dose q12h; 20-40 25% q12h; 5-20 10% q24h.

## Model and parameters
No population model. Section 12.3 (p. 17-18):

```yaml
parameters:
  - {name: Cmax 2 g, value: 280.9, unit: mcg/mL, source: "Table 7, p. 18", note: "mean (SD 45.9); 12 healthy volunteers, 2 g over 15 min"}
  - {name: Tmax 2 g, value: 0.25, unit: h, source: "Table 7, p. 18", note: "median (0.25-0.33)"}
  - {name: AUC0-inf 2 g, value: 509.9, unit: mcg.h/mL, source: "Table 7, p. 18", note: "SD 89.3"}
  - {name: t1/2 2 g, value: 2.01, unit: h, source: "Table 7, p. 18", note: "SD 0.28; terminal"}
  - {name: CL 2 g, value: 4.03, unit: L/h, source: "Table 7, p. 18", note: "SD 0.68; total clearance"}
  - {name: Vz 2 g, value: 11.50, unit: L, source: "Table 7, p. 18", note: "SD 1.53"}
  - {name: Cmax 3 g >=120 kg, value: 197.7, unit: mcg/mL, source: "Table 8, p. 18", note: "SD 46.6; model-derived; 12 patients >= 120 kg, 3 g over 30 min"}
  - {name: AUC0-inf 3 g >=120 kg, value: 598.6, unit: mcg.h/mL, source: "Table 8, p. 18", note: "SD 206.6; model-derived"}
  - {name: t1/2 3 g >=120 kg, value: 2.36, unit: h, source: "Table 8, p. 18", note: "SD 0.687; model-derived"}
  - {name: CL 3 g >=120 kg, value: 5.52, unit: L/h, source: "Table 8, p. 18", note: "SD 1.72; model-derived"}
  - {name: Vz 3 g >=120 kg, value: 17.51, unit: L, source: "Table 8, p. 18", note: "SD 3.63; model-derived"}
  - {name: Cpeak 1 g (older data), value: 185, unit: mcg/mL, source: "12.3, p. 17", note: "approximate mean; about 4 mcg/mL at 8 h"}
  - {name: serum t1/2 (older data), value: 1.8, unit: h, source: "12.3, p. 17", note: "approximate, IV"}
  - {name: urinary unchanged, value: "0.60 at 6 h; 0.70-0.80 at 24 h", unit: fraction of dose, source: "12.3, p. 18", note: "excreted unchanged"}
```
Consistency checks (derived): Table 7 CL x t1/2 / ln 2 = 4.03 x 2.01 / 0.693 = 11.7 L against Vz 11.50; dose / AUC = 2000 / 509.9 = 3.92 L/h against CL 4.03 (means of individual values, so close agreement is all that is expected). Table 8: 5.52 x 2.36 / 0.693 = 18.8 L against Vz 17.51. Also: a constant infusion of 3.5 mg/kg for 1 h then 1.5 mg/kg over 2 h gave about 28 mcg/mL at 3 h. Bile up to 5 times serum without obstruction, under 1.0 mcg/mL with obstruction; synovial fluid equals serum at about 4 h; prompt placental transfer; very low milk concentrations. No protein binding is stated, and all concentrations are total.

## Pharmacodynamics and end points
- 12.1: "an antibacterial drug"; 12.2: the PK/PD relationship has not been evaluated in patients.
- 12.4 Microbiology (p. 19): cefazolin is "a bactericidal agent" acting by inhibition of cell-wall synthesis. Active against S. aureus, S. epidermidis, S. agalactiae, S. pneumoniae, S. pyogenes, E. coli, P. mirabilis; methicillin-resistant staphylococci uniformly resistant; indole-positive Proteus, Enterobacter, Morganella, Providencia, Serratia and Pseudomonas mostly resistant. Interpretive criteria are deferred to fda.gov/STIC.
- No MIC, MBC, breakpoint or time-above-MIC target is given.

## Covariates
Renal function (dose tables 5 and 6; 8.6), body weight (prophylaxis 3 g at >= 120 kg; Table 8), paediatric age 10-17 (simulation-based exposure matching against adults given 2 g, 12.3 Specific Populations). Probenecid inhibits renal excretion (7). Pregnancy (8.1): no established drug-associated risk; crosses the placenta. No pregnancy PK.

## Caveats
A single-product label: the 1/2/3 g fixed doses and the dextrose diluent are specific to it. Table 7 is 12 healthy volunteers; Table 8 values are model-derived in 12 patients >= 120 kg, with no model described. The 185 mcg/mL and 1.8 h figures are older class statements without source. Adverse reactions in 6.1 are listed without rates except in the paediatric prophylaxis studies (nausea 14.8%, infusion-site pain 6.6%, headache 4.9%; n = 61).

## Used by stanpumpR
- inst/help/models/antibiotics.md:45 (at 8baa733): "FDA labels, read" lists "cefazolin (cefazolin injection, revised 1/2021, Reference ID 4739968)" and says none of the labels gives an MBC and the cephalosporin labels say only that the drug is bactericidal.

## Verification against stanpumpR
- antibiotics.md:45, no MBC and "only that the drug is bactericidal": this label gives no MIC or MBC and calls cefazolin "a bactericidal agent" (12.4, p. 19); 12.2 says the PK/PD relation has not been evaluated. **matches** for the content.
- antibiotics.md:45, the document identity: the help cites a 1/2021 revision with Reference ID 4739968 and calls the product "cefazolin injection"; the archived file is the 8/2024 revision, Reference ID 5435584, of "Cefazolin for Injection and Dextrose Injection" (B. Braun DUPLEX). **cannot verify** the cited revision from this file; the statement it supports is the kind that does not change between revisions, but the citation and the archive do not point to the same document.
- Not used by the repo, but relevant to drugs_cefazolin.R (Komatsu 2024 unbound model): the label's total CL 4.03 L/h, Vz 11.5 L and t1/2 2.0 h in healthy volunteers are a total-concentration check that the repo does not cite.
