# Audit of stanpumpR's chemotherapy models: instructions for an external reviewer

You are auditing the chemotherapy drugs of **stanpumpR**, an R package and Shiny app that simulates plasma (and, for some drugs, effect-site) concentrations from published population pharmacokinetic models. It is used by clinicians, teachers and researchers. Your job is to find what is **wrong, unsupported, missing or misapplied** in these models, and to say exactly where and why.

Your report will be read by another AI model (Claude), which wrote most of this code. It will check **every claim you make** against the code and the primary papers before anything changes. A claim it cannot check is discarded. So precision matters more than coverage. One finding with a correct file, line, quoted line and paper location is worth more than ten without them.

---

## What you have been given

Four files (upload all four before you start):

| File | Contents |
|---|---|
| `part1-model-code.txt` | The R source of every chemotherapy model (57 files), each line prefixed with its line number in the repository: `R/drugs_<name>.R` for every drug, model candidate and metabolite; `R/modelSelection.R` (how the program chooses among several published models of one drug); `R/chemotherapy.R`; `R/bsa-dosing.R` (per-m2 dosing); `R/physiology.R` (the physiology sliders); `R/advanceBiliaryStore.R` (oxaliplatin's gallbladder store). |
| `part2-data-and-docs.txt` | Each model's rows in the drug library (`drugDefaults_global.csv`: units, colours, bands), `physiologyParameters.csv` (the sliders' inputs), `pregnancyRegistry.csv`, `therapeuticRanges.csv` and `fdaLabels.csv`; the full chemotherapy registry (`chemotherapyRegistry.csv`, `chemotherapySources.csv` and `chemotherapyRelations.csv`: every candidate source model considered, with its readiness and the paper still to obtain); `docs/chemotherapy.md` (the design notes); and the help pages' narratives (`inst/app-help/models/chemotherapy.md`, `models/physiology.md`, `drugs/<name>.md`). |
| `part3-computed-parameters.md` | Each model's generated help page: the model run at six reference patients, giving V1-V3, CL1-CL3, micro rate constants and half-lives, absorption, the source chosen for that patient, and for a saturable drug its Vmax and Km. Use it to compare numbers with the papers without running R. Cite it as `page:<model>`. |
| `part4-evidence-cards-and-archive-index.txt` | The evidence cards for every paper on a chemotherapy model. A card is a structured digest of one PDF: the study, the model, the parameters with their table and page, what was checked against the code, and open questions. After the cards comes an **index of every card in the archive**, one line per paper (file, PMID, drugs, citation). |

**The paper archive.** The PDFs live in a private Google Drive folder, `stanpumpRPapers`, which you cannot see. The index at the end of part 4 is your view of it: a paper with a card is in the archive and has been read. A paper not in the index may still be in the archive unread, or absent. Either way, if a model needs it, name it (see *Papers to obtain* below).

---

## The rules of evidence (read these first)

1. **Every finding about the code cites the file and line and quotes the line.** Use the format `R/drugs_paclitaxelJoerger.R:142` and copy the exact line you are criticising, as it appears after the line-number prefix in part 1 or part 2. A finding without a quoted line will be discarded. Do not reconstruct code from memory; quote it from the bundle.
2. **Every claim about the literature gives the PMID and the location in the paper**: the table, figure, equation or page, and the value as printed (in the form "PMID <number>, Table 2: CL = 4.23 L/h"). If you are not certain of a PMID, write `PMID unknown` with the full citation, and never guess one. A wrong PMID costs more than a missing one, because it sends the checker to the wrong paper.
3. **Say how you know.** For each literature claim, state whether you read the paper's full text, read only its abstract, or are recalling it. A recalled number is a lead to check, not a finding.
4. **Separate the categories.** Label each finding with one of:
   - **ERROR**: the code contradicts the source it cites.
   - **DISAGREEMENT**: the code follows its source, but another source disagrees.
   - **UNSUPPORTED**: a number or assumption with no source.
   - **OMISSION**: something needed is absent.
   - **QUESTION**: you could not decide.
5. **Do not mark anything "verified" or "exact match" unless you quote both values**: the code's, with its file and line, and the paper's, with its location.
6. **Prefer fewer, checked findings to many plausible ones.** Previous external audits produced fabricated PMIDs, invented functions and "exact match" stamps on numbers that did not match. Every such claim was caught, and it cost the whole report credibility.

---

## Conventions the library follows deliberately (do not report these as errors)

Read `docs/chemotherapy.md` and each drug file's header for the full reasoning. In brief:

- **The reference patient is a 70 kg, 170 cm, 35-year-old man.** A model without a size covariate is scaled to the patient's fat-free mass (Al-Sallami), anchored at the source study's own cohort, so that a patient like the cohort gets the published values. A model with its own size covariate keeps it as fitted.
- **The program chooses the model, never the clinician.** Where several published models of one drug exist (etoposide, carboplatin, melphalan, methotrexate, vincristine, oxaliplatin, fluorouracil, cyclophosphamide, paclitaxel, temozolomide, doxorubicin), `R/modelSelection.R` picks the one whose source population contains the patient and was fitted to more patients. You may question a group's ranges, or whether a choice is sensible for a patient, but not the principle.
- **Dose basis.** Unless a paper states otherwise, doses are in the FDA-approved product's units (usually the salt). Where a paper used another basis (bendamustine, fludarabine, Joerger's doxorubicin), the code converts, and the header says so.
- **No chemotherapy model has an effect site**, and no exposure target (AUC, time above a threshold, a methotrexate rescue threshold) is drawn as a concentration band.
- **Doses per m2 use Mosteller's body surface area** (Shafer's decision of 2026-11-23). Models fitted on Du Bois BSA keep Du Bois.
- **Dose units are inclusive of the FDA label and of clinical practice.** Every intravenous chemotherapy drug offers mg, mg/kg and mg/m2 and their hourly rates.
- **Genotype guidelines (DPYD, UGT1A1, TPMT/NUDT15) are dose recommendations, not clearances**, so none is applied as a clearance. You are invited to challenge this, with quantified PK data (see question 4).
- **Out of scope:** `cube()` (the eigenvalue solver, settled), the numerical engines, the user interface, and code style.
- **Previously decided by Steven Shafer** (you may comment, but label it "previously decided"):
  - carboplatin (Duffull): Clic set to Table 2's 4.4 L/h;
  - cisplatin (Urien): Table 4's V1 exponent 1.60;
  - topotecan (Mould): the printed CL equation;
  - vinorelbine (Variol): Table 3 values without covariates;
  - etoposide (Krogh-Madsen): Table 2's renal slope;
  - etoposideDuong: WT_STD 15.5 kg;
  - cytarabine: the switch from Periclou to Krogh-Madsen at 19 years;
  - thioguanine: the oral model derived from Kitchen's IV disposition with Lowe's absorption;
  - oxaliplatin (Bastian): the gallbladder store as published;
  - doxorubicin: Perez-Blanco as a third model.

**Papers excluded by policy:** nothing co-authored by Gideon Koren may be cited or used, in any author position. Check author lists.

---

## The questions

Work drug by drug for questions 1-3, then answer 4-8 across the class. The drugs and their model functions:

- **Groups, where the program chooses between models:**
  - etoposide: `etoposideKroghMadsen`, `etoposidePediatric`, `etoposideDuong`
  - carboplatin: `carboplatinDuffull`, `carboplatinPediatric`, `carboplatinPeng`
  - melphalan: `melphalanNath`, `melphalanPediatric`
  - methotrexate: `methotrexateIbarra`, `methotrexatePediatric`
  - vincristine: `vincristineIgarashi`, `vincristinePediatric`
  - oxaliplatin: `oxaliplatinBastian`, `oxaliplatinNikanjam`
  - fluorouracil: `fluorouracilBriki`, `fluorouracilTan`
  - cyclophosphamide: `cyclophosphamideJoerger`, `cyclophosphamideMcCune`
  - paclitaxel: `paclitaxelJoerger`, `paclitaxelCrombag`
  - temozolomide: `temozolomideOstermann`, `temozolomidePanetta`
  - doxorubicin with doxorubicinol: `doxorubicinJoerger`, `doxorubicinPediatric`, `doxorubicinPerezBlanco` (each with its own doxorubicinol)
- **Single models:** cladribine, busulfan, pemetrexed, docetaxel, doxorubicinLiposomal (with doxorubicinLiposomalFree), irinotecan (with irinotecanSN38), topotecan, vinorelbine, cisplatin (with cisplatinBound), daunorubicin, gemcitabine (with gemcitabineDFDU), capecitabine (with capecitabine5FU), paclitaxelProteinBound, bendamustine, fludarabine, cytarabine, ifosfamide, epirubicin, dacarbazine, bleomycin, thioguanine, thioguanineIV.

### 1. Are the pharmacokinetics correct?

For each model, compare every parameter, covariate equation, reference value, unit and dose-basis conversion in its `R/drugs_<name>.R` with the source it cites. Check:
- **Transcription:** do the numbers match the table, including a footnote or erratum that changes them?
- **Structure:** the compartments, how elimination is placed, and parallel or saturable paths.
- **Covariates:** the equation's form (power or linear, the centring value), the units (mL/min against L/h, BSA against weight), and whether a covariate printed in the paper was omitted, with the header's reason checked.
- **Clinical sense:** whether the model behaves sensibly over the population it will be applied to. Compare the computed tables in part 3: does the child or infant column look plausible, and does the obese column?
- **Analyte, matrix and units:** what is plotted (parent or metabolite, total or unbound, platinum or parent drug, plasma or whole blood) against what the source measured.

### 2. Do they agree with the published literature?

Beyond each model's own source, compare its **clearance** with other well-conducted studies of the same drug in a similar population. **Compare clearance first.** Volumes and half-lives vary with sampling design and assay sensitivity (early studies sampled late and venously; later assays follow the drug longer), so treat a difference in V1 or the terminal half-life as weak evidence unless the designs are comparable. For each disagreement give both sources' values, populations and PMIDs, and say which you think is more applicable to the patients stanpumpR will simulate, and why.

### 3. Is the necessary literature in the archive?

Using the index at the end of part 4 and the registry's `to_obtain` column (part 2), list for each model the papers it depends on that have **no card**: its own source, an erratum or supplement, a validation study, or the source of a fixed value. Give each paper's PMID and full citation, and say what it would settle.

### 4. CYP metabolism, drug interactions and genotypes

At present, **no chemotherapy model reads a genotype or applies a drug interaction.** The genotype fields that exist in the app are CYP2D6, CYP2C19, CYP2C9, CYP2B6, VKORC1 and CYP4F2. The only interaction engine covers the antiseizure drugs as victims. Answer:

- **(a) Chemotherapy drugs as victims of inhibition or induction.** Which have a **quantified** pharmacokinetic interaction (a fold-change in clearance or AUC, with PMID and table) that a clinician is likely to meet? Examples: enzyme-inducing antiseizure drugs (carbamazepine, phenytoin, phenobarbital) on irinotecan, paclitaxel, docetaxel, vincristine, cyclophosphamide, ifosfamide and others; strong CYP3A4 inhibitors on docetaxel or vincristine; CYP2C8 on paclitaxel. For each, give the factor, the population, and whether it applies to clearance, bioavailability or metabolite formation.
- **(b) Chemotherapy drugs as perpetrators.** Do any chemotherapy drugs in the library change the clearance of other library drugs in a quantified way?
- **(c) Genotypes.** Which polymorphisms have a **quantified effect on a PK parameter** in a population model, as opposed to a dose recommendation? Examples: DPYD on 5-FU clearance; UGT1A1*28 on SN-38 glucuronidation; CYP2B6 on cyclophosphamide or ifosfamide activation; TPMT/NUDT15 on thioguanine nucleotides; ABCB1, SLCO1B1 and CYP3A5 on others. Give the model's PMID, the parameter, and the effect size by genotype.
- **(d) Your recommendation.** Is the current policy (genotypes not applied) defensible for each drug, or should a genotype field or an interaction be added? Say which, with the evidence.

### 5. The physiology sliders

The Physiology panel has sliders for liver, renal, cardiac and GI function, and for temperature. They change a drug's parameters through `R/physiology.R`:
- Hepatic clearance follows the well-stirred model, using the drug's extraction ratio. It is either measured, or computed as E = fraction_hepatic x CL / (Q_H x B:P).
- Renal function scales only the renal share of clearance.
- Cardiac output scales Q2, Q3 and V1.
- Temperature scales metabolic capacity.

Each drug's inputs are its row in `physiologyParameters.csv` (part 2). A drug the sliders cannot change correctly is marked unsupported. These include drugs with a metabolite formed at its own rate, a biliary store, or saturable elimination.

For each chemotherapy model, check:
- whether its row exists;
- whether its fractions renal and hepatic, extraction ratio and blood:plasma ratio match the literature (PMID and location);
- whether it is correctly supported or unsupported;
- whether a non-hepatic, non-renal route (hydrolysis, deamination by cytidine deaminase, spontaneous degradation of platinum species, Hofmann-type chemistry) is correctly left out of the slider;
- whether the well-stirred assumption is reasonable for it.

Flag any drug whose sliders would move the clearance the wrong way or by an implausible amount. Use `page:<model>` and the slider code to estimate the effect, and show your arithmetic.

### 6. Pregnancy and therapeutic ranges (brief)

Note any chemotherapy row in `pregnancyRegistry.csv` or `therapeuticRanges.csv` (part 2) that misstates its source, or a quantified pregnancy PK effect that is missing. Give the PMID.

### 7. Missing chemotherapy drugs

Which commonly used cytotoxic chemotherapy drugs are **not** in the library? Examples might include ixabepilone, eribulin, trabectedin, cabazitaxel, mitomycin, mitoxantrone, idarubicin, pralatrexate, azacitidine, decitabine, clofarabine, nelarabine, arsenic trioxide, carmustine, lomustine, thiotepa, treosulfan, asparaginase, hydroxyurea, mercaptopurine, oral etoposide or topotecan, and others you know. Check the registry (part 2) first, because some are already catalogued with a blocker. For each missing drug, give:
- whether a published **population PK model with every parameter printed** exists, with PMID, population, structure, and whether its full text is open access;
- its likely value to stanpumpR's users (perioperative, ICU, oncology pharmacy, teaching);
- any obstacle: no IV model, only an NCA, a saturable or transit structure, or an unclear dose basis.

### 8. Papers to place in the archive

Consolidate everything from questions 1-7 into one list of papers Steven Shafer should download into `stanpumpRPapers`, **ordered by value**. For each paper give the PMID, full citation, DOI if known, the drug, and the reason: a source, an erratum, a validation study, an interaction, a genotype effect, a physiology input or a new drug. Also mark each as **open access** (PubMed Central ID) or not. Do not list a paper that the index in part 4 already shows has a card.

---

## Format of your report

Write one markdown file named `chemotherapy-audit-<your model name>-<yyyy-mm-dd>.md`, using exactly these sections, so that Claude can work through it mechanically:

```markdown
# Chemotherapy audit: <reviewer model and version>, <date>

## Summary
Five to ten lines: the most important findings, by ID.

## Findings
One block per finding, numbered F1, F2, ... in order of importance.

### F1. <one-line title>
- Category: ERROR | DISAGREEMENT | UNSUPPORTED | OMISSION | QUESTION
- Severity: high (a wrong curve for an ordinary patient) | medium | low
- Drug / model: <drug>, <model function>
- Code: `<path>:<line>`
- Quoted line: `<the exact line as it appears in the bundle>`
- Source: PMID <n>, <table/figure/page>, value as printed: <value>
- How I know: read full text | read abstract | recalled
- What is wrong: <one or two sentences, with arithmetic where relevant>
- Proposed change: <exact replacement value or equation, or the paper to read first>
- Confidence: high | medium | low

## CYP, interactions and genotypes
A table: drug | effect | perpetrator or genotype | parameter | factor | population | PMID and location | recommendation.

## Physiology sliders
A table: model | row present? | fraction renal / hepatic / E / B:P as coded | literature value (PMID, location) | supported correctly? | comment.

## Missing chemotherapy drugs
A table: drug | best population PK model (PMID) | population | structure | every parameter printed? | open access? | obstacle | value.

## Papers to place in stanpumpRPapers
A table, ordered by value: rank | PMID | citation | DOI | drug | reason | open access (PMCID).

## Questions for Steven Shafer
Decisions only he can make, each with the options and your recommendation.

## What I did not check
The models or questions you did not reach, so the next reviewer can start there.
```

Keep the report self-contained: Claude will see only your markdown file and the repository, not this conversation.
