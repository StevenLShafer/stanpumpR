# Chemotherapy: audit of the 2026-11-04 recommendations, and the plan

Drafted by Claude Code, 2026-11-05, at the request of Steven L. Shafer.

## 1. What was supplied, and what was done with it

Shafer supplied *Chemotherapy pharmacokinetics for stanpumpR: final recommendations after audit*
(4 November 2026), a handoff prepared with ChatGPT and Gemini that itself corrects an earlier
Gemini ("Argon") audit of a seed corpus. He asked that it be reviewed carefully, audited, and
incorporated. Under the repository's rule on external LLM reviews (`AGENTS.md`, *Adversarial
review by another LLM*), every specific claim in it was treated as a lead: each PMID, author,
year, DOI and PMCID was checked against PubMed, each statement about the code against the code,
and no number was taken from it.

The handoff's central recommendation is sound and is what was done: build a registry of sources
and candidate models first, keep every unresolved model visible with its blocker, and implement a
model only after its equations, units, covariates, observation and population have been read in
the primary document. What was built:

- `data-raw/chemotherapyRegistry.R`, which writes three tables to `inst/extdata/`:
  `chemotherapySources.csv` (111 documents: 89 PubMed records, 3 guidelines outside PubMed,
  5 regulatory documents, 14 labels), `chemotherapyRegistry.csv` (82 candidate models of 35
  drugs and products) and `chemotherapyRelations.csv` (which document supports which drug, in
  what role, with cross-cutting guidance marked `global`).
- `R/chemotherapy.R` (readers), `tests/testthat/test-chemotherapy-registry.R`, and the help page
  `inst/help/models/chemotherapy.md`, whose model table and reading list are generated from the
  registry.
- The category `Chemotherapy` (`CHEMOTHERAPY_DRUG_CATEGORY`) and its first two drugs, the only
  two whose complete models could be read here (section 6).

The handoff proposed a JSON template extension (its section 9). The repository has no such
template; its pattern for an evidence-gated drug class is the registry CSVs of the anticoagulants,
antihypertensives, vasoactive drugs and Parkinson drugs, so the same pattern was used, with the
handoff's fields (review depth, source roles, engine assessment, readiness) as columns. The
readiness states follow the existing registries (`needs_full_text`, `needs_engine`,
`complete_not_implemented`, `implemented`), with `no_source_identified` in place of
`no_suitable_model`, because a review that found no source has not shown that none exists.

## 2. The bibliography

All 89 PMIDs resolve, and each is about the drug and population the handoff says.

**The handoff's own corrections, all confirmed:** C07's first author is de Jonge (2004); P15 is
Kontny 2013, *Cancer Chemother Pharmacol* 71(3):749-763; P20 is Oyaga-Iriarte 2019, *Eur J Clin
Pharmacol* 75(4):529-542; P05's pages are 763-778; C26 is Hsu 2018; C05 reports 40 patients; C06
reports 56 and an enterohepatic recirculation component; P18's ages are 0.04-33.9 years with a
saturable beta-tubulin binding compartment; C37b is cladribine in multiple sclerosis; P19 is Klein
CE, *Clin Pharmacol Ther* 72(6):638-647, 2002. The 19 DOIs of its section 6 and every PMCID it
gives match PubMed.

**Errors the handoff itself introduced** (first authors in its section 7, each confirmed
directly against PubMed):

| ID | PMID | Handoff | PubMed |
|---|---|---|---|
| C04 | 30261554 | Nath 2019 | **Duong** JK (Nath is third author), *Br J Clin Pharmacol* 2019;85:136-146 |
| C22 | 22212298 | Bogason 2012 | **Krogh-Madsen** M (no Bogason among the authors), *Cancer Chemother Pharmacol* 2012;69:1155-63 |
| C30 | 20490798 | You 2011 | **Urien** S (no You among the authors), *Cancer Chemother Pharmacol* 2011;67:597-603 |
| C42 | 6183016 | Crooke 1982 | **Hall** SW (no Crooke among the authors), *Cancer Chemother Pharmacol* 1982;9:22-25 |

This is the pattern the repository's rule anticipates: a document that announces itself as the
corrected version still carries confabulated authors. The topics of all four are right; only the
names were invented.

**Smaller points.** For twelve records (P05, P12, P23, G01, G03, G08, G10, C04, C05, C09, C20,
C30) PubMed's date is the e-pub year, one before the print year the handoff gives; the sources
table uses the print year. N02 (Kitchen 1997) is titled a paediatric study, but its patients were
4-25 years old (median 18) and mostly had solid tumours. The erratum the handoff reports for C02
(Duffull 1997) could not be found: PubMed has no erratum for it, linked or under that journal's
1997 errata (searched 2026-11-06), and neither the paper's PDF nor Duffull's 1997 review in *Clin
Pharmacokinet* mentions one. The claim is unconfirmed and is not repeated in the registry. Several
records have a PMCID the handoff does not list (P04 PMC1884526, P13 PMC13197298, and others); the
sources table carries PubMed's.

## 3. The handoff's statements about the code

Its section 5 was checked against the current checkout and is right in substance; line numbers
have moved by up to a dozen lines since the commit it inspected. Confirmed: `advanceMichaelisMenten()`
validates a `michaelisMenten` block with lower-case `vmax`, `km` and an optional `clLinear`, and
carries a central and at most one peripheral compartment; `advanceTransitChain()` takes an integer
`n` and `ktr`, the first `n - 1` compartments emptying at `ktr` and the last at `ka_PO`, and
`simCpCe()` dispatches to it directly; `getDrugPK()` refuses a transit chain alongside an external
metabolite, Michaelis-Menten elimination, parallel systems and the rest; the metabolite interface
resolves one level only (`docs/adding-a-drug.md`); `R/renalFunction.R` defines
`creatinineClearanceCG`, `egfrSchwartz`, `egfrBedsideSchwartz`, `bsaDuBois`, `egfrCKDEPI2009`,
`egfrDeindexed` and `egfrMDRD`, and its `ekfc` is a local helper for a child's normal creatinine,
not an EKFC GFR; `pkSizeFactors()` returns `volume`, `clearance`, `ffm`, `ffmReference` and
`pkWeight`, and no `bsa`, `ibw` or `lbm`. Its warnings against the earlier audit's invented
structures (`drug_table.xlsx`, `drugPkSelector.R`, `.PK_CREATORS`, `Vmax12`/`Km12`) are correct:
none exists, and none was created.

Its two genuine engine points stand and are recorded per model in the registry's `engine` column:
a cascade of metabolites (irinotecan, capecitabine, cyclophosphamide), enzyme autoinduction
(cyclophosphamide, ifosfamide, thiotepa), saturable distribution in three compartments
(nab-paclitaxel, probably solvent paclitaxel), saturable tissue binding (vincristine),
enterohepatic recirculation (oxaliplatin), carrier release (the liposomal products) and
intracellular formation (gemcitabine) each need an engine the library does not have, and none is
to be approximated without measuring the approximation against the source's equations.

## 4. What could be read here

None of the papers is in the `stanpumpRPapers` archive. The network policy blocks the NCBI
E-utilities and the publishers, so the only full text available was what the PubMed tool returns
from PubMed Central, which usually drops tables and equations, and with them the parameters. The
PubMed Central texts were checked for whether they print the complete typical-patient model (a
lead from a delegated check, to be confirmed against the PDF before any number is used):

| Source | Text available | Complete? |
|---|---|---|
| Lindemalm 2005, cladribine (N04) | PMC1079880 | Yes: Table 3 is in the text |
| Briki 2026, 5-FU (P10) | PMC13661680 | Yes, with the table's subscripts stripped; the parameters are identified by table order and the text's own percentages (since confirmed on the PDF) |
| Blackman 2026, HDMTX adults (P13) | PMC13197298 | All values printed; the covariate exponents identified only by table position, and the sex term (0.01) hard to square with the text's 25.7-point improvement for sex |
| Taylor 2020, MTXPK.org (C12) | PMC7484917 | Values printed, the final covariate equation stripped |
| Langenhorst 2019, fludarabine (C34) | PMC6451721 | Values printed, the clearance equation stripped |
| Nijstad 2022, vincristine (P18) | PMC9556337 | Values printed, the saturable-binding equation stripped |
| McCune 2014, busulfan (P23) | PMC3946385 | No: Table 2 (V1, Q, V2, maturation) absent |
| Tan 2026, 5-FU (P09) | PMC13356066 | No: the parameter table absent |
| Taylor 2023 (C13), Beechinor 2019 (C15), Langenhorst 2020 (C35), Thompson 2025 (P05) | yes | No |
| Pérez-Blanco 2016 (P16), Duong 2019 (C04) | yes | The tool returned no text |

Everything else is abstract only. The registry records each paper's review depth, and the help
page lists the papers to obtain, first wave first, which is what Shafer offered to supply from
Lane Library.

## 5. Decisions

- **No number from the handoff or its audit** is used. Its section 7 deliberately omits
  coefficients; every one in the earlier audit is unverified.
- **No implementation from an abstract.** A model goes into the library only from a full text
  in which every parameter it uses was read.
- **No band from an exposure target.** An AUC target, a time above a threshold, or a rescue
  threshold is not a concentration band (the handoff's section 10.5).
- **No effect site.** No chemotherapy `ke0` is derived from a half-life.
- **Genotype guidelines are not clearances** (DPYD, UGT1A1, TPMT/NUDT15).
- **Cystatin C, white-cell count, performance status and genotypes** that some models
  need are not Patient Profile fields (albumin became one on 2026-11-06, section 16); such a model either waits for the field or is implemented
  at the source's reference value, saying so, never with serum creatinine standing in for
  cystatin C (the chlorthalidone decision, `docs/antihypertensives.md`).

## 6. The first two drugs

Both were implemented from the PubMed Central text, read in this session. Shafer supplied
Briki 2026's PDF on 2026-11-06, and its Table 3 (with subscripts) and printed equations confirm
every value and identification below; fluorouracil's source is now `parameters_verified`.
Lindemalm 2005's PDF, supplied the same day, confirms cladribine's Table 3 likewise; both
sources are `parameters_verified`. That is a departure from the rule that a model's
numbers are confirmed on the PDF (`AGENTS.md`, *Source papers*), taken because the publisher's
text is not the scrambled rendering the rule guards against, and recorded here so that the PDF
check is not forgotten.

- **`fluorouracil`** (`R/drugs_fluorouracil.R`; Briki 2026, P10). Two compartments with
  parallel Michaelis-Menten and linear elimination, run by `advanceMichaelisMenten()` through
  its `clLinear` and one peripheral compartment, so no engine change was needed. Table 3 lost
  its subscripts in that text; each value was identified from its units and the text, and the
  identification is held by the test to the four percentages the paper states (men's Vmax +18%,
  80 against 40 years -17%, an 80-year-old woman against a 40-year-old man -30%, BSA 1.5 against
  2.0 -53%). The size covariates (BSA on the linear clearance, weight on V2) are evaluated at the
  pharmacokinetic weight with the switch on, as for cefazolin; Vmax is not scaled by fat-free
  mass, because its sex term already carries lean mass and scaling it again would count sex
  twice; age is held to the 24-90 years studied. The test checks the engine's plateau during a
  46-h infusion against an independent solution of R = CL C + Vmax C/(Km + C). Not represented:
  DPD deficiency, the slow rise of the plateau during an infusion, circadian variation. The AUC
  target (20-29 mg h/L over 46 h) is recorded, `not_applicable`, and not drawn.
- **`cladribine`** (`R/drugs_cladribine.R`; Lindemalm 2005, N04). Three compartments, IV,
  subcutaneous (F 1, ka 2.48/h) and oral solution (F 0.353, ka 1.31/h), no covariates; its
  half-lives reproduce the text's 0.2, 1.3 and 16 h. No renal term, although about half its
  clearance is renal, because no patient had severe impairment; the oral route is the solution,
  not the multiple-sclerosis tablet.

Neither drug has measured pharmacokinetics in pregnancy (registry rows `unknown`).

## 7. Carboplatin

Shafer supplied Ekhart 2006, Chatelut 1995, Duffull 1997 and Schmitt 2009 (2026-11-06) and, of
the four, chose Duffull 1997 for the library's `carboplatin` (`R/drugs_carboplatin.R`): the only
complete fitted model with a renal term, though in only twelve women. Ekhart 2006 is complete
but has no renal term (fitted in creatinine clearance above 50 mL/min); Chatelut 1995 prints a
renal clearance formula but no fitted volumes, only means of individual fits; Schmitt 2009 needs
cystatin C and does not print its volumes. Duffull's paper contradicts itself on the
intercompartmental clearance (equation 5.4 against Table 2), and Shafer chose Table 2's level in
the equation's form. The erratum the handoff reported for it was not found.

## 8. Busulfan

McCune 2014 (`R/drugs_busulfan.R`), from the article's Table 2 as Shafer transcribed it from the
PDF (2026-11-06), the Methods in PubMed Central and the supplement he supplied. The clearance falls
to 0.932 of its first value from 6 h and to 0.919 from 36 h after the course starts; no engine
carried a step timed from a drug's own first dose, so `getDrugPK()` gained a `courseSteps` block
(one PK set per step and the minutes after the first dose at which it takes over) and
`simCpCe()` builds the switching events from the dose table, as it does for the antiseizure
interactions, on the event engine, which advances the steps exactly. Table S7's worked example
does not follow the fat-free-mass equation the Methods state (49.7 against 56.1 kg for its man);
the stated equation is used, and the example's dose still gives its target within 3%.

## 9. High-dose methotrexate in children and young adults

Taylor 2023 (`R/drugs_methotrexatePediatric.R`, named for its population), from the PDF Shafer
placed on Google Drive and its supplement. Three compartments by BSA over 1.73 m2 and the
patient's own creatinine referenced to 40 uM. Its Table 2 footnote describes Q3 as chained to
V2, which the stated ADVAN11 TRANS4 and the supplement contradict; equation 3 and Table 2 differ
on the Down syndrome factor (0.83, 0.84). The covariates with no Patient Profile field (Down
syndrome, pleural effusion, infusion length) are held at the reference; severe
hypoalbuminaemia reads the Serum albumin field (section 16). The model
reproduces the paper's Table 5 end-of-infusion mean for 5 g/m2 over 24 h (85.9 against 84.8
umol/L); its later means lie above the typical curve, as means of individual predictions should.

## 10. High-dose methotrexate in adults

Ibarra 2023 (`R/drugs_methotrexate.R`), from the PDF Shafer supplied: three compartments
allometric in weight, creatinine and albumin on clearance. Table 2 reproduces the paper's own
half-lives (1.5, 4.9, 40 h) and Vss (20 L). Its reference creatinine is not printed as a number;
the healthy-adult means in the same paragraph of the Methods (0.92 and 0.77 mg/dL) are used.
68% of the courses were hypoalbuminaemic, so the albumin term matters; it reads the Serum
albumin field (section 16).

## 11. A second 5-FU model

Tan 2026 (`R/drugs_fluorouracilTan.R`), from the PDF Shafer supplied, offered beside Briki 2026
as levodopaSimon is beside levodopa: two compartments with saturable elimination alone, BSA on
Vmax. It reproduces the paper's Table 3 median AUCs to within 2%. For the same patient Briki's
model gives about 14% less exposure; the help pages of both say so, and neither is preferred.

## 12. Etoposide in children

`etoposidePediatric` is Urien 2011 (C30; PDF supplied 2026-11-06, Table 3 and
the allometric equation read on the page images): two compartments, every
parameter allometric in body weight at 70 kg (3/4 on CL 2.05 L/h and Q 0.68
L/h, 1 on Vc 9.3 L and Vp 5.7 L). The paper's three misprints (a 1/4
exponent in the Methods, CL70 printed for every parameter in Table 3's
covariate column, and a 15-kg check of 0.75 L/h that its own equation puts at
0.646) do not touch the model; the test pins 0.646. An adult etoposide model
waits: Nguyen 1998 (P21), read in full, never prints its intercompartmental
clearance, so only a one-compartment reduction could be built from it.

## 13. Pemetrexed

`pemetrexed` is Latz 2006 (C20; PDF supplied 2026-11-06, Table 3 read on the
page images): two compartments, CL = 43.0 + 47.2 x CrCL/92.6 mL/min with
Cockcroft-Gault in mL/min, V1 = 6.13 x BSA^1.32 L, Q 14.5 mL/min (14.4 in the
text), V2 3.38 L. It reproduces the paper's terminal half-life (3.50 h) and
Vss (16.3 L at its own V1). The Cockcroft-Gault weight and the BSA formula
are unnamed (total weight and Mosteller). The intercept keeps clearance at 43
mL/min or more whatever the kidneys, untested below about 40 mL/min. The
patients had no folate or B12 supplementation. Pregnancy: the creatinine
clearance takes the rise in filtration (applied, physiological).

## 14. Melphalan

`melphalan` is Nath 2010 (C33; PDF supplied 2026-11-06), the total-melphalan
model of Table 5: CL = 17 x (HCT/34)^0.462 x (FFM/50)^0.75 + 11.1 x CLcr/88
L/h, CLcr Cockcroft-Gault normalised to 70 kg; V1 = 13.2 x FFM/50 L, Q 30.6
L/h, V2 15 L. It reproduces CL 28.1 L/h (40% renal), Table 7's half-lives
and the median AUC. Haematocrit has no field and is held at 34%; the FFM
reference is 50 kg as printed, though the Methods centre on 53.3.

## 15. MTXPK.org compared (2026-11-06)

MTXPK.org (MtxSim 2.3.1) computes on its server (`POST /api/MtxData`); a
synthetic patient was sent to it. Whatever the age or indication, it runs
"Taylor-3C-V3", referenced to Taylor 2020 (C12, *Clin Pharmacol Ther*
108:635-643), not Taylor 2023: population values CL 11.0, Q2 0.602, Q3 0.111
L/h, V1 16.5, V2 4.55, V3 13.1 L (per its own scaling), with DuBois BSA by
default. Its other models (Taylor-2C/2E/2U/3C/3E/3U, "unpublished";
Kawakatsu-2C; Trevino-2C) can be selected through the API. So neither
library model is expected to match it, and neither does:

| Case | Quantity | MTXPK.org | methotrexatePediatric (Taylor 2023) | methotrexate (Ibarra 2023) |
|---|---|---|---|---|
| girl, 9 y, 30 kg, 130 cm, Scr 0.4; 5 g/m2 (10% over 0.5 h, 90% over 23.5 h) | conc. at 24 h, uM | 68.8 | 85.0 | - |
| | at 48 h / 72 h, uM | 0.40 / 0.11 | 0.38 / 0.068 | - |
| | AUC, uM h | 1817 | 2228 | - |
| | time to 0.1 uM, h | 78.7 | 65.0 | - |
| man, 60 y, 70 kg, 170 cm, Scr 0.92; 3.5 g/m2 over 4 h | conc. at 24 / 48 / 72 h, uM | 1.18 / 0.15 / 0.082 | 1.96 / 0.18 / 0.051 | 2.95 / 0.22 / 0.086 |
| | AUC, uM h | 1407 | 1930 | 2351 |
| | time to 0.1 uM, h | 60.1 | 56.6 | 65.8 |

Each library model reproduces its own paper (the tests); the difference is
between source models. To agree with MTXPK.org, Taylor 2020 (PubMed Central
PMC7484917) would be added as a third methotrexate drug and checked against
the API.

## 16. Vincristine

`vincristine` is Igarashi 2021 (C31; PDF supplied 2026-11-06, Table 2 read on
the page image): three compartments, CL 31.1 x (BSA/1.60)^1.74 x
(age/60)^-0.485 L/h (age floored at 18), V1 13.2 L, Q2 60.2 L/h and V2 33.9 L
(fixed in the source), Q3 81.1 L/h, V3 544 L; plotted in ng/mL, dosed in mg
as labelled. Mosteller BSA (the paper names none).

## 17. Docetaxel

`docetaxel` is Bruno 1996 (P24; PDF supplied 2026-11-06, an older scan read
on the page images): three compartments, CL = BSA x (22.1 - 3.55 AAG - 0.095
age + 0.225 albumin) x (1 - 0.334 HEP12) L/h, V1 8.31 L, k12 1.07, k21 1.74,
k13 1.28, k31 0.0787 /h. It reproduces Table II at the mean covariates. AAG
(1.46 g/L) and HEP12 (0) are held; albumin reads the Patient Profile field
(g/dL x 10), the mean 40.1 g/L when blank. Pregnancy is `unquantified`
pending Janssen 2021's full text.

## 18. Cyclophosphamide

`cyclophosphamide` is Joerger 2007 (C08; PDF supplied 2026-11-06): one
compartment, CL 4.23 L/h (x 1.89 with an enzyme-inducing antiepileptic, held
at none), V 34.6 L. The prodrug only: 4-hydroxycyclophosphamide was not
measured, and one first-cycle profile per patient cannot show autoinduction
(de Jonge 2005 and Hassan 1999, in the registry, model both).

## 19. Doxorubicin and doxorubicinol

`doxorubicin` is Joerger 2007 (C08): two compartments, CL 47.6 x
(BSA/1.8)^1.4 x (AST/21)^-0.24 x (age/56)^-0.54 L/h (AST held at 21 U/L), V1
12.3 L, Q 60.3 L/h, V2 421 L, all of the clearance forming the metabolite-only
`doxorubicinol` (one compartment, V3/fm 1580 L, CL_DOL/fm 108 x (CLcr/80)^0.54
L/h, Cockcroft-Gault), fitted jointly (METABOLITE_FORMATION_BASIS `joint`).
Doses are the hydrochloride as labelled (Table VII's mean AUC supports it),
so every doxorubicin volume and clearance is divided by 0.937 and the
metabolite forms at 545.54/579.98 mg per mg dosed. Sampling to 24 h makes the
terminal half-life (11 h) short. Pérez-Blanco 2016 (adults, samples to 3 h,
every volume fixed) and Völler 2015 (children) are in the registry; the
children's model waits.

## 20. Irinotecan and SN-38

`irinotecan` is Klein 2002 (P19; PDF supplied 2026-11-06, Shafer's choice of
2026-11-06 over Xie 2002, whose 13-state lactone-carboxylate network needs a
general linear engine): irinotecan in three compartments forming the
metabolite-only `irinotecanSN38` (two compartments, divided by Fmsn), fitted
jointly. SN-38G, the third generation, is not carried. The SN-38 mixture
group is the 466-L one (Table I's 58/42 split appears swapped against Fig 2
and the validation set's 31/69, and its Fmsn was fixed at 12%). No covariate:
the paper's are printed as percentages against references that contradict
Table I. Doses as labelled, with no salt conversion (the paper is silent).

Note for the registry: the Xie paper Shafer supplied is J Clin Oncol
2002;20:3293-3301 (the 13-state model), not the Clin Pharmacol Ther 2002
paper registered as C40.
## 21. The Serum albumin field (2026-11-06)

Shafer decided on 2026-11-06 to add a serum albumin field to the Patient Profile and to apply
it in both methotrexate models. It is wired as the serum creatinine and the gestational age
are: argument `albumin` in g/dL, passed by `getDrugPK()` only to models that name it in their
signature, shown only while such a drug is in the dose table (`relevantField("albumin")`),
with a hover tip naming its readers built from the signatures (`patientFieldTooltip()`), and
accepted by the Simulation API as `patient.albumin`. The accepted range is `MIN_ALBUMIN` to
`MAX_ALBUMIN`, 0.5 to 6 g/dL (`R/constants.R`). Blank (NULL) means each model's own
reference:

- **methotrexate** (Ibarra 2023): clearance × (albumin / 4)^0.594; blank is 4 g/dL. At the
  cohort's median, 3.3 g/dL, clearance is 11% lower; at 2.5 g/dL, 24% lower. The albumin the
  power sees is floored at `MIN_ALBUMIN`, so the term stays finite.
- **methotrexatePediatric** (Taylor 2023): severe hypoalbuminaemia, an albumin of 2.5 g/dL or
  less, multiplies Q2 by 1.17 and Q3 by 1.34 (Table 2). The covariate is categorical, so it
  does not grade below the threshold; blank or above 2.5 applies neither factor.

The albumin is held at the entered value through the course, though it was time-varying in
both sources. Other chemotherapy candidates with an albumin term (the registry's notes say
which) may now read the field when they are implemented.

Docetaxel (section 17) reads the same field: its albumin term is linear in
g/L, so the field is multiplied by 10, and a blank field is the study mean.

## 22. Topotecan

`topotecan` is Mould 2002 (C29; PDF supplied 2026-11-06): two compartments,
CL = (5.77 + 12.8 x RF) x (WT/70)^0.75 x exp(-0.12 (ECOG - 1)) L/h with RF =
CLCR/70 x 70/WT (Cockcroft-Gault capped at 150), V1 19.3 and V2 45.7 L x
WT/70, Q 90.4 L/h x (WT/70)^0.75; total topotecan in ng/mL. Shafer decided
(2026-11-06) to use the printed RF, which the Methods support; Table VI's
worked clearances and the Discussion's median AUC follow the raw
Cockcroft-Gault instead, so they are not test pins. ECOG is held at 1. The
creatinine clearance takes pregnancy's rise in filtration.

## 23. Vinorelbine

`vinorelbine` is Variol 2002 (C32; PDF supplied 2026-11-06): three
compartments with a lagged first-order oral input, Table 3's values (CL 41.9
L/h, Vc 22.2 L, fast peripheral 2.46/0.402 /h, deep 1.39/0.0337 /h; F 0.36, ka
0.481 /h, lag 0.15 h), whole blood in ng/mL. Shafer chose Table 3
(2026-11-06) over Eq. 1, which gives 53.1 L/h at the mean BSA; so no
covariate is applied.

## 24. Cisplatin

`cisplatin` is Urien 2004 (P04; PDF supplied 2026-11-06): unbound
(ultrafilterable) platinum in two compartments, V1 23.4 x (BSA/1.7)^1.60 L
(Table 4's exponent, Shafer's decision of 2026-11-06; the text's 2.3 is the
total-platinum model's), CL 35.5 x (BSA/1.7)^0.83 x (CLCr/81)^0.36 L/h, Q
8.64 L/h, V2 12.0 L. The irreversible binding to plasma proteins is the
metabolite-only `cisplatinBound`, whose volume is 1/(fm/Vm) and clearance
(CLm0/Vm)/(fm/Vm), so it obeys the paper's dCm/dt = (fm/Vm) CL C1 - (CLm0/Vm)
Cm; total platinum is the sum of the two panels. Protein (70 g/L) and the
dose per m2 (25) are held. Doses mg cisplatin, concentrations ug/L of
platinum, as fitted.

## 25. Etoposide in adults

`etoposide` is Krogh-Madsen 2012 (C22; PDF supplied 2026-11-06), which
reports the intercompartmental clearance Nguyen 1998 omits: two compartments,
CL 0.99 + 0.00996 x CLCR L/h (Cockcroft-Gault), Vc 9.13 L (x 0.721 in women),
Q 1.14 L/h, Vp 4.75 L; baseline white count held at its median. Table 2's
renal slope is used although the text (-30%/+24% at 60/160 mL/min) and Fig
4's AUCs imply about 0.0133. The same paper's cytarabine (complete) and
daunorubicin (largely its prior's, Bogason 2011) are in the registry,
`complete_not_implemented`.

## 26. Doxorubicin in children

`doxorubicinPediatric` is Voller 2015 (C39; Eq. 1 confirmed on the page
image): three compartments, every parameter scaled by 1 + 1.30 (BSA - 0.77)
from Table 2's per-m2 values at the median child, CL also by (1 +
(age/5.32)^0.286)/2, half of it forming the metabolite-only
`doxorubicinolPediatric` (one compartment, fM 0.5 fixed in the source); ng/mL.
It reproduces the paper's three printed clearances. Mosteller BSA (the
paper's formula is in its supplement); doses as labelled, no conversion.
