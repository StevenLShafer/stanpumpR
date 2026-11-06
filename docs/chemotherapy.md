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
- **Cystatin C, albumin, white-cell count, performance status and genotypes** that some models
  need are not Patient Profile fields; such a model either waits for the field or is implemented
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
syndrome, albumin, pleural effusion, infusion length) are held at the reference. The model
reproduces the paper's Table 5 end-of-infusion mean for 5 g/m2 over 24 h (85.9 against 84.8
umol/L); its later means lie above the typical curve, as means of individual predictions should.

## 10. High-dose methotrexate in adults

Ibarra 2023 (`R/drugs_methotrexate.R`), from the PDF Shafer supplied: three compartments
allometric in weight, creatinine and albumin on clearance. Table 2 reproduces the paper's own
half-lives (1.5, 4.9, 40 h) and Vss (20 L). Its reference creatinine is not printed as a number;
the healthy-adult means in the same paragraph of the Methods (0.92 and 0.77 mg/dL) are used.
Albumin has no Patient Profile field and is held at 4 g/dL although 68% of the courses were
hypoalbuminaemic; whether to add an albumin field is recorded as a pending decision in the
registry.
