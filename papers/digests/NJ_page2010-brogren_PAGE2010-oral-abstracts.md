---
file: NJ_page2010-brogren_PAGE2010-oral-abstracts.pdf
pmid: none
doi: none
citation: "PAGE 2010 (Population Approach Group in Europe, 19th meeting, Berlin, June 2010) programme and oral abstracts booklet. Poster I-15: Brogren J (with Nyberg per the repo). Transit Compartment Model Useful for Describing Absorption of Quetiapine XR and IR. PAGE abstract 1686 (per the repo). https://www.page-meeting.org/wp-content/uploads/page/page2010/OralPAGEAbstractsAndProgram2010.pdf"
kind: other
drugs: [quetiapine, quetiapineXR]
source_read: "partial - searched the whole text layer (82 pp) for Brogren, quetiapine, 1686 and transit; the only hit is the programme listing on PDF p. 5 = printed p. 5 (Posters Wednesday Morning (group I), Applications - CNS, I_15), re-read from the page image 2026-10-14. The booklet prints the oral-presentation abstracts only; the Brogren poster abstract text is not in this PDF."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
revised: 2026-10-14 (page locator p. 5; linked the archived abstract and its digest)
---

# Brogren 2010: Transit compartment model for quetiapine XR and IR (PAGE poster I-15)

## Study
This archived PDF is the PAGE 2010 programme plus the oral abstracts. It lists, under "Posters Wednesday
Morning (group I)", section "Applications- CNS", poster **I_15: Jacob Brogren, "Transit Compartment Model
Useful for Describing Absorption of Quetiapine XR and IR"** (programme listing, PDF p. 5 = printed p. 5).
Poster abstracts are not reproduced in the booklet, so the design, population, doses, sampling, assay and
software of the poster are **not available from this document**. Co-author names (Nyberg) and the
abstract number (1686) are not printed here.

## Model and parameters
Not reported in this document. The title establishes only that a transit-compartment absorption model
was applied to both the XR and the IR formulations of quetiapine.
```yaml
parameters: []   # none in this PDF; the poster abstract is not in the oral-abstract booklet
```

## Pharmacodynamics and end points
None in this document. (Not an opioid; no MEAC/ke0 content expected.)

## Covariates
Not reported in this document.

## Caveats
- Conference poster, not peer reviewed. This file is the wrong part of the PAGE record for checking the
  repo, but the abstract itself is already archived separately as
  `NJ_page2010-brogren_abstract_2026-10-12.pdf` (web print of PAGE abstract 1686), with its own digest
  `NJ_page2010-brogren_abstract_2026-10-12.md`. Checked there on page images 2026-10-14: authors Jacob Brogren
  and Svante Nyberg (PDF p. 2); MTT 4.65 h (XR) and 1.14 h (IR), CL/F 96.8 L/h (PDF p. 3). The n = 58 healthy
  volunteers is in that file's text layer on PDF p. 2 but sits under a cookie banner on the rendered page, so it
  was not seen on the image.
- The poster title says the model covers XR **and IR**; the repo describes it only as an XR model.

## Used by stanpumpR
- R/drugs_quetiapine.R:30 - reason no XR profile is registered (abstract lacks transit number, Q/F, volumes).
- inst/help/drugs/quetiapine.md:7 - same statement in the help page.
- R/antipsychotics.R:25 and :82 - `quetiapineXR` profile, `modelStatus = "blocked"`, pkSource
  "Brogren & Nyberg 2010, PAGE abstract 1686", sourcePopulation "58 healthy volunteers".
- docs/antipsychotics.md:68 - "Not registered" list: CL/F and mean transit times given, not transit number, Q/F, volumes.

## Verification against stanpumpR
- **Existence, year, first author, topic** (all locations): the booklet confirms a PAGE 2010 poster by
  Jacob Brogren on a transit-compartment absorption model for quetiapine XR and IR. **matches**.
- **"CL/F and the mean transit times are given; transit number, Q/F and volumes are not"**
  (R/antipsychotics.R:25-26, docs/antipsychotics.md:68-69, R/drugs_quetiapine.R:30-31,
  inst/help/drugs/quetiapine.md:7): the abstract text is not in this PDF. **cannot verify** from this file;
  the archived abstract (`NJ_page2010-brogren_abstract_2026-10-12.md`) gives MTTs and CL/F and is where the
  statement is checked.
- **"58 healthy volunteers"** (R/antipsychotics.R:81) and **"abstract 1686"** (R/antipsychotics.R:82):
  not in this PDF. **cannot verify** from this file; see the abstract digest (abstract number 1686 and
  n = 58 are there).
- **Co-author Nyberg**: not printed in the programme listing; the archived abstract names Svante Nyberg
  (`NJ_page2010-brogren_abstract_2026-10-12.pdf`, PDF p. 2).
- No numbers from this source reach the plot (the profile is blocked, `drug = NA`), so the unverifiable
  statements carry no simulation risk.
