---
file: NJ_dailymed-7b48ddec.pdf
pmid: null
doi: null
citation: "Acthar Gel (repository corticotropin injection), for intramuscular or subcutaneous use. Keenova ARD LLC. Prescribing information, Medication Guide, Instructions for Use and SPL product data; NDA008372; Highlights revised 10/2026. DailyMed setid 7b48ddec-e815-45f4-9ca0-5c0daaf56f30 (downloaded 2026-10-21)."
kind: label
drugs: []   # no stanpumpR drug; registry-only (Acthar Gel, PK_UNAVAILABLE)
source_read: "text layer (chunks map one-to-one to PDF pages, checked on renders of p5 and p15): Highlights p1-2, Contents p2-3, sections 1 (p4), 2 Dosage and Administration (p5-6, rendered p5 for the BSA formula image), 3-4 (p6-7), 7 and 8 Use in Specific Populations (p13-14), 11 and 12.1 (p14), 12.3 Pharmacokinetics (p15, rendered), 13-14 (p15-16), 16 (p16), SPL product data (p61-64); sections 5-6 and the Medication Guide / IFU pages skimmed only"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Acthar Gel (repository corticotropin injection) label, Keenova, 10/2026

## Study
DailyMed SPL for Acthar Gel, Keenova ARD LLC, NDA008372 (marketing start 01/07/2013 for the vial NDCs, 02/29/2024
for the SelfJect pre-filled injectors; p61-64). Highlights "Revised: 10/2026", with recent major changes to
Contraindications (4) and Warnings and Precautions (5.10) dated 10/2026 (p1-2); initial US approval 1952. The
document is what its name says: the current label of the product in the registry row.

Product (section 11, p14): a naturally sourced complex mixture of adrenocorticotropic hormone analogues and other
pituitary peptides from porcine pituitary extract; a major component is N-25 deamidated porcine ACTH (1-39); in 16%
gelatin to give prolonged release after IM or SC injection; 0.5% phenol. Strengths (section 3, p6): 400 USP Units/5
mL (80 USP Units/mL) multi-dose vial for IM or SC; 40 USP Units/0.5 mL and 80 USP Units/mL single-dose SelfJect
injectors for SC use by adults only. Intravenous use is contraindicated (section 4).

## Model and parameters
None. Section 12.3 (PDF p15, rendered): after intravenous administration ACTH disappears rapidly, plasma half-life
about 15 minutes in people (endogenous/IV ACTH, not Acthar Gel); the pharmacokinetics of Acthar Gel have not been
adequately characterised. The section adds that a fixed dose gives a linear increase in adrenocortical secretion
with increasing duration of the infusion. No Cmax, Tmax, AUC, bioavailability, volume or clearance is given. There is
no section 12.2.

```yaml
parameters:
  - {name: t_half_IV_ACTH, value: 15, unit: min, source: "section 12.3, PDF p15", note: "about 15 min; IV ACTH in people, not the gel depot"}
```

## Pharmacodynamics and end points
Section 12.1 (p14): the mechanism in infantile spasms is unknown. Acthar Gel, like endogenous ACTH, stimulates the
adrenal cortex to secrete cortisol, corticosterone, aldosterone and weak androgens, and is also reported to bind
melanocortin receptors; trophic effects appear cAMP-mediated. No concentration-effect data.

Section 14 (p15-16), infantile spasms: responder = complete suppression of clinical spasms and hypsarrhythmia on a
full sleep-cycle video EEG at 2 weeks, read blind. 13/15 (86.7%) on Acthar Gel 75 U/m2 IM twice daily vs 4/14
(28.6%) on prednisone 1 mg/kg PO twice daily (p < 0.002); 7/8 prednisone non-responders then responded to Acthar.
Supportive trial: 150 U/m2 once daily for 3 weeks (n = 30) vs 20 U once daily for 2 weeks (n = 29); nominal
superiority for cessation of spasms, not for hypsarrhythmia resolution.

## Dosing (section 2, p5-6)
- Infantile spasms (under 2 years), monotherapy: **IM only, with the vial** (the SelfJect injector must not be used);
  150 U/m2/day as 75 U/m2 IM twice daily for 2 weeks, then a 2-week taper (one schedule: 30 U/m2 each morning for 3
  days, 15 U/m2 for 3 days, 10 U/m2 for 3 days, 10 U/m2 every other morning for 6 days). BSA (rendered p5) =
  sqrt(weight kg x height cm / 3600) (Mosteller).
- Multiple sclerosis exacerbations (adults): 80-120 units daily IM or SC for 2-3 weeks.
- Other indications (adults and children over 2): usually 40-80 units IM or SC every 24-72 hours; taper after
  prolonged use to avoid adrenal insufficiency.

## Covariates
None for PK. Section 8: pregnancy (may cause fetal harm by its steroidogenic effect; hypoadrenalism in exposed
infants), lactation (no data), pediatric use (infantile spasms; growth effects of concern, 5.12). No renal or hepatic
PK subsections. Section 7: no formal interaction studies; may accentuate diuretic electrolyte loss.

## Caveats
Doses are in USP units of a peptide mixture, not mass of a defined moiety; the 15-min half-life refers to IV ACTH,
not to the depot. Sections 5 and 6 were skimmed, not digested.

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:9` (built by `data-raw/antiseizureRegistry.R:88-92`): moiety "Repository
  corticotropin (Acthar Gel)", routes "IM/SC depot (infantile spasms)", us_status PK_UNAVAILABLE, evidence_tier
  proposed_unestimated, dose_basis "corticotropin (ACTH peptides)", analyte "not characterised", structural_model
  "none (depot PK inadequately characterised)", pd_status no_pk, stanpumpr_drug NA, source_url this label, note
  "PK_UNAVAILABLE: a peptide mixture dosed in units whose effect runs through cortisol; no small-molecule PK."
- Also relied on (same claim, not citing the URL): `tests/testthat/test-antiseizure-registry.R:59-65` (Acthar has
  no PK, stanpumpr_drug NA) and `AGENTS.md:85` ("Acthar Gel is `PK_UNAVAILABLE`").

## Verification against stanpumpR
- **PK_UNAVAILABLE, "depot PK inadequately characterised", analyte not characterised, no curve: matches** section
  12.3 (PDF p15). The status vocabulary in `data-raw/antiseizureRegistry.R:28` ("label states PK inadequately
  characterised") is exactly what the label says.
- **Peptide mixture dosed in units: matches** sections 3 and 11 (USP units; ACTH analogues and other pituitary
  peptides).
- **Route "IM/SC depot (infantile spasms)": discrepancy (minor wording).** The label allows IM or SC for the vial in
  general, but for infantile spasms, the indication this registry is about, it must be given IM with the vial
  (section 2.2, p5); SC is for other indications and the SelfJect is adults-only SC.
- **"effect runs through cortisol": discrepancy (overstated).** Section 12.1 (p14) says the mechanism in infantile
  spasms is unknown, lists corticosterone, aldosterone and androgens beside cortisol, and notes melanocortin receptor
  binding. Cortisol-mediated action is not what the label supports for this indication.
- **Internal note, not from the label:** evidence_tier `proposed_unestimated` is defined at
  `data-raw/antiseizureRegistry.R:32` as "a structure is proposed, constants missing", while the same row's
  structural_model is "none"; the tier and the structure field do not agree. The label proposes no structure.
