# NSAID population models

Which NSAID models are in the drug library, and why three are not. The criterion was a
reproducible mixed-effects model with at least two **systemic parent-drug** compartments; a gut,
presystemic, metabolite or CSF compartment does not count. Drafted by Claude Code,
2026-10-10, at the request of Steven L. Shafer, from a literature summary he supplied; the
meloxicam and ketorolac parameter tables were checked against the full papers.

| Drug | Model | Status in stanpumpR |
|---|---|---|
| Ibuprofen | Morse 2022, intravenous and oral, 2 compartments | In the library (`R/drugs_ibuprofen.R`), fasted tablet |
| Diclofenac | Standing 2011, intravenous and oral, 3 compartments | Added: dispersible tablet, two lagged oral depots, exact |
| Meloxicam | Aoyama 2017, apparent oral, 2 compartments, CYP2C9; ANJESO (FDA review 2020), intravenous, 3 compartments | Added: oral (the zero-order path approximated as first-order) and, as a separate route-limited system, intravenous |
| Ketorolac | Cloesmeijer 2021, S (3) and R (2) enantiomers, intravenous | Added: S + R as parallel systems; oral provisional |
| Aspirin | Koh 2025 | **Not added** |
| Celecoxib | FDA reviews, Hannam 2023 | **Not added** |
| Naproxen | Välitalo 2012 | **Not added** |

## Not added, and what would change that

**Aspirin.** Koh et al. 2025 (*Drug Des Devel Ther*, doi:10.2147/DDDT.S533428; 44 adults,
100 mg enteric-coated) has **one** systemic aspirin compartment and two salicylate
compartments, with a presystemic pool and a zero-order plus first-order absorption that does
not fit the engine. The salicylate peripheral compartment is not a second aspirin compartment,
and the low-dose model should not be extended to analgesic doses, where salicylate elimination
saturates. Needed: a two-compartment population model of parent aspirin, or a decision to
accept a one-compartment parent with salicylate as an active metabolite.

**Celecoxib.** The FDA clinical pharmacology review (NDA 211759) describes two-compartment
population fits but does not publish the coefficients. Hannam et al. 2023 (*Paediatr Anaesth*,
doi:10.1111/pan.14590; 10 adults) is complete but has one plasma compartment plus CSF exchange
(CL/F 49 L/h, V/F 346 L, ka 1.98 /h, lag 0.621 h). Absolute bioavailability is unknown.
Needed: the FDA coefficients, or a refit.

**Naproxen.** Välitalo et al. 2012 (*J Clin Pharmacol*, doi:10.1177/0091270011418658; 53
children, oral suspension) is two-compartment, but the accessible report gives only
CL/F 0.62 L/h/70 kg (linear in weight) and Vss/F 12.5 L/70 kg, not Q/F, Vc/F, Vp/F or ka. The
earlier PAGE 2010 poster's estimates are preliminary and differ; do not merge them. The larger
adult analysis (PMC3099376) chose one compartment. Needed: the final parameter table.

## CYP2D6

None of the models fitted a CYP2D6 phenotype, so none of them takes `cyp2d6`; a multiplier of
one means no modelled effect, not proof of none. Meloxicam keeps the CYP2C9 covariate Aoyama
estimated, as a `cyp2c9` argument of `meloxicam()` (the app has no CYP2C9 input and simulates
\*1/\*1).

## Ibuprofen formulations

Morse 2022 also gives food and formulation factors for the absorption half-time and lag
(fasted suspension 0.719 / 0.984, fasted sachet 0.235 / 0.539, fed tablet 1.59 / 3.65, fed
suspension 2.45 / 2.52, fed sachet 3.79 / 0.178), with bioavailability shared. They are not
offered: further oral formulations (`oralFormulations`, `R/getDrugPK.R`) must share the
default lag, and these do not.

## Meloxicam: intravenous

The intravenous route uses the ANJESO population model of the FDA clinical pharmacology
review (NDA 210583, 2020, section 4.1.1, Table 3): three compartments; CL 0.416 L/h ×
(WT/70)^0.761 × (eGFR/91)^0.554; Vc, V2, V3 4.16, 2.06, 3.28 L × (WT/70)^0.776; Q2, Q3 6.171,
0.835 L/h × (WT/70)^0.761; the 0.64 study factor of the two early studies not applied. The
values come from the literature summary Steven L. Shafer supplied on 2026-10-10. The review
could not be retrieved from the session that added them, so they still need checking against
Table 3, as does the eGFR equation (CKD-EPI 2009 is assumed). As a consistency check, Vss is
9.5 L at 70 kg against the label's terminal volume of 9.63 L. The terminal half-life is 17 h
against the label's "approximately 24 hours", the late-time misfit the reviewer noted.

No study fitted oral and intravenous meloxicam together, so neither fit's bioavailability is
used to bridge them. Oral doses go to the Aoyama system and intravenous doses to the ANJESO
system (`routes` on a parallel system, `docs/adding-a-drug.md`), and the curves are added.
The oral TXB2 model of Aoyama, and any analgesic exposure–response, are not plotted. The FDA
reviewer did not accept the sponsor's intravenous exposure–response model.
