# ChatGPT's chemotherapy audit of 2026-11-23: what was checked and what was done

ChatGPT (Codex, GPT-6) audited the chemotherapy models with the instructions in
`chemotherapy-audit-instructions.md`. It returned 20 findings (F1-F20), a
full-text check of the seven sources that have no card, and tables of
interactions and genotypes, of the physiology sliders, of missing drugs and of
papers to obtain. As AGENTS.md requires of external reviews, every claim was
treated as a lead. Claude checked each against master at 39cce8e, the cards and
the open sources before anything changed (2026-11-24). The report itself is not
kept in the repository.

The audit was run on the bundle that Gemini's audit used, so it predates the
fixes in PR #856 (`chemotherapy-audit-gemini-2026-11-23-triage.md`). Several
findings were therefore already settled.

**Overall.** It was more careful than Gemini's audit. Its quotations matched
the code, and every finding it could test reproduced at its core. Its one
error of substance was F1, Gemini's F2 again, which still does not reproduce.

## Fixed in this pull request

| Finding | What was wrong | What was done |
|---|---|---|
| F15 5-FU target | The therapeutic-range rows for `fluorouracil` and `fluorouracilBriki` attributed 20-29 mg h/L to the IATDMCT guideline, which they had not read. Beumer 2019 (PMID 29923599, read in full on PubMed Central, section 8.4) recommends 20-30. The 20-29 is Briki's own dose-adjustment grid. | Rows, header, help page and `docs/chemotherapy.md` now give 20-30 from the guideline and 20-29 as Briki's. The plateau range is 0.43-0.65 mg/L. The Briki header no longer says that a per-m2 dose must be converted by hand. |
| F17 liposomal doxorubicin | The header compared the free pool's CLfree 2.68 L/h with "108 L/h", which is doxorubicinol's CL/fm, not the parent's clearance. It also said only a ratio was identifiable, but with complete release the free area is dose/CLfree. | The comparison is now with Joerger's 47.6 L/h, about 18-fold. The text says the free curve is release-limited, in the header, both help pages and `docs/chemotherapy.md`. |
| F20 dacarbazine | The header said every patient's derived V1 lies near Table 2's Ve, and a test tolerance of 0.45 let that pass. Patient 7's V1 is 43% below (0.160 against 0.281 L/kg); the other five are within 4%. | Header corrected. The test now holds the five to 4% and patient 7 to its own ratio. |
| F19 thioguanine | F 0.234 was described as "read from Lowe's own data". | The header says it is a derived oral input fraction, not a measured bioavailability, computed with the infusion's clearance. |
| F14 ifosfamide in pregnancy | The registry row said no comparison existed (comparator NA, read from the abstract). Mir 2011's Table 3 compares the same woman at 29 weeks and on day 10 post-partum, and the card already records this. | Finding, comparator, evidence (full text) and `to_obtain` corrected. Nothing is applied (n = 1). This also answers the card's own `[decision]` question, which proposed the same change with nothing applied; revert if Shafer disagrees. |

## Already settled before this audit (PR #856)

- **F4** doxorubicin: Pérez-Blanco unreachable. The Joerger range is now 29-81 years, so Pérez-Blanco is chosen at 26-29 and above 81.
- **F16** Taylor 2023's authors: corrected in #856.
- **F1** oxaliplatin: did not reproduce, as with Gemini's F2. Both models plot ultrafilterable platinum per mg of oxaliplatin. Bastian is no longer chosen for anyone, since its range became 41-79 years.

## Not done, recorded

- **F18** Peng 1995: whether the median of 88 mL/min/m2 is model-independent or the two-compartment fits' clearance. The paper is in neither the archive nor PubMed Central, so this was added to the Peng row of `papers/pending-work.csv`.
- **F5** docetaxel HEP12: Bruno's term needs alkaline phosphatase as well as aminotransferases, and the Patient Profile has no alkaline phosphatase field. The header already says HEP12 is held at 0 and that impairment lowers clearance by a third. Adding a field is a decision.
- **F13** bleomycin under 3 years: the header and help page already say the under-3 clearance (70.5 mL/min/m2) is not applied.

## Decisions for Steven Shafer (not done)

These add to the list in the Gemini triage.

1. **Etoposide: Duong 2019 can never be chosen (F3, new).** Its range (1.7-8.3 years, 8.6-31 kg) lies inside Urien 2011's, and Urien studied more patients, so `etoposidePediatric` wins at every age and weight. This was confirmed in R. The choices are: add a regimen criterion (high-dose neuroblastoma), keep Duong as a model that is never chosen, or remove it from the group.
2. **The selector's nearest rule (F2)** = Gemini F9/F10/F17. Carboplatin goes to Peng, with no renal term, up to about 34 years.
3. **Model switches and additional candidates.** Pemetrexed: de Rouw 2021 in renal impairment (F6, = Gemini F25). 5-FU: Briki against Tan at bolus concentrations (F7). Gemcitabine: Sugiyama 2010, CDA (F9). Adult cytarabine: white count held at the median (F8).
4. **The physiology rows.** Bendamustine's hepatic share is a judgement (F10). Topotecan's is unresolved (F11, = Gemini F11). Fludarabine's renal 0.4 from the label against the model's eGFR term of about 65% (F12): this was Shafer's decision of 2026-11-17, and the audit asks only that the panel say which experiment the slider is.
5. **Interactions and genotypes.** The audit's table (docetaxel with ketoconazole, irinotecan with the inducers, vincristine with the inducers, pemetrexed with ibuprofen, methotrexate with PPIs and NSAIDs, capecitabine with warfarin, UGT1A1, CDA, CYP2C8) extends Gemini's F14/F32/F33. Each row is a lead whose PMID must be checked before use.
