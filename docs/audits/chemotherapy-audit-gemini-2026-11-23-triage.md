# Gemini's chemotherapy audit of 2026-11-23: what was checked and what was done

Gemini audited the chemotherapy models with the instructions in
`chemotherapy-audit-instructions.md` and returned 37 findings (F1-F37), a table
of interactions and genotypes, a physiology-slider table, 26 missing drugs and
62 papers to obtain. Under AGENTS.md's rule on external reviews every claim
was a lead: Claude checked each against the code, the evidence cards, the FDA
labels in `papers/labels/` and, where open, the papers through PubMed, before
anything changed (2026-11-24, PR #856). Gemini's report itself is not kept in
the repository; this file is the record.

**Overall.** Of the findings that could be checked against the code, all but
one held at their core (F2 did not reproduce). As with Carbon's audits, the
errors were in detail and reach: a 27% "unrecovered radiolabel" that was an
assay recovery (F11), an infant clearance "12-35 times too low" built by
setting a 0.5-year-old against older children's group results (F8), a
pregnancy effect described as more bound platinum where it was a failure to
fall (F16), and an external validation of Taylor 2020 read as one of the Taylor
2023 model the library uses (F18).

## Fixed in PR #856

| Finding | What was wrong | What was done |
|---|---|---|
| F1 fludarabine | Cockcroft-Gault took the patient's weight, then the clearance took (WT/70)^0.75 again: renal clearance about weight^1.75, 16% low in a 60 kg woman, 14% high at 120 kg. Langenhorst 2020 (PMID 31957334) defines the eGFR "in L/hour/70 kg"; the 2019 Discussion calls it "body size-adjusted". | Cockcroft-Gault computed per 70 kg; header, help, tests. Shafer's 2026-11-07 decision on units is unchanged. |
| F3 oxaliplatin selector | Bastian 2003 coded 18 years and over; its Table 1 prints 41-79. | Coded 41-79. Nikanjam 2015 is now chosen for every patient; Bastian is in the library but not chosen. |
| F7 doxorubicin selector | Joerger 2007 coded 18 and over; its cohort was 29-81. Its age term, extrapolated, made clearance jump 80% at 18; Perez-Blanco 2016 was never chosen. | Coded 29-81 for doxorubicin and doxorubicinol. Voller to about 22 years, Perez-Blanco about 22-29 and above 81, Joerger 29-81. |
| (beside F7) cyclophosphamide selector | The same Joerger 2007 cohort, also coded 18 and over. | Coded 29-81: McCune 2009 (children, autoinduction) to about 19 years, Joerger above. |
| F4 nab-paclitaxel | No size scaling, on the precedent of Crombag's paclitaxel, which the rule of 2026-11-19 overturned. | Volumes and maximal rates scaled to fat-free mass, anchored at Chen 2014's cohort (74 kg, 57 y, 60 men, 90 women; height from the BSA). |
| F5 liposomal doxorubicin | The same. | Scaled jointly (encapsulated and free) at the reference anchor, Hsu 2018 printing no demographics; a `[decision]` in the header on anchoring at a typical woman. |
| F6 doxorubicinJoerger | Held AST at 21 U/L "because the Patient Profile has no field"; the field has existed since 2026-11-10. | Reads Serum AST: CL x (AST/21)^-0.24. |
| F16 cisplatin in pregnancy | The bound platinum, which cannot read the pregnancy flag, kept fm/Vm at the unraised creatinine clearance: about 16% too high relative to Urien's equations. | Pregnancy's 1.5^-0.36 carried on cisplatin's `kFormation`; pregnancy registry row reworded. |
| F19 vincristinePediatric | Labelled "unbound plasma vincristine"; Nijstad measured total plasma vincristine, "unbound" meaning not bound to the tubulin pool. The physiology row said "Adults only" where the cut is 34 years. | Relabelled (code, help, therapeutic-range row); row note corrected. The curve is unchanged. |
| F20 help pages | 26 narratives told the user to multiply a per-m2 dose by the BSA, though the units now exist. Cladribine lacked mg/m2 PO and SC, bleomycin U/kg/hr and U/m2/hr. | Every sentence rewritten; `mg/m2 SC` and `U/m2/hr` added as units and given to those drugs; a test keeps the old sentence out. |
| F21 Duffull 1997 | The header said no erratum exists; MEDLINE lists one (Cancer Chemother Pharmacol 1997;41(1):91), which has no PMID. | Header, registry and card corrected; the erratum is in the registry row's `to_obtain`. |
| F23 nab-paclitaxel threshold | The therapeutic-range row gave solvent-based paclitaxel's 0.05 umol/L; Chen 2014 found 720 ng/mL best and 0.05 not significant. | Row and help page corrected. |
| F24 missing cards | Seven implemented sources had no card; Urien 2004's card had `drugs: []`. | Seven rows in `papers/pending-work.csv`, which also move those papers to the front of the digest queue; Urien's card lists cisplatin and cisplatinBound. |
| F35 (warning) | The double-counting warning named creatinine but not Serum AST with the liver slider. | Added to the panel and the physiology help page. |
| F36 text errors | Taylor 2023's co-authors; vinorelbine's matrix; etoposide.md on Duong; "for each doubling" (temozolomide); 14.5 for 15.5 kg (melphalan); "Danish" for a Swedish model (daunorubicin); NONMEM for MonolixSuite (Ibarra). | Each corrected. |
| F37, F11, F12 (notes) | The liver slider says nothing of prodrugs, nor of studies that found no change in liver disease (topotecan, vinorelbine, etoposide in jaundice). | Notes on the rows and a paragraph on the physiology help page; values unchanged (see the decisions below). |
| F14, F34 interactions | Label interactions absent from the help pages. | Bullets citing the labels: capecitabine and 5-FU (warfarin boxed warning, phenytoin), irinotecan (CYP3A4/UGT1A1), methotrexate (penicillins, PPIs, NSAIDs, probenecid), busulfan (metronidazole, itraconazole, deferasirox, acetaminophen, phenytoin), paclitaxel (CYP2C8/3A4); cross-references on warfarin and phenytoin. |

## Did not reproduce

- **F2.** Nikanjam and Bastian both plot measured ultrafilterable platinum
  per mg of oxaliplatin; there is no 2.04-fold unit error. The 1.9-fold late
  difference at the 86/87-year switch was Bastian's gallbladder bolus, now
  moot after F3.

## Decisions for Steven Shafer (not done)

1. **The selector's nearest rule (F9, F10, F17).** Age gap is ranked before
   weight gap, and an unprinted range counts as unbounded. So melphalanPediatric
   (Duong, 1.7-8.3 y, no renal term) is chosen from 8.3 to about 22 years at
   any weight; carboplatinPeng (no creatinine term) to about 34 years (his
   decision of 2026-11-17); and fluorouracilTan (no ranges printed) for
   anyone outside Briki's 24-90 years and 32-126 kg, infants included. Options:
   weigh the weight gap, cap how far a children's model extends, or keep the rule.
2. **topotecan and vinorelbine liver fractions (F11, F12).** Studies found no
   change in liver disease, while the rows give the liver 0.49 and 0.89 of
   clearance. Lower `fraction_hepatic` (topotecan to the measured
   hepatobiliary 0.2, or 0), or keep the well-stirred prediction with the
   notes now added. Vinorelbine's `oral_first_pass` is also open.
3. **V1 under the cardiac slider in one-compartment models (F15).** V1 is
   scaled by cardiac output in every model, so in temozolomide and
   cyclophosphamideJoerger, whose V1 is the whole volume, a cardiac output of
   0.25 shortens the half-life fourfold with clearance unchanged. The exponent
   is an assumption, not his decision; exempting such models needs one.
4. **Periclou's cytarabine in infants (F8).** Every parameter is age x BSA, so
   clearance is near zero at birth (1.3 L/h/m2 at 6 months). Periclou did
   include infants and found lower clearance in them, but the magnitude is an
   artefact of the model's form. A floor, a plateau, or another source.
5. **Liposomal doxorubicin's anchor (F5)**: reference man or a typical woman.
6. **Model switches** (F13, F27, F25, F26, F28-F31): Gemini proposes Janssen
   2021 for epirubicin (Ralph 2003 over-predicted concentrations by 81% in its
   own validation), Xie 2002 for irinotecan's SN-38, de Rouw 2021 for
   pemetrexed in renal impairment, and notes disagreements for gemcitabine,
   liposomal doxorubicin, bleomycin (no renal term), cyclophosphamide
   (adult autoinduction), dacarbazine (high-dose saturation) and paediatric
   cisplatin. Each needs its paper read; none was changed.
7. **Interactions as models, not warnings (F14, F32, F33).** Capecitabine and
   5-FU as CYP2C9 perpetrators on warfarin and phenytoin; the antiseizure
   inducers acting on irinotecan, vincristine, etoposide, paclitaxel and
   busulfan; DPYD, UGT1A1 and CDA genotypes. Warnings were added; modelling
   is a decision.
8. **New drugs (Q7).** Gemini ranks cabazitaxel, clofarabine, treosulfan,
   mitoxantrone, CPX-351, Erwinia asparaginase and hydroxyurea as ready from
   open-access tables, and HIPEC mitomycin as the most useful in the
   operating room. Its 62-paper list is a lead list: every PMID must be
   checked before download.

## Not done, recorded

- **F22. Done (2026-11-25).** fluorouracilBriki, capecitabine and adult
  cytarabine were not anchored at their cohorts. fluorouracilBriki: V1 and Q
  anchored at Briki 2026's Table 2 (three studies entered as groups, heights
  printed), and Vmax, which carries the source's sex term and had not been
  scaled at all, at the cohort's patient of the patient's own sex
  (`cohortSexSizeFactors()`); CL keeps its BSA term and V2 its weight term.
  Adult cytarabine: anchored at `ETOPOSIDE_KM_COHORT`, the PDF (pages
  1156-1157) confirming that Krogh-Madsen fitted all three drugs in one set
  of 23 patients. Capecitabine had already been anchored in PR #841 (its
  pending row, PMID 12630975, is marked done), which this entry predated.
- **F35 (lookup).** `physiologyRow()` finds only the group's name, so a
  candidate called directly in R gets no sliders. The app never does that;
  mapping candidates to their group's row could give a target-binding child
  model a row meant for adults, so it was left.
