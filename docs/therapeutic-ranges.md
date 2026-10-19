# Therapeutic reference ranges

The shaded band on a drug's panel is its **therapeutic reference range**, wherever one is
established for the analyte, matrix and use the row plots. This page is the review behind the
bands of the antiseizure, antidepressant and antipsychotic drugs (2026-10-19). The data are in
`inst/extdata/therapeuticRanges.csv`, read by `R/therapeuticRanges.R`, printed on each drug's
help page, and held to the band columns of `drugDefaults_global.csv` by
`tests/testthat/test-therapeutic-ranges.R`. The rule for new drugs is in
[adding-a-drug.md](adding-a-drug.md), *The therapeutic range*.

Every range is a trough (or stated) concentration at steady state in serum or plasma. It is a
population window, not a target: many patients do well outside it, and the band says nothing
about a single dose.

## How far each source was read

The network available to this review reached PubMed, PubMed Central and search excerpts, not
publishers' sites. So the **verification** column of the CSV says exactly what was read:
`full text` or `abstract` (PubMed/PMC), a value **quoted** by another paper read in full (the
ILAE values, as quoted by Reimers 2018), a **search excerpt**, or a value **as recorded in the
library** before this review and not re-read. Nothing was filled in from memory.

The AGNP-TDM consensus guideline was updated in July 2026 (Hart XM et al., *Pharmacopsychiatry*
2026;59:166-295, doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)): ranges for
160 drugs, 88 of them revised. **Its Table 5 has not been read.** The antidepressant and
antipsychotic bands use the AGNP task force's drug-by-drug systematic reviews where they exist
(they are the evidence the 2026 update was built on) and the 2017 update otherwise; every row
of those categories should be re-checked against Table 5 when the PDF is in hand.

## Antiseizure drugs

| Drug | Band | Source | Notes |
|---|---|---|---|
| phenytoin | 10–20 mg/L total | ILAE 2008 | Unbound 1–2 mg/L. Dilantin label: 10–20 mcg/mL |
| carbamazepine | 4–12 mg/L | ILAE 2008 | Norway 4–11. Non-US Tegretol leaflet: 4–12 |
| valproate | 50–100 mg/L total | ILAE 2008 | Depakote label: 50–100 mcg/mL; thrombocytopenia above 110 (women) / 135 (men) |
| phenobarbital | 10–40 mg/L | ILAE 2008 | Norway 12–30; sedation frequent above 30 |
| pentobarbital | 20–40 mg/L | **no source read** | Barbiturate coma, titrated to EEG; orientation only |
| ethosuximide | 40–100 mg/L | ILAE 2008 | Zarontin label: 40–100 mcg/mL |
| topiramate | 5–20 mg/L | ILAE 2008 | Norway lowered to 2–10 (Reimers 2018); Koristkova 2021 advises no change |
| lacosamide | 10–20 mg/L | Patsalos 2018 | Based on samples 0–3 h after a dose; trough-based 3–10 (Norway) |
| lamotrigine | 3–15 mg/L | ILAE 2008 | Norway 3–13 |
| zonisamide | 10–40 mg/L | ILAE 2008 | |
| tiagabine | 20–200 ng/mL | ILAE 2008 | Not re-read |
| levetiracetam | 12–46 mg/L | ILAE 2008 | Norway 5–41; AGNP 2017 10–40 |
| eslicarbazepine | 3–26 mg/L | Svendsen 2017, adopted by Reimers 2018 | Was attributed to Patsalos 2018, whose 3–36 is racemic licarbazepine's |
| cenobamate | none | Steinhoff 2024 | "A general therapeutic range cannot be defined" |
| gabapentin | 4.1–9.4 mg/L | Healy 2023 (analgesic exposure) | Epilepsy: ILAE 2–20, Norway 3–21 |
| pregabalin | 1.3–5.4 mg/L | Chan 2021 | Norway 2–6 |
| clonazepam | 20–70 ng/mL | ILAE 2008 | Norway 13–38 |

## Antidepressants

| Drug | Band | Source | Replaced |
|---|---|---|---|
| escitalopram | **20–40 ng/mL** | Eichentopf 2022 (full text) | 15–80 (AGNP 2017) |
| citalopram | 50–110 ng/mL | AGNP 2017 | |
| sertraline | 10–150 ng/mL | AGNP 2017 | |
| paroxetine | 20–65 ng/mL | AGNP 2017 | |
| duloxetine | **20–120 ng/mL** | Amann 2024 (full text) | 30–120 (AGNP 2017) |
| mirtazapine | 30–80 ng/mL | AGNP 2017 | |
| bupropion | 850–1500 ng/mL bupropion + hydroxybupropion, drawn on hydroxybupropion | AGNP 2017 | |
| fluoxetine, norfluoxetine | not drawn: 120–500 ng/mL is for the sum | AGNP 2017 | |
| venlafaxine | not drawn: 140–600 ng/mL is for the active moiety | Lense 2023 (full text) | 100–400 (AGNP 2017) |
| desvenlafaxine (ODV from venlafaxine) | **85–380 ng/mL** | Lense 2023 (full text) | none |

## Antipsychotics

| Drug | Band | Source | Replaced |
|---|---|---|---|
| aripiprazole | **120–270 ng/mL** parent | Hart 2022 (abstract) | 100–350 (AGNP 2017) |
| dehydroaripiprazole | not drawn: 180–380 ng/mL is the active moiety | Hart 2022 | 150–500 |
| olanzapine | **20–40 ng/mL**, 12 h after a dose | Wesner 2023 (abstract) | 20–80 (AGNP 2017) |
| quetiapine | **100–500 ng/mL** | AGNP 2011, 2017, and a 2026 excerpt | |
| risperidone, 9-hydroxyrisperidone | not drawn: 20–60 ng/mL is the active moiety, split by CYP2D6 | AGNP 2017 (2026 excerpt agrees) | |
| haloperidol | **pending**: 1–10 (AGNP 2017) or 1–5 (2026 excerpt) | neither read in full | |
| haloperidolIV | not applicable: the range is for oral maintenance, not IV delirium | | |
| droperidol, droperidolIM | none: antiemetic and sedative use has no concentration target | | |

## Product monographs (FDA, EMA)

Only the older antiseizure drugs' labels state a range: Dilantin (10–20 mcg/mL total,
1–2 unbound), Depakote (50–100 mcg/mL), Zarontin (40–100 mcg/mL), and Tegretol outside
the US (about 4–12 mcg/mL). These were read through search excerpts of the labels, and
the CSV says so. Sezaby (neonatal phenobarbital) gives weight-based dosing and observed
concentrations, but no target. The other labels were not read in this review, so the
CSV records them as "Not checked". To my knowledge the newer antiseizure drugs, and
the antidepressants and antipsychotics, define no therapeutic range in their FDA labels
or EMA SmPCs, but that is still to be confirmed label by label (DailyMed;
the EMA product information pages).

## Open items

1. Read Table 5 of the 2026 AGNP update and re-check every antidepressant and antipsychotic row;
   then set haloperidol's band.
2. Find a primary source for pentobarbital's 20–40 mg/L, or remove the band.
3. Read the labels marked "Not checked".
4. A summed parent-plus-metabolite line would let the active-moiety ranges (risperidone,
   aripiprazole, venlafaxine, fluoxetine) be drawn.
5. Extend `THERAPEUTIC_RANGE_CATEGORIES` to the other classes. Of the 193 rows of the library,
   about 109 have no band, many of them correctly; each needs a recorded reason.

## References

- Patsalos PN, Berry DJ, Bourgeois BFD, et al. Antiepileptic drugs—best practice guidelines for
  therapeutic drug monitoring: a position paper by the subcommission on therapeutic drug
  monitoring, ILAE Commission on Therapeutic Strategies. *Epilepsia* 2008;49:1239-1276.
  doi [10.1111/j.1528-1167.2008.01561.x](https://doi.org/10.1111/j.1528-1167.2008.01561.x)
- Patsalos PN, Spencer EP, Berry DJ. Therapeutic drug monitoring of antiepileptic drugs in
  epilepsy: a 2018 update. *Ther Drug Monit* 2018;40:526-548.
  doi [10.1097/FTD.0000000000000546](https://doi.org/10.1097/FTD.0000000000000546)
- Reimers A, Berg JA, Burns ML, et al. Reference ranges for antiepileptic drugs revisited: a
  practical approach to establish national guidelines. *Drug Des Devel Ther* 2018;12:271-280.
  doi [10.2147/DDDT.S154388](https://doi.org/10.2147/DDDT.S154388)
- Svendsen T, Brodtkorb E, Reimers A, et al. Pharmacokinetic variability, efficacy and
  tolerability of eslicarbazepine acetate. *Epilepsy Res* 2017;129:125-131.
  doi [10.1016/j.eplepsyres.2016.12.001](https://doi.org/10.1016/j.eplepsyres.2016.12.001)
- Steinhoff BJ, Georgiou D, Dietmann D, Intravooth T. Cenobamate plasma levels in patients with
  epilepsy: correlation with efficacy and tolerability? *J Clin Med* 2024;13:2757.
  doi [10.3390/jcm13102757](https://doi.org/10.3390/jcm13102757)
- Hiemke C, Bergemann N, Clement HW, et al. Consensus guidelines for therapeutic drug
  monitoring in neuropsychopharmacology: update 2017. *Pharmacopsychiatry* 2018;51:9-62.
  doi [10.1055/s-0043-116492](https://doi.org/10.1055/s-0043-116492)
- Hart XM, Gründer G, Hiemke C, et al. Consensus guidelines for therapeutic drug monitoring in
  neuropsychopharmacology: update 2026. *Pharmacopsychiatry* 2026;59:166-295.
  doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)
- Hart XM, Hiemke C, Eichentopf L, et al. Therapeutic reference range for aripiprazole in
  schizophrenia revised. *Psychopharmacology* 2022;239:3377-3391.
  doi [10.1007/s00213-022-06233-2](https://doi.org/10.1007/s00213-022-06233-2)
- Wesner K, Hiemke C, Bergemann N, et al. Therapeutic reference range for olanzapine in
  schizophrenia. *J Clin Psychiatry* 2023;84:22r14626.
  doi [10.4088/JCP.22r14626](https://doi.org/10.4088/JCP.22r14626)
- Eichentopf L, Hiemke C, Conca A, et al. Systematic review and meta-analysis on the
  therapeutic reference range for escitalopram. *Front Psychiatry* 2022;13:972141.
  doi [10.3389/fpsyt.2022.972141](https://doi.org/10.3389/fpsyt.2022.972141)
- Amann F, Kochtyrew M, Zernig G, Gründer G, Hart XM. Therapeutic reference range for
  duloxetine in the treatment of depression revised. *Neurosci Appl* 2024;3:104077.
  doi [10.1016/j.nsa.2024.104077](https://doi.org/10.1016/j.nsa.2024.104077)
- Lense XM, Hiemke C, Funk CSM, et al. Venlafaxine's therapeutic reference range in the
  treatment of depression revised. *Psychopharmacology* 2024;241:275-289.
  doi [10.1007/s00213-023-06484-7](https://doi.org/10.1007/s00213-023-06484-7)
