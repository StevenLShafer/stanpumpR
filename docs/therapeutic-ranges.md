# Therapeutic reference ranges

The shaded band on a drug's panel is its **therapeutic reference range**, wherever one is
established for the analyte, matrix and use the row plots. This page is the review behind the
bands of the antiseizure, antidepressant and antipsychotic drugs (2026-10-19, revised
2026-10-20 against the AGNP update 2026). The data are in `inst/extdata/therapeuticRanges.csv`,
read by `R/therapeuticRanges.R`, printed on each drug's help page, and held to the band columns
of `drugDefaults_global.csv` by `tests/testthat/test-therapeutic-ranges.R`. The rule for new
drugs is in [adding-a-drug.md](adding-a-drug.md), *The therapeutic range*.

Every range is a concentration at steady state in serum or plasma, sampled at the time the
source states (a trough unless noted). It is a population window, not a target: many patients
do well outside it, and the band says nothing about a single dose.

## The primary source

**Hart XM, Gründer G, Hiemke C, et al. Consensus guidelines for therapeutic drug monitoring in
neuropsychopharmacology: update 2026.** *Pharmacopsychiatry* 2026;59:166-295, doi
[10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861). Open access. Its Table 5 (pp.
184-217 of the journal) gives, for 160 drugs, the time between the last dose and sampling, the
therapeutic reference range and the indication it is valid for, the laboratory alert level, the
level of recommendation to use TDM (1 strongly recommended, 2 recommended, 3 useful, 4
potentially useful) and a comment on where the range comes from; a `+` after a drug's name
marks a range revised or new since the 2017 update. It supersedes the 2017 update (Hiemke
2018) and covers the antiseizure drugs as well as the psychiatric ones. Table 5 was read in
full on 2026-10-20, and every row of the CSV that cites it says so.

For an antiseizure drug Table 5 omits (phenobarbital as a drug of its own, ethosuximide), the
band is the ILAE position paper's (Patsalos 2008), as quoted in Reimers 2018.

## Antiseizure drugs

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| phenytoin | 10–20 mg/L total | 50 | Hart 2026 | Unchanged. Dilantin label: 10–20 mcg/mL |
| carbamazepine | 4–12 mg/L | 20 | Hart 2026 | Unchanged (4–10 for mania) |
| valproate | 50–100 mg/L total | 120 | Hart 2026 | Unchanged. Depakote label: 50–100 mcg/mL |
| phenobarbital | 10–40 mg/L | — | ILAE 2008 | Not its own row; Hart 2026's primidone row gives phenobarbital 10–30 |
| pentobarbital | 20–40 mg/L | — | **no source read** | Barbiturate coma, titrated to EEG; orientation only |
| ethosuximide | 40–100 mg/L | — | ILAE 2008 | Not in Table 5. Zarontin label: 40–100 mcg/mL |
| topiramate | 5–20 mg/L | 30 | Hart 2026 | Unchanged |
| lacosamide | **1–12 mg/L** | 24 | Hart 2026 | Was 10–20 (Patsalos 2018), which was peak-based |
| lamotrigine | 3–15 mg/L | 20 | Hart 2026 | Unchanged |
| zonisamide | 10–40 mg/L | 80 | Hart 2026 | Unchanged |
| tiagabine | 20–200 ng/mL | 400 | Hart 2026 | Unchanged |
| levetiracetam | **6–46 mg/L** | 50 | Hart 2026 | Was 12–46 (ILAE) |
| eslicarbazepine | **3–35 mg/L** | 40 | Hart 2026 | Was 3–26 (Svendsen 2017 / Reimers 2018, eslicarbazepine-specific) |
| cenobamate | **10–25 mg/L** | 50 | Hart 2026 (level 4) | Newly introduced; Steinhoff 2024 found no general range |
| gabapentin | 4.1–9.4 mg/L (analgesic) | — | Healy 2023 | Hart 2026 gives 2–20 for epilepsy and pain; not applied |
| pregabalin | 1.3–5.4 mg/L | — | Chan 2021 | Hart 2026 gives 1.5–8 for epilepsy; not applied |
| clonazepam | **15–80 ng/mL** | 100 | Hart 2026 | Was 20–70 (ILAE) |

Gabapentin and pregabalin are in the library as analgesics, and their bands are the analgesic
exposures; the 2026 epilepsy ranges are recorded in the CSV's notes.

## Antidepressants

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| escitalopram | 20–40 ng/mL | 160 | Hart 2026 (from Eichentopf 2022) | Was 15–80 (2017) |
| citalopram | 50–110 ng/mL | 220 | Hart 2026 | Unchanged |
| sertraline | **10–50 ng/mL** | 150 | Hart 2026 | Was 10–150 (2017) |
| paroxetine | **30–80 ng/mL** | 120 | Hart 2026 | Was 20–65 (2017) |
| duloxetine | 20–120 ng/mL | 240 | Hart 2026 (from Amann 2024) | Was 30–120 (2017) |
| mirtazapine | 30–80 ng/mL | 160 | Hart 2026 | Unchanged |
| bupropion | 850–1500 ng/mL **of hydroxybupropion**, drawn on that row | 1500 | Hart 2026 | Was described as a range for the sum |
| fluoxetine, norfluoxetine | not drawn: **150–300 ng/mL** is for the sum | 1000 | Hart 2026 | Was 120–500 (2017) |
| venlafaxine | not drawn: 140–600 ng/mL is for the active moiety | 1200 | Hart 2026 (from Lense 2023) | Was 100–400 (2017) |
| desvenlafaxine (ODV) | 85–380 ng/mL | 450 | Hart 2026 (from Lense 2023) | New |

## Antipsychotics

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| aripiprazole | 120–270 ng/mL parent | 540 | Hart 2026 (from Hart 2022) | Was 100–350 (2017) |
| dehydroaripiprazole | not drawn: 180–380 ng/mL is the active moiety | — | Hart 2026 | Was 150–500 |
| olanzapine | 20–40 ng/mL, 12–15 h after a dose | 80 | Hart 2026 (from Wesner 2023) | Was 20–80 (2017) |
| quetiapine | 100–500 ng/mL | 1000 | Hart 2026 | Unchanged |
| risperidone, 9-hydroxyrisperidone | not drawn: 20–60 ng/mL is the active moiety, split by CYP2D6 | 80 | Hart 2026 | Unchanged |
| haloperidol | **1–5 ng/mL**, 12 h after a dose | 12 | Hart 2026 (level 1) | Was 1–10 (2017) |
| haloperidolIV | not applicable: the range is for oral or depot maintenance, not IV delirium | | | |
| droperidol, droperidolIM | none: not in Table 5; antiemetic and sedative use has no concentration target | | | |

## Product monographs (FDA, EMA)

Only the older antiseizure drugs' labels state a range: Dilantin (10–20 mcg/mL total,
1–2 unbound), Depakote (50–100 mcg/mL), Zarontin (40–100 mcg/mL), and Tegretol outside
the US (about 4–12 mcg/mL). These were read through search excerpts of the labels, and the CSV
says so. Sezaby (neonatal phenobarbital) gives weight-based dosing and observed concentrations,
but no target. Hart 2026 takes primidone's range from its SPC. The other labels were not read
in this review and are recorded as "Not checked".

## Open items

1. Find a primary source for pentobarbital's 20–40 mg/L, or remove the band.
2. Read the labels marked "Not checked".
3. A summed parent-plus-metabolite line would let the active-moiety ranges (risperidone,
   aripiprazole, venlafaxine, fluoxetine) be drawn.
4. Extend `THERAPEUTIC_RANGE_CATEGORIES` to the other classes. Table 5 also covers drugs the
   library holds under other categories (the benzodiazepines, methadone, morphine, oxycodone,
   buprenorphine, ketamine, lithium, the stimulants, the antihistamines); their bands have not
   been checked against it.

## References

- Hart XM, Gründer G, Hiemke C, et al. Consensus guidelines for therapeutic drug monitoring in
  neuropsychopharmacology: update 2026. *Pharmacopsychiatry* 2026;59:166-295.
  doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)
- Hiemke C, Bergemann N, Clement HW, et al. Consensus guidelines for therapeutic drug
  monitoring in neuropsychopharmacology: update 2017. *Pharmacopsychiatry* 2018;51:9-62.
  doi [10.1055/s-0043-116492](https://doi.org/10.1055/s-0043-116492)
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
