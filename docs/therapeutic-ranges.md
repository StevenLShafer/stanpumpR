# Therapeutic reference ranges

The shaded band on a drug's panel is its **therapeutic reference range**, wherever one is
established for the analyte, matrix and use the row plots. This page is the review behind the
bands of the antiseizure, antidepressant and antipsychotic drugs (2026-10-19, revised
2026-10-20 against the AGNP update 2026), and of the other library drugs Table 5 covers:
the stimulants, benzodiazepines and hypnotics, opioids, ketamine, gabapentin and pregabalin
(2026-10-27), and of every other category the same day. The data are in `inst/extdata/therapeuticRanges.csv`,
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
185-217 of the journal) gives, for 160 drugs, the time between the last dose and sampling, the
therapeutic reference range and the indication it is valid for, the laboratory alert level, the
level of recommendation to use TDM (1 strongly recommended, 2 recommended, 3 useful, 4
potentially useful) and a comment on where the range comes from; a `+` after a drug's name
marks a range revised or new since the 2017 update. It supersedes the 2017 update (Hiemke
2018) and covers the antiseizure drugs as well as the psychiatric ones. Table 5 was read in
full on 2026-10-20, and every row of the CSV that cites it says so.

For an antiseizure drug Table 5 omits (ethosuximide), the band is the ILAE position paper's
(Patsalos 2008, Table 1, read in full 2026-10-27; also quoted in Reimers 2018). Its Table 1 also
gives phenobarbital 10–40 mg/L, the band until 2026-10-27, and gabapentin 2–20 mg/L, the range
Table 5 carries, and calls pregabalin's not established. Phenobarbital has no row of its own in
Table 5, but its "Primidone and phenobarbital" row (p. 210) gives phenobarbital's range as well
as primidone's, so that is drawn.

## Antiseizure drugs

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| phenytoin | 10–20 mg/L total | 50 | Hart 2026 | Unchanged. Dilantin label: 10–20 mcg/mL |
| carbamazepine | 4–12 mg/L | 20 | Hart 2026 | Marked + in Table 5 (revised or new since 2017; what changed is not stated); 4–10 for mania |
| valproate | 50–100 mg/L total | 120 | Hart 2026 | Unchanged. Depakote label: 50–100 mcg/mL |
| phenobarbital | **10–30 mg/L**, 12 h | 50 | Hart 2026 ("Primidone and phenobarbital" row, from the SPC) | Was 10–40 (ILAE 2008); changed 2026-10-27 by decision of Steven L. Shafer |
| pentobarbital | **4–21 mg/L**, steady state, **treatment of status epilepticus** only | — | Helfer 2026 (full text), citing Lowenstein 1988 (not read) | Was none (20–40 mg/L candidate, unsourced); drawn 2026-10-28 by decision of Steven L. Shafer. Concentrations observed during burst-suppression coma; not for sedation (Helfer: 1–5 peak, 5–15 deep); above 45 mg/L potentially toxic |
| ethosuximide | 40–100 mg/L | — | ILAE 2008 | Not in Table 5. Zarontin label: 40–100 mcg/mL |
| topiramate | 5–20 mg/L | 30 | Hart 2026 | Marked + in Table 5 (revised or new since 2017; what changed is not stated) |
| lacosamide | **1–12 mg/L** | 24 | Hart 2026 | Was 10–20 (Patsalos 2018), drawn from samples 0–3 h after a dose |
| lamotrigine | 3–15 mg/L | 20 | Hart 2026 | Unchanged |
| zonisamide | 10–40 mg/L | 80 | Hart 2026 | Unchanged |
| tiagabine | 20–200 ng/mL | 400 | Hart 2026 | Unchanged |
| levetiracetam | **6–46 mg/L** | 50 | Hart 2026 | Was 12–46 (ILAE) |
| eslicarbazepine | **3–35 mg/L** | 40 | Hart 2026 | Was 3–26 (Svendsen 2017 / Reimers 2018, eslicarbazepine-specific) |
| cenobamate | **10–25 mg/L** | 50 | Hart 2026 (level 4) | Newly introduced; Steinhoff 2024 found no general range |
| gabapentin | **2–20 mg/L**, 12 h | 25 | Hart 2026 | Was 4.1–9.4 (Healy 2023, postherpetic neuralgia), which lies inside it |
| pregabalin | **1.5–8 mg/L**, 10 h after IR | 10 | Hart 2026 | Was 1.3–5.4 (Chan 2021, focal seizures) |
| clonazepam | **15–80 ng/mL** | 100 | Hart 2026 | Was 20–70 (ILAE) |

Gabapentin and pregabalin are in the library as analgesics. Table 5 gives gabapentin's range
for epilepsy and neuropathic pain alike, so it is drawn. For pregabalin it gives 1.5–8 mg/L for
epilepsy, 10 h after an immediate-release dose, and 1.4–2.1 mg/L for neuropathic pain 22 h
after an extended-release dose; the library models the immediate-release product only, so the
epilepsy range is drawn and the neuropathic one recorded.

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
| venlafaxine | not drawn: 140–600 ng/mL is for the active moiety | 1200 | Hart 2026 (from Lense 2024) | Was 100–400 (2017) |
| desvenlafaxine (ODV) | 85–380 ng/mL | 450 | Hart 2026 (from Lense 2024) | New |

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

## Stimulants

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| mixedAmphetamineSalts | **23–45 ng/mL d-amphetamine**, peak at 3.3 h | 120 | Hart 2026 (McGough 2003) | New. Steady-state peak of Adderall IR 10 mg once daily, 9 children |
| lisdexamfetamine | **44–160 ng/mL d-amphetamine**, peak at 3.5 h | — | Hart 2026 (Boellner 2010) | New. Single doses of 30, 50 and 70 mg in 18 children |
| methylphenidate, methylphenidatePediatric | none: two time-specific ranges for OROS, in whole blood | | Hart 2026 (Chermá 2017) | 3.8–7.7 ng/mL at 1 h and 6.6–13 at 6 h; no one band |

Both amphetamine ranges are the concentrations the studied doses produced in children, not
windows of efficacy, and Table 5 has no adult data. The table prints 30–59 ng/mL "for d + l
amphetamine" in the lisdexamfetamine row, but it is the sum of the mixture's two ranges
(23 + 7, 45 + 14); and its d-amphetamine alert level, 120 ng/mL, is below lisdexamfetamine's
upper limit, so no alert level is recorded for lisdexamfetamine.

Both sources were read in full (2026-10-27). The mixture's range is McGough's Table 3: the
steady-state Cmax of immediate-release Adderall 10 mg once daily, 33.8 ng/mL (SEM 3.7, n = 9) at
3.3 h, with the SEM turned into an SD (3.7 × 3 = 11.1), so 22.7–44.9; Table 5 describes it as
51 children, the number enrolled. The model, fitted to McGough, gives 33.2 ng/mL at 3.3 h after
the seventh daily dose (37.8 kg). Lisdexamfetamine's range is Boellner's Table I, the mean ± SD
of the single-dose peak: 53.2 (9.62), 93.3 (18.2) and 134 (26.1) ng/mL at about 3.5 h. Boellner's
children are not in the model's data (Tsuda 2020), and it underpredicts them: 45, 75 and 105
ng/mL at 4.5 h for a 36 kg child, 16–22% low and an hour late.

## Benzodiazepines, hypnotics, opioids and ketamine

| Drug | Band | Alert | Source | Change |
|---|---|---|---|---|
| alprazolam | **5–44 ng/mL**, 10 h | 100 | Hart 2026 | Was 20–40 (Greenblatt and Wright 1993), which lies inside it |
| temazepam | **200–750 ng/mL**, 1–2 h | 1,500 | Hart 2026 | Was 250–600 (Saletu 1986, van Steveninck 1994) |
| zolpidem | 80–200 ng/mL, 1–3 h | 400 | Hart 2026 | Same range, marked + in Table 5 (revised or new since 2017; what changed is not stated); was attributed to Cha 2024 |
| clonazepam | see the antiseizure table | | | |

These keep their own band, because Table 5's range is for another use or analyte. Each is a
`band_differs` row, and its help page prints the guideline's range beside the band:

| Drug | Table 5 | Band kept |
|---|---|---|
| ketamine | 60–410 ng/mL after 0.5 mg/kg over 40 min, depression | 0.1–0.16 mg/L, the analgesic threshold (Mion 2013, citing Clements 1982) |
| methadone | 90–620 ng/mL at 24 h, opioid use disorder | 48–120 ng/mL, postoperative analgesia (Gourlay 1984) |
| morphine | 5–400 ng/mL pain; 50–520 opioid use disorder | 6.4–16 ng/mL, around the MEAC |
| oxycodone | 5–100 ng/mL at 12 h, pain | 10–20 ng/mL, around the MEAC |
| buprenorphine | 0.5–2.5 ng/mL at 24 h, opioid use disorder (alert 4) | **0.2–1 ng/mL**, typical 0.4, at the effect site: simulated analgesic exposure, its lower edge the concentration simulated for the average patient at the median end of analgesia in McQuay 1980 (read in full), an illustrative estimate rather than a measured threshold. Was Table 5's range, drawn 2026-10-20 to 2026-10-27, which had replaced 1.25–3 |
| lorazepam | 5–100 ng/mL at 12 h, anxiety, oral | **51–152 ng/mL**, typical 104, moderate sedation (Barr 2001, read in full); was 34–104 |
| midazolam | 50–150 ng/mL at 1–2 h, sedation after oral premedication (level 4) | **101–304 ng/mL**, typical 208, moderate sedation (Barr 2001); was 40–120, source not recorded. See below |
| diazepam | 100–1,500 ng/mL of diazepam plus nordiazepam | **606–1,824 ng/mL** of diazepam, typical 1,248: midazolam's moderate-sedation band × 6; was 150–600. Nordiazepam is not modelled |

Diamorphine is an illicit drug and so has no band; its row records Table 5's 70–350 ng/mL for
heroin-assisted treatment.

**Midazolam.** Table 5 attributes 50–150 ng/mL to Matsuda 2022 (PMID 35488482, full text read
2026-10-27). Matsuda does not measure it: the Discussion quotes it ("the serum concentration of
midazolam required for sedation is 50–150 ng/ml") from a textbook, Coté, Lerman and Anderson,
*A Practice of Anesthesia for Infants and Children* (Elsevier Saunders 2013), p. 141. Matsuda's
own children (0.5 mg/kg by mouth before surgery, 6 months to 8 years, 98 analysed) had median
serum concentrations of 47.6 ng/mL (IQR 35.5–61.3) when sedated and 49.8 (44.6–63.5) when not,
and 73.5% were sedated at concentrations the authors call "slightly lower than the level
required for sedation". The guideline rates the range level 4. The library's midazolam is
intravenous, so its band comes from the study that gives lorazepam's: Barr 2001 (PMID 11506097,
read in full), target-controlled midazolam and lorazepam infusions after surgery in 24 ICU
patients. Its Model H gives plasma midazolam with an even chance of a Ramsay score of at least
2, 3, 4, 5 and 6 as 68, 101, 208, 304 and 375 ng/mL (lorazepam 34, 51, 104, 152 and 188), 18%
lower for each decade of age and with fentanyl; the observed median midazolam concentrations at
scores of 3 and 4 were 79 and 119 ng/mL.

The end point for the sedatives is **moderate sedation** (Steven L. Shafer, 2026-10-27): a
Ramsay score of 3 or 4, as Barr defined it. Both bands therefore run from the concentration
with an even chance of at least 3 to that with an even chance of at least 5, where the median
patient scores 3 or 4, with the even chance of at least 4 as the typical value: midazolam
101–304 ng/mL (typical 208) and lorazepam 51–152 ng/mL (typical 104). Their time-until-threshold
levels are the band's lower edge (101 and 51). No study relates diazepam to a sedation score, so
its band is midazolam's times 6, the potency ratio from both the DSST (Mould 1995, 6.4) and the
EEG (Bührer 1990, 6.3): 606–1,824 ng/mL, typical 1,248, threshold 606. Both ratios are from
volunteers and total plasma; Barr's C50s are from ICU patients mostly on fentanyl. A ratio of
unbound concentrations would be sounder, since both drugs are highly and differently bound to
plasma proteins, but no study gives both drugs' unbound sedation concentrations. In rats, the
in vivo affinity of nine GABA-A modulators estimated from unbound plasma concentrations
correlated with in vitro receptor affinity (r = 0.93), and their intrinsic efficacy, which
differs between benzodiazepines, with the in vitro GABA shift (r = 0.95; Visser 2003, PMID
12490579); a potency ratio from total concentrations carries both drugs' binding in it, and
affinity alone would not give the level of sedation a drug can reach.

## Every other category (2026-10-27)

Since 2026-10-27 `THERAPEUTIC_RANGE_CATEGORIES` is every category, so every drug a user can
dose, and every metabolite it forms, has a row. None of these drugs is in Hart 2026 Table 5.
The sources were FDA labels from accessdata.fda.gov, PubMed Central full texts and PubMed
abstracts. They were read by research agents, and each quoted sentence was checked against
the saved text before it was recorded. DailyMed, the EMA, Health Canada and most publishers
could not be reached from the session; the CSV's `verification` column says how far each
source was read. A status was added, `library_band`: no therapeutic range is established for
what the row plots, and the band is the library's own. That covers an anaesthetic's effect-site
target, an opioid's MEAC multiple, an antibiotic's MBC band and the orientation bands of what
ordinary doses reach. The help page says so and names what the band is.

### Drawn

| Drug | Band | Source |
|---|---|---|
| digoxin, digoxinIV, digoxinIVPediatric | 0.5–2 ng/mL serum, before the next dose or ≥ 6 h after | LANOXIN label (2019) §2.5: below 0.5 less effective, above 2 more toxic without benefit. Heart failure: 0.5–0.8 (Rathore 2003, abstract). AF guideline: below 1.2 (Joglar 2024, full text) |
| milrinone, milrinonePediatric | 100–300 ng/mL, typical 167 | PRIMACOR label (2003); Hornik 2019 (full text) |
| amiodarone | 1.0–2.5 mg/L, typical 1.5 | CORDARONE label (2018) §12.2: "much below 1 mg/L are often ineffective… above 2.5 mg/L are generally not needed". The Health Canada monograph the model cited was not reachable |

| warfarin, fluindione, acenocoumarol | INR 2.0–3.0, typical 2.5 | Coumadin label §2.2 and Holbrook 2012 (abstract). Fluindione and acenocoumarol take the vitamin K antagonist class target; the Previscan and Sintrom product information is still to be read |
| heparinCPB | ACT 480–600 s, typical 480 | EACTS/EACTAIC/EBCP 2024 §9.3.1 (full text): an ACT above 480 s on bypass. No source gives an upper limit; 600 s is Steven L. Shafer's choice. STS/SCA/AmSECT 2018 abstract only |

| procainamide | 3–10 mg/L, typical 6.5 | Hospira injection label (read in full, 2026-10-28): therapeutic plasma levels "reported to be 3 to 10 mcg/mL"; 6.5 mcg/mL at 50 mcg/kg/min and a 3 h half-life, where the model gives 7.4 at its 3.8 h. Drawn 2026-10-28; the 4–10 mg/L the help page had quoted was never sourced |

Digoxin, milrinone and amiodarone were already drawn; only their sources were checked. The
anticoagulant bands are new, drawn on Steven L. Shafer's decision of 2026-10-27 as references
for reading the curve. The anticoagulants remain research models, not dosing guidance.

### Recorded, not drawn

| Drug | Range | Why not drawn |
|---|---|---|
| heparin | anti-Xa 0.3–0.7 U/mL | `not_applicable`: the model plots anti-IIa activity (Jia 2015). Smythe 2016 (full text): "widely accepted… despite limited data" |
| salicylate | 150–300 mg/L | `not_applicable`: anti-inflammatory doses (FDA OTC monograph M013), where this low-dose linear model does not apply |
| magnesiumSulfate | 2.5–7.5 mEq/L (label); 2–3.5 mmol/L (Okusanya 2016, full text) | `not_applicable`: the curve is the rise above baseline |
| calcium | ionized calcium reference range | `not_applicable`: the curve is the rise above baseline |
| diltiazem | "minimum therapeutic plasma levels… 50 to 200 ng/mL" | `not_applicable`: CARDIZEM tablets label, chronic oral; the model is intravenous rate control |
| amiodaroneIV | 1.0–2.5 mg/L | `not_applicable`: a chronic oral window, not intravenous loading |

### No range established

- **Library bands, now labelled as such.** These are the hypnotics without a Table 5 row
  (propofol, dexmedetomidine, etomidate, remimazolam), the MEAC-based opioids, and the display
  bands of hydrocodone, oxymorphone, codeine, tramadol and desmetramadol. Also: the
  neuromuscular blockers and their reversal drugs, the corticosteroids, ondansetron,
  aprepitant, fosaprepitant, fenoldopamPediatric, naloxone, oxytocin, acetaminophen,
  ibuprofen, lidocaine and mannitol.
- **Antibiotics.** The bands are MBC bands. Abdul-Aziz 2020 (full text) states beta-lactam
  targets only relative to the MIC. Vancomycin's 2020 target is an AUC24/MIC of 400–600, an
  exposure rather than a concentration; trough monitoring was the 2009 surrogate (Rybak 2020,
  abstract; the full text could not be reached). Gentamicin's label expects adult peaks of
  4–6 mcg/mL and says to avoid peaks above 12 and troughs above 2; it is not a range.
- **Lidocaine.** The intravenous antiarrhythmic label states no range, only CNS reactions above
  6 mcg/mL. The help page no longer says the band is used for ventricular arrhythmias.
- **Mannitol.** Cook 2020 (full text) calls 320 mOsm/kg common practice, says it does not
  predict kidney injury, and suggests the osmolar gap instead.
- **No band at all.** These are the DOACs and fondaparinux (their labels advise against
  monitoring; Smythe 2016: fondaparinux targets "not established"), enoxaparin (targets "not
  clinically validated"), bivalirudin and argatroban (clotting-time targets only), the
  antidiabetics (metformin: Kajbaf 2016 found 65 different "therapeutic" values), the
  antihypertensives, the vasoactive drugs (each label read: none), the other NSAIDs, the
  regional local anaesthetics (toxic thresholds only), the inhaled agents (MAC) and the illicit
  drugs.

## Product monographs (FDA, EMA)

Only the older antiseizure drugs' labels state a range: Dilantin (10–20 mcg/mL total,
1–2 unbound), Depakote (50–100 mcg/mL), Zarontin (40–100 mcg/mL), and Tegretol outside
the US (about 4–12 mcg/mL). These were read through search excerpts of the labels, and the CSV
says so. Sezaby (neonatal phenobarbital) gives weight-based dosing and observed concentrations,
but no target. Hart 2026 takes primidone's range from its SPC. The other labels were not read
in this review and are recorded as "Not checked".

## Open items

1. Read Lowenstein 1988 (Neurology 38:395-400), Helfer 2026's source for pentobarbital's 4–21 mg/L; not available through Lane Library.
2. Read the labels marked "Not checked".
3. A summed parent-plus-metabolite line would let the active-moiety ranges (risperidone,
   aripiprazole, venlafaxine, fluoxetine) be drawn.
4. `THERAPEUTIC_RANGE_CATEGORIES` is every category (done 2026-10-27). Still to read: the
   source of a NAPA range (the procainamide label gives none); the full text of
   Rybak 2020 (AJHP 77:835-864) and of the STS/SCA/AmSECT 2018 bypass guideline; the Previscan
   and Sintrom product information; the Health Canada amiodarone monograph; and the sources,
   not recorded in the models, of the propofol, remimazolam, rocuronium and oxytocin bands.
5. Read the primary papers behind the new bands (below).

## Papers that would complete the verification

Each was identified in PubMed (2026-10-27); only abstracts have been read, except Matsuda 2022,
Barr 2001, McGough 2003, Boellner 2010, Laffont 2022, Lesser 1992, Hosie and Nimmo 1991, Johannessen Landmark 2015, Chermá 2017, Patsalos 2008, Mion 2013, Drover 2004, Clements 1982, Ku 2018, Gourlay 1984, Dahlström 1982, Nasser 2014 and Mould 1995, read in full.

| Drug | Paper | PMID | Why |
|---|---|---|---|
| midazolam | Coté, Lerman, Anderson, *A Practice of Anesthesia for Infants and Children*, 2013, p. 141 | (book) | Where the 50–150 ng/mL Table 5 takes from Matsuda 2022 (read in full) comes from |
| midazolam, lorazepam | Barr 2001, *Anesthesiology* 95:286 | 11506097 | Read in full: the source of both bands |
| midazolam | Crevoisier 1983, *Br J Clin Pharmacol* 16 Suppl 1:51S | 6138082 | Concentration and effect, oral and IV |
| midazolam | Link 2008, *Br J Clin Pharmacol* 66:473 | 18537963 | Concentrations after 10 mg by mouth |
| alprazolam | Ciraulo 1990, *J Clin Psychopharmacol* 10:27 | 2407754 | Dose to steady state; response above 20 ng/mL |
| alprazolam | Lesser 1992, *Am J Psychiatry* 149:1556 | 1415824 | Read in full: 50% major panic response at 15 ng/mL, 75% at 48; sedation 40, ataxia 70, slurred speech 100 |
| alprazolam | Wincor 1991, *J Clin Psychopharmacol* 11:48 | 2040715 | The same |
| alprazolam | Greenblatt and Wright 1993, *Clin Pharmacokinet* 24:453 | 8513649 | The former 20–40 ng/mL |
| temazepam | Hosie and Nimmo 1991, *Br J Anaesth* 66:20 | 1997054 | Read in full: 211 ng/mL awake, 785–940 sedated; above 200 for sedation |
| zolpidem | Drover 2004, *Clin Pharmacokinet* 43:227 | 15005637 | Read in full: states no range, only the kinetics (10 mg peaks at 120 µg/L at 1.4 h) |
| zolpidem | Greenblatt 1998, *Clin Pharmacol Ther* 64:553 | 9834048 | The same |
| buprenorphine | Laffont 2022, *Front Pharmacol* 13:1052113 | 36467036 | Read in full: 2–3 ng/mL sustained for use and craving (68–75% occupancy); withdrawal controlled from 1 ng/mL; sublingual troughs 0.7–1.4 |
| buprenorphine | Nasser 2014, *Clin Pharmacokinet* 53:813 | 25015174 | Read in full: Emax 91.4%, EC50 0.67 ng/mL, as the library uses; 70% occupancy at about 2 ng/mL |
| buprenorphine | Elarabi 2022, *Psychopharmacology* 239:3213 | 35953563 | One of the pharmacokinetic studies behind 0.5–2.5 |
| gabapentin, pregabalin | Johannessen Landmark 2015, *Seizure* 28:88 | 25758302 | Read in full: laboratory ranges (gabapentin 70–120 µmol/L, some labs from 10–20; pregabalin 10–30), no effect data |
| gabapentin | Larsen Burns 2019, *Acta Neurol Scand* 139:446 | 30710348 | TDM of gabapentin by indication |
| gabapentin | Healy 2023, *Pharmacol Res Perspect* 11:e01138 | 37803937 | The former band (open access) |
| pregabalin | Chan 2021, *Clin Pharmacol Ther* 110:132 | 33280106 | The former band |
| mixedAmphetamineSalts | McGough 2003, *J Am Acad Child Adolesc Psychiatry* 42:684 | 12921476 | Source of 23–45 ng/mL, and the model's fit |
| lisdexamfetamine | Boellner 2010, *Clin Ther* 32:252 | 20206783 | Source of 44–160 ng/mL |
| methylphenidate | Chermá 2017, *Eur J Drug Metab Pharmacokinet* 42:295 | 27220743 | Read in full: medians and interquartile ranges, in whole blood, of the 36 of its 50 Concerta patients on the three doses taken by more than five (Table 4); Table 3's medians for all 50 are 5.8 and 10 ng/mL |
| methylphenidate | Stevens 2010, *J Child Adolesc Psychopharmacol* 20:49 | 20166796 | The alert level |
| methadone | Gourlay 1984, *Anesthesiology* 61:19 | 6742480 | Read in full: MEC 57.9 ± 15.2 ng/mL in whole blood, 16 surgical patients; the MEAC of 60 |
| methadone | Inturrisi 1987, *Clin Pharmacol Ther* 41:392 | 3829576 | Quotes Gourlay; 290 ng/mL in tolerant patients |
| morphine | Dahlström 1982, *Clin Pharmacokinet* 7:266 | 7094501 | Read in full: MEC 16 ± 9 ng/mL (men 21, women 13) on PCA, specific assay; twice the library's MEAC of 8 |
| morphine, oxycodone | Thaulow 2023, *Ther Drug Monit* 45:777 | 37296504 | Dose-specific chronic pain ranges behind Table 5 |
| morphine | Mitchell 2003, *Drug Alcohol Depend* 72:85 | 14563546 | The opioid use disorder range |
| oxycodone, many | Schulz 2020, *Crit Care* 24:195 | 32375836 | The compilation behind oxycodone's range (open access) |
| ketamine | Mion 2013, *CNS Neurosci Ther* 19:370 | 23575437 | Read in full: the source of the band ("Moen" in the old code), citing Clements 1982 |
| ketamine | Clements 1982, *J Pharm Sci* 71:539 | 7097501 | Read in full: analgesia consistently above 160 ng/mL, not below 80; means 186 (no or mild pain) and 101 (moderate or severe) |
| diazepam | Ku 2018, *CPT Pharmacometrics Syst Pharmacol* 7:718 | 30267478 | Read in full: 200–600 ng/mL is a literature surrogate Ku adopted, not tested in children; no exposure-response found |
| diazepam | Mould 1995, *Clin Pharmacol Ther* 58:35 | 7628181 | Read in full: DSST EC50 116 ng/mL pooled (98 and 132 by dose; individuals 0.1–522), the band's floor |
| diazepam | Friedman 1992, *Clin Pharmacol Ther* 52:139 | 1505149 | Concentration and effect of oral diazepam |
| diazepam | Chevassus 2004, *BMC Clin Pharmacol* 4:3 | 15102335 | The kept band's 300 ng/mL, which this insulin-secretion study cites only in passing (open access) |
| temazepam | van Steveninck 1994, *Clin Pharmacol Ther* 55:546 | 8181199 | The former band's top |
| temazepam | Saletu 1986, *Acta Psychiatr Scand Suppl* 332:67 | 2883830 | The former band's floor |
| phenobarbital, ethosuximide, gabapentin | Patsalos 2008, *Epilepsia* 49:1239 | 18397299 | Read in full: Table 1 gives phenobarbital 10–40 (drawn until 2026-10-27), ethosuximide 40–100, gabapentin 2–20 mg/L |
| antiseizure drugs | Patsalos 2018, *Ther Drug Monit* 40:526 | 29957667 | The 2018 update |

## References

- McGough JJ, Biederman J, Greenhill LL, et al. Pharmacokinetics of SLI381 (ADDERALL XR), an
  extended-release formulation of Adderall. *J Am Acad Child Adolesc Psychiatry*
  2003;42:684-691. doi [10.1097/01.CHI.0000046850.56865.CB](https://doi.org/10.1097/01.CHI.0000046850.56865.CB)
- Boellner SW, Stark JG, Krishnan S, Zhang Y. Pharmacokinetics of lisdexamfetamine dimesylate
  and its active metabolite, d-amphetamine, with increasing oral doses of lisdexamfetamine
  dimesylate in children with attention-deficit/hyperactivity disorder. *Clin Ther*
  2010;32:252-264. doi [10.1016/j.clinthera.2010.02.011](https://doi.org/10.1016/j.clinthera.2010.02.011)
- Matsuda H, Takenaka J, Kawabata M, et al. Serum concentration of oral midazolam as pediatric
  preanesthetic medication and factors related to the sedation level. *Acta Anaesthesiol Scand*
  2022;66:818-822. doi [10.1111/aas.14077](https://doi.org/10.1111/aas.14077)
- Laffont CM, Ngaimisi E, Gopalakrishnan M, et al. Buprenorphine exposure levels to optimize
  treatment outcomes in opioid use disorder. *Front Pharmacol* 2022;13:1052113.
  doi [10.3389/fphar.2022.1052113](https://doi.org/10.3389/fphar.2022.1052113)
- Chermá MD, Josefsson M, Rydberg I, et al. Methylphenidate for treating ADHD: a naturalistic
  clinical study of methylphenidate blood concentrations in children and adults with optimized
  dosage. *Eur J Drug Metab Pharmacokinet* 2017;42:295-307.
  doi [10.1007/s13318-016-0346-1](https://doi.org/10.1007/s13318-016-0346-1)
- Barr J, Zomorodi K, Bertaccini EJ, Shafer SL, Geller E. A double-blind, randomized comparison
  of i.v. lorazepam versus midazolam for sedation of ICU patients via a pharmacologic model.
  *Anesthesiology* 2001;95:286-298. doi [10.1097/00000542-200108000-00007](https://doi.org/10.1097/00000542-200108000-00007)
- Hosie HE, Nimmo WS. Temazepam absorption in patients before surgery. *Br J Anaesth*
  1991;66:20-24. doi [10.1093/bja/66.1.20](https://doi.org/10.1093/bja/66.1.20)

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
