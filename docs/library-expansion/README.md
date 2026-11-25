# Rounding out stanpumpR: the library expansion

Steven L. Shafer chose the drugs on 2026-11-18, with a regular return to the list (daily for now; see *The weekly routine*):
- **Antibiotics:** tazobactam, imipenem, ceftazidime/avibactam, teicoplanin. Tazobactam became Zosyn and Zerbaxa, and avibactam Avycaz, the same day (see *Combination products* below).
- **Antifungals:** fluconazole, posaconazole, isavuconazole, caspofungin, micafungin.
- **Antimalarials** and **antivirals**.
- **Transplant:** mycophenolic acid, to go with tacrolimus.
- **Anesthesia:** single models of esketamine, ciprofol, landiolol, levosimendan and nalbuphine.
- **Biologics** relevant to surgery and to sepsis.

A second tranche followed the same day: the drugs most prescribed in US outpatients that stanpumpR lacks, from a report ChatGPT wrote for Steven (17 November 2026; see *The outpatient tranche* below).

The list will be revisited every week or so. Each drug is still added the ordinary way: `docs/adding-a-drug.md`, all seven items, every number read in the paper. This directory only finds the papers and keeps track.

## Files

| File | What it is | Edited by |
|---|---|---|
| `combination-only.csv` | One row per component given only in a combination product (Steven L. Shafer, 2026-11-18 and 2026-11-23: "Clavulanate isn't given as a drug by itself, so it should not be included in stanpumpR"). Gives its role, the US products that carry it, whether DailyMed has a single-ingredient label, and `status`: `not_included`, or `included` for a component kept as a drug of its own (ritonavir, a booster labelled alone). | Hand, checked against DailyMed. The cross-check reads it to set apart these components in `docs/nlmixr2lib/missing-drugs.md`. |
| `plan.csv` | One row per planned drug. Gives its category, the names nlmixr2lib files it under, priority, `status` and notes (product, dose basis, US approval). `status` is `planned`, `in_progress`, `blocked`, `done`, `library_change` for a change to a drug the library already has, which needs no paper, or `not_included` for a drug decided against (a component given only in a combination with no US product, as piperaquine; `combination-only.csv`). The candidates script and the weekly routine leave the last two alone. | Hand. The weekly routine updates `status`. |
| `candidates.csv` | Every nlmixr2lib model of every planned drug, ranked by study size. `pick` marks the papers to get first: the largest study, and the most recent if that is another paper. | `data-raw/libraryExpansionCandidates.R` |
| `outpatient-leads.csv` | The papers and FDA documents ChatGPT's report gave for each outpatient drug, one row per lead, with the report's evidence letter (P population fit, C compartmental, D descriptive, R FDA review or label) and what was checked. For a drug nlmixr2lib does not model, these are the papers to get. | Hand, from the report. |
| `archive-status.csv` | For each pick, and each lead of a drug nlmixr2lib does not model: its PMID and PMC ID (from PubMed's ID converter, or a PubMed search by DOI for a paper not in PMC), and whether it is in the stanpumpR paper archive (a Drive search by file name). | The weekly routine, or by hand. |
| `download-ids.txt` | The papers to download as a bare list, one PMID per line (a DOI where there is none): the picks not in the archive, then the leads of drugs nlmixr2lib does not model. Paste it into the local session that downloads papers. | `data-raw/libraryExpansionCandidates.R` |
| `downloads.md` | The picks not yet in the archive, by category, with DOI, PubMed and free-PMC links and the archive file name to save each under. | `data-raw/libraryExpansionCandidates.R` |

Rebuild after `plan.csv` or `archive-status.csv` changes, or after a fresh run of `data-raw/nlmixr2libCrossCheck.R`:

```bash
Rscript --vanilla data-raw/libraryExpansionCandidates.R .
```

## The outpatient tranche

ChatGPT's report ranked the drugs stanpumpR lacked against ClinCalc's 2024 US top 300 (prescription counts from MEPS), flagged FDA's oral narrow-therapeutic-index list, and gave starting papers for each. Its claims were checked before anything was recorded here:
- **The NTI claim holds.** FDA's April 2026 product-specific guidance list (p. 11) names 14 oral NTI ingredients. Of these, stanpumpR lacked levothyroxine and liothyronine. Levothyroxine has since been added (#771, with insulin aspart and insulin degludec), so **liothyronine** is the one left, at priority 1.
- **Every PMID is real, and none is by Gideon Koren.** Two papers are mislabelled: PMID 24888255 is a glucose-clamp study of IDegAsp, not a population fit; PMID 34201093 is a paediatric trial-design model. The adult levothyroxine DOI resolves to PMID 40348586. The insulin degludec DOI 10.1007/s00210-026-06074-2 is not in PubMed.
- **Several drugs are already in the antihypertensive or antidiabetic registries** with a status of their own (amlodipine's Donnelly 1993 is ready for source reproduction; losartan, furosemide, spironolactone, clonidine and nifedipine have records awaiting source extraction). `plan.csv` notes each.
- **Hydroxychloroquine** was already planned under Antimalarials, so it is not repeated. **Desvenlafaxine** is in the library only as venlafaxine's metabolite; its row is a route to add, not a new drug.

Priorities: 1 for an FDA oral NTI drug, 2 for ranks 1-50, 3 for ranks 51-147. The report's caveats (endogenous baselines for the hormones, prodrugs and active metabolites, release formulations, a drug whose only data are descriptive) are in each row's notes. They decide whether a drug can be modelled at all, before any parameter is read.

The report is a lead, not a source, like any other model's reading (AGENTS.md): every number in a model is still read in the paper.

## Status on 2026-11-18

- **First tranche: none of the 107 picks was in the archive.** Every one has a PMID, checked against a Drive search by `PMID_` and DOI file name. PubMed's ID converter knows only papers in PubMed Central, so five PMIDs came from a PubMed search by DOI instead. 101 of the 107 are free in PubMed Central, so they need no library login.
- **Outpatient tranche: 43 more picks, none in the archive**, 38 free in PubMed Central. Of the 42 report leads for drugs nlmixr2lib does not model, one is in the archive (losartan's Lo 1995, PMID 8529329, which the antihypertensive registry already cites).
- **`download-ids.txt` holds 191 PMIDs**: 150 picks and 41 leads.
- **The picks are chosen mechanically.** The "most recent" pick is sometimes a small or specialised study, such as a PBPK model or neonates only. Read the `candidates.csv` rows of a drug before settling on its source.

### Drugs nlmixr2lib does not model: leads from a PubMed search (2026-11-18)

| Drug | Leads |
|---|---|
| Idarucizumab | Wang 2020, healthy Chinese volunteers, PK/PD (PMID 32691242). Athavale 2020, a pharmacokinetic analysis of incomplete responses (PMID 32267185). The FDA's BLA review of Praxbind is the likely complete source. |
| Andexanet alfa | No human population model in PubMed. The FDA's BLA clinical pharmacology review of Andexxa is the likely source. |
| Alteplase | No population model found by this search. The label and the classic healthy-volunteer studies are to be found. Tenecteplase has Tang 2022 (PMID 36278839, already a pick). |
| Prothrombin complex concentrate | The search returns mostly factor IX papers. A four-factor PCC model (Kcentra) is to be found, probably in its FDA review. |
| Recombinant factor VIIa (eptacog alfa) | Older non-compartmental studies only, on a first look. |
| Vilobelimab | Lim 2023, PK in critically ill ventilated COVID-19 patients (PANAMO, PMID 37332066). |

### Weekly run, 2026-11-18 (the first)

- **nlmixr2lib moved** from e2b902f to d8a308a (4,228 to 4,581 models). The picks went from 150 to 153, and nine new picks were given PMIDs and PMC IDs. Two report leads dropped out because nlmixr2lib now models their drugs.
- **114 papers arrived** in the archive on 2026-11-18, all under `PMID_` names: 111 picks and 3 leads. **106 of the 153 picks are now in the archive**, covering 85 drugs. None arrived under a `DOI_` name.
- **Tazobactam and imipenem**, the first two drugs in plan order, were taken up. Both are `blocked`. `plan.csv` gives the numbers read and the reasons:
  - **Tazobactam:** its pick, Zhang 2021, is a *ceftolozane*-tazobactam fit. Whether it could stand for Zosyn's tazobactam, and the dose units, were put to Steven; he answered the same day (*Combination products* below).
  - **Imipenem:** its pick, Patel 2022, is read. Its Supplementary Methods (the Cockcroft-Gault weight) and the sources the antibiotic band needs (an imipenem MBC/MIC ratio and a plain-imipenem *P. aeruginosa* MIC) are still to get. The model's volumes and clearance change with pneumonia and with ventilation, and stanpumpR has no infection field. Steven decided on 2026-11-23: "Add an infection field. However, that should be a separate project." Imipenem and Zerbaxa wait for it.

### Weekly run, 2026-11-19

- **nlmixr2lib has not moved** (still d8a308a); the cross-check was rerun against stanpumpR 7e9b4f4, which adds liothyronine and two more shared papers.
- **67 more papers had arrived** in the archive on 2026-11-18 after that day's check (filed 15:34-18:00 UTC), all under `PMID_` names: 38 picks (among them hydroxychloroquine's Carmichael 2003, whose PMID 14639053 the status file lacked) and 29 report leads. **144 of the 153 picks are now in the archive.** None arrived under a `DOI_` name, and no supplement arrived for any pick read this run.
- **No drug could be implemented.** The first eight planned drugs in plan order were taken up and every one is now `blocked`, each with what was read and what would unblock it in `plan.csv`:
  - **Waiting on a supplement:** ceftazidime and Avycaz (Franzese 2022 prints its parameters only in Tables S2 and S3), posaconazole (van Iersel 2018's covariate equations are only in its Fig. S5).
  - **Waiting on a decision of Steven's:** Zerbaxa (Zhang 2021 is fully read, all in its Table 2; the infection state the curve represents is the only obstacle, and it is shared with imipenem and Avycaz), isavuconazole (Weibull oral absorption: discretise, or offer IV only), caspofungin (Yang 2022's steps at 70 kg and albumin 35 g/L), teicoplanin (Chae 2026's distribution comes from profiles simulated from two other models).
  - **The pick is the wrong population:** fluconazole (infants under 750 g; adults on CRRT). An adult source is needed.

### Weekly run, 2026-11-20

- **Nothing arrived.** No pick, report lead or requested supplement reached the archive since the 2026-11-19 run (the supplements `PMID_34687548_sup1`, `PMID_29712663_sup1` and `PMID_34704389_sup1` included). nlmixr2lib has not moved (still d8a308a); the cross-check was rerun against master 8739b9d. This run was built on the 2026-11-19 run's pull request (#799), which was still open.
- **Tafenoquine was implemented** (Thakkar 2018, 675 healthy volunteers and patients with *P. vivax* malaria) in its own draft pull request. Its curve is the patients'; whether a prophylaxis curve should use the healthy volunteers' volumes is a decision for Steven.
- **Micafungin is blocked on a better paper.** Its pick, Maseda 2018, is a nonparametric fit whose means and medians are far apart, the reason piperacillin's Felton 2014 was rejected. Wasmann 2019 (PMID 30649375) and Jullien 2017 (PMID 27609051) are the adult sources to get.
- **Four antimalarials are blocked without being read**, because the papers cannot settle what blocks them: artemether and lumefantrine are given only as Coartem, and piperaquine only with dihydroartemisinin, which the combination-product rule below makes a decision of Steven's; artesunate's US product is intravenous and its picks are rectal and oral.

### Combination products (Steven L. Shafer, 2026-11-18)

"Tazobactam should not be a drug at all. It is only used in combination with other antibiotics." So a component given only in combination is not a drug of its own. The product is the drug, dosed in the units of its FDA label:
- **Zosyn** (piperacillin/tazobactam), the name Steven chose: "I prefer generic names, but for combination products that can be a problem. Call it Zosyn." Dosed in g of the combined product for adults (2.25, 3.375 and 4.5 g; 8:1), mg/kg of the combined product for children (90 and 112.5 mg/kg). It is stanpumpR's piperacillin (Kong 2025), which already takes the product and plots piperacillin, so it is a `library_change` and needs no paper. Today it is dosed in mg pip-tazo.
- **Zerbaxa** (ceftolozane/tazobactam): g of the combined product for adults (1.5 g = 1 g ceftolozane + 0.5 g tazobactam; 3 g for HABP/VABP), mg/kg of the combined product for children (30, 60 and 75 mg/kg). A new drug, from Zhang 2021's ceftolozane model; the curve is labelled ceftolozane, as Zosyn's is piperacillin.
- **Avycaz** (ceftazidime/avibactam), by the same rule, replaces the planned avibactam; its curve is labelled ceftazidime.

A brand name is a valid R name (MEGX and MDMA are drugs already), so Zosyn needs no display name. "The PK should be labelled piperacillin": the drug is Zosyn and its curve is labelled piperacillin. Renaming the drug piperacillin leaves links that name it to be mapped. Steven named the other two the same way the same day: "Use Zerbaxa and Avycaz too." Both wait for the infection field (decided 2026-11-23, a separate project).

On 2026-11-23 Steven applied the rule to the rest of the plan ("Augmentin is a good example. Clavulanate isn't given as a drug by itself, so it should not be included in stanpumpR") and asked for such components to be recorded explicitly: `combination-only.csv` lists them, checked against DailyMed. The plan's rows follow it:
- **Augmentin** (amoxicillin/clavulanate) replaces the amoxicillin/clavulanate row; its curve is amoxicillin, clavulanate not plotted.
- **Coartem** (artemether/lumefantrine) replaces the artemether and lumefantrine rows. Both components are drawn (Steven, 2026-11-24, below).
- **Kaletra** (lopinavir/ritonavir) and **Paxlovid** (nirmatrelvir/ritonavir) replace the lopinavir and nirmatrelvir rows; their curves are lopinavir and nirmatrelvir.
- **Piperaquine** is `not_included`: its only product, Eurartesim (with dihydroartemisinin), is not US-approved.
- Cobicistat and ritonavir are labelled alone (Tybost, Norvir) though given almost only as boosters. Steven decided on 2026-11-23: cobicistat is `not_included` ("Leave out Cobicistat"), and ritonavir is `included`, a drug of its own with a row in `plan.csv` ("I would leave ritonavir in as a model, because the pharmacokinetics of CYP3A4 inhibition are interesting").

### Which moieties a combination product draws (Steven L. Shafer, 2026-11-24)

"For Coartem, both components are active. Both should appear. However, for combination products where one of the moieties exists solely to change the clinical efficacy of the other moiety, only show the clinically active moiety. For example, paxlovid should only show nirmatrelvir, not ritonivir. Zosyn should only show piperacillin."

So:
- **Every component with an effect of its own is drawn.** Coartem draws artemether (with its active metabolite dihydroartemisinin) and lumefantrine, each from its own model. Artemether's disposition is Ding 2026's (the Coartem row). This needs the product-expansion mechanism, in which one dose-table row becomes a row for each component, each simulated on its own model.
- **A component that is there only to change the other's efficacy or exposure is not drawn.** That covers a beta-lactamase inhibitor (tazobactam, clavulanate, avibactam, relebactam) and a pharmacokinetic booster (ritonavir in Paxlovid and Kaletra, cobicistat). So Zosyn draws piperacillin, Zerbaxa ceftolozane, Avycaz ceftazidime, Augmentin amoxicillin, Paxlovid nirmatrelvir and Kaletra lopinavir. These are single drugs plotting the active moiety, as Zosyn already is, and need no expansion.
- **The booster's effect is still in the curve.** A source for a boosted product is fitted in patients taking the booster, so the drawn moiety's disposition is its boosted disposition. Ritonavir on its own (Norvir) remains a drug of its own (2026-11-23); drawing its effect on another drug given separately would need an interaction model, a separate question.

## Downloading

`downloads.md` lists each paper with the file name the archive expects: `PMID_<pmid>.pdf`, or, for a paper with no PMID, `DOI_<doi>.pdf` with every character but letters, digits and dots made `_` (`DOI_10.2133_dmpk.13.376.pdf`). Save a supplement under the same name with a suffix, `_sup1`, `_sup2` and so on (or the publisher's own `_mmc1`, `_ESM1`, `_SDC1`), never under the paper's own name, which would replace it. The archive's pipeline takes it from there.

Lane Library is needed only for the few papers not free in PMC. Claude in Chrome can do the downloading, but only in a session on Steven's own computer, where Chrome runs: the Claude desktop app, or `claude remote-control` in the stanpumpR folder. A cloud session cannot reach that browser.

## The weekly routine

A scheduled task, **"stanpumpR weekly library expansion"**, runs a fresh cloud session every day at 8:41 pm Pacific (weekly on Sundays as first scheduled; Steven made it daily on 2026-11-18, "for now"). The first run was a trial on 2026-11-18, started by hand. Steven created it on 2026-11-18 from claude.ai's routines page, with the StevenLShafer/stanpumpR repository and the Google Drive and PubMed connectors. Each run:
1. refreshes the nlmixr2lib cross-check and this directory;
2. checks which picks and leads have reached the archive;
3. implements at most two planned drugs whose source papers are now in the archive, each as its own draft pull request, by the full procedure;
4. updates `plan.csv`;
5. ends with a report of what arrived, what was implemented, what to download next, and any decision only Steven can make.

It never merges, and it records every question for Steven where AGENTS.md says questions go.

Its prompt is `weekly-routine-prompt.md`, below the `---` line. A change to what the routine does is made in both places: that file, and the task's own prompt on the routines page. The page is where it runs from: a scheduled task made from a cloud session carries no connectors, so the task must be created or edited there.

### Weekly run, 2026-11-21

- **nlmixr2lib had not moved** (d8a308a); the lists were regenerated anyway.
- **63 picks and leads marked missing were in the archive** (filed 2026-11-18, `PMID_` names); `archive-status.csv` was stale. 10 picks and 10 leads are still to download (`downloads.md`); Pai 2025 (PMID 41159735, Zerbaxa's newest pick) and Ciprofol's PMID 38440939 are among them.
- **No drug was implemented this run.** Zerbaxa stays blocked on the infection-state decision; ceftazidime (Avycaz's partner) is next in plan order and needs the full seven items plus the antibiotic band sources, which this run did not complete.
