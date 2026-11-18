# Rounding out stanpumpR: the library expansion

Steven L. Shafer chose the drugs on 2026-11-18, with a weekly return to the list:
- **Antibiotics:** tazobactam, imipenem, ceftazidime/avibactam, teicoplanin.
- **Antifungals:** fluconazole, posaconazole, isavuconazole, caspofungin, micafungin.
- **Antimalarials** and **antivirals**.
- **Transplant:** mycophenolic acid, to go with tacrolimus.
- **Anesthesia:** single models of esketamine, ciprofol, landiolol, levosimendan and nalbuphine.
- **Biologics** relevant to surgery and to sepsis.

The list will be revisited every week or so. Each drug is still added the ordinary way: `docs/adding-a-drug.md`, all seven items, every number read in the paper. This directory only finds the papers and keeps track.

## Files

| File | What it is | Edited by |
|---|---|---|
| `plan.csv` | One row per planned drug. Gives its category, the names nlmixr2lib files it under, priority, `status` and notes (product, dose basis, US approval). | Hand. The weekly routine updates `status`. |
| `candidates.csv` | Every nlmixr2lib model of every planned drug, ranked by study size. `pick` marks the papers to get first: the largest study, and the most recent if that is another paper. | `data-raw/libraryExpansionCandidates.R` |
| `archive-status.csv` | For each pick: its PMID and PMC ID (from PubMed's ID converter), and whether it is in the stanpumpR paper archive (a Drive search by file name). | The weekly routine, or by hand. |
| `download-ids.txt` | The same papers as a bare list, one PMID per line (a DOI where there is none), to paste into the local session that downloads papers. | `data-raw/libraryExpansionCandidates.R` |
| `downloads.md` | The picks not yet in the archive, by category, with DOI, PubMed and free-PMC links and the archive file name to save each under. | `data-raw/libraryExpansionCandidates.R` |

Rebuild after `plan.csv` or `archive-status.csv` changes, or after a fresh run of `data-raw/nlmixr2libCrossCheck.R`:

```bash
Rscript --vanilla data-raw/libraryExpansionCandidates.R .
```

## Status on 2026-11-18

- **None of the 107 picks was in the archive.** Every one has a PMID, checked against a Drive search by `PMID_` and DOI file name. PubMed's ID converter knows only papers in PubMed Central, so five PMIDs came from a PubMed search by DOI instead.
- **101 of the 107 are free in PubMed Central**, so they need no library login.
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

## Downloading

`downloads.md` lists each paper with the file name the archive expects: `PMID_<pmid>.pdf`, or, for a paper with no PMID, `DOI_<doi>.pdf` with every character but letters, digits and dots made `_` (`DOI_10.2133_dmpk.13.376.pdf`). Save a supplement under the same name with a suffix, `_sup1`, `_sup2` and so on (or the publisher's own `_mmc1`, `_ESM1`, `_SDC1`), never under the paper's own name, which would replace it. The archive's pipeline takes it from there.

Lane Library is needed only for the few papers not free in PMC. Claude in Chrome can do the downloading, but only in a session on Steven's own computer, where Chrome runs: the Claude desktop app, or `claude remote-control` in the stanpumpR folder. A cloud session cannot reach that browser.

## The weekly routine (proposed, not yet scheduled)

The plan is a scheduled task that runs a fresh cloud session each week, on Sunday evening, Pacific time. Each run would:
1. refresh the nlmixr2lib cross-check and this directory;
2. check which picks have reached the archive;
3. implement at most two planned drugs whose source papers are now in the archive, each as its own draft pull request, by the full procedure;
4. update `plan.csv`;
5. end with a report of what arrived, what was implemented, what to download next, and any decision only Steven can make.

It would never merge, and it would record every question for Steven where AGENTS.md says questions go.

Creating the task from the session of 2026-11-18 was refused by its permission system, so it awaits Steven's go-ahead. That means either a permission rule for scheduled tasks, or creating it himself with the prompt in `weekly-routine-prompt.md`.
