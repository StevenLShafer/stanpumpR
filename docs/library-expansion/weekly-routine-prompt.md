# The weekly routine's prompt

This is the prompt of the scheduled task "stanpumpR weekly library expansion" (claude.ai routines page; Sundays at 8:41 pm Pacific, `CRON_TZ=America/Los_Angeles 41 20 * * 0`; the StevenLShafer/stanpumpR repository and the Google Drive and PubMed connectors). Keep this copy and the task's own prompt the same.

---

Weekly library-expansion run for the stanpumpR repository (github.com/StevenLShafer/stanpumpR), requested by its maintainer, Steven L. Shafer, on 2026-11-18 ("return to this list every week or so to help round out stanpumpR").

Read AGENTS.md first and follow it throughout. Above all:
- every number in a model is read in the paper itself;
- every drug has the seven items of `docs/adding-a-drug.md`;
- run the concurrency preflight in "Several sessions run at once";
- use no paper by Gideon Koren;
- use nlmixr2lib for its data only, never its code.

Then read `docs/library-expansion/README.md`, `docs/library-expansion/plan.csv` and `docs/nlmixr2lib/README.md`.

Do this, in order:

1. **Refresh the lists.** Run:

   ```bash
   git clone --depth 1 https://github.com/nlmixr2/nlmixr2lib <scratch dir>
   ```

   ```bash
   Rscript --vanilla data-raw/nlmixr2libCrossCheck.R <clone> .
   ```

   ```bash
   Rscript --vanilla data-raw/libraryExpansionCandidates.R .
   ```

2. **Check the archive.** Take every pick in `docs/library-expansion/candidates.csv`, and every lead `downloads.md` lists for a drug nlmixr2lib does not model (from `docs/library-expansion/outpatient-leads.csv`), that `docs/library-expansion/archive-status.csv` marks as not in the archive.
   - Search the stanpumpR paper archive on Google Drive (folder `stanpumpRPapers`; see the "Source papers" section of AGENTS.md) by exact file name, several joined with `or`: `title = 'PMID_<pmid>.pdf'` where the pick has a PMID, and also `title = 'DOI_<doi>.pdf'` where it has a DOI (the DOI with every character but letters, digits and dots made `_`, the name `downloads.md` gives), since a paper may have been saved under its DOI before its PMID was known.
   - Mark a paper as archived only when the paper itself is found. A supplement alone (`_sup1`, `_mmc1`, `_ESM1`, `_SDC1`) does not count; note it in `archiveFile` and leave `inArchive` FALSE.
   - Include one file you know is there, to check the search works.
   - For any new picks, add PMIDs and PMC IDs with the PubMed connector's ID converter.
   - Update `archive-status.csv` and rerun the candidates script.

3. **Implement.** Choose at most TWO drugs from `plan.csv` (status `planned`, lowest priority number first) whose chosen source paper, with any supplement it needs, is now in the archive. For each:
   - Run the preflight (master and open PRs). If someone else is on the drug, skip it.
   - Add the drug by the full procedure of `docs/adding-a-drug.md`. Read every number in the PDF: download it from Drive as AGENTS.md describes, run `pdftotext -layout`, and look at the page images for tables.
   - Give each drug its own draft pull request. Never merge.
   - If a decision only Shafer can make blocks a drug, write it where AGENTS.md says: the paper's digest card `open_questions:` tagged `[decision]`, or the registry row's notes. Then set the drug's status to `blocked` with the reason in `notes`, and move on.

4. **Update the plan.**
   - In `plan.csv`, set each drug's status: `in_progress` with the PR link, `blocked`, or `done` once merged.
   - Regenerate `downloads.md`.
   - Commit these, with the refreshed `docs/nlmixr2lib/` and `docs/library-expansion/` files, in one draft pull request of their own.

5. **Report.** Put a short report in that pull request's description, and as your final message:
   - which papers arrived;
   - which drugs were implemented, with links;
   - what to download next: the top of `downloads.md`, noting which are free in PubMed Central;
   - any decision waiting for Shafer.

   If nothing changed since last week, say so in one line and open no pull request.
