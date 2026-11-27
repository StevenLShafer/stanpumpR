stanpumpR reference archive - build scripts (2026-10-07)

WHERE THE ARCHIVE LIVES (2026-10-30)
The archive is in Google Drive: My Drive\stanpumpRPapers (steveshafer@gmail.com, private, shared with no one,
because the PDFs are copyrighted).  Google Drive for desktop runs in STREAM mode (never Mirror: Mirror would
copy all of My Drive, years of backups, onto the computer) and shows My Drive as the D: drive; the folder
D:\My Drive\stanpumpRPapers is marked Offline access > Available offline, so its 1.8 GB stays on the computer
and the scripts never wait for a download.  C:\dev\stanpumpRPapers is a JUNCTION to it, so every script,
update.bat and every session's notes use the old path unchanged.  (A junction works into the Drive volume; a
symbolic link would need administrator rights.)  The pre-move copy was kept as C:\dev\stanpumpRPapers.pre-drive.
Kept OUT of Drive, per machine, regenerable (machine.py): %LOCALAPPDATA%\stanpumpRPapers\ holds http_cache\,
master_snapshot\ and digest_scratch\.
One computer at a time: Drive does not merge two computers' edits to one file, it keeps both as "name (1)".
Before running update.bat or digests.bat on another computer, let Drive finish syncing (tray icon: up to date).
The digest run lock names its computer (digest_run.py HOST), and a run elsewhere is respected for 12 hours.
A cloud Claude session reads the PDFs and _build\text\ and _build\context\ through the claude.ai Google Drive
connector (verified 2026-10-30, after Shafer disconnected and reconnected it; before that it saw only files it had
created itself).  The folder's Drive ID is 1Hi6HmnULQsIlETZw4lo5rhxkfog33vRR.
  - Find a paper with search_files, e.g. title = 'PMID_8357297.pdf'.
  - Read it with read_file_content, which returns the full text, tables included.
  - download_file_content returns the whole file as base64 inside the conversation.  That is fine for a small
    file but impractical for a large PDF, so page-image checks of numbers belong to a local session.
  - Never follow a file's viewUrl: Shafer's Drive opens PDFs in a third-party viewer app by default.
  - The connector reaches cloud sessions; a cloud session cannot run update.bat (that needs this toolchain and
    the computer's Downloads folder).

Setting up another computer (e.g. the home desktop):
 1. Google Drive for desktop, signed in as steveshafer@gmail.com; Preferences > Google Drive > STREAM files.
 2. In File Explorer: Google Drive (its letter) > My Drive > right-click stanpumpRPapers > Offline access >
    Available offline.  Wait for it to finish downloading.
 3. PowerShell:  New-Item -ItemType Junction -Path C:\dev\stanpumpRPapers -Target "<letter>:\My Drive\stanpumpRPapers"
 4. Python 3.14 at C:\Python314 (update.bat pins that path) with pypdf, openpyxl, pyyaml, ocrmypdf, and pypdfium2 +
    pillow (the digest agents render pages with them); Git for Windows (its mingw64 pdftotext.exe is used);
    Tesseract-OCR and Ghostscript in C:\Program Files for ocr_scans.py; GitHub CLI (gh auth login); the Claude
    CLI, with CLAUDE_CODE_OAUTH_TOKEN set as a Windows USER environment variable (from `claude setup-token`) for
    the unattended digests.
 5. git clone https://github.com/StevenLShafer/stanpumpR C:\dev\stanpumpR.  The cards worktree
    C:\dev\stanpumpR-digests is created by cards.sync() on the first digest run.  NEVER put a git checkout
    inside Drive: Drive's sync corrupts .git folders.

Normal use: run ..\update.bat  (works from PowerShell or cmd; it pins C:\Python314, which has openpyxl).
It runs, in order:

file_downloads.py   MOVES stanpumpR reference PDFs from Downloads and its subfolders (any file name) into the
                    archive as PMID_<pmid>.pdf: copy, check byte-identical, then send the original to the Recycle
                    Bin (restorable; recycle.py). A Downloads PDF identical to one already archived is recycled too. Each PDF is identified from its own text: the article's own DOI (the
                    first DOI on page 1) confirmed by its title or first author, or the reference's title at
                    the top of page 1 plus its first author. Word overlap alone is never accepted (it once
                    filed an NEJM renal-artery paper as Cockcroft-Gault 1976). Scans with no text layer, and
                    title-without-author hits, are reported for renaming by hand.
                    A SECOND COPY of an archived paper (same paper, different bytes: a re-download the publisher
                    stamped differently) is recycled too when copy_equivalent() finds the archived copy lacks nothing:
                    same page count, and the same words once publishers' download-stamp lines are dropped (at least 97%
                    in common), or no text layer against an archived one with text (our OCR).  Anything else (a cover
                    page, an erratum) and any copy the Recycle Bin refuses (open in another program) is listed under
                    "Needs a look" on every run.  Until 2026-11-01 such copies were skipped in silence, and 53 had built up.
                    A PDF it CANNOT IDENTIFY at all was skipped in silence too, until 2026-11-10 (four Lane downloads of
                    listed papers sat unfiled: PubMed's typo "Disporition", an "a-" for "alpha", two OCR'd scans).  For
                    14 days after download it is now reported: under "Needs a look" with the likeliest reference when
                    page 1 shows that title, its first author with 3/4 of the title's words, or its citation line (a
                    hint only, never filed); otherwise in one summary line, the files listed in unplaced_downloads.txt.
                    Logs: filing_log.json, recycled_log.json.  Cache of files already checked: filing_cache.json
                    (re-checked whenever the reference list changes).
merge.py            -> works.json      overrides applied, records collapsed to distinct works
build.py            -> ..\stanpumpR-references.xlsx and ..\stanpumpr-queue.csv; marks a work "In archive"
                    whenever its PMID_<pmid>.pdf exists in the archive folder.

Built once, not part of update.bat:
extract/*.jsonl      citations pulled from the stanpumpR repo (master @ 72d079b), one object per work per file group
resolve.py           -> resolved.json   PMIDs/DOIs via NCBI E-utilities (ecitmatch, esearch, esummary) and Crossref
overrides.json       hand corrections from review (wrong repo PMIDs, short-form identifications, label groupings)
holdings.py          indexes ShaferPapers, Reprints (reprints_resolved.json) and C:\dev\Corpus by PMID/DOI
fetch_pmc.py         fetched the 23 PMC open-access PDFs from the PMC Article Datasets bucket (pmc_fetch_log.json)
http_cache/          cached API responses, so re-runs are fast and offline

New citations added to stanpumpR after 72d079b need a fresh extraction before they appear here.
(Extractions G, H1-H3, I, J, K1a-K34 and L1-L6 have since brought the list to master 781a35c.)

Registry tables (inst/extdata/*.csv), from round Q on: two scripts, both run on a checkout of master.
extract_registries.py  the source, source_url and to_obtain columns.  A later round writes its output to a scratch
                     path and keeps only the records whose key is not in an earlier *_registries.jsonl, saved as
                     extract/<round>_registries.jsonl, so the line-indexed overrides on earlier files stay valid.
                     It parses only "Author AB. Title. Journal 2000;1:2" citations; "Author AB et al. 2020, Journal
                     1:2" and remarks come through as bare text and are settled in review.
extract_registry_prose.py  every OTHER column (notes, finding, verification, blocking_issue, evidence_read ...),
                     added 2026-10-27 (round T): author-year mentions, PMIDs and DOIs in running text, keeping only
                     those whose PMID/DOI or (surname, year +-1) no work on the list has.  Its output is noisy
                     (journal names, places), so every record is reviewed.
diff_cites.py        code, help, tests and docs: the citations on lines ADDED between two commits that the list lacks
                     (round U 7d8b50b..296d8f5, round V 296d8f5..e4145b5 on 2026-10-30).  NEXT ROUND STARTS AT e4145b5.
                     [decision, Steven L. Shafer] ROUND W NEEDS ITS SCOPE SETTLED BEFORE IT IS RUN.
                     Measured 2026-11-26: e4145b5..master is 1419 files and 160,845 added lines, and
                     diff_cites.py offers 9,460 records -- against round V's 16 new works.  It is not
                     that stanpumpR cited 9,460 papers in four weeks.  Four documents added since
                     e4145b5 are THEMSELVES bibliographies of papers the library does NOT cite, and
                     they supply the bulk: docs/nlmixr2lib (11,541 location hits; another library's
                     transcriptions, kept to cross-check ours), docs/library-expansion (2,638;
                     candidates for future drugs), docs/literature-search-2026-11-16.md (794) and
                     docs/independent-extraction (226; the papers given to the external readers).
                     The genuine citation sites are the ordinary ones -- inst/app-help 453,
                     R/physiology.R 103 and the like.  Run as it stands, a round would pull another
                     library's whole bibliography onto the works list, and every record is reviewed
                     by hand.  The question: should PATHS exclude those four (and any later catalogue
                     of papers CONSIDERED rather than USED), or should a round take them separately
                     under a different rule?  Nothing is lost meanwhile -- the papers in them are
                     candidates, not sources, and anything a model actually cites is picked up from
                     R/ and inst/app-help as usual.
                     The cost of the delay is real and measured: works.json can only hold works
                     resolved at the last round, so a paper cited by code merged since 2026-10-30 is
                     invisible to file_supplements.py and to file_downloads.py's by_doi/title route.
                     Observed 2026-11-26: the Vuong 2025 fluconazole supplement could not be placed
                     ("article not in the reference list") although its article is archived, because
                     the article reached master after round V.
                     The list goes stale as soon as master adds a citation: round V was prompted by a cloud session
                     not finding Singhvi 1988 (PMID 2967089), cited by the fosinopril model merged the day after round U.
                     Run all three extractors to scratch, keep only new registry records (key = pmid / doi / pmcid / url /
                     text), review every record into overrides.json ([<round>-round review <date>]), then resolve.py,
                     merge.py, build.py, fetch_pmc.py, unpaywall_fetch.py and update.bat.
A work added by hand (extract/<round>_hand.jsonl) or by a round reaches the filer only after resolve.py and merge.py:
update.bat runs file_downloads.py BEFORE merge.py, so run merge.py first or update.bat twice.

Non-journal sources (labels, FDA reviews, guidelines, web pages), added 2026-10-12:
fda_fetch.py         FDA-approved labels (the most recent label PDF per application) and clinical pharmacology
                     reviews from accessdata.fda.gov, located through the Drugs@FDA index (fda\drugsatfda.zip, from
                     https://www.fda.gov/media/89850/download). Saved as ..\NJ_<group>_<applno>_<date>.pdf.
web_fetch.py         other free documents (thesis, posters, EUCAST tables, FDA safety communications) and web pages
                     printed to PDF with Microsoft Edge headless (the header/footer keep the URL and print date).
nonjournal_manifest.json  group -> archived NJ_ files with source URL, application, date and notes. build.py and
                     tiers.py count a non-journal reference as "In archive" when its manifest files exist.
Cloudflare-protected pages (e.g. apsf.org) print only a "Just a moment..." challenge headless: print those by hand.

Open-access copies, added 2026-10-28 (not part of update.bat; run update.bat after):
fetch_pmc.py         the PMC Article Datasets bucket (works marked "free at PMC").
unpaywall_fetch.py   every not-yet-archived work with a DOI, through the Unpaywall API (what the R package roadoi
                     wraps; it needs a real email, Shafer's).  Takes a location's PDF only if its first pages carry the
                     title and first author and it has at least half the published pages.  Published version ->
                     PMID_n.pdf; authors' accepted manuscript -> PMID_n_aam.pdf (the work stays "To download"; both are
                     kept when the publisher PDF arrives, and digests\STATUS.md flags the re-check); preprints never.
                     Failures are held in unpaywall_tmp\.  Most publishers (Wiley, ASPET, Elsevier...), PMC and Europe
                     PMC answer scripts with 403: those links go on the Lane page as "free at PMC" / "open, publisher".

Digests, unattended (added 2026-10-28): update.bat ends by starting ..\digests.bat 20 in its own minimized window.
digest_run.py        takes a lock (digest_run.lock), exports origin/master with git archive into master_snapshot\,
                     orders the STATUS.md queue (drug-model sources first, labels last; scans, OCR'd scans and
                     accepted manuscripts no model depends on are skipped), and runs `claude -p` (Claude Code headless)
                     on two papers at a time with digest_agent_prompt.md plus the unattended rules: never guess, put
                     what cannot be settled in the card's `open_questions` ([decision] / [source] / [check]), and
                     confirm every number stanpumpR uses on a rendered page.  Reports: digest_runs\<time>.md and
                     digest_run_log.json.  It then reruns digest_prep.py, which writes ..\digests\REVIEW.md: the open
                     questions, every unattended card not yet `reviewed:`, and the older discrepancy/partly-verified cards.
cards.py             where the cards are written: the worktree C:\dev\stanpumpR-digests (branch digests/working), which
                     the digest session also commits hand-written cards to.  So (2026-10-29) sync() resets it to
                     origin/master only when that loses nothing -- uncommitted cards, or committed ones master has never
                     held, are kept and master is merged in instead; a run commits only the cards it wrote (plus a crashed
                     run's), and pushes them as one commit cut from origin/master, so its PR holds nothing else.
                     The queue never rewrites a card the runner did not write, or one revised or reviewed since, and skips
                     any card waiting in an unmerged digest branch; the report's "not queued" line says which and why.
                     Writing a card by hand: file the PDF FIRST.  A PDF newer than its card makes the card stale.

A HISTORY OF THESE SCRIPTS (2026-11-20)
The scripts and notes here are now under version control, so a lost or clobbered edit can be recovered.  The
PDFs are not, and never will be: only update.bat, the root review and handoff notes, and _build's .py, .sh,
.md and .txt (the tests included) are tracked -- 0.7 MB of source against 28 MB of regenerated JSON, text,
OCR and PNG output, which an allowlist in the repository's info/exclude keeps out along with digests\ and
every PDF.  The git directory is C:\dev\stanpumpRPapers-history, OUTSIDE the Drive folder, because Drive
syncs loose objects and the index independently across machines and would corrupt an in-tree .git; nothing
was written inside the archive, so there is no .git here to find.  Line-ending rewriting is off, so a file
comes back byte for byte.
   C:\dev\papers-history.bat snap               commit everything that changed, with a timestamp
   C:\dev\papers-history.bat log --oneline      what changed, and when
   C:\dev\papers-history.bat diff               what is uncommitted right now
   C:\dev\papers-history.bat restore <file>     put a clobbered file back as last committed
Any other git command works the same way; the wrapper only points git at the right place.  Take a snapshot
before and after editing anything here, which is what makes the history worth having.  WHY: on 2026-11-19
two Claude sessions edited digest_run.py within the same hour and nothing but hand coordination kept one
write from erasing the other -- Drive keeps its own file versions, but it does not merge, and nobody reads
them.  This does not PREVENT a collision; it makes one visible and reversible.  The rule that prevented harm
that night still stands: re-read a file immediately before you write it, and say which part you are holding.
