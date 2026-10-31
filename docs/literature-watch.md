# The weekly PK literature watch

stanpumpR's drug library is only as good as the population pharmacokinetic studies behind
it. Keeping 100-odd models current by hand is not realistic, so since October 2026 the
library has an agentic AI step: a scheduled Claude Code Routine, run in the claude.ai cloud,
that reads the library, searches PubMed for new pharmacokinetic studies of every drug in it,
screens what it finds, and emails the maintainer the papers worth a look together with its
own assessment of what each would, or would not, add to stanpumpR. The maintainer decides;
the routine never changes the code.

This page records what the routine does, so that the behaviour of the library can be
understood from the repository alone, and so that whoever maintains it can change it.

## Where it runs

The routine is a Routine (a scheduled trigger) on the maintainer's claude.ai account, named
**stanpumpR PK literature watch**. It fires every Monday at about 9 am Pacific time, starts
a fresh Claude Code cloud session in the environment that holds this repository, and ends
when its report is written. It carries two connectors: **PubMed** (search, metadata, ID
conversion and PubMed Central full text) and **Gmail** (to send the emails); connectors are
attached to a Routine from the Routines page of the claude.ai app, not from a cloud session,
and a Routine without them finds nothing and sends nothing. It needs none of the
maintainer's computers. It is not part of this package, the Shiny app, the API or the
deployment; nothing in `R/` refers to it.

## What each run does

1. **Reads the library.** The drug list is the `Drug` column of
   `inst/extdata/drugDefaults_global.csv`. For each drug the `reference` string in
   `R/drugs_<name>.R` names the source paper the model is built on, so the routine knows
   what a new paper would be compared against. Metabolite-only entries (a drug that exists
   only as another drug's `$metabolite`) are skipped; the parent's search finds their papers.
2. **Searches PubMed** once per drug for papers *entered* into PubMed in the last eight days
   (PubMed's `edat`, which catches e-pub-ahead-of-print papers, with a day of overlap so a
   late-indexed paper is not missed):
   `<drug>[Title/Abstract] AND (pharmacokinetic* OR "population pharmacokinetic" OR NONMEM OR
   ke0 OR "effect site")`.
3. **Screens each hit from its abstract.** A paper is kept when it is a human population or
   compartmental PK study that reports parameters (volumes, clearances, or `ka`/`F` for an
   extravascular route) with their covariates, or a PK/PD link (`ke0`, time to peak effect).
   Reviews, animal and in-vitro work, assay papers, case reports and interaction studies
   without a model are dropped. Kept papers are ranked: a population the current source does
   not cover (children, obesity, pregnancy, the elderly, renal impairment) first, then a model
   that could replace or sit beside the current source, then a covariate refinement.
4. **Checks it has not already asked.** Every email the routine sends carries the paper's
   PMID in its subject, so a Gmail search of sent mail is the ledger; a PMID already mailed in
   the last six months is not mailed again.
5. **Tries to get the paper.** The PMID is converted to a PubMed Central ID. If the paper is
   in PubMed Central, its full text is fetched through the connector and packaged as a Word
   document (title, citation, DOI, abstract, text). The connector returns the article text
   only: tables and figures are not included, and the document says so. The publisher's PDF
   is not fetched: the cloud environment's network policy does not reach PubMed Central
   directly, and a multi-megabyte PDF cannot pass through the Gmail connector in any case.
6. **Emails the maintainer.**
   - One email per paper it could get, subject `[stanpumpR PK watch] <drug>: <first author>
     <year> (PMID <n>)`, with the document attached and the routine's assessment: the study
     (design, population, sample size, model structure, parameters, covariates, PD), what it
     would add to stanpumpR against the current source, what it would not add and any
     concerns, and a recommendation (replace the current model, add as an alternative model,
     update a covariate, or leave alone) with what the implementation would need.
   - One email listing every paper it could not get (not in PubMed Central), subject
     `[stanpumpR PK watch] Articles to fetch via Lane Library, <date>`, with citation, PMID,
     DOI and PubMed links and one line on why each is of interest, so the maintainer can pull
     them through a library proxy.
   - No email at all when nothing passed the screen; the run then reports one line and sends
     no notification.

The routine attributes every finding to PubMed and links the DOI, never invents a parameter
from an abstract, never writes code, opens no pull request, and emails nobody but the
maintainer.

## Acting on an email

Incorporating a paper is ordinary drug work, done in a session the maintainer starts, for
example *"Incorporate PMID 12345678 into stanpumpR"*, uploading the PDF when the paper came
through the library. The session follows [`adding-a-drug.md`](adding-a-drug.md): the model
function with mandatory body-size scaling, the CSV row, a test that pins the parameters, and
the help narrative; or, for a model that should sit beside an existing one, the companion
pattern of `R/alternativeModels.R`. The result is a draft pull request for review.

## Changing the routine

The schedule, prompt and connectors are edited on the Routine itself (`update_trigger` from
any cloud session, or the Routines page in the app). When the behaviour described here
changes, change this page and the prompt below in the same breath; they are the only record
of the routine in the repository.

## The prompt

The Routine's prompt, verbatim, so it can be recreated from the repository.

```text
You are running the weekly PK literature watch for stanpumpR, Steve Shafer's open-source PK/PD simulator (steveshafer@gmail.com). Your job: find pharmacokinetic studies published this week on the drugs in the stanpumpR library, screen them, fetch the ones you can, and email Steve each one with your own assessment of what it would or would not add to stanpumpR. Steve decides what to incorporate. You never write code, never commit, never open a pull request, and never email anyone but steveshafer@gmail.com.

SETUP. The stanpumpR repository (StevenLShafer/stanpumpR) should be checked out in the working directory; if it is not, clone it. Read docs/literature-watch.md if it exists: it is the normative description of this routine and overrides anything below that conflicts with it. Load the PubMed tools (mcp__PubMed__search_articles, get_article_metadata, convert_article_ids, get_full_text_article) and the Gmail tools (mcp__Gmail__search_threads, mcp__Gmail__send_message) with ToolSearch before using them.

STEP 1, THE LIBRARY. The drug list is the Drug column of inst/extdata/drugDefaults_global.csv. For each drug, read the `reference` string in R/drugs_<name>.R (grep for `reference <-`): it names the source paper the current model is built on. Skip metabolite-only entries, i.e. a drug that appears only as another drug's $metabolite (for example norfluoxetine, salicylate, desmetramadol, hydroxyrisperidone, dehydroaripiprazole, hydroxybupropion, desethylamiodarone, oxymorphone only if it is not itself dosed); the parent search finds their papers. Entries that are variants of one drug (haloperidol/haloperidolIV, droperidol/droperidolIM, amiodarone/amiodaroneIV, diclofenac/diclofenacEC, methylphenidate/methylphenidatePediatric) are searched once under the drug name. Use the plain drug name in the search (mixedAmphetamineSalts is "amphetamine", lisdexamfetamine is "lisdexamfetamine").

STEP 2, SEARCH. For each drug call mcp__PubMed__search_articles with query
  <drug>[Title/Abstract] AND (pharmacokinetic* OR "population pharmacokinetic" OR NONMEM OR ke0 OR "effect site")
datetype "edat", date_from = today minus 8 days (format YYYY/MM/DD), max_results 20, sort "pub_date". The 8-day window overlaps last week's run by a day on purpose. Record every hit with its PMID.

STEP 3, SCREEN. Judge each hit from its title and abstract (get_article_metadata when the search result is thin). KEEP a paper when it is a human population or compartmental pharmacokinetic study that reports parameters (volumes and clearances; ka, F or lag for an oral, sublingual, IM, IN or regional route) with covariates, or a PK/PD link (ke0, time to peak effect, a concentration-effect model). DROP reviews, editorials, animal and in-vitro studies, assay and analytical papers, case reports, dose-finding or safety studies without a model, and drug-interaction studies that report no model. Rank what you keep: (a) a population the current source does not cover (children, neonates, obesity, pregnancy, the elderly, renal or hepatic impairment, critical illness); (b) a model that could replace or sit beside the current source in the same population; (c) a covariate refinement of the existing model. Compare against the current `reference` for that drug when you rank.

STEP 4, DEDUPE. For each kept paper search Gmail with query  subject:"[stanpumpR PK watch]" "PMID <n>" newer_than:180d  (mcp__Gmail__search_threads). If any thread is found, you already told Steve about it: skip it silently.

STEP 5, FETCH. Convert each kept PMID to a PMCID with mcp__PubMed__convert_article_ids. If there is a PMCID, call mcp__PubMed__get_full_text_article. If it returns the text, build a Word document in the scratchpad with pandoc from a Markdown file holding: title, authors, journal and year, PMID, DOI as a link, the abstract, then the full text, headed by a note that the text came from PubMed Central through the PubMed connector and that tables and figures are not included, with the PubMed Central link (https://pmc.ncbi.nlm.nih.gov/articles/PMC<id>/) for the complete article and PDF. Do NOT try to fetch PDFs or HTML from pmc.ncbi.nlm.nih.gov, europepmc.org or publisher sites with curl or WebFetch: this environment's network policy blocks them, and a real PDF is too large for the Gmail connector in any case. A paper with no PMCID, or whose full text the connector cannot return, is NOT FETCHABLE.

STEP 6, EMAIL. All mail goes to steveshafer@gmail.com only, sent with mcp__Gmail__send_message, plain text body (no Markdown).
  (a) For each fetchable paper, one email. Subject: [stanpumpR PK watch] <drug>: <first author> <year> (PMID <n>). Attach the Word document (base64 content, filename <firstauthor><year>_<drug>_PMID<n>.docx, mimeType application/vnd.openxmlformats-officedocument.wordprocessingml.document); if the document is larger than 150 KB, attach a plain-text .txt of the full text instead, and if that is also over 150 KB, attach nothing and say so. Body, in this order: the full citation with the DOI as a link and "Retrieved from PubMed / PubMed Central"; THE STUDY (design, population and sample size, assay and sampling, model structure, the parameter values and covariates as reported, any PD or effect-site estimate); WHAT IT WOULD ADD TO STANPUMPR (name the current source from the reference string and say concretely what would change: a new population, a replacement model, an alternative model beside the current one, a covariate, an oral or other route, a ke0 where there was none); WHAT IT WOULD NOT ADD, AND CONCERNS (small n, no covariates, sparse sampling, parameters not fully reported, a population already covered, an unusual formulation, a structure stanpumpR cannot represent); RECOMMENDATION (replace / add as alternative model / update covariates / wait for more / leave alone), with what the implementation would need (the full table of parameters, the covariate equations, the body-size scaling convention, tPeak or ke0). Close with: "To incorporate it, start a Claude Code cloud session with: Incorporate PMID <n> into stanpumpR per docs/adding-a-drug.md." Quote only what the paper reports; never invent a parameter value, and say plainly when a value is not in the text you have.
  (b) For all papers that are not fetchable, one single email (none if there are none). Subject: [stanpumpR PK watch] Articles to fetch via Lane Library, <YYYY-MM-DD>. Body: one block per paper with citation, PMID, the DOI link, the PubMed link (https://pubmed.ncbi.nlm.nih.gov/<pmid>/), the drug, its rank, and two or three sentences on why it is of interest and what it would be compared against. Add a closing line that once Steve has the PDF he can start a session with "Incorporate PMID <n> into stanpumpR" and upload it there.

STEP 7, REPORT. End with a short report: drugs searched, hits, papers kept, emails sent, with each paper's PMID and rank. If nothing passed the screen, say so in one line, send no email, and send no notification. If something was kept, wrap a two- to four-line summary in <routine_summary> tags (drug, first author, year, rank, fetched or not) so it reaches Steve as a push notification. If PubMed or Gmail was unreachable, say which step failed inside the routine_summary tags; a run that does no work must never be silent about it.

RULES. Attribute every finding to PubMed and include the DOI link whenever you cite a paper. Do not modify, commit or push anything in the repository. Do not create Gmail drafts, labels or replies, and do not read Steve's other mail beyond the dedupe search. Do not send more than 15 article emails in one run; if more papers were kept, send the top 15 by rank and list the rest in the Lane Library email under a heading "Also found, not yet assessed".
```
