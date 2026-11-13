# FDA prescribing information

This folder holds the US product labels, the FDA-approved prescribing information, for the drugs in
stanpumpR's library. Unlike the papers behind the models, which are copyrighted and kept in the private
archive (`../README.md`), a label is a public document, so the PDFs are kept here, beside the code and
the digest cards that read them.

## The index

`inst/extdata/fdaLabels.csv` lists each drug and its labels. It has one row per drug and label, so a
label shared by several drugs (Lanoxin for `digoxin`, `digoxinIV` and `digoxinIVPediatric`) has one row
for each. Each drug's help page in the app lists its labels from this index, with links to the files
here (`helpDrugFdaLabelHTML()`, `R/fda-labels.R`). The columns are:

| column | meaning |
|---|---|
| `drug` | the library's drug name |
| `status` | `label` (a file is here), `no_us_label` (there is no FDA prescribing information for the drug as modelled) or `not_obtained` (a label exists but is not here yet) |
| `file` | the PDF in this folder |
| `title` | the label's title: the digest card's citation for an `archive` file, DailyMed's title for a `dailymed` one |
| `application` | NDA, ANDA or BLA number, where known |
| `published` | the label's revision or posting date, where known |
| `source` | `archive`: copied from the stanpumpRPapers archive, which its digest card (`card`) reads page by page, so it stays the version the card cites. `dailymed`: the current label, as FDA's Structured Product Labeling, downloaded from [DailyMed](https://dailymed.nlm.nih.gov/) (the National Library of Medicine's distribution of the labels FDA holds) by its `setid`. `drugsatfda`: saved by hand from [Drugs@FDA](https://www.accessdata.fda.gov/scripts/cder/daf/), for a label DailyMed no longer carries |
| `setid` | DailyMed set id (for a `dailymed` file) |
| `card` | the digest card in `../digests/` that reads this file, if any |
| `md5` | checksum of the file |
| `note` | for `no_us_label` and `not_obtained`, why |

File names follow the archive's convention, `NJ_label-<product>_<application>_<date>.pdf`, so a file
here and its copy in the archive have the same name. The archive's older DailyMed downloads keep their
names (`NJ_dailymed-<setid prefix>.pdf`), as their cards cite them.

## How the collection was built (2026-11-13)

1. The drugs are the 225 rows of `inst/extdata/drugDefaults_global.csv` with a `Category`.
2. The 235 digest cards of `kind: label` showed which drugs already had a label in the archive. The
   archive's US labels for library drugs (165 files) were copied here unchanged.
3. For each drug with no archived label, the label was looked up on openFDA (`api.fda.gov`), with the
   reference product's own label preferred to a repackager's, and its current version downloaded as a PDF
   from DailyMed (112 files). Where the reference product has left the market, a generic's label stands
   in for it. A drug given by several routes may have one label for each route (morphine has injection,
   tablet and oral solution labels).
4. Drugs with no FDA label are listed with a note: the illicit research models, the drugs approved only
   outside the US (fluindione, acenocoumarol, vildagliptin, Lecigon), the investigational cetagliptin,
   and plain aspirin (an OTC monograph product). Osmolex ER (`amantadineER`) has been withdrawn and is no
   longer on DailyMed. Its labels are on Drugs@FDA, which refuses automated downloads, so it is
   `not_obtained` at first. It was added by hand from Drugs@FDA on 2026-11-13, its original 2018
   approval label (`source` `drugsatfda`).

Drugs@FDA (`accessdata.fda.gov`) is the FDA's own archive of every approved label version, but it blocks
scripted downloads. DailyMed carries the same labels, current version only, and permits them.

## Keeping it current

`data-raw/fdaLabels.R` checks every file against the index (present, checksum, no unlisted PDF, every card named present); CI runs that check on every push and pull request (`.github/workflows/fda-labels.yaml`), since R CMD check never sees this folder. `refreshFdaLabels(download = TRUE)` downloads newer
versions of the `dailymed` labels. An archive file is never replaced
here, because its card cites its pages. A new label for an archive drug goes into the archive first, gets
its card, and is then copied here. `tests/testthat/test-fda-labels.R` checks that every drug a user can
dose has a row in the index.

A label is frozen at approval. Where a later study disagrees with it, stanpumpR follows the study (see
*A label is frozen at approval* in `AGENTS.md`).
