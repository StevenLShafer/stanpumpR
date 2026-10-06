# Help content

This directory is the hand-written half of the stanpumpR help system. The
other half is generated from the code at run time (see `R/help-content.R`,
`R/help-drugs.R` and `R/help-scenarios.R`).

## Layout

| Path | What it is |
|---|---|
| `<page>.md` | A page. Its id is the file name without `.md`; `models/effect-site.md` is the page `models/effect-site`. |
| `drugs/<drug>.md` | The narrative appended to the generated page of that drug. One file per row of `inst/extdata/drugDefaults_global.csv`; the tests require it. |
| `scenarios/<id>.md` | The narrative appended to the generated page of that teaching scenario, defined in `R/help-scenarios.R`. The tests require it. |
| `references.md` | The methods bibliography, appended to the generated list of drug-model citations. |

## Writing a page

* Register the page in `helpStaticPages()` in `R/help-content.R` (id, title,
  section). The title is added as the `<h1>`, so **files start at `##`**.
* GitHub-flavoured Markdown: tables, fenced code, strikethrough. Rendered with
  `shiny::markdown()` (commonmark); no pandoc, no MathJax. Write equations as
  text or in a code block.
* Link to another page as `[text](help:page-id)`.
* Add a button that loads a scenario as `[text](scenario:scenario-id)`.
* External links open in a new tab automatically.
* A page with three or more `##` headings gets a table of contents.
* Screenshots live in `inst/www/help/` and are referenced as
  `stanpumpr-assets/help/<file>.png` (the app serves `inst/www` at that path).
  Wrap them in `<figure class="help-figure">` with an `<img>` and an optional
  `<figcaption>`; add `help-figure-medium` or `help-figure-narrow` to limit the
  width. Capture at 2x device scale so they stay crisp. The tests fail if a
  referenced image is missing.

## Tests

`tests/testthat/test-help-content.R` renders every page and resolves every
`help:` and `scenario:` link; `test-help-drugs.R` requires a narrative for
every drug; `test-help-scenarios.R` checks each scenario against the drug
library and runs it through the engine.
