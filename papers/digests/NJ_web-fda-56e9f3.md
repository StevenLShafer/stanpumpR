---
file: NJ_web-fda-56e9f3.pdf
pmid: null
doi: null
citation: "FDA-Approved Insulin Products (storage, switching and insulin pump use by product). US Food and Drug Administration web document, https://www.fda.gov/media/195052/download, undated (lists approvals of April and July 2026); retrieved 2026-10-21."
kind: other
drugs: [insulinLispro]
source_read: "text layer, all 20 pages; PDF p1 and p20 rendered to check for a date, author or footnote (none)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# FDA "FDA-Approved Insulin Products": storage, switching and pump use, by product

## Study
A 20-page product list, not a label or review: for each US insulin product, the application number, manufacturer,
customer-service number, a DailyMed link, then three headings: storage, switching, and insulin pump use. Grouped as
rapid-acting (PDF p1-7), short-acting regular (p7-9), intermediate (p9-11), long-acting (p11-15), once-weekly (p15-16)
and premixed (p16-20). The document carries no date, author, FDA logo or version on its pages; asterisks after some names
(Admelog, Garzulys, Kirsty, Merilog, Basaglar, Langlara, Rezvoglar, Semglee) are not explained anywhere in it. Entries
dated by approval: Langlara "approved April 2026", Garzulys "approved July 2026". Its identity as an FDA page rests on
the URL alone.

## Model and parameters
No pharmacokinetics. Class statements only (onset, peak, duration ranges per class, e.g. rapid-acting onset within 15 min,
peak about 1-2 h, duration 3-5 h, PDF p1).

```yaml
parameters: []  # no PK parameters; a product list of storage, switching and pump-use statements
```

Pump-use statements for the products the stanpumpR registry cites it for:

| Product | Page | Pump statement |
|---|---|---|
| Apidra (glulisine) | p2 | approved for compatible CSII pumps (reservoir every 48 h) |
| Garzulys (aspart-fsan) | p4 | to be verified against labelling "upon availability" |
| Humalog U-100 / U-200 (BLA020563 / BLA205747) | p5 | U-100 approved for pumps; U-200 must NOT be used |
| Lyumjev U-100 / U-200 (lispro-aabc) | p6 | U-100 approved; U-200 must NOT be used |
| Merilog (aspart-szjj) | p6 | not approved for pump use; SC injection only |
| Rezvoglar (glargine-aglr) | p13 | not approved for pump use, not IV |
| Semglee (glargine-yfgn) | p14 | not approved for pump use, not IV |

Also: Humulin R U-500, Humulin N, Novolin N, all glargines, Tresiba, Awiqli and all premixes (Humulin 70/30,
Humalog Mix 50/50 and 75/25, Novolin 70/30, NovoLog Mix 70/30) not for pumps; Humulin R U-100 "not approved for routine
pump use"; Novolin R not recommended (precipitation).

## Pharmacodynamics and end points
None.

## Covariates
None.

## Caveats
- A secondary summary that itself defers to each product's prescribing information (Garzulys, Langlara: "verify against
  current FDA-approved labeling"). Undated and unauthored on the page.

## Used by stanpumpR
`inst/extdata/antidiabeticRegistry.csv` `source_url` for rows 26 (Humalog U-200), 28-29 (Lyumjev U-100, U-200), 34
(Merilog), 35 (Garzulys), 36 (Apidra), 39 (Semglee), 40 (Rezvoglar): the `csii` (pump permitted) column.

## Verification against stanpumpR
- **antidiabeticRegistry.csv:26** Humalog U-200 `csii` FALSE, "must not be used in a pump": **matches** p5.
- **:28** Lyumjev U-100 `csii` TRUE and **:29** Lyumjev U-200 FALSE "Not for pumps": **matches** p6.
- **:34** Merilog FALSE, "SC injection only, not pump-approved": **matches** p6.
- **:35** Garzulys `csii` NA, "Pump use pending current label": **matches** p4.
- **:39** Semglee and **:40** Rezvoglar FALSE, "no pump use": **matches** p13-14.
- **:36** Apidra `csii` NA, "Pump use only where the label permits; not checked here": the cited document states Apidra
  is approved for CSII pumps (p2), so the cell could be TRUE from this source; left NA by choice. Not a contradiction;
  no Apidra model exists, so no effect on the app.
- `tests/testthat/test-antidiabetic-registry.R:56-59` (the pump-forbidden list incl. Humalog U-200, Lyumjev U-200,
  Merilog, Semglee, Rezvoglar; Garzulys NA): consistent with this document.
