---
file: NJ_web-pmda-2fa4a2.pdf
pmid: none
doi: none
citation: "Ministry of Health, Labour and Welfare / Pharmaceuticals and Medical Devices Agency (Japan). Pharmaceuticals and Medical Devices Safety Information No. 352, April 2018 (English translation), archived 2026-10-21. https://www.pmda.go.jp/files/000223901.pdf"
kind: other
drugs: [teneligliptin, anagliptin, omarigliptin]
source_read: "text layer, all 23 pages (one form-feed chunk per page). Read: contents p.1, outline and abbreviations p.2-3, item 2 DPP-4 inhibitors p.13-16 (acute pancreatitis p.13-15, anagliptin pemphigoid p.16), Revision of Precautions No. 293 p.20-21, EPPV list p.22-23; the connector and tolvaptan sections p.4-12 only searched (whole text searched for gliptin names and PK terms). Page images: p.1, p.13."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# PMDSI No. 352 (April 2018): safety bulletin naming anagliptin and teneligliptin

**Source file problem.** The file is what its URL says (PMDA's English PMDSI No. 352, image p.1), but it is a
safety bulletin, not a pharmacokinetic or pharmacodynamic source. It contains no PK or PD data for any DPP-4
inhibitor and does not mention omarigliptin at all. Nothing in the registry's PK or PD status can be verified
from it.

## Study
Not a study. Contents (p.1): (1) ISO 80369 small-bore connectors for neuraxial anaesthesia, p.4-7;
(2) Important Safety Information: tolvaptan p.8-12; anagliptin, linagliptin, teneligliptin, and
teneligliptin/canagliflozin p.13-15; anagliptin p.16; sterile talc p.17-19; (3) Revision of Precautions No. 293
(notifications of March 20, 2018) p.20-21; (4) products under Early Post-marketing Phase Vigilance p.22-23.

## Model and parameters
None. Facts about the named products:
- Acute pancreatitis added to Important Precautions and clinically significant adverse reactions for anagliptin
  (Suiny Tab. 100 mg), linagliptin (Trazenta 5 mg), teneligliptin (Tenelia 20 mg) and teneligliptin/canagliflozin
  (Canalia) (p.13). Cases April 2014 to December 2017: 2, 5, 4 and 0, none fatal; estimated users in the past
  year about 140 000, 910 000, 470 000 and 57 000; launched in Japan November 2012, September 2011, September 2012,
  September 2017.
- Two anagliptin case summaries (p.13-15): a man in his 70s at 100 mg once daily (pancreatitis at day 36,
  amylase up to 2739 IU/L) and a man in his 80s at 100 mg then 100 mg twice daily (amylase 1407 IU/L).
- Anagliptin pemphigoid added (p.16): 1 case; a man in his 80s on 200 mg/day, bullous pemphigoid after about 7
  months.
- Doses appearing only in the case reports: anagliptin 100 mg once daily and 100 mg twice daily (200 mg/day).

```yaml
parameters: []  # safety bulletin: no PK or PD parameters for any product
```

## Pharmacodynamics and end points
None reported. Not an opioid.

## Covariates
None.

## Caveats
English translation; the Japanese original prevails (p.1). Adverse-event counts are spontaneous reports with
causality not ruled out, not incidences.

## Used by stanpumpR
- inst/extdata/antidiabeticRegistry.csv:81 teneligliptin (Tenelia), :82 anagliptin (Suiny), :83 omarigliptin
  (Marizev, weekly): registry tier international, Japan; spec_pk_status and pk_status PK-CONSTRAINT,
  spec_pd_status and pd_status PD-CONSTRAINT, PD end point "DPP-4 inhibition", not implemented; source_url this
  file. Built by data-raw/antidiabeticRegistry.R:305-310.

## Verification against stanpumpR
- **Product identity, Japan, teneligliptin = Tenelia, anagliptin = Suiny**: p.13. **matches**.
- **PK-CONSTRAINT and PD-CONSTRAINT (DPP-4 inhibition) for teneligliptin and anagliptin**: the bulletin carries
  no PK or DPP-4 inhibition data. **cannot verify**; the source does not support a constraint status.
- **Omarigliptin (Marizev) row**: omarigliptin does not appear anywhere in the document (text search and the
  product lists on p.13, p.20-23). **discrepancy**: the row's source URL does not cover the drug.
