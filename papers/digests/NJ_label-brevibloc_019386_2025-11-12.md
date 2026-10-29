---
file: NJ_label-brevibloc_019386_2025-11-12.pdf
pmid: ""
doi: ""
citation: "Baxter Healthcare. BREVIBLOC Double Strength (esmolol hydrochloride in sodium chloride) injection 2,000 mg/100 mL: container and carton labeling. NDA 019386, FDA file 019386Orig1s055lbl.pdf (accessdata.fda.gov, 2025-11-12); signature page dated 2025-09-23."
kind: label
drugs: [esmolol]
source_read: "page images of all 10 PDF pages rendered and read (contact sheet) plus the text layer; there is no prescribing information in the file"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: not-applicable
revised: 2026-10-27 (Source file problem names the new PI cards NJ_label-brevibloc_019386_2023-06 and NJ_label-brevibloc_019386_s039, which verify the docs line)
---

# FDA 2025: Brevibloc Double Strength container labels (NDA 019386)

**Source file problem.** The file is named and catalogued as the Brevibloc prescribing information, but its 10 pages
are packaging art only: three small container labels (PDF p1, NDC 10019-075-87; p2, image-only; p3,
NDC 10019-668-10), three overwrap bag labels (p4, NDC 10019-668-10, Novaplus; p6, 10019-666-10, PremierProRx; p8,
10019-075-87), the bag front panels (p5, p7, p9), and a digital-signature page (p10, "Julia Pinto", 9/23/2025). No section of a prescribing information (dosage, clinical pharmacology, populations) is present. Nothing in
this card is verified from it; the full Brevibloc PI in the archive is `NJ_dailymed-38a8b23a`.
Since 2026-10-27 the Brevibloc PI itself is archived: `NJ_label-brevibloc_019386_2023-06` (and the older `NJ_label-brevibloc_019386_s039`) verifies the `docs/antihypertensives.md:1291` esmolol statement (matches).

## Study
Not applicable. The labels state: esmolol hydrochloride 2,000 mg/100 mL (20 mg/mL) in sodium chloride, single-dose
100 mL bag, for intravenous use only; each mL contains 20 mg esmolol hydrochloride, 4.1 mg sodium chloride, 2.8 mg
sodium acetate trihydrate and 0.546 mg glacial acetic acid, pH 5.0 (4.5-5.5); iso-osmotic; store at 25 C, protect from
freezing; "recommended dosage: see prescribing information".

## Model and parameters
```yaml
parameters: []   # container labels only; no pharmacokinetic content
```

## Pharmacodynamics and end points
None.

## Covariates
None.

## Caveats
Wrong document for its catalogue entry (see the Source file problem above). The underlying FDA URL
(`019386Orig1s055lbl.pdf`) should be re-fetched, or the entry relabelled as carton/container labeling.

## Used by stanpumpR
- `docs/antihypertensives.md:1291` (context gave 1279): the exclusion table says esmolol is in the registry because its
  label and perioperative use concern pressure as well as rate (sotalol, landiolol excluded).

## Verification against stanpumpR
- `docs/antihypertensives.md:1291`: **cannot verify** from this file, which carries no indications. (The DailyMed
  Brevibloc PI, `NJ_dailymed-38a8b23a`, section 1.2, does list intraoperative and postoperative tachycardia and
  hypertension, which supports the statement; that is a different document.)
