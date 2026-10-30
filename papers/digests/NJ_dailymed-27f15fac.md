---
file: NJ_dailymed-27f15fac.pdf
pmid: none
doi: none
citation: "RYBELSUS (semaglutide) tablets and OZEMPIC (semaglutide) tablets, for oral use, DailyMed structured product label (Novo Nordisk Pharmaceutical Industries, LP), set id 27f15fac-7d98-4114-a2ec-92494a91da98, label revised 1/2026, retrieved 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=27f15fac-7d98-4114-a2ec-92494a91da98"
kind: label
drugs: [semaglutide, semaglutideOral]
source_read: "text layer, 62 pages: Highlights and contents (p1-3), 2 Dosage and Administration incl. 2.3 Table 1 (p5-6), 8 Use in Specific Populations (p14-16), 11 Description (p17), 12 Clinical Pharmacology incl. 12.3 (p18-20), 14.1 (p22); Medication Guide and package display panels (p36-62) skimmed. Pages 6, 19 and 52 confirmed from rendered images"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# DailyMed SPL, Rybelsus and Ozempic tablets (1/2026)

## Study
Not a study: the DailyMed rendering of the joint US label of the two oral semaglutide products, "Revised: 1/2026"
(p3). Its prescribing information matches the Drugs@FDA PDF of the same label (Reference ID 5737308), digested in
`NJ_label-ozempictablets_213051_2026-07-31.md`; I compared the key passages (2.1, 2.3 Table 1, 2.4, 11, 12.3, 8.6)
and found the same wording and numbers. The SPL adds the product listing and package panels.

## Model and parameters
No model. The same facts as the FDA PDF:
- Not substitutable on a mg-to-mg basis (Highlights; 2.1). Rybelsus 3 -> 7 -> 7 or 14 mg; Ozempic tablets 1.5 -> 4
  -> 4 or 9 mg daily (2.2). Switching after day 30: Rybelsus 7 mg = Ozempic tablets 4 mg, 14 mg = 9 mg (2.3,
  Table 1, p6 rendered). From Ozempic injection 0.5 mg: one week later, 7 or 14 mg Rybelsus or 4 or 9 mg Ozempic
  tablets (2.4).
- 12.3 (p19, rendered): mean Css 6.7 and 14.6 nmol/L at Rybelsus 7 and 14 mg in T2D; steady state after 4-5 weeks;
  absolute F about 0.4-1% (Rybelsus 3/7/14 mg) and 1-2% (Ozempic tablets 1.5/4/9 mg); tmax about 1 h; no clinically
  significant difference in steady-state AUC0-24 and Cmax between the paired strengths in healthy subjects; V about
  8 L, CL about 0.04 L/h, t1/2 about 1 week in T2D; > 99% albumin bound; 3% excreted intact in urine.
- 11: molecular weight 4113.58 g/mol.

```yaml
parameters:
  - {name: F Rybelsus 3/7/14 mg, value: "0.4-1", unit: "%", source: "12.3, p19"}
  - {name: F Ozempic tablets 1.5/4/9 mg, value: "1-2", unit: "%", source: "12.3, p19"}
  - {name: Css mean Rybelsus 14 mg, value: 14.6, unit: nmol/L, source: "12.3, p19", note: "T2D; 7 mg 6.7 nmol/L"}
  - {name: CL (absolute), value: 0.04, unit: L/h, source: "12.3, p19-20", note: "approximately, T2D"}
  - {name: V (absolute), value: 8, unit: L, source: "12.3, p19", note: "approximately, T2D"}
  - {name: t1/2, value: 1, unit: week, source: "12.3, p19-20", note: "approximately"}
```

Package panels (p40-62): Ozempic tablets 1.5, 4, 9 mg; Rybelsus 3, 7, 14 mg bottles and blisters; and **Rybelsus
1.5, 4 and 9 mg bottles marked "New Formulation (Formulation R2)"** (p52-54, rendered p52), with a pharmacist's note
that individual strengths of Rybelsus are not substitutable mg to mg. So the 1.5/4/9 mg formulation is also packaged
under the Rybelsus name.

## Pharmacodynamics and end points
As in the FDA PDF: 12.2 glucose changes are from semaglutide injection 1 mg weekly; no exposure-response.

## Covariates
12.3 Specific Populations: no clinically significant PK differences by age, sex, race, ethnicity, weight 40-188 kg,
upper GI disease, hepatic or renal impairment. No dose change in renal or hepatic impairment (8.6, 8.7).

## Caveats
- A web rendering retrieved 2026-10-21; DailyMed set ids carry later versions at the same URL, so the live page may
  differ from this file.
- Summary label values without study sizes or variability.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:6` (and `data-raw/antidiabeticRegistry.R:68-71`, which writes it): row
  `semaglutide_po_ozempic_tablets`, "Ozempic tablets (2026)", PO, US, PK-CONSTRAINT, not implemented, source this
  set id; note "Not mg-for-mg substitutable with Rybelsus; may not inherit its F."

## Verification against stanpumpR
- **Registry row 6**: the product exists in this label (1.5, 4, 9 mg, oral, US); "not mg-for-mg substitutable with
  Rybelsus" matches 2.1; "may not inherit its F" is supported by 12.3, which gives the Ozempic tablets a different
  absolute bioavailability (1-2% vs 0.4-1%). The label also gives a dose mapping (Table 1) and paired-strength
  exposure similarity (12.3), which a future model could use; the registry leaves the product unmodelled, a design
  choice. **matches**
- Naming note for `antidiabetics.md:35` and `semaglutideOral.md:29`: "Rybelsus" in the repo means the 3/7/14 mg
  tablets; this SPL also packages the 1.5/4/9 mg R2 formulation as Rybelsus (p52-54). The repo text is not wrong,
  but "Rybelsus" alone does not now identify one formulation.
