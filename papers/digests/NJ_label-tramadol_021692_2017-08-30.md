---
file: NJ_label-tramadol_021692_2017-08-30.pdf
pmid: none
doi: none
citation: ULTRAM ER (tramadol hydrochloride) extended-release tablets, US prescribing information, NDA 021692, revised 08/2017 (Reference ID 4145206). https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/021692s015lbl.pdf
kind: label
drugs: [tramadol]
source_read: text layer; read Highlights, contents, 2 Dosage and Administration, 8.4-8.7, 12.1-12.3 in full; skimmed boxed warning and section 5
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# ULTRAM ER label (NDA 021692, 2017): once-daily extended-release tramadol tablets

## Study
FDA label for ULTRAM ER 100, 200 and 300 mg extended-release tablets (racemic tramadol HCl), chronic pain.
PK data are from healthy-subject studies summarised by the sponsor (Table 3: n = 32 steady-state crossover).

## Model and parameters
Section 12.3:
- **Relative bioavailability**: ULTRAM ER 200 mg once daily vs ULTRAM IR 50 mg every 6 h, healthy subjects,
  approximately **85 to 90%**. Lag time in absorption (ER design). Tramadol and M1 peaks at about 12 and 15 h.
  Steady state within 4 days.
- Dose proportionality 100-400 mg approximate; 400 mg AUC 26% higher than predicted from 200 mg.
- **No absolute oral bioavailability is stated** in this label (no 0.70 or 0.75 figure).

Table 3, steady state, mean (%CV), n = 32:

| Parameter | Tramadol ER 200 mg qd | Tramadol IR 50 mg q6h | M1 ER | M1 IR |
|---|---|---|---|---|
| AUC0-24 (ng·h/mL) | 5975 (34) | 6613 (27) | 1890 (25) | 2095 (26) |
| Cmax (ng/mL) | 335 (35) | 383 (21) | 95 (24) | 104 (24) |
| Cmin (ng/mL) | 187 (37) | 228 (32) | 69 (30) | 82 (27) |
| Tmax (h) | 12 (27) | 1.5 (42) | 15 (27) | 1.9 (57) |
| % fluctuation | 61 (57) | 59 (35) | 34 (72) | 26 (47) |

Ratio of AUC0-24 ER/IR = 5975/6613 = 0.90 (derived).

Other 12.3 facts: high-fat meal (200 mg single dose) lowers Cmax 28% and AUC 16%, Tmax 14 h fasting to 17 h fed.
Vd 2.6 L/kg (male) and 2.9 L/kg (female) after 100 mg IV; plasma protein binding about 20%. Terminal t½ after ER:
tramadol about 7.9 h, M1 about 8.8 h. About 30% of dose excreted unchanged in urine, 60% as metabolites.
Elderly (IR data): over 75 y, Cmax 208 vs 162 ng/mL and t½ 7 vs 6 h compared with 65-75 y. CYP2D6 poor
metabolisers (about 7%): tramadol about 20% higher, M1 40% lower (popPK of IR). Quinidine: tramadol exposure
+50-60%, M1 -50-60%. Hepatic impairment (mild/moderate, ER 100 mg): M1 exposure down about 50%. Renal impairment
(CLcr 30-80): M1 exposure up 20-40%; dialysis removes < 7% in 4 h.

```yaml
parameters:
  - {name: F_rel_ER_vs_IR, value: "0.85-0.90", unit: fraction, source: "12.3 Absorption", note: "200 mg qd vs 50 mg q6h, steady state, healthy"}
  - {name: Vd, value: 2.6, unit: L/kg, source: "12.3 Distribution", note: "male, 100 mg IV; female 2.9"}
  - {name: protein_binding, value: 0.20, unit: fraction, source: "12.3"}
  - {name: t_half_tramadol_ER, value: 7.9, unit: h, source: "12.3 Elimination"}
  - {name: t_half_M1_ER, value: 8.8, unit: h, source: "12.3 Elimination"}
  - {name: Tmax_IR_ss, value: 1.5, unit: h, source: "Table 3", note: "IR 50 mg q6h"}
```

## Pharmacodynamics and end points
- **MEAC**: section 12.2 says the minimum effective analgesic concentration varies widely and may rise with
  tolerance; **no value given**.
- **Potency**: M1 is 6 times more potent than tramadol for analgesia and 200 times more potent in mu-opioid binding,
  in animal models (12.3 Metabolism). No equianalgesic ratio to morphine; section 2.2 states there are no
  established conversion ratios from other opioids.
- No ke0, t½ke0, tPeak or hysteresis.
- QTcF: no significant effect at 600 mg/day in 68 healthy subjects.

## Covariates
Qualitative/descriptive: age > 75 y, sex (females somewhat higher dose-normalised AUC; no adjustment), hepatic and
renal impairment, CYP2D6 phenotype, CYP2D6 and CYP3A4 interactions.

## Caveats
- ER product, not the IR capsule or tablet that stanpumpR models.
- Bioavailability is relative to IR, not absolute.
- Section 8.7 refers to severe renal impairment as "Child-Pugh Class C", a label error (Child-Pugh is hepatic).

## Used by stanpumpR
- R/drugs_tramadol.R:55-58 — header: bioavailability 0.70 "the regulatory figure for capsules"; 100 mg IR tablet
  quoted at 0.75; a 24-hour ER product at 85 to 90% of its IR comparator, not represented. (Value in code:
  `bioavailability_PO <- 0.70`, R/drugs_tramadol.R:176; help inst/help/drugs/tramadol.md:3.)

## Verification against stanpumpR
- "24-hour extended-release product at 85 to 90 per cent of its immediate-release comparator" (R/drugs_tramadol.R:56-57):
  matches 12.3 Absorption (ULTRAM ER 200 mg qd vs ULTRAM 50 mg q6h). **matches**; this label is evidently the
  source of that clause.
- "Bioavailability is 0.70, the regulatory figure for capsules" (R/drugs_tramadol.R:55) and "100 mg IR tablet quoted
  at 0.75" (:55-56): neither figure appears in this label. **cannot verify** from this document; the source is
  presumably the ULTRAM IR label (NDA 020281) or a capsule label/SmPC, not archived here.
- The comment names no label or agency, so the citation is untraceable as written (as the context notes say).
