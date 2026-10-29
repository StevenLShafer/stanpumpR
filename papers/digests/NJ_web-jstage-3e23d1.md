---
file: NJ_web-jstage-3e23d1.pdf
pmid: null
doi: 10.1248/bpb.b16-00998
citation: "Samukawa Y, Mutoh M, Chen S, Mizui N. Mechanism-based pharmacokinetic-pharmacodynamic modeling of luseogliflozin, a sodium glucose co-transporter 2 inhibitor, in Japanese patients with type 2 diabetes mellitus. Biol Pharm Bull 2017;40(8):1207-1218. (J-STAGE PDF, https://www.jstage.jst.go.jp/article/bpb/40/8/40_b16-00998/_pdf)"
kind: pkpd-model
drugs: [luseogliflozin]
source_read: "text layer, all 12 pages (PDF page = printed page - 1206). Rendered pages 2 (Table 1, scrambled in the text layer) and 7 (Table 4, Figures 4-5). Tables 2, 3 and 5 are readable in the text layer. Supplementary Figure S1 is not in the archived PDF. The DOI is the article's J-STAGE DOI pattern, inferred from the URL and not printed in the text layer."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Samukawa 2017: mechanism-based PK-PD model of luseogliflozin UGE (Japanese T2DM)

This is a peer-reviewed journal article, archived under an NJ_ stem because it was linked by its J-STAGE URL. The
text is the article named in the registry: luseogliflozin, Biol Pharm Bull 40(8), 12 pages.

## Study
- **Data:** a clinical pharmacology study (Sasaki et al., ref 9) by Taisho Pharmaceutical, whose authors also
  wrote this paper.
- **Subjects:** 31 Japanese T2DM outpatients, 26 men and 5 women.
- **Dosing:** luseogliflozin 0.5 mg (n = 7), 1 mg (n = 8), 2.5 mg (n = 8) or 5 mg (n = 8) once daily before
  breakfast for 7 days. Standardized meals of about 600 kcal at 0, 4 and 12 h.
- **Baseline (Table 1, p2, rendered):**
  - age 57.6 ± 9.0 years (43-74);
  - weight 69.98 ± 12.74 kg (51.9-99.3);
  - eGFR 90.6 ± 17.8 mL/min (59-116);
  - FPG 158.2 ± 30.3 mg/dL (113-229);
  - UGE 27.5 ± 23.9 g/d (0.329-81.4).
- **Sampling:**
  - plasma luseogliflozin at 0-24 h on days 1 and 7, and to 96 h after day 7;
  - plasma glucose 15 times a day on days −1, 1 and 7;
  - urine glucose in nine intervals a day.
- **PK analysis:** individual two-compartment fits with first-order absorption and lag, in WinNonlin 6.2 (a
  two-stage approach, not a population PK model).
- **PD and PK-PD analysis:** population analysis in NONMEM 7.2, FOCE.

## Model and parameters
**PK (individual fits, geometric mean and CV; p6):**

```yaml
parameters:
  - {name: CL/F, value: 2.78, unit: L/h, source: "Results p6", note: "geometric mean of individual fits; CV 14.9%"}
  - {name: V1/F, value: 21.1, unit: L, source: "Results p6", note: "CV 25.7%"}
  - {name: V2/F, value: 18.9, unit: L, source: "Results p6", note: "CV 35.1%"}
  - {name: Q/F, value: 3.69, unit: L/h, source: "Results p6", note: "CV 75.4%"}
  - {name: ka, value: 20.3, unit: 1/h, source: "Results p6", note: "CV 49.2%"}
  - {name: tlag, value: 0.174, unit: h, source: "Results p6", note: "CV 40.7%"}
  - {name: "protein binding", value: 96, unit: "%", source: "Methods p5", note: "literature value; free fraction drove the PD"}
  - {name: "Vmax2 (SGLT2 capacity)", value: 374, unit: g/d, source: "Table 3, p6", note: "population, PD model with Kg; individual mean 376 (range 228-465)"}
  - {name: "Vmax1 (SGLT1 capacity)", value: 31.9, unit: g/d, source: "Table 3, p6", note: "individual mean 30 (14-53)"}
  - {name: "Km1 / Km2", value: "1.8 / 4.9", unit: mM, source: "Methods p5", note: "fixed, in vitro literature"}
  - {name: "Ki2, model 2", value: 0.336, unit: nM, source: "Table 4, p7", note: "in vitro 1.10 nM (ratio 0.31); model 1: 0.251"}
  - {name: "Kon, model 2", value: 0.303, unit: 1/nM/h, source: "Table 4, p7", note: "in vitro 0.084 (ratio 3.6)"}
  - {name: "Koff, model 2", value: 0.102, unit: 1/h, source: "Table 4, p7", note: "derived by the authors as Ki2 x Kon; in vitro 0.11"}
  - {name: "T1/2,off", value: 6.81, unit: h, source: "Table 4, p7", note: "ln2/Koff; in vitro 7.0 h"}
  - {name: "Kg (excretion lag)", value: 2.20, unit: 1/h, source: "Table 4, p7", note: "model 2; 1.19 /h without drug (Table 3)"}
  - {name: Ki1, value: 1950, unit: nM, source: "Methods p5", note: "assumed as 1770 x Ki2(in vitro); model insensitive to 195-19500"}
```

**PD structure:**
- Michaelis-Menten glucose reabsorption by SGLT1 and SGLT2 along the proximal tubule. eGFR is the tubular flow.
- Inhibition is competitive. In model 2, SGLT2 binding has explicit Kon and Koff (Eq. 3-6).
- A first-order lag Kg sits between the tubule and urine.
- Model 2 improved on model 1: ΔOBJ −176.441, residual 5.43 → 4.71 g.

**Arithmetic checks (derived):**
- Koff = 0.336 × 0.303 = 0.102 /h.
- ln2/0.102 = 6.8 h.
- Ki1 = 1770 × 1.10 = 1947 nM.

**Cross-check by the authors (p10):** Sasaki's EC50 of 254 ng·h/mL (daily UGE against AUC0-24h) converts to a
24-h mean free concentration of 0.974 nM and an in vivo Ki2 of 0.312 nM. That is close to model 2's 0.336 nM.

## Pharmacodynamics and end points
- **End point:** urinary glucose excretion rate in each collection interval.
- **Main finding:** sustained UGE after supper is attributed to the slow dissociation of luseogliflozin from SGLT2
  (T1/2,off 6.81 h).
- The paper fits no FPG, HbA1c or effect-compartment ke0.

## Covariates
- eGFR correlated with individual Vmax1 (p = 0.047) and Vmax2 (p < 0.001) (Figure 5). It was not retained as a
  NONMEM covariate.
- PK covariates were not modelled.

## Caveats
- n = 31, Japanese patients only, 7-day dosing. PK was by individual two-stage fits.
- Several in vitro constants were fixed (Km1, Km2, Ki1, K1).
- **Source inconsistencies:**
  - K1 units are 1/(mg/dL)/h in Methods but 1/(mg/mL)/h in Results.
  - The K1 sensitivity values listed differ: 0.3, 1 and 3 in Methods; 0.3, 1, 3 and 10 in Results.
  - Table 1's total HbA1c mean (8.33%) does not match the dose groups: derived n-weighted mean 7.93% from 8.26,
    7.61, 8.30 and 7.59.
  - The Table 4 legend defines ωKon, but the column is ωKg.
- The paper names no product brand.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:78`, key `luseogliflozin`, confirmed at 8baa733. It is generated by
  `data-raw/antidiabeticRegistry.R:298-299`.
- Fields:
  - product: Lusefi;
  - PO, international tier, jurisdiction Japan;
  - PK-AUDIT / PD-AUDIT, PD end point UGE;
  - not implemented;
  - note: "No borrowed parameters."

## Verification against stanpumpR
- **PD end point "UGE"**: the model's end point is the UGE rate. **matches**
- **Jurisdiction Japan**: Japanese patients; the drug is described as marketed for T2DM, at a recommended
  2.5 mg (or 5 mg) once daily (p1). **matches**
- **Product name "Lusefi"**: cannot verify from this paper, which gives no brand name.
- **PK-AUDIT / PD-AUDIT, not implemented, no borrowed parameters**: consistent. If the drug is used later, note
  that the PK here is a two-stage individual-fit summary, not a population model, and that the PD needs plasma
  glucose time courses and fixed in vitro constants.
