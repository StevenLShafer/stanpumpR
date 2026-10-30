---
file: NJ_page2010-bartels_poster.pdf
pmid: none
doi: none
citation: "Bartels C, Armogida M, Hamrén B (Novartis). Population PK model for pooled data of different oral diclofenac formulations. Poster, Population Approach Group Europe (PAGE) meeting, Athens, 7-10 June 2010. https://www.page-meeting.org/wp-content/uploads/pdf_assets/3455-213_v5_ChristianBartels_final.pdf"
kind: pk-model
drugs: [diclofenacEC, diclofenac]
source_read: "text layer (one vector page; layout scrambled), tables reconstructed from positioned text extraction (pypdf glyph coordinates) for Tables 1-5 and Figure 2 labels"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Bartels 2010 (PAGE poster): pooled popPK of four oral diclofenac formulations

## Study
- Pooled rich-sampling data from healthy subjects (age, weight, sex not reported). Table 1:

| Formulation | Dosing | Subjects | Obs above LOQ | Samples below LOQ |
|---|---|---|---|---|
| Immediate release | single dose | 117 | 2042 | 298 |
| Mixed release | 5 days, b.i.d. (0 h, 12 h) | 21 | 650 | 43 |
| Slow release | single dose | 21 | 109 | 47 |
| Enteric coated | 5 days, t.i.d. (0 h, 4 h, 12 h) | 12 | 598 | 221 |

- BLQ data excluded. NONMEM VI 2.0, FOCE, log-transformed data; no covariate analysis.
- Salt/product of each arm not named. Plasma diclofenac (total).

## Model and parameters
Structure (Figure 2): two-compartment disposition; two parallel first-order absorption paths. Fraction Ffast goes
through a lag, then two transition compartments (kT1 = kT2 = 100 /h), then kfast; 1 - Ffast is absorbed at kslow.
Ffast estimated as an odds ratio.

Table 2 (disposition, apparent, F of immediate release): CL/F 40.3 L/h (RSE 2.05%), VC/F 23.5 L (6.26%),
VP/F 21.3 L (4.16%), Q/F 10.6 L/h (7.42%).

Table 3 (absorption; estimate, RSE%):

| Parameter | Immediate | Slow | Enteric | Mixed |
|---|---|---|---|---|
| Frelative (to IR) | 1 FIX | 1 FIX | 0.784 (9.27) | 0.856 (6) |
| kfast, 1/h | 4.84 (16.3) | 0.204 (22.5) | 0.503 (43.3) | 0.994 (19.7) |
| kslow, 1/h | - | 0.0148 (37.3) | - | 0.0546 (10.7) |
| Ffast | 1 FIX | 0.76 (31.1) | 1 FIX | 0.22 (15.3) |
| Lag, h | 0.15 FIX | 0.15 FIX | 0.932 (4.62) | 0.15 FIX |

Table 4: IIV on CL 14.9% CV (RSE 18.4%); IOV on kfast and kslow 143% (23.1%); exponential.
Table 5 (residual SD, additive on log scale, %): immediate 57.7, slow 59.9, enteric 96.4, mixed 47.2.

```yaml
parameters:
  - {name: CL/F, value: 40.3, unit: L/h, source: "Table 2", note: "apparent, IR reference"}
  - {name: Vc/F, value: 23.5, unit: L, source: "Table 2"}
  - {name: Vp/F, value: 21.3, unit: L, source: "Table 2"}
  - {name: Q/F, value: 10.6, unit: L/h, source: "Table 2"}
  - {name: Frel_EC, value: 0.784, unit: fraction, source: "Table 3", note: "relative to IR"}
  - {name: kfast_EC, value: 0.503, unit: 1/h, source: "Table 3"}
  - {name: lag_EC, value: 0.932, unit: h, source: "Table 3"}
  - {name: Ffast_EC, value: 1, unit: fraction, source: "Table 3", note: FIX}
  - {name: kT1_kT2, value: 100, unit: 1/h, source: "Figure 2", note: "two transition compartments, fast path"}
  - {name: IIV_CL, value: 14.9, unit: "%CV", source: "Table 4"}
  - {name: IOV_ka, value: 143, unit: "%CV", source: "Table 4"}
  - {name: sigma_EC, value: 96.4, unit: "% SD log scale", source: "Table 5"}
```

## Pharmacodynamics and end points
None: no ke0, no concentration-effect, no analgesic concentration. Not an opioid.

## Covariates
None analysed.

## Caveats
- Conference poster, not peer reviewed. The improved candidate model (IOV on lag, formulation on variability) failed
  to converge (rounding errors) despite a large OFV drop; FOCE failed without the transition compartments.
- Enteric-coated arm: 12 subjects, highest residual error, multiple peaks in some profiles; RSE on its kfast 43%.
- Absolute bioavailability unknown: everything is apparent relative to the IR arm.

## Used by stanpumpR
R/drugs_diclofenacEC.R (header 2-42, constants 72-73, PK list 84-96, reference 101-106); inst/help/drugs/diclofenacEC.md:3,
11, 15; tests/testthat/test-drugs-diclofenacEC.R:1-27; inst/help/drugs/diclofenac.md:13; docs/nsaid-models.md:13.

## Verification against stanpumpR
- Disposition CL/F 40.3, Vc/F 23.5, Q/F 10.6, Vp/F 21.3 (R:29, 85-90; test:12-13; help:3): **matches** Table 2,
  L/h divided by 60 for L/min.
- Enteric Frel 0.784, Ffast 1 FIX, kfast 0.503 /h, lag 0.932 h (R:33-35, 72, 93-95; test:14-16): **matches** Table 3
  (kfast /60 to per minute, lag x 60 to minutes).
- Transition compartments 2 x 100 /h carried as their mean transit 2/100 h = 1.2 min added to the lag (R:35-38, 73):
  **matches** Figure 2; the replacement of two transit stages by a pure delay is an approximation, stated.
- Arm sizes and dosing (R:24-27, help:3; 117/21/21/12; EC t.i.d. 5 days): **matches** Table 1. diclofenac.md:13 "171
  adults": 117 + 21 + 21 + 12 = 171 (derived) **matches**; the poster says "healthy subjects", not adults, so "adults"
  is an inference.
- IIV CL 14.9%, IOV 143%, residual 96.4% EC vs 57.7% IR (R:46-49, help:15): **matches** Tables 4-5.
- Half-times about 18 min and 1.9 h (help:3): derived from Table 2, 0.30 h and 1.86 h: **matches**.
- No covariates, no effect site (R:56-63): **matches**.
