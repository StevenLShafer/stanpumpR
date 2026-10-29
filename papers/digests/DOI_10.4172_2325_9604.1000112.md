---
file: DOI_10.4172_2325_9604.1000112.pdf
pmid: ""
doi: 10.4172/2325-9604.1000112
citation: "Frank T. Population pharmacokinetics of lixisenatide, a once-daily human glucagon-like peptide-1 receptor agonist, in healthy subjects and in patients with type 2 diabetes. J Pharm Drug Deliv Res 2013;2(1). doi:10.4172/2325-9604.1000112"
kind: pk-model
drugs: [lixisenatide]
source_read: "partial: a 15-page browser print of the SciTechnol web page (printed 2026-10-17); article text on PDF p1-9, references p9-11, publisher boilerplate p11-15. Tables 1, 2 and 3 and Figures 1-4 are link icons only, not content (confirmed on rendered PDF p3 and p8); equations 3-5 are images (rendered p5); the abstract is truncated and overprinted in the text layer (p1-2)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Frank 2013: lixisenatide population PK in healthy subjects and type 2 diabetes

**Source file limitation.** The archived file is a web print in which the parameter table (Table 2), the study table
(Table 1) and the bias table (Table 3) did not render. Only the numbers quoted in the running text can be checked
here; the repo's Table 2 and Table 1 values came from an image supplied by Steven L. Shafer.

## Study
Pooled data from 7 Sanofi studies (2 phase 1, 2 phase 2, 3 phase 3, GetGoal programme): 1223 subjects, 75 healthy (single
SC dose) and 1148 with T2DM (multiple SC doses, usually 30 min before breakfast, or breakfast and dinner for BID)
(PDF p3, p6). Demographics (PDF p7): mean age 56.5 y, 46.9% women; 59.6% Caucasian, 32.5% Asian, 3.3% Black; 16
subjects over 75 y; 22 at or below 50 kg, 53.5% over 80 kg; 48.5% BMI over 30. Assay: total lixisenatide (bound and
unbound to anti-drug antibodies), double-antibody sandwich ELISA, LLOQ 12 ng/L (8.8 ng/L in POP6053) (PDF p3).
All antibody-positive or antibody-unknown visits discarded; BLQ handled by M1 (phase 3) or M6 (phase 1-2). 9623
concentrations reduced to 5770, then 162 suspected antibody-positive outliers removed, leaving 5608 (2738, 32.8%, BLQ
at LLOQ/2) (PDF p6-7). NONMEM 7.2, FOCE-I on log data; IIV and IOV; residual error with additive constant M (eq 2-3).
Covariates by forward addition (p < 0.01) and backward elimination (p < 0.001); CLCR by Cockcroft-Gault, tested on
CL/F only; continuous covariates as power (eq 4) or linear fractional (eq 5) terms (PDF p4-5).

## Model and parameters
One compartment, first-order absorption (ADVAN2/TRANS1), parameterised by MAT (about 1/ka), V/F and CL/F; F unknown
(PDF p4). Final covariates: weight on V/F, CLCR on CL/F, dose on MAT; SGPT dropped from CL/F, BMI on MAT dropped
(0.009 per kg/m2, 95% CI -0.001 to 0.018), IOV on MAT dropped (PDF p7-8). From the text (PDF p8):
- CL/F falls 9.9% per 1 L/h of CLCR below the population median, for a person of 82.1 kg.
- moderate renal impairment (CLCR 1.8-3.0 L/h) gives 1.5- to 1.8-fold exposure (stated at 20 µg).
- MAT 3.25 h; V/CL 1.07 h; so flip-flop, t1/2z about ln 2 x MAT, 2.3 h; ka > ke in only 6 subjects (0.5%).
- IIV (log domain) after covariates: V/F 30.3% (from 61.4%), MAT 30.0% (from 33.2%), CL/F increased (value in
  Table 2); IOV 45% on CL/F, 59% on V/F (PDF p8-9). Bias significant for CL/F and V/F, under 20% (PDF p9).

```yaml
parameters:
  - {name: MAT, value: 3.25, unit: h, source: "Results text, PDF p8", note: "typical; dose reference not stated in the text"}
  - {name: V_over_CL, value: 1.07, unit: h, source: "Results text, PDF p8", note: "mean elimination time"}
  - {name: CLCR_effect_on_CL/F, value: 0.099, unit: "fraction per L/h", source: "Results text, PDF p8", note: "9.9% per 1 L/h below the median"}
  - {name: reference_weight, value: 82.1, unit: kg, source: "Results text, PDF p8", note: "person for whom the renal effect is stated"}
  - {name: IIV_V/F, value: 30.3, unit: "%", source: "Results text, PDF p8", note: final model}
  - {name: IIV_MAT, value: 30.0, unit: "%", source: "Results text, PDF p8", note: final model}
  - {name: IOV_CL/F, value: 45, unit: "%", source: "Results text, PDF p9"}
  - {name: IOV_V/F, value: 59, unit: "%", source: "Results text, PDF p9"}
  # CL/F, V/F, the weight exponent, the CLCR median and the dose exponent on MAT are in Table 2, which is not in this file
```

## Pharmacodynamics and end points
None; PK only.

## Covariates
Final: CLCR (Cockcroft-Gault, L/h) linear on CL/F; weight on V/F; dose on MAT (form and coefficients in Table 2, not
rendered). Antibodies raise AUC and Cmax, delay tmax and prolong t1/2; about 70% antibody-positive by week 24 (PDF p4,
p7). Age, weight and sex not kept separately because they are inside Cockcroft-Gault (PDF p8).

## Caveats
Antibody-negative data only, which removes a large fraction of phase 3 exposure; absorption-limited kinetics, so the
terminal slope is absorption; IOV is large; F unknown, all apparent. Sponsor-authored, low-profile journal. The web
print lacks every table.

## Used by stanpumpR
- `R/drugs_lixisenatide.R:10-70` (header: Table 2 "supplied ... as an image", CLCR median 6.21 L/h, checks) and :72-127
  (constants; function; reference string :118-125): CL/F 38.6 L/h x (1 + 0.099 (CLCR - 6.21)), V/F 41.2 L x
  (WT/82.1)^1.56, MAT 3.25 h x (dose/15 mcg)^0.236; weight held 43.8-163 kg, dose 5-30 mcg (Table 1, supplied).
- `inst/help/drugs/lixisenatide.md:3-16`; `tests/testthat/test-drugs-lixisenatide.R:1-76`;
  `inst/help/models/antidiabetics.md:12`; `inst/extdata/antidiabeticParameters.csv:129-136`;
  `inst/extdata/antidiabeticRegistry.csv:16`.

## Verification against stanpumpR
- One compartment, ADVAN2, MAT parameterisation, 1223 subjects in 7 studies, antibody-negative visits only, CLCR by
  Cockcroft-Gault, final covariates weight/CLCR/dose: **matches** the text.
- 0.099 per L/h on CL/F (code :101, CSV :130): **matches** "9.9% per 1 L/h".
- MAT 3.25 h (CSV :135), V/CL 1.07 h (header :29, test :13): 41.2 / 38.6 = 1.067 h (derived): **matches**.
- 1.5- to 1.8-fold at CLCR 1.8-3.0 L/h (header :27-28, help :10, test :21-23): 1 / (1 + 0.099 (3.0 - 6.21)) = 1.47,
  1 / (1 + 0.099 (1.8 - 6.21)) = 1.78 (derived), which supports the 6.21 L/h median: **matches**.
- t1/2 about 2.3 h (help :6): **matches**.
- CL/F 38.6 L/h, V/F 41.2 L, weight exponent 1.56, dose exponent 0.236 with reference 15 mcg, weight range 43.8-163 kg,
  dose range 5-30 mcg (code :72-105, CSV :129, 132-136, registry :16): **cannot verify**; Tables 1 and 2 are absent from
  the archived file. 82.1 kg as the weight reference is consistent with the text.
- Help :16 and header :58-60 (peak 88 pg/mL at 1.8 h after 20 mcg; AUC 520 pg h/mL): not in the paper; reproduced from
  the repo's parameters (derived tmax 1.82 h, Cmax 88.3 pg/mL; 20000 / 38.6 = 518).
