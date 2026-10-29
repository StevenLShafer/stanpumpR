---
file: NJ_fda-review-nda202880_202880_nodate.pdf
pmid: none
doi: none
citation: "US FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 202880, Zohydro ER (hydrocodone bitartrate extended-release capsules 10-50 mg, Zogenix; 505(b)(2), listed drug Vicoprofen NDA 20-716). Clinical pharmacology review D. Lee, pharmacometrics J.-Y. Lee, V.A. Bhattaram, Y. Xu (Reference ID 3245215); ONDQA biopharmaceutics review M. Hughes, J. Duan, signed 01/15/2013. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2013/202880Orig1s000ClinPharmR.pdf"
kind: review
drugs: [hydrocodone]
source_read: "text layer for the whole document (110 pp.: biopharmaceutics review, clinical pharmacology review sections 1-2.6, appendix 4.3 pharmacometric review, filing forms); page image (Flate-decoded) of PDF p.93 for the pharmacometric Table 1, which is an image. Many PK tables (Tables 3-38) and the proposed label (review pp.69-91) are images or not present in the text layer and were not read. This PDF is byte-identical to NJ_fda-review-nda202880_202880_202880Orig1s000ClinPharmR.pdf (duplicate archive copy; same digest)."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# FDA 2013: NDA 202880 Zohydro ER clinical pharmacology review

## Study
Six Phase 1 and two Phase 2 studies of HC-ER (SODAS bead capsule, q12h). Population PK (appendix 4.3, sponsor's
analysis): pooled Elan studies ELN-0901001, ELN-0302002, ELN154088-201, ELN154088-203 and Zogenix ZX002-0901,
-1001, -1002. The review does not give the number of subjects in the population analysis and does not name an
author or publication for it. Assay LC-MS/MS, 0.1-100 ng/mL for hydrocodone, hydromorphone, norhydrocodone.

## Model and parameters
Two-compartment, linear elimination, complex absorption: two sequential first-order processes, the first delayed
by transit compartments (p.92). IIV on absorption rate constants, CL/F, Vc/F. Covariates CLcr on CL/F and BSA on
Vc/F. Table 1, p.93 (source: sponsor's report p.49), final estimates (%SEM):

```yaml
parameters:
  - {name: CL/F, value: 64.4, unit: L/h, source: "p.93 Table 1", note: "SEM 2.25%; IIV 32.4% CV"}
  - {name: Vc/F, value: 714, unit: L, source: "p.93 Table 1", note: "SEM 2.83%; IIV 30.2% CV; cov(IIV CL/F, Vc/F) 0.0780"}
  - {name: Vp/F, value: 151, unit: "not printed (L implied)", source: "p.93 Table 1", note: "SEM 34.8%"}
  - {name: CLd/F, value: 0.910, unit: "not printed (L/h implied)", source: "p.93 Table 1", note: "SEM 18.2%"}
  - {name: Ka1, value: 2.27, unit: 1/h, source: "p.93 Table 1", note: "IIV 115%"}
  - {name: Lag, value: 2.39, unit: h, source: "p.93 Table 1"}
  - {name: F1, value: 0.382, unit: fraction, source: "p.93 Table 1", note: "fraction to first process; routing not described"}
  - {name: Ka2, value: 0.434, unit: 1/h, source: "p.93 Table 1", note: "IIV 31.8%"}
  - {name: Ktr, value: 8.52, unit: 1/h, source: "p.93 Table 1", note: "IIV 81.7%"}
  - {name: CLcr_exponent_on_CL/F, value: 0.223, unit: "-", source: "p.93 Table 1", note: "power; reference CLcr not given"}
  - {name: BSA_exponent_on_Vc/F, value: 0.486, unit: "-", source: "p.93 Table 1", note: "power; reference BSA not given"}
  - {name: residual_SD, value: 0.112, unit: "log scale (implied)", source: "p.93 Table 1"}
```
Derived from the disposition: hybrid rates 0.0916 and 0.00594 /h (half-times 7.6 h and 117 h).

Noncompartmental (summary pp.3-8): 30 mg HC-ER vs Vicoprofen (2 x 7.5 mg twice) Cmax 32 vs 46 ng/mL, AUC 513 vs
559 ng.h/mL; ER/IR GMR AUC0-t 91.1%, AUCinf 93.2%, Cmax 68.7%. Single dose 10/20/30/40 mg (bunionectomy):
Cmax 8.9/17.9/31.7/37.5 ng/mL, AUC 137/256/481/596 ng.h/mL. Food (20 mg): Cmax fed 28.8 vs fasted 22.7 ng/mL, AUC
338 vs 345; median tmax 6 h fasted, 8 h fed; t1/2 6.5 h fasted, 4.9 h fed. Alcohol 40%: Cmax 109 vs 46 ng/mL.
Hepatic moderate AUC +26%; renal moderate/severe AUC +57%/+44%. At steady state (ELN154088-203, p.27) hydromorphone AUC was 1.1-1.4% and norhydrocodone
31.6-38.0% of hydrocodone AUC; peak-to-trough fluctuation 50-60%.

## Pharmacodynamics and end points
Opioid audit: no MEAC or minimum analgesic concentration is reported; in ELN154088-201 VASPI vs plasma
concentration showed no significant correlation (r 0.31 hydrocodone, 0.31 norhydrocodone, 0.08 hydromorphone;
p.18). No ke0, t1/2ke0, tPeak or hysteresis. Potency: the Phase 3 entry criterion equates hydrocodone 30 mg/day
with 45 mg/day oral morphine equivalents (p.11), i.e. 1.5 (derived), a conversion convention, not measured.

## Covariates
CL/F = 64.4 x (CLcr/ref)^0.223; Vc/F = 714 x (BSA/ref)^0.486; reference values not printed.

## Caveats
Apparent oral parameters only; no IV data. The filing form's "Absolute bioavailability" row is blank, but the
review text nowhere states that absolute bioavailability has never been measured. Units of Vp/F and CLd/F are
not printed. Absorption routing (which depot F1 enters, whether the lag applies to the transit or the second
process) cannot be reconstructed from the table.

## Used by stanpumpR
- R/drugs_hydrocodone.R:11 — "two separate FDA reviews say so" (absolute F never measured).
- R/drugs_hydrocodone.R:25-28 — Melhem 2013 model "reproduced in the FDA review", 220 subjects, CL/F 64.4 L/h,
  Vc/F 714 L, Q/F 0.910 L/h, Vp/F 151 L; 0.910 not 91.0.
- R/drugs_hydrocodone.R:101-102 — reference line, "numerical model on p.93".
- inst/help/drugs/hydrocodone.md:3 — help text of the same model; tests/testthat/test-drugs-hydrocodone.R:53 —
  pins the four values.

## Verification against stanpumpR
- R/drugs_hydrocodone.R:26-28, test:53-57: CL/F 64.4 L/h, Vc/F 714 L, Q/F 0.910 L/h, Vp/F 151 L. Table 1 p.93:
  identical (CLd/F printed 0.910). **matches**. Help md:3: CL 1.07 L/min (64.4/60) and Q 0.015 L/min (0.910/60),
  **matches**. Eigenvalues 0.0916 and 0.00594 /h (R/drugs_hydrocodone.R:36), derived here identically.
- R/drugs_hydrocodone.R:101-102: "numerical model on p.93" — Table 1 is on PDF/review p.93. **matches**.
- R/drugs_hydrocodone.R:25-26 "Melhem 2013 ... from 220 subjects": the review names no author or publication
  and gives no subject count. **cannot verify** from this document.
- R/drugs_hydrocodone.R:29-33: covariate reference values not recovered: correct, Table 1 gives only exponents
  0.223 (CLcr) and 0.486 (BSA). **matches**.
- R/drugs_hydrocodone.R:11 "absolute bioavailability ... never measured; two separate FDA reviews say so": this
  review does not say so explicitly (only an empty checklist row). **cannot verify**; the second review is
  unnamed in the repo.
