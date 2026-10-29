---
file: NJ_fda-l1-14_022063_022063Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "FDA Center for Drug Evaluation and Research. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 022063 (Mydayis, SHP465, mixed salts of a single-entity amphetamine, extended-release capsules 12.5/25/37.5/50 mg; Shire). Resubmission dated 12/20/2016; OCP reviewers Kumi, Bewernitz, Krudys, Zhu; Reference ID 4109794 (2017). Includes the 2007 original review (A. Jackson). https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/022063Orig1s000ClinPharmR.pdf"
kind: review
drugs: [mixedAmphetamineSalts]
source_read: "text layer of the whole review, plus page images for the image-only tables: PDF p7 (Figure 1, SHP465 vs Adderall XR + IR), p24-25 (Tables 6 and 7, pediatric NCA by age), p41 (Table 4.3.2, final d-amphetamine popPK model, Run 56), p45-46 (covariate forest plots). Sections read: 1 Executive Summary, 2 Summary of Clinical Pharmacology, 3.2-3.3.6, 4.2 Study SHP465-111, 4.3 Population PK, 4.4, and the 2007 review's summary, BE and food sections. Dissolution and assay-validation appendices skimmed."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# FDA 2017: Mydayis (SHP465) clinical pharmacology review

## Study
Regulatory review of a once-daily, three-bead (IR bead plus two delayed-release bead populations) extended-release product containing the same four amphetamine salts as Adderall and Adderall XR; its formulation is based on Adderall XR's (2007 review, executive summary). Key studies:
- SHP465-103: 19 healthy adults, single 37.5 mg Mydayis vs Adderall XR 25 mg + mixed amphetamine salts IR 12.5 mg 8 h later, fasting; sampling 0-60 h. BE met for Cmax and AUC of d- and l-amphetamine despite different profiles.
- SHP465-105: food effect, 50 mg, n = 14-16.
- SHP465-107/110: dose escalation 12.5-75 mg, 7 days each, healthy adults.
- SHP465-111: 27-28 patients with ADHD, 6-17 y (children mean 37.03 kg, SD 7.63; adolescents 59.94 kg), single 12.5 mg (children) or 25 mg (adolescents) after an 8-h fast; sampling to 72 h.
- Assay LC-MS/MS, ranges 0.5-75 and 0.1-100 ng/mL.

PopPK: 74 healthy adults + 28 pediatric patients, 2918 concentrations of each isomer; NONMEM 7.3; separate d- and l-amphetamine models of the same structure.

## Model and parameters
Structure (section 4.3): dual zero-/first-order absorption. Two depots (compartments 1 and 3) filled by zero-order inputs of duration D1 and D3, fractions F1 and F3 (F1 + F3 = 1; absolute F unknown), each emptying first-order (Ka1, Ka3) into one central compartment (compartment 2); a separate Ka3 for study 110; food lengthens D1. Linear elimination, apparent CL/F and V/F. Weight was the only covariate retained (allometric).

Final d-amphetamine model (Run 56; Table 4.3.2, PDF p41, image):
```yaml
parameters:
  - {name: CL/F, value: 25.2, unit: L/h, source: "Table 4.3.2", note: "typical, 70 kg; x (WT/70)^0.75; 95% CI 23.1-27.4; IIV %CV 18.7"}
  - {name: CL_female_effect, value: 0.999, unit: ratio, source: "Table 4.3.2"}
  - {name: V/F, value: 419, unit: L, source: "Table 4.3.2", note: "x (WT/70)^1; 95% CI 402-436; IIV %CV 11.1"}
  - {name: V_female_effect, value: 0.936, unit: ratio, source: "Table 4.3.2"}
  - {name: Ka1, value: 0.765, unit: 1/h, source: "Table 4.3.2", note: "IIV %CV 46.7"}
  - {name: Ka3, value: 1.40, unit: 1/h, source: "Table 4.3.2", note: "IIV %CV 118"}
  - {name: Ka3_study110, value: 4.73, unit: 1/h, source: "Table 4.3.2"}
  - {name: D1, value: 0.702, unit: h, source: "Table 4.3.2", note: "fasted; IIV %CV 25.3"}
  - {name: D1_fed, value: 13.6, unit: h, source: "Table 4.3.2"}
  - {name: D3, value: 7.18, unit: h, source: "Table 4.3.2"}
  - {name: F1, value: 0.385, unit: fraction, source: "Table 4.3.2"}
  - {name: F3, value: 0.615, unit: fraction, source: "Table 4.3.2", note: "1 - F1"}
  - {name: sigma_prop, value: 0.0282, unit: variance, source: "Table 4.3.2", note: "%CV 16.8"}
  - {name: sigma_add, value: 0.250, unit: variance, source: "Table 4.3.2", note: "SD 0.500 ng/mL"}
  - {name: CL/F_l, value: 23.7, unit: L/h, source: "section 3.3.6 text", note: "l-amphetamine, 70 kg (Table 4.3.3 itself not rendered)"}
```
(derived: d-amphetamine terminal half-life of the 70 kg typical adult = 0.693 x 419 / 25.2 = 11.5 h.)

Non-compartmental (section 2.1 and Tables 6-8):
- Adults, SHP465 37.5 mg: d-amphetamine Tmax 8.2 h, Cmax 50.3 ng/mL, AUC0-inf 1085 ng.h/mL; t1/2 10.1 h (d) and 12.5 h (l); Adderall XR arm t1/2 9.7 h (d), 11.7 h (l).
- Children 6-12 y, 12.5 mg, d-amphetamine (Table 6, PDF p24): Cmax 29.22 ng/mL (SD 7.26), Tmax 9.30 h (median 8.0), AUC0-inf 651.97 ng.h/mL, CL/F 15.3 L/h (SD 3.81), t1/2 9.91 h, Vz/F 217.0 L (n = 11). Dose basis of CL/F not stated.
- Adolescents 13-17 y, 25 mg, d-amphetamine: Cmax 40.57, AUC0-inf 892.57, CL/F 22.1 L/h, t1/2 11.37 h, Vz/F 353.8 L.
- l-amphetamine, children (Table 7, PDF p25): CL/F 14.9 L/h, t1/2 11.8 h, Vz/F 251.5 L.
- Dose-proportional 12.5-75 mg; accumulation ratio 1.5; steady state day 5-8; protein binding about 40%; high-fat meal delays Tmax about 5 h without changing Cmax or AUC.

## Pharmacodynamics and end points
No EC50 or concentration-effect model is reported in this review. Efficacy end points were PERMP (math test at 2-16 h, Phase 2) and ADHD-RS-IV / CGI-I (Phase 3). The review mentions a "strong concentration-response relationship" only as a reason to take the drug consistently with or without food; no numbers are given. Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Weight on CL/F (exponent 0.75) and V/F (exponent 1), reference 70 kg; sex effects estimated near 1 and judged not clinically relevant; age and sex had no effect after weight.

## Caveats
- Different product (three bead populations, about 16-h design) from Adderall XR (two bead populations); the absorption model (two zero-order-filled depots) is product-specific.
- Absolute bioavailability unknown; all parameters apparent.
- The sponsor's dose basis for CL/F (mg of salts, of total base, or of d-base) is not stated in the review. (derived: 12.5 mg salts x 0.75 = 9.375 mg divided by 652 ng.h/mL = 14.4 L/h, close to the reported 15.3 L/h mean; if so, per d-amphetamine base (12.5 x 0.4749 = 5.94 mg) the children's CL/F would be about 9.1-9.7 L/h and V/F about 137 L. This is an inference, not a reported value.)
- Pediatric single-dose groups small (n = 13-14).

## Used by stanpumpR
- `R/drugs_mixedAmphetamineSalts.R:22-23` - says no published population PK model of Adderall itself exists; the FDA Mydayis (SHP465) analysis is a different product and its input model does not transfer. Mentioned only to explain why it was not used.

## Verification against stanpumpR
- `R/drugs_mixedAmphetamineSalts.R:22-23`: the review confirms Mydayis is a distinct three-bead product (formulation derived from Adderall XR) and that its popPK input is a product-specific dual zero-/first-order depot model, so "input model does not transfer" is supported. **matches**.
- Side observations (not claims made by the repo): the children's NCA here (37 kg, t1/2 d-amphetamine 9.9 h) agrees with the 9 h pediatric half-life stanpumpR takes from the Adderall XR label; the sponsor's weight exponents (0.75 on CL/F, 1 on V/F) differ from the Tsuda 2020 exponents stanpumpR borrows (0.600, 0.776). At 37.8 kg the Mydayis typical d-amphetamine values would be (derived) CL/F 15.9 L/h and V/F 226 L, against stanpumpR's 10.1 L/h and 132 L per mg d-base; the difference is consistent with a different dose basis, which the review does not state. Not a discrepancy, but a possible independent check if the dose basis were confirmed.
