---
file: NJ_dailymed-57f8aac6.pdf
pmid: null
doi: null
citation: "Trandolapril tablets USP (2 mg), repackaged by Bryant Ranch Prepack (ANDA078438). US prescribing information (old non-PLR format), SPL revised 5/2024. DailyMed setid 57f8aac6-3bd3-4ae1-aab2-759c8bc23292; downloaded 2026-10-21."
kind: label
drugs: [trandolapril]
source_read: "text layer, all 21 PDF pages (form-feed chunks = pages). Read Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism, Special Populations, Pharmacodynamics and Clinical Effects (pp1-5), Indications, Warnings and Precautions incl. Pediatric/Geriatric Use (pp5-14), Overdosage and Dosage and Administration (pp18-19), product data (pp19-21). All numbers taken are from clean prose; no tables needed rendering."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Trandolapril tablets (generic, Bryant Ranch) US label, 2024

## Study
This is a generic trandolapril TABLETS label, relabelled by Bryant Ranch Prepack from NDC 57237-090 (ANDA078438),
covering the 2 mg yellow tablet (p19-21). The marketing dates are 12/06/2019 to 05/30/2024 for the 30-count
package. The text describes 1, 2 and 4 mg tablets (p2). The SPL matches its DailyMed setid and is the named
drug. No capsule form appears anywhere in it.

**Dosage (D&A, pp18-19):**
- Hypertension without a diuretic: start at 1 mg once daily (non-black patients) or 2 mg (black patients), and
  adjust at intervals of at least 1 week. Most need 2-4 mg once daily; there is little experience above 8 mg.
  Patients poorly controlled on 4 mg once daily may take it twice daily.
- On a diuretic: stop the diuretic 2-3 days before starting, or start at 0.5 mg.
- Post-MI heart failure or LV dysfunction: start at 1 mg and titrate toward 4 mg once daily.
- CrCl <30 mL/min or hepatic cirrhosis: start at 0.5 mg daily.

## Model and parameters
No compartmental model is given. Descriptors from Pharmacokinetics and Metabolism (pp2-3):
```yaml
parameters:
  - {name: "F (absolute), as trandolapril", value: 0.10, unit: fraction, source: "Pharmacokinetics, p2", note: "about 10%"}
  - {name: "F (absolute), as trandolaprilat", value: 0.70, unit: fraction, source: "Pharmacokinetics, p2", note: "about 70%"}
  - {name: Tmax trandolapril, value: 1, unit: h, source: "p2", note: "fasting, about 1 h"}
  - {name: Tmax trandolaprilat, value: "4-10", unit: h, source: "p2", note: "range"}
  - {name: t1/2 trandolapril, value: 6, unit: h, source: "p2", note: "elimination half-life, about"}
  - {name: effective t1/2 trandolaprilat (steady state), value: 22.5, unit: h, source: "p2", note: "plus a prolonged terminal phase attributed to ACE binding"}
  - {name: V trandolapril, value: 18, unit: L, source: "p3", note: "about"}
  - {name: CL trandolapril (IV about 2 mg), value: 52, unit: L/h, source: "p3", note: "total plasma clearance"}
  - {name: CL trandolaprilat (IV about 2 mg), value: 7, unit: L/h, source: "p3", note: "total plasma clearance"}
  - {name: CLrenal trandolaprilat, value: "1-4", unit: L/h, source: "p3", note: "dose-dependent"}
  - {name: protein binding trandolapril, value: 0.80, unit: fraction, source: "p3", note: "concentration-independent"}
  - {name: protein binding trandolaprilat, value: "0.65-0.94", unit: fraction, source: "p3", note: "65% at 1000 ng/mL to 94% at 0.1 ng/mL (saturable)"}
```
- Excretion: 33% in urine (mostly as trandolaprilat) and 66% in faeces (p3).
- Linearity: Cmax and AUC of trandolapril and Cmax of trandolaprilat are dose-proportional over 1-4 mg. The
  trandolaprilat AUC is less than dose-proportional (p3).
- Food slows trandolapril absorption without changing the trandolaprilat AUC or Cmax (pp2-3).
- Arithmetic check (derived): one compartment with V 18 L and CL 52 L/h implies a trandolapril half-life of
  about 14 min, not the stated about 6 h. The V, CL and t1/2 cannot belong to one compartment. The label does not
  say which phase each describes.

## Pharmacodynamics and end points
- **ACE inhibition (p4):**
  - A single 2 mg dose inhibits plasma ACE by 70-85% at 4 h, with about a 10% decline by 24 h and about half the
    effect still present at 8 days.
  - Maximum inhibition occurs at a plasma trandolaprilat of 2 ng/mL. Inhibition depends on trandolaprilat
    concentration, not on trandolapril.
  - Trandolaprilat is about 8 times more active than trandolapril (p2).
- **Blood pressure (pp4-5):**
  - Dose-response studies in 827 patients at 0.25-16 mg/day. The minimal effective dose was 1 mg (non-black) and
    2 mg (black), with no further response above 4 mg.
  - At 6 weeks, 2-4 mg once daily lowered trough BP by 7-10/4-5 mmHg against placebo (non-black) and 4-6/3-4
    mmHg (black). Trough/peak ratios were 0.5-0.9.
- **TRACE (p5):** n = 876 against 873. Mortality fell 16% (p = 0.042) and heart-failure progression 20%.
- **Not given:** no concentration-BP coefficient. Not an opioid: no MEAC or ke0.

## Covariates
- Renal function: below a CrCl of 30 mL/min, trandolapril and trandolaprilat concentrations about double and
  renal clearance falls about 85% (p3-4).
- Alcoholic cirrhosis: trandolapril 9-fold and trandolaprilat 2-fold higher concentrations (p4).
- Elderly: trandolapril higher; trandolaprilat and ACE inhibition similar (p3).
- Not studied below 18 years (p3).
- No equations.

## Caveats
- This is a repackager generic label, not the innovator (Mavik) label. Its content follows the reference
  labelling.
- The single-compartment inconsistency above means the V, CL and t1/2 cannot be combined naively.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:126` (8baa733): `trandolapril__capsule__oral__catalog__none__label`
  (drug trandolapril, active moiety trandolaprilat, oral, formulation **capsule**, `official_label_descriptive`,
  `descriptive_only`, no stanpumpR drug, source this setid).
  - Limitations: "Prodrug identity only; parent and active-moiety PK and BP are missing."
- The row is generated by `data-raw/antihypertensiveRegistry.R:129-132`.
- `tests/testthat/test-antihypertensive-registry.R:519` lists trandolapril among the ACE inhibitors the registry
  must contain.

## Verification against stanpumpR
- **Drug, prodrug and active moiety (trandolapril → trandolaprilat), oral**: matches pp1-2. **matches**
- **Formulation "capsule"** (model_id and formulation field): the cited document is trandolapril **tablets** USP
  (1, 2 and 4 mg tablets; product data "trandolapril tablet", 2 mg). No capsule is described. **discrepancy**
- **"parent and active-moiety PK and BP are missing"**: the label is not silent. It gives F (10% as parent and
  70% as trandolaprilat), Tmax, half-lives (parent about 6 h, trandolaprilat effective 22.5 h), V 18 L, IV CL
  (52 and 7 L/h), concentration-dependent binding, and placebo-subtracted BP falls. What is missing is a jointly
  estimated, internally consistent vector: V, CL and t1/2 conflict in one compartment, as derived above. There is
  also no concentration-BP coefficient. `descriptive_only` is therefore justified, but "missing" overstates it.
  **discrepancy** (wording; the readiness classification itself is supported).
