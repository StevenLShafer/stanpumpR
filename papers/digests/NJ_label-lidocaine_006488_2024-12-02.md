---
file: NJ_label-lidocaine_006488_2024-12-02.pdf
pmid: none
doi: none
citation: XYLOCAINE / XYLOCAINE-MPF (lidocaine HCl) and XYLOCAINE WITH EPINEPHRINE injection, for infiltration, nerve block, epidural and caudal. Prescribing information, NDA 006488 (Fresenius Kabi USA), revised 10/2024; FDA action 2024-12-02 (006488Orig1s100lbl). https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/006488Orig1s100lbl.pdf
kind: label
drugs: [lidocaine]
source_read: text layer, partial (Highlights, contents, sections 2 Dosage and Administration incl. 2.6 maximum dosage, 8.4-8.6, 10, 12 Clinical Pharmacology incl. 12.3; pages after the signature page are carton/label images, not read)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Xylocaine label 2024: lidocaine for infiltration and nerve block

## Study
Not a study: US prescribing information for lidocaine HCl 0.5-2% with or without epinephrine 1:100,000 or
1:200,000, for local and regional anesthesia (infiltration, peripheral nerve block, epidural, caudal,
IV regional). No PK study design, population or assay is described; section 12.3 summarises
"diverse formulations, concentrations and usages".

## Model and parameters
No compartmental model. Section 12.3 (label p. 18):
- Absorption: completely absorbed after parenteral administration; rate depends on site and on
  vasoconstrictor. Highest blood levels (apart from intravascular) after intercostal block, lowest after
  subcutaneous injection. Systemic plasma levels do not correlate with local efficacy.
- Distribution: binding is concentration dependent (falls as concentration rises); 60-80% bound at
  1-4 mcg free base/mL; depends on alpha-1-acid glycoprotein. Crosses blood-brain and placental barriers.
- Elimination half-life after IV bolus typically 1.5-2 h.
- Metabolism: hepatic (N-dealkylation to MEGX and GX, ring hydroxylation, amide cleavage, conjugation);
  ~90% excreted as metabolites, <10% unchanged in urine.
- Hepatic dysfunction: half-life may be prolonged two-fold or more. Renal dysfunction: no effect on
  lidocaine kinetics, metabolites may accumulate.

```yaml
parameters:
  - {name: F_parenteral, value: 1, unit: fraction, source: "12.3 Absorption", note: "'completely absorbed' (qualitative, no number given)"}
  - {name: t_half_elim, value: "1.5-2", unit: h, source: "12.3 Elimination", note: after IV bolus, typical}
  - {name: protein_binding, value: "60-80", unit: "%", source: "12.3 Distribution", note: "at 1-4 mcg free base/mL"}
  - {name: fraction_excreted_unchanged, value: "<10", unit: "%", source: "12.3 Excretion"}
  - {name: max_dose_with_epi, value: 7, unit: mg/kg, source: "2.6", note: "not to exceed 500 mg total"}
  - {name: max_dose_plain, value: 4.5, unit: mg/kg, source: "2.6", note: "not to exceed 300 mg total"}
  - {name: max_dose_IVRA_adult, value: 4, unit: mg/kg, source: "2.5"}
  - {name: max_paracervical, value: 200, unit: mg per 90 min, source: "2.6"}
```

Dosing (section 2.6): repeat maximum doses no more often than every 90 min in continuous epidural/caudal use.
Paediatric example: 5-year-old ~23 kg, ~75-100 mg (3.3-4.4 mg/kg); IV regional in children <=3 mg/kg
with 0.25-0.5% solutions. Geriatric and hepatic impairment: reduced doses (8.5, 8.6).

## Pharmacodynamics and end points
- Toxic concentration (12.2): objective adverse manifestations become increasingly apparent above
  6 mcg free base/mL in venous plasma (total, not free fraction; "free base" refers to the base form).
- No therapeutic (analgesic or antiarrhythmic) concentration range, no ke0, no tPeak.
- Toxicity signs listed (5.1): restlessness, anxiety, tinnitus, twitching, drowsiness progressing to
  convulsions and cardiorespiratory depression.

## Covariates
Qualitative only: hepatic impairment (t½ two-fold or more), age (reduced dose), site of injection and
epinephrine (absorption rate). No equations.

## Caveats
- "Completely absorbed" is a summary statement with no data, site, or method; it is not a measured
  absolute bioavailability for any block.
- The 1.5-2 h half-life is a typical range with no population given.
- The label is for infiltration/nerve block products, not IV antiarrhythmic lidocaine.

## Used by stanpumpR
- `R/drugs_lidocaine.R:55-56`: comment says F = 1 for regional anesthesia is an assumption, supported by
  the label's description of complete absorption after parenteral injection (`bioavailability_RA = 1`).

## Verification against stanpumpR
- `R/drugs_lidocaine.R:55-56` and `bioavailability_RA = 1` (line 63): the label states complete absorption
  after parenteral administration (12.3). The code correctly calls F = 1 an assumption and notes no
  site-specific absolute bioavailability has been measured. **matches**.
- Not a cited location, noted for audit: the model's rate constants (k10 0.227, k12 0.636, k21 0.14 /min)
  give a terminal half-life of about 21 min (derived: roots of s² − (k10+k12+k21)s + k10·k21), far shorter
  than the label's 1.5-2 h after IV bolus. The code comment at lines 57-61 already concedes the model
  cannot produce the long tail. (The disposition is Schnider 1996, a lidocaine model for computer-controlled
  infusion in pain therapy; outside this label's scope.)
- The `typical` window 0.5-1.5 mcg/mL (lines 14-16) is well below the label's 6 mcg/mL toxicity
  threshold; the label gives no therapeutic range to compare with.
