---
file: NJ_label-mirtazapine_020415_s023s024.pdf
pmid: none
doi: none
citation: "Organon / Schering Corporation. REMERON (mirtazapine) Tablets: prescribing information (pre-PLR format) and Medication Guide. NDA 020415, supplements S-023 and S-024 (FDA file 020415s023s024.pdf); revision printed as Rev. X/XX, copyright 1996, 2007. Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [mirtazapine]
source_read: "text layer, all 29 PDF pages (prescribing information printed pp1-21 = PDF pp1-21; Medication Guide printed pp1-8 = PDF pp22-29): boxed warning and Description p1; Clinical Pharmacology pp1-4 (Pharmacokinetics p2, Special Populations pp2-3); Indications and Contraindications p4; Warnings pp5-7; Precautions pp7-14 (renal and hepatic p9, Drug Interactions pp11-12, Pregnancy and Nursing p13, Pediatric pp13-14, Geriatric p14); Adverse Reactions from p14 (through drug abuse, p19); Overdosage p19; Dosage and Administration p20; How Supplied pp20-21. Page image of p2 (Pharmacokinetics) checked."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: matches
---

# FDA (about 2007-2009): Remeron (mirtazapine) tablets prescribing information, NDA 020415 S-023/S-024

The full Remeron prescribing information the archive lacked: it replaces, as evidence, the carton-label file
`NJ_label-mirtazapine_020415_2026-05-21`. The context title mentions RemeronSolTab, but this file covers REMERON
Tablets (15, 30, 45 mg) only; no orally disintegrating product appears in it. Distributed by Schering Corporation,
manufactured by N.V. Organon.

## Study
Not a study. Indication (p4): major depressive disorder, established in four 6-week placebo-controlled trials in adult
outpatients (titration 5 to 35 mg/day; completers' mean 21 to 32 mg/day) plus a relapse-prevention trial (p3).

## Model and parameters
No compartmental model. Pharmacokinetics (p2, page image) and Special Populations (pp2-3), as printed:

```yaml
parameters:
  - {name: F_absolute, value: 50, unit: "%", source: "Pharmacokinetics, p2", note: "absolute bioavailability about 50%; the same paragraph calls absorption rapid and complete"}
  - {name: t_half, value: "20-40", unit: h, source: "Pharmacokinetics p2 (twice); Gender p3; Dosage and Administration p20", note: "mean elimination half-life range across age and sex subgroups"}
  - {name: t_half_female, value: 37, unit: h, source: "p2; p3", note: "mean; significantly longer than males"}
  - {name: t_half_male, value: 26, unit: h, source: "p2; p3", note: mean}
  - {name: tmax, value: 2, unit: h, source: "p2", note: "within about 2 h"}
  - {name: accumulation_ratio, value: 1.5, unit: "", source: "p2", note: "steady state within 5 days, about 50% accumulation; dosing interval not stated"}
  - {name: protein_binding, value: 85, unit: "%", source: "p2", note: "0.01-10 mcg/mL"}
  - {name: linear_dose_range, value: "15-80", unit: mg, source: "p2"}
  - {name: excretion_urine, value: 75, unit: "%", source: "p2", note: "15% in feces"}
  - {name: CL_elderly_male_change, value: -40, unit: "%", source: "Geriatric, p2", note: "oral clearance vs younger males; females -10%; 20 mg/day for 7 days, ages 25-74"}
  - {name: CL_moderate_renal_change, value: -30, unit: "%", source: "Renal Insufficiency, p3", note: "total body clearance; Clcr 11-39 mL/min/1.73 m2"}
  - {name: CL_severe_renal_change, value: -50, unit: "%", source: "Renal Insufficiency, p3", note: "Clcr below 10 mL/min/1.73 m2"}
  - {name: CL_hepatic_change, value: -30, unit: "%", source: "Hepatic Insufficiency, p3", note: "oral clearance, single 15 mg dose"}
  - {name: dose_range, value: "15-45", unit: mg/day, source: "Dosage and Administration, p20", note: "start 15 mg at bedtime; change no faster than every 1-2 weeks"}
```
Metabolism (p2): demethylation and hydroxylation then glucuronidation; CYP2D6 and 1A2 form the 8-hydroxy metabolite,
CYP3A the N-desmethyl and N-oxide; active metabolites present at very low levels. The (-) enantiomer's half-life is
about twice the (+) enantiomer's, giving about three times its plasma levels. Food has minimal effect. Interactions
(pp11-12): phenytoin and carbamazepine each about double mirtazapine clearance (concentrations down 45% and 60%);
cimetidine raises AUC by more than 50%; ketoconazole raises Cmax about 40% and AUC about 50%.

## Pharmacodynamics and end points
No concentration-effect data, therapeutic range, ke0 or potency ratio. Receptor profile only (pp1-2): central alpha2
antagonist; potent 5-HT2, 5-HT3 and H1 antagonist; moderate peripheral alpha1 and muscarinic antagonist. Somnolence 54%
vs placebo 18% in US trials (p8). Not an opioid.

## Covariates
Sex (female half-life longer), age (lower oral clearance, mainly in men), renal function (by creatinine clearance) and
hepatic impairment, as in the table; no dose adjustment is specified for any, only caution (pp3, 9, 14, 20). Race not
studied. Pediatric safety and efficacy not established; two pediatric MDD trials (n = 258) did not support use (pp13-14).

## Caveats
- Undated (Rev. X/XX), an older edition; current Remeron labels were not compared.
- Mean values without n or variability for most statements.
- (derived) For once-daily dosing, an accumulation ratio of 1.5 corresponds to an effective half-life near 15 h
  (1 / (1 - exp(-k x 24)) = 1.5 gives k = 0.0458 /h); a 20 to 40 h half-life would give about 1.8 to 2.9. The label
  prints both and does not reconcile them; the dosing interval behind the 1.5 is not stated.

## Used by stanpumpR
Locations found by content at 408eee2:
- `R/drugs_mirtazapine.R:50-51` (context :51): CL and V are apparent, divided by an unmeasured F of about 50% by the
  label.
- `R/drugs_mirtazapine.R:62`: model half-life ln 2 x 310 / 28.9 = 7.4 h against the label's 20 to 40 h.
- `inst/help/drugs/mirtazapine.md:17`: the same contrast, called a trough-data artefact.
- `inst/help/drugs/mirtazapine.md:21` (not in the context): bioavailability about 50%.
- `tests/testthat/test-drugs-mirtazapine.R:129`: test name, half-life 7.4 h far below the label's 20 to 40 h.
- `inst/help/models/antidepressants.md:23` (context :22): mirtazapine about 7 h against the label's 20 to 40.

## Verification against stanpumpR
- F about 50% (`R/drugs_mirtazapine.R:50-51`, `mirtazapine.md:21`): **matches**; the label gives absolute
  bioavailability about 50% (p2).
- Label half-life 20 to 40 h (`R/drugs_mirtazapine.R:62`, `mirtazapine.md:17`, `antidepressants.md:23`, test name at
  `test-drugs-mirtazapine.R:129`): **matches** (p2, p3, p20). (derived) With the model's CL/F 28.9 L/h, a 20 to 40 h
  half-life would need V/F of about 830 to 1670 L, against the model's 310 L, consistent with the repo's statement
  that the volume, not the clearance, is what the trough data fail to identify.
- The repo still says "the label" without naming it; this card identifies a matching one (Remeron, NDA 020415,
  S-023/S-024). The label's own accumulation ratio (see Caveats) is not used by the repo.
