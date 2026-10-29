---
file: NJ_dailymed-0fc34cd8.pdf
pmid: ""
doi: ""
citation: "ALTACE (ramipril) capsules, for oral use. Prescribing information, Pfizer Laboratories Div Pfizer Inc. DailyMed SPL setid 0fc34cd8-86e6-4034-73bd-4263a68ba046; revised 5/2026; retrieved 2026-10-21."
kind: label
drugs: [ramipril]   # not a stanpumpR drug model; antihypertensive registry catalogue row only
source_read: "text layer, all 32 PDF pages: Highlights and contents (p1-3), 1 Indications (p3-5), 2 Dosage and Administration incl. 2.5 adjustment (p5-6), 5 Warnings (p7-15), 8 Use in Specific Populations (p15-16), 10 Overdosage (p16), 12.1-12.3 Clinical Pharmacology (p17-19), 14 Clinical Studies (p20-25), 16 How Supplied (p25); page-to-text mapping checked with pypdfium2 for p6, p18, p19. No PK table in the label"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Altace (ramipril) prescribing information, DailyMed SPL 0fc34cd8

## Study
US label for ramipril capsules 1.25, 2.5, 5, 10 mg (boxed warning: fetal toxicity). Indications: hypertension;
reduction of MI, stroke and CV death in patients 55 and over at high risk; heart failure after MI.
Dosing (2.1-2.5, PDF p5-6): hypertension 2.5 mg once daily initially (no diuretic), maintenance 2.5-20 mg/day once or in
two doses; risk reduction 2.5 mg x 1 week, 5 mg x 3 weeks, then 10 mg daily; post-MI heart failure 2.5 mg twice daily,
target 5 mg twice daily. Creatinine clearance 40 mL/min or below: 25% of the usual dose; 1.25 mg once daily initially,
maximum 5 mg/day (hypertension). Capsule may be opened onto applesauce or into water or apple juice.

## Model and parameters
12.3 Pharmacokinetics (PDF p18-19), descriptive only:
- Ramipril Cmax within 1 h; absorption at least 50-60%, extent not affected by food (rate reduced).
- Hydrolysed (mainly hepatic) to ramiprilat, about 6 times as active; ramiprilat peaks 2-4 h after the dose.
- Absolute bioavailability (5 mg oral vs IV): ramipril 28%, ramiprilat 44%.
- Protein binding: ramipril about 73%, ramiprilat about 56%.
- Ramiprilat declines triphasically: initial t1/2 2-4 h (distribution and ACE binding), apparent elimination t1/2
  9-18 h (free drug), terminal over 50 h (ACE complex dissociation, not contributing to accumulation). After 5-10 mg
  daily the half-life within the therapeutic range is 13-17 h. Steady state by the fourth daily dose.
- Plasma ramipril and ramiprilat rise with dose but not strictly proportionally; ramiprilat 24-h AUC dose-proportional
  over 2.5-20 mg.
- Excretion: about 60% urine, 40% faeces; under 2% unchanged ramipril in urine.

```yaml
parameters:
  - {name: tmax_ramiprilat, value: "2-4", unit: h, source: "12.3, PDF p18", note: "after oral ramipril"}
  - {name: tmax_ramipril, value: 1, unit: h, source: "12.3, PDF p18", note: "within 1 h"}
  - {name: t_half_effective_ramiprilat, value: "13-17", unit: h, source: "12.3, PDF p19", note: "multiple 5-10 mg daily doses"}
  - {name: t_half_apparent_elimination, value: "9-18", unit: h, source: "12.3, PDF p19", note: "free ramiprilat"}
  - {name: t_half_terminal, value: ">50", unit: h, source: "12.3, PDF p19", note: "ACE-bound ramiprilat"}
  - {name: F_ramipril, value: 0.28, unit: fraction, source: "12.3, PDF p18", note: "absolute, 5 mg"}
  - {name: F_ramiprilat, value: 0.44, unit: fraction, source: "12.3, PDF p18", note: "absolute, 5 mg"}
```

## Pharmacodynamics and end points
- 12.2 (PDF p18): single 2.5-20 mg doses inhibit ACE activity about 60-80% at 4 h and 40-60% at 24 h; multiple doses
  of 2.0 mg or more over 90% at 4 h and over 80% at 24 h.
- 14.1 (PDF p20): single 5-20 mg doses lower BP within 1-2 h, peak 3-6 h, lasting 24 h; trough about 50-60% of peak;
  long-term 2.5-10 mg daily about 6/4 mmHg more than placebo at 24 h; smaller response in Black patients.
- No concentration-effect parameters.

## Covariates
Renal (8.6, PDF p16): single 10 mg dose, ramiprilat AUC0-24 about 1.7, 3.0 and 3.2 times normal in mild (40-80),
moderate (15-40) and severe (<15 mL/min) impairment; CrCl < 40 mL/min/1.73 m2 doubles peak and up to quintuples trough.
Hepatic: ramipril about 3-fold higher, ramiprilat peak unchanged. Elderly: higher ramiprilat peak and AUC (8.5).

## Caveats
Ranges and "about" values only; non-linear, ACE-binding kinetics that a linear compartment model cannot reproduce
over the full time course.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:112`: ramipril catalogue row, `official_label_descriptive`,
  `descriptive_only`: "Label active Tmax 2-4 h, effective half-life 13-17 h".

## Verification against stanpumpR
- Row 112: oral capsule, active moiety ramiprilat, analyte ramipril and ramiprilat, plasma; Tmax 2-4 h (ramiprilat
  peak, 12.3) and effective half-life 13-17 h (12.3): **matches**. "BP coefficients are missing": correct, the label
  gives none. The PBPK statement in the row is not from this label (not checked here).
