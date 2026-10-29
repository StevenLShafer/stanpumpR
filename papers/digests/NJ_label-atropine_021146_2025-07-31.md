---
file: NJ_label-atropine_021146_2025-07-31.pdf
pmid: null
doi: null
citation: "Hospira, Inc. Atropine Sulfate Injection, USP (0.05 and 0.1 mg/mL syringes), for intravenous use. US prescribing information, NDA 021146 (supplement s037), Revised 7/2025, Reference ID 5634591. https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/021146s037lbl.pdf"
kind: label
drugs: [atropine]
source_read: "text layer, all 8 pages (Highlights, sections 1-16); page images rendered and read for PDF pages 1 (Highlights, revision date), 2 (Table 1 dosing, 2.3-2.4, 5.1) and 7 (12.3 Pharmacokinetics, 16 How Supplied)"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Hospira 2025: Atropine Sulfate Injection US label (NDA 021146)

## Study
Not a study: the FDA-approved prescribing information (PLR format) for Hospira's atropine sulfate injection in
prefilled syringes, initial US approval 1960, revised 7/2025 (PDF p1, Highlights). It contains no pharmacokinetic
table, no concentration-time data and no study populations beyond literature summaries (8.1 cites observational
pregnancy cohorts: 401 first-trimester and 797 second/third-trimester pregnancies; 381 exposed newborns with 18
major defects vs 16 expected; 50 first-trimester pregnancies; PDF p4).

Dosing (Table 1, PDF p2, confirmed on the rendered page; adults, IV):

| Use | Dose (adults) | Repeat |
|---|---|---|
| Antisialagogue or other antivagal | 0.5 to 1 mg | 1-2 hours |
| Organophosphorus or muscarinic mushroom poisoning | 2 to 3 mg | 20-30 minutes |
| Bradyasystolic cardiac arrest | 1 mg | 3-5 minutes; 3 mg maximum total dose |

- Titrate on heart rate, PR interval, blood pressure and symptoms (2.1, PDF p2).
- Pediatric: not well studied; usual initial dose 0.01 to 0.03 mg/kg (2.3, PDF p2).
- Coronary artery disease: limit total dose to 0.03 to 0.04 mg/kg (2.4); 5.1 restates this as 2 to 3 mg total
  (maximum 0.03 to 0.04 mg/kg) (PDF p2).
- Strengths: 0.05 mg/mL (0.25 mg/5 mL) and 0.1 mg/mL (1 mg/10 mL, 0.5 mg/5 mL) (section 3, PDF p2; section 16,
  PDF p7). Each mL of 0.1 mg/mL atropine sulfate monohydrate equals 0.083 mg atropine base; 0.05 mg/mL equals
  0.042 mg base (section 11, PDF p5, text layer). The label doses are expressed as atropine sulfate.
- Overdose (section 10, PDF p4-5): fatal adult dose not known; in pediatric populations 10 mg or less may be fatal;
  physostigmine 1 to 4 mg IV (0.5 to 1 mg pediatric) as antidote; not removed by dialysis.

## Model and parameters
No compartmental model and no numeric clearance, volume or half-life. Section 12.3 (PDF p7, rendered page read)
gives only qualitative/semi-quantitative facts:
- PK is nonlinear after IV 0.5 to 4 mg.
- Plasma protein binding about 44%, saturable in the 2-20 μg/mL concentration range.
- 13 to 50% excreted unchanged in urine; much is destroyed by enzymatic hydrolysis, mainly hepatic; metabolites
  noratropine, atropin-n-oxide, tropine, tropic acid; metabolism inhibited by organophosphate pesticides.
- Exercise before/after IM dosing increases absorption and decreases clearance.
- Crosses placenta; not found in amniotic fluid; traces in milk.

```yaml
parameters:
  - {name: protein binding, value: 44, unit: "%", source: "12.3, PDF p7", note: "about 44%; saturable over 2-20 ug/mL"}
  - {name: fraction excreted unchanged in urine, value: "13-50", unit: "%", source: "12.3, PDF p7", note: range}
  - {name: dose range with nonlinear PK, value: "0.5-4", unit: mg IV, source: "12.3, PDF p7", note: qualitative statement only}
  # no clearance, volume, half-life value or therapeutic concentration is given in this label
```

## Pharmacodynamics and end points
- Section 12.2 (PDF p6, text layer): after IV dosing (rapid constant infusion over 3 minutes), peak heart-rate
  effect and minimum saliva flow lag drug administration by 7 to 8 minutes, and both are non-linearly related to the
  amount in the peripheral compartment. After IM 0.5 to 4 mg, plasma atropine and heart rate time courses closely
  overlap; for behavioural (CNS) impairment, PK is said not to be the primary rate-limiting step. No ke0, t½ke0 or
  EC50 value is given.
- Low doses may transiently slow heart rate before tachycardia (12.2).
- No therapeutic or toxic plasma concentration range is stated anywhere in the label. The only concentration
  figure (2-20 μg/mL) concerns protein-binding saturation, not efficacy.
- Not an opioid: no MEAC or potency ratio applies.

## Covariates
Elimination half-life more than doubled in children under 2 years and in the elderly (>65 years) versus other
ages (12.3, PDF p7; restated in 8.2). No gender effect on PK or on heart-rate PD (12.3). No equations.

## Caveats
- Label, not primary data; the PK/PD statements are unreferenced literature summaries.
- Doses are in atropine sulfate; atropine base is about 83% of the sulfate monohydrate mass (section 11), which
  matters if a concentration in ng/mL of base were ever paired with these doses.
- Pediatric recommendations are not trial-based (8.4).

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv` (context gives line 199; the atropine row is at line 200 of the snapshot,
  line 199 being vasopressin): registry source for the atropine row, which records no therapeutic concentration
  range (band_status none_established, lower/upper/typical 0, units ng/mL, plasma), citing this label (Hospira,
  rev. 07/2025).

## Verification against stanpumpR
Checked in `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- `inst/extdata/therapeuticRanges.csv:200` (atropine row): source text says no therapeutic concentration range;
  titrated to effect; label read: atropine, Hospira, rev. 07/2025. The label (rendered PDF p1) is Hospira's and
  is revised 7/2025; section 2.1 (rendered PDF p2) says to titrate on heart rate, PR interval, blood pressure and
  symptoms; no section states a therapeutic or toxic concentration range (all 8 pages read in the text layer).
  `none_established` with 0/0/0 is therefore supported. **matches**.
- The units column (ng/mL) and matrix (plasma) are placeholders for an empty band; the label offers no basis for or
  against them, and no number is plotted. **not applicable**.
- Minor: the context file's location (`:199`) points one line above the atropine row (line 199 is vasopressin);
  this is a context-file offset (possibly a header-row count), not a repository error.
