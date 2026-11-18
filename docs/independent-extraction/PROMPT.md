You are one of several independent readers transcribing a published pharmacokinetic paper. Your transcription will be compared, number by number, with the others. Accuracy matters more than completeness: a value marked uncertain is worth more than a value guessed.

## Rules

1. **Use only the attached document(s).** Do not use your memory of this paper, other papers, reviews, drug labels, software libraries or model repositories.
2. **Transcribe every number exactly as printed:** the same digits, rounding and units. Never convert, round, recompute or "correct" a value. If you also want to give a value you derived, put it in `notes`, labelled as derived.
3. **Locate every value.** Give its page in the PDF file, and also the printed page number if that differs. Give the table, figure or section it comes from. Quote the exact words or table cells it comes from, at most 15 words.
4. **Report disagreements; do not settle them.** If a value is unreadable or ambiguous, or the paper gives it differently in two places (text and table, abstract and table, equation and table), report every version and say which is which.
5. **Give the uncertainty the paper prints, and what the paper says it is:** SE, RSE %, SD, 95% CI, bootstrap CI or CV %. If the paper prints a ± and never says what it is, write `not stated`.
6. If the paper holds several models (base and final, one per population or formulation, a PK and a PD model), report each, and name it in the `model` column the way the paper does.

## What to extract

A. **Structure of each final model.** The number of compartments. The absorption (first-order, zero-order, transit compartments, lag time, bioavailability). The elimination (linear or saturable). Any effect compartment or metabolite. The units of dose and of concentration, including whether doses are stated as a salt or as the free base.

B. **Every final-model parameter estimate.** This means:
   - the typical values;
   - every covariate coefficient, with its reference value (for example "per 70 kg" or "CrCL/100");
   - every covariate equation, copied exactly as printed.

C. **Between-subject and between-occasion variability, and residual error.**

D. **The population studied.** The number of subjects, and their age, weight, sex, disease and other characteristics. Give the ranges and the summary statistics as printed.

E. **Anything that looks internally inconsistent.** For example, an equation that does not reproduce the table, units that cannot be right, or a typical value that contradicts a figure.

## Output (exactly this, nothing else)

1. One fenced code block in CSV format, with this header row:

```
model,parameter,symbol_as_printed,value_as_printed,units,estimate_type,uncertainty,uncertainty_type,pdf_page,location,quote,notes
```

   - `estimate_type` is one of `typical`, `covariate`, `reference_value`, `iiv`, `iov`, `residual`, `fixed`, `derived`.
   - Put any value containing a comma in double quotes.
   - Write one row per number. A covariate equation with three numbers is three rows; repeat the equation in `quote`.

2. Then these headed sections, in this order:
   - **Structural model**
   - **Covariate equations (verbatim)**
   - **Population**
   - **Inconsistencies**
   - **What I could not read**
