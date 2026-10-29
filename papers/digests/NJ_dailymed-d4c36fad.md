---
file: NJ_dailymed-d4c36fad.pdf
pmid: null
doi: null
citation: "Pfizer Laboratories Div Pfizer Inc. CEREBYX (fosphenytoin sodium) injection, for intravenous or intramuscular use. US prescribing information, Revised 8/2024 (initial US approval 1996). DailyMed SPL setid d4c36fad-0ba2-4cd4-9c5e-dcf843f38a5a; archived 2026-10-21."
kind: label
drugs: [phenytoin]
source_read: "text layer, 39 PDF pages: Highlights and contents p1-3; 1 Indications and 2 Dosage and Administration p4-8 (2.5 monitoring p7, 2.6 substitution and 2.7-2.9 p8); 8 Use in Specific Populations p21-23; 10 Overdosage and 11 Description p24-25; 12.1-12.3 Clinical Pharmacology p25-28; 12.5 Pharmacogenomics p29. Warnings, adverse reactions, interactions and patient information skimmed. Pages 25, 26 and 27 (12.3 and Figure 1) checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Cerebyx (fosphenytoin) US label, Revised 8/2024

## Study
Not a study: the PLR-format US label of Cerebyx, Pfizer, 2 mL (100 mg PE) and 10 mL (500 mg PE) vials at
50 mg PE/mL, for IV or IM use (11 Description, p24-25). Indications: generalized tonic-clonic status epilepticus,
prevention and treatment of seizures during neurosurgery, and short-term substitution for oral phenytoin (1, p4).
The only PK study described with an n is Figure 1 (p27): healthy subjects, N = 12, 1200 mg PE Cerebyx at 100 or
150 mg PE/min versus 1200 mg Dilantin at 50 mg/min, unbound phenytoin sampled to 96 h. No assay, model or software
is named; 2.5 (p7) warns that immunoassays (TDx, Emit) overestimate phenytoin before conversion is complete and that
chromatographic assays and EDTA tubes are needed.

## Model and parameters
No population model. Label values (12.3, p25-28, unless stated):

Dose basis (2.1 p4; 11 p24): every dose, concentration and rate is in phenytoin sodium equivalents (PE); 1.5 mg of
fosphenytoin sodium is 1 mg PE; one mmol of fosphenytoin yields one mmol of phenytoin (p26). MW of fosphenytoin
406.24 (p25).

Fosphenytoin:
- IV: peak at the end of infusion. IM: completely bioavailable, peak about 30 min postdose (p25).
- Protein binding 95-99%, saturable; displaces phenytoin. V 4.3 to 10.8 L, rising with dose and rate (p26).
- Conversion half-life to phenytoin about 15 min (p26, and again p27). Not excreted in urine (p26).

Phenytoin after Cerebyx:
- Complete conversion after IV and IM; after IM, total phenytoin peaks at about 3 h (p27).
- 2.6 (p8): phenytoin from Cerebyx is 100% bioavailable IM and IV; Dilantin capsules about 90% orally.
- Unbound fraction about 12% without fosphenytoin, up to 30% during conversion (about 0.5-1 h after infusion) (p27).
- Mean total phenytoin half-life 12.0 to 28.9 h, longer at higher concentrations (p28).
- Single IV doses of 400-1200 mg PE: peak total phenytoin proportional to dose and little affected by rate; peak
  unbound rises with dose and rate; AUC (total and unbound) rises more than in proportion to dose, saturable
  metabolism, CYP2C9 mainly and CYP2C19 less (p27-28).
- Urine: mainly 5-(p-hydroxyphenyl)-5-phenylhydantoin and its glucuronide; 1-5% of the dose unchanged (p28).

```yaml
parameters:
  - {name: conversion_t_half, value: 15, unit: min, source: "12.3 Elimination and Absorption, PDF p26-27", note: "approximately; fosphenytoin to phenytoin"}
  - {name: fosphenytoin_per_PE, value: 1.5, unit: "mg fosphenytoin sodium per mg PE", source: "11 Description, PDF p24", note: "definition of PE"}
  - {name: F_IM_fosphenytoin, value: 1.0, unit: fraction, source: "12.3, PDF p25", note: "completely bioavailable"}
  - {name: tmax_IM_fosphenytoin, value: 30, unit: min, source: "12.3, PDF p25", note: approximate}
  - {name: tmax_IM_total_phenytoin, value: 3, unit: h, source: "12.3, PDF p27", note: approximate}
  - {name: V_fosphenytoin, value: "4.3-10.8", unit: L, source: "12.3, PDF p26", note: "range; rises with dose and rate"}
  - {name: fu_phenytoin, value: 0.12, unit: fraction, source: "12.3, PDF p27", note: "up to 0.30 during conversion"}
  - {name: t_half_total_phenytoin, value: "12.0-28.9", unit: h, source: "12.3, PDF p28", note: "range of means after 400-1200 mg PE"}
  - {name: F_oral_Dilantin_capsule, value: 0.9, unit: fraction, source: "2.6, PDF p8", note: approximate}
  - {name: therapeutic_total, value: "10-20", unit: mcg/mL, source: "2.5, PDF p7", note: "unbound 1-2 mcg/mL"}
```

## Pharmacodynamics and end points
No concentration-effect model or ke0. Therapeutic range total 10-20 mcg/mL, unbound 1-2 mcg/mL (2.5, p7; 8.7,
p23). Toxicity (10, p24): lateral gaze nystagmus usually at 20 µg/mL, ataxia at 30, dysarthria and lethargy above 40;
50 µg/mL reported without toxicity; adult lethal dose estimated 2-5 g. Maximal serum phenytoin about 10-20 min after
the end of an IV infusion (2.3, p5). Infusion rate limit 150 mg PE/min adults; 2 mg PE/kg/min or 150 mg PE/min,
whichever is slower, in children (boxed warning, 2.3). An infusion of 15-20 mg PE/kg at 100-150 mg PE/min gives free
phenytoin close to phenytoin sodium at 50 mg/min (p26).

Dosing (Tables 1-3, p5-6): status epilepticus load 15-20 mg PE/kg (adult and pediatric); non-emergent load 10-20
mg PE/kg adult, 10-15 pediatric; adult maintenance 4-6 mg PE/kg/day; pediatric 2-4 mg PE/kg 12 h after the load,
then 4-8 mg PE/kg/day. IM ordinarily not for status epilepticus or children.

## Covariates
Age: fosphenytoin PK unaffected over 5-98 years; phenytoin clearance 20% lower over 70 than at 20-30 years (p28).
Sex and race: no effect. Renal or hepatic disease and hypoalbuminemia: higher unbound fraction, conversion may be
faster without faster phenytoin clearance (2.7, 8.6). Pregnancy: phenytoin clearance rises, peaking in the third
trimester (p28). CYP2C9: *1/*3 and *2/*2 intermediate, *2/*3 and *3/*3 poor metabolisers have lower clearance; poor
phenotype 2-3% in White populations (12.5, p29). No equations.

## Caveats
- A label: ranges and "approximately" values, one healthy-volunteer figure, no population or covariate model.
- The IM fosphenytoin absorption rate is not given, only the 30 min fosphenytoin peak and 3 h phenytoin peak.
- Half-lives of phenytoin depend on concentration, so the 12.0-28.9 h range is dose-specific.

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:17` (at 8baa733): moiety Fosphenytoin, routes "IV/IM prodrug (phenytoin
  sodium equivalents)", `US_SEIZURE_LABELED`, `published_population_fit`, dose basis "fosphenytoin (prodrug, PE)",
  analyte total plasma phenytoin, model "conversion compartment feeding phenytoin MM", `stanpumpr_drug` phenytoin,
  this setid as source; notes "Implemented as a route of phenytoin (mg PE), 15-min conversion half-life, IM ka
  2.47/h." Built at `data-raw/antiseizureRegistry.R:128-132`.
- Related (not this label's row): `R/drugs_phenytoin.R:96-106` (fosphenytoin block, citing the Cerebyx label and
  Boucher 1989), `:332-333` (`kConversion = log(2) / 15`, `ka_IM = 2.47 / 60`), `:77-82` (PE dose basis);
  `inst/extdata/antiseizureParameters.csv:8` (conversion half-life 15 min, label-derived); `inst/help/drugs/phenytoin.md:11`.

## Verification against stanpumpR
- Conversion half-life: code `log(2) / 15` per min (`R/drugs_phenytoin.R:332`), parameters CSV 15 min
  "label-derived"; label about 15 min (p26-27). **matches**.
- PE basis: code comment "1.5 mg of fosphenytoin sodium is 1 mg PE" (`R/drugs_phenytoin.R:81-82`) and PE doses
  multiplied by 252.27/274.25 = 0.9199 to phenytoin acid (`:77`, `:241`); label defines PE as phenytoin sodium
  equivalents, 1.5 mg fosphenytoin sodium = 1 mg PE, 1:1 molar (p24, p26). The 0.92 acid/sodium factor is the
  molecular-weight ratio, consistent with PE meaning mg of phenytoin sodium. **matches**.
- IM complete (`bioavailability_IM = 1`, `:334`): label completely bioavailable (p25). **matches**.
- IM ka 2.47/h: not in the label (the code attributes it to Boucher 1989). As a check against the label (derived:
  Euler simulation of the code's structure, 70 kg without the FFM adjustment, V 65.4 L, Vmax and Km as at
  `:279-282`, 1000 mg PE IM): conversion compartment peaks at 23 min (label fosphenytoin peak about 30 min) and total
  phenytoin at 2.8 h (label about 3 h). **consistent**.
- Analyte total plasma phenytoin, band 10-20 (`R/drugs_phenytoin.R:324-326`, `lowerTypical 10`, `upperTypical 20`
  mg/L): label total 10-20 mcg/mL (p7). **matches**.
- Registry status `US_SEIZURE_LABELED`: indications include status epilepticus and neurosurgical seizures (p4).
  **matches**. The unmodelled early displacement (code `:104-106`, help page) is the label's up-to-30% unbound
  during conversion (p27); the omission is stated in the repo.
- Not used by the repo: the label's 90% oral bioavailability of Dilantin capsules (2.6, p8). The model gives the ER
  capsule F = 1 "on Odani's scale" (`R/drugs_phenytoin.R:89`), a deliberate choice since Odani's Vmax was fitted
  assuming F = 1; noted, not a discrepancy.
