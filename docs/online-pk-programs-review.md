# Other pharmacokinetic programs: what stanpumpR can learn from them

Survey of 2026-11-06. It covers three groups: anesthesia TCI simulators, open-source PK engines and their model libraries, and clinical dosing / therapeutic-drug-monitoring (TDM) calculators. Section 5 adds a pass through the bio.tools registry. For each it records the drugs offered, how the code computes where the code could be read, and what stanpumpR might adopt.

**How it was done, and what that limits.** The session's network reached GitHub and opentci.org. Almost every other host was refused: eurosiva.org (TivaTrainer), clincalc.com, tdmx.eu, doseme-rx.com, draeger.com, the App Store, simtiva.app and PubMed Central. So:

- Closed products are described from search-result text, not their own pages.
- Open-source projects were shallow-cloned and **read, never run**.

Most of this was gathered by research agents. The external code findings were re-checked against the cloned source (marked **verified**). So were the claims about stanpumpR's own code and the near-tie error measurement. Everything else is a **lead**, to be checked against the program or the paper before anything changes. That follows the rule in `AGENTS.md` on external LLM reviews.

**Licences matter before code does.** stanpumpR is MIT.

- **Copyleft, so their code cannot be copied into the package:** mrgsolve (GPL >= 2), rxode2 and rxode2lincmt (GPL >= 3), linpk (GPL-3), pharmsol/Pmetrics (GPL-3), nlmixr2lib (GPL >= 2), the R package `tci` (GPL-2), SimTIVA (GPL-3), the Python Anesthesia Simulator (GPL-3), VancoSim (GPL-2), Tucuxi, posologyr and PKNCA (AGPL-3).
- **MIT, so code may be borrowed with attribution:** InsightRX's PKPDsim, PKPDmap and clinPK, opentiva, PyTCI, and vitaldb/tivatci.
- **Copyleft code can still serve as a test oracle** in `Suggests`, or have its mathematics re-derived.

## 1. Anesthesia simulators and TCI tools

### Open source (code read)

**SimTIVA**
- Source: github.com/luktinghin/simtiva. GPL-3, JavaScript progressive web app, active in 2025.
- A direct port of STANPUMP: `cube()` and a per-second recursion in `pharmacology.js`.
- Models:
  - Propofol: Marsh, Schnider, Paedfusor, Eleveld (opioid switch, optional postmenstrual age).
  - Remifentanil: Minto, Eleveld.
  - Fentanyl: Shafer. Alfentanil: Maitre.
  - Dexmedetomidine: Hannivoort 2015.
  - Ketamine: Kamp 2020, with a ke0 from Navarrete 2025.
- Features stanpumpR lacks:
  - An **infusion scheme** that turns TCI targets into manual bolus and rate steps for an ordinary syringe pump.
  - An **RSI planner**: target, time to reach it and bolus speed, with the predicted Ce overshoot.
  - A bolus-speed limit.
  - An **"Infusulator"** that rebuilds Cp/Ce from a retrospective list of TCI targets.
  - A probability-of-tolerance-of-laryngoscopy isobologram.
- Its Eleveld code (`pharmacology.js`, about lines 5762-5900) agreed with the paper and with stanpumpR on the points the agent checked.

**opentiva**
- Source: github.com/opentiva/opentiva. MIT, Python/Cython.
- The broadest anesthesia library found:
  - Propofol: 8 models.
  - Remifentanil: 4.
  - Fentanyl, sufentanil and alfentanil: 4, 2 and 3.
  - Dexmedetomidine: 4.
  - Ketamine: 6.
  - Remimazolam: 2.
  - Neuromuscular blockers: rocuronium, vecuronium, atracurium, cisatracurium.
  - Others: etomidate, thiopental, midazolam.
- **Every model declares the age, weight and BMI range it was studied in, and warns outside it** (`validate_anthropometric_values` in `model.py`).
- Its effect-site TCI can cap plasma overshoot (`cp_limit`, default 1.2 x target).
- Maximum rate is in mL/h, with infusions shorter than `bolus_time` exempt.
- Its effect-site integration is linear-up/log-down with no guard for slope = -ke0. stanpumpR's `calculateCe()` has one.
- **Two bugs, verified** (see 1.3).

**R package `tci`**
- Source: github.com/jarretrt/tci (CRAN 0.2.1, GPL-2, last commit January 2023).
- Rcpp closed forms. Models: Marsh, Schnider, Eleveld propofol PK and BIS, Minto, Kim, Eleveld remifentanil.
- Distinctive feature: **Bayesian closed-loop control**. `bayes_update()`, `clc()` and `simulate_clc()` in `R/simulate.R` make MAP updates of the PK/PD parameters from noisy BIS and re-target.
- Effect-site TCI hands over to plasma targeting near the target (`tci_effect()`).
- **One bug, verified** (see 1.3).

**Python Anesthesia Simulator (PAS)**
- Source: github.com/AnesthesiaSimulation/Python_Anesthesia_Simulator. GPL-3, published in JOSS.
- Computes with a discretised state space at 1 s.
- Models:
  - Propofol: Schnider, Marsh, Schüttler, Eleveld.
  - Remifentanil: Minto, Eleveld.
  - Norepinephrine: Beloeil, Oualha, Li 2024.
  - Rocuronium and atracurium.
- Pharmacodynamics: BIS (Bouillon 2004, Eleveld with an age-dependent delay) and loss-of-consciousness and laryngoscopy response surfaces (Kern 2004, Mertens 2003, Johnson 2008, Bouillon).
- Simulation features:
  - Inter-individual variability sampled from the published omegas.
  - Blood loss and shock.
  - Surgical-stimulus disturbances.
- Uses uncapped James lean body mass, which stanpumpR deliberately caps (`lbmJamesCapped()`).
- **One bug, verified** (see 1.3).

**Smaller projects**
- **vitaldb/tivatci** (MIT): about 20 propofol, remifentanil, remimazolam, rocuronium, sufentanil and dexmedetomidine models, with a STANPUMP-style peak search and a rate clamp.
- **PyTCI** (MIT): forward Euler at 1 s. Its effect-site bolus assumes a fixed 90 s time to peak.
- **ysuzuki1978**'s remimazolam simulator (Masui 2022, web): its changelog records 2.6-4% Ce errors from Euler integration before an RK4 fix. That is a reminder of what stanpumpR's closed forms avoid.
- **DMEPUMP** (licence not stated).
- **fra-mar/tci_simulator** (GPL-3).

**opentci.org**
- Hosts STANPUMP, STELPUMP, IVA-SIM and MATLAB ports.
- Also hosts **raw data sets**:
  - 16 propofol sets: Schnider 1998, Gepts 1987, Kataria 1994, Cortínez 2010 obese, Doufas, Struys 2007 and others.
  - 2 remifentanil sets: Minto 1997, Ross 2001.
- These could test stanpumpR's models against observed concentrations (section 4, item 2).

### Closed tools (search text only)

- **TivaTrainer** (Engbers): many propofol variants, remimazolam, sufentanil and others. IVassist (targets turned into manual doses), BIS, propofol-opioid interaction, decrement times, side-by-side model comparison.
- **AnestAssist** (Palma, iPad): IV and inhaled agents, probabilities of analgesia, sedation and neuromuscular block, opioid interactions, user-defined drugs, a case library recorded from the anaesthesia record.
- **iTIVA** (Universidad del Valle): 11 drugs, about 40 models; agrees with TivaTrainer to about 5%.
- **RUGLOOP II** (Ghent): research TCI and closed-loop BIS control.
- **GE Navigator**: 11 IV and 6 inhaled agents, future Ce, opioid-hypnotic interaction, the EC95-normalised display of Syroid 2002.
- **Dräger SmartPilot View**: a combined hypnotic-analgesic effect on an isobologram.
- **PK/PD Tools for Excel** (Minto & Schnider) appears to be historical.

### 1.3 Errors found in other implementations (verified in the cloned code)

These are other projects' bugs, not stanpumpR's. They are recorded for two reasons:
- an "independent reference" must itself be checked before it is trusted;
- the owners might want to know.

Nothing has been reported to them.

- **`tci`, `R/poppk_mods.R:393` (Eleveld remifentanil).**
  - Code: `V3 = V2ref * SIZE * faging(theta[4])*exp(theta[6]*(TBW-70))`. It uses V2ref (8.82 L) where V3ref (5.03 L) belongs.
  - Effect: V3 is about 1.75x too large. Q3 too, since `Q3 = Q3ref * (V3/V3ref)^0.75`.
- **opentiva, `biometrics.py:100` (Al-Sallami fat-free mass, male branch).**
  - Code: `(1 - 0.8)` where the published equation has `(1 - 0.88)`.
  - Effect: adult male fat-free mass is about 8% too high. That feeds every Eleveld size term, and the reference man too.
- **opentiva, `remifentanil.py` (Eleveld remifentanil).**
  - Code: `if sex: ksex = 1` in a module that codes 0 = male, 1 = female.
  - Effect: the female KSEX increase is applied to men.
- **PAS, `pk_models.py:414-415` (Eleveld remifentanil).**
  - Code: `V2ref = 8.882` and `v2 = V2ref * SIZE * faging(theta[3])`.
  - Effect: KSEX is missing from V2, and 8.882 is not exp(2.17717) = 8.82. stanpumpR (`R/drugs_remifentanil.R:155`), SimTIVA and `tci` all use 8.82 with KSEX.
- **Leads not verified:**
  - PAS `pd_models.py` may swap the variabilities of gamma_r and gamma_p.
  - PAS `tci_control.py` reads the plasma index in effect-site mode near the target.
  - PyTCI's Eleveld propofol Q2 uses `(1 - q3mat/q3matref)`.

### stanpumpR's choices that these tools make differently

- **Eleveld propofol is always the "with opioids" model.**
  - The code: `R/drugs_propofol.R:212-213` applies the opioid-present ageing of V3 and CL unconditionally.
  - It is documented: `inst/help/drugs/propofol.md`, line 14.
  - SimTIVA, opentiva, PAS and `tci` all offer a switch.
  - A switch set automatically when an opioid is in the dose table is a decision for Shafer, not a defect.

## 2. Open-source PK engines and model libraries

### Engines

| Project | How it solves | Near-equal rate constants | Steady state |
|---|---|---|---|
| mrgsolve (R/C++) | LSODA, plus closed-form 1-3 compartment | Stops if \|ka - k10\| < 2.2e-14; the 3-compartment cubic calls `acos` unclamped | Repeated dosing to convergence |
| rxode2lincmt (C++) | Closed form with exact gradients; `matExp()`; ODE solvers | 1-compartment switches to the limiting form below sqrt(eps); 2-3 compartment unguarded | Closed form, 1/(1 - e^(-tau k)) |
| linpk (R) | Eigen-decomposition, modal superposition | Refuses repeated eigenvalues | Closed-form geometric series |
| PKPDsim (R/Rcpp, MIT) | ODE plus 1-3 compartment closed forms | Unguarded | Simulates 45 days |
| pharmsol / Pmetrics (Rust) | ODE plus closed forms; nonparametric NPAG/NPOD | Unguarded; panics on complex roots | Not checked |
| Tucuxi-core (C++) | Closed forms; RK4 for Michaelis-Menten, Erlang and gamma absorption | Unguarded | Not checked |

How stanpumpR compares:

- **Robustness.** `cube()` clamps its `acos` argument, so it is more robust at near-coincident roots than mrgsolve, PKPDsim or pharmsol.
- **Transit chains.** No engine surveyed solves an Erlang transit chain exactly as `advanceTransitChain.R` does. Tucuxi integrates it with RK4; PKPDsim approximates it.
- **Ties.** No engine handles ties better than `separateEigenvalues()` plus `calculateCe()`'s limiting form.

**A small window, measured here.** `separateEigenvalues()` (`R/metaboliteCoefficients.R`) nudges a rate only when two rates are within a relative 1e-10 of each other. Just above that window the partial-fraction difference `(e^(-k t) - e^(-ka t))/(ka - k)` loses precision by cancellation. Computed directly, against the exact `-e^(-k t) expm1(-(ka-k) t)/(ka-k)`, with k = 0.01/min and t from 1 to 500 min:

| relative separation | 1e-4 | 1e-6 | 1e-7 | 1e-8 | 1e-9 | 1e-10 |
|---|---|---|---|---|---|---|
| worst relative error | 1e-11 | 2e-9 | 1e-7 | 4e-7 | 3e-6 | 5e-5 |

- **Clinically, nothing:** at most 5e-5, just above the threshold.
- **It is real, though,** and larger than the 1e-8 nudge, which itself costs about 4e-7.
- **The fix** would be an `expm1`-based divided difference for the two-rate terms, or raising the threshold to about 1e-6 so the nudge fires earlier.
- **Status:** recorded here, not acted on. It touches every closed-form engine and `test-separated-rates.R`.

### Validation approaches worth copying

- **rxode2.** Its tests compare the closed forms with the ODE on NONMEM's reference event data set (`nlmixr2data::nmtest`): bolus, infusion, modelled rate and duration, lag, F, steady state, ADDL, reset (`tests/testthat/test-nmtest.R`). Replaying that data set through stanpumpR's dose-table path would test its event handling against NONMEM.
- **Tucuxi.** A separate repository, `tucuxi-numericalvalidation`, compares predictions, AUC0-24, Cmin and Cmax with NONMEM runs of the same models.
- **pharmsol.** Asserts closed form = ODE within a tolerance.
- **nlmixr2lib.** One vignette per model with a **source trace table** mapping each parameter to the paper's table row, then reproduces a published table or figure, using PKNCA for the noncompartmental analysis.

### Model libraries

**nlmixr2lib**
- Source: github.com/nlmixr2/nlmixr2lib, GPL >= 2.
- Size: 4,228 drug-specific models plus about 420 generic ones.
- Each model records:
  - reference with DOI, and units;
  - compartment data with a **`verified`** flag;
  - covariates with the paper's own name (`source_name`);
  - the population: n, age and weight ranges, sex, disease, dose range;
  - inter-individual variability and residual error.
- Covariate names come from a register enforced by `checkModelConventions()`.
- **It is built largely by an AI extraction skill**, and most entries are `verified = FALSE`. Treat a value from it as a lead.
- About 25 source papers are transcribed both there and in stanpumpR: Abboud 2009, Li 2024 norepinephrine, Du 2019, Berges 2007, Cirincione 2017/2018, Niebecker 2015, Baron 2016, Fediuk 2021, Retlich 2015, Landersdorfer 2012, Zhang 2012, Hwang 2023, Faisal 2019, Watson 2010, Jorga 2000 and others.
- Three were spot-checked and agree to the printed digits. One question was left open: Li 2024's propofol covariate unit reads "per 100 µg/mL" in one and "per 100, per mcg/mL" in the other.

**Tucuxi drugs**
- Source: github.com/sotalya/tucuxi-drugs, AGPL-3.
- 15 XML drug files validated against an XSD:
  - vancomycin x5;
  - busulfan, cefepime, daptomycin, meropenem, piperacillin, tobramycin;
  - tacrolimus;
  - imatinib x2, ponatinib.
- Schema fields worth borrowing:
  - **`domain/constraints`**: the studied covariate range, as a hard or soft check with a message;
  - revision history;
  - molar mass and analyte-to-moiety formula;
  - therapeutic targets with best, min, max, toxicity alarm and inefficacy alarm.

**PKPDsim models**
- MIT.
- 47 + 13 JSON files: vancomycin x18, meropenem x11, piperacillin x8, cefepime x5, busulfan, voriconazole, pegaspargase.
- Each carries references, omegas and residual error.

**Others**
- OSP PK-Sim (about 40 PBPK substance models, among them propofol, midazolam, alfentanil, sufentanil, vancomycin): licence not checked.
- mrgsolve, Pmetrics and campsis carry templates only, no drug library.

## 3. Clinical dosing and TDM calculators

### The tools

**Free tools**
- **ClinCalc vancomycin**
  - Population prior: Buelga 2005 by default, one of four chosen by obesity and critical illness.
  - With one measured level it does a Bayesian update; with two, Sawchuk-Zaske. That is a good teaching contrast.
  - Its aminoglycoside calculator offers the Hartford, Barnes-Jewish, Rochester and Urban-Craig nomograms.
- **VancoCalc** (Oommen 2022, *JMIR Form Res*, PMID 35353046)
  - Runs MCMC **in the browser**, so no patient data leaves the device.
  - Chooses its prior by critical illness and BMI.
- **VancoSim** (GPL-2, Shiny)
  - Goti 2018 with a dialysis covariate.
  - JAGS MCMC, reporting AUC24/MIC with 95% prediction intervals.
- **TDMx** (Wicha, tdmx.eu)
  - Three modules: probability of target attainment, Bayesian MAP, and optimal sampling times.
  - Drugs: gentamicin, amikacin, meropenem, piperacillin.
- **NextDose** (Holford, Auckland)
  - busulfan, gentamicin/amikacin/vancomycin (one pooled neonate-to-adult model), methotrexate, warfarin, voriconazole, dabigatran, caffeine, linezolid, ganciclovir.

**Commercial tools**
- **InsightRX Nova**
  - More than 80 models.
  - Vancomycin is a "Modified Thomson" model, with an in-house model above BMI 40. A company post reports Thomson more predictive with CrCl capped at 150 mL/min.
  - Modules cover beta-lactams, azoles, transplant drugs (tacrolimus, mycophenolate, cyclosporine, everolimus), conditioning drugs (busulfan and others) and IBD biologics.
  - Its MIT pieces are PKPDsim, PKPDmap and clinPK. clinPK has Calvert carboplatin dosing, KDIGO acute-kidney-injury staging, creatinine assay conversion, and a cefepime neurotoxicity model.
- **DoseMeRx**: vancomycin by Buelga, Goti (including intermittent haemodialysis) and Sabourenkov (obese).
- **PrecisePK**: more than 20 drugs.
- **MwPharm++**: about 180 drugs.
- **ID-ODS**, **AutoKinetics**: antibiotic dosing.
- **BestDose** (USC): nonparametric.
- **ISBA** (Limoges): immunosuppressants.

**Opioid conversion calculators** (MDCalc MME, ClinCalc, GlobalRPh)
- These multiply by a fixed ratio: oral only, at steady state, with no time course. MDCalc's own page restricts it to adults and excludes buprenorphine.
- That is the opposite of what stanpumpR does.

### Shared models: cross-checks

**Vancomycin, Thomson 2009.** stanpumpR's `R/drugs_vancomycin.R` uses the same model as Tucuxi's `ch.tucuxi.vancomycin.thomson2009.tdd` and as InsightRX's base model.

- **Verified:**
  - Tucuxi's file computes `TVCL = theta_CL * clcr * theta_scr` (line 481), dropping the published "-66" centring.
  - The file itself says "TODO: The use of CLcr still have to be verified."
  - stanpumpR uses the published `2.99 * (1 + 0.0154 * (crcl - 66))` (line 137).
  - The two differ by a constant 0.049 L/h.
- **Not applied in stanpumpR:** the 150 mL/min CrCl cap that InsightRX reports. That is a question for Shafer, to be settled against the paper and that report.

**Other shared drugs**
- Gentamicin (stanpumpR: Smit 2020), warfarin and dabigatran are offered by NextDose under different models. The same patient run through both shows how far the models disagree.

## 4. What stanpumpR could adopt, ranked

For each item: what it is, where it comes from, and what it would take.

1. **AUC24, AUC24/MIC and time above MIC for the antibiotics.**
   - Read directly off curves already drawn. Vancomycin's AUC24 target is 400-600 mg·h/L (Rybak 2020).
   - Every TDM tool reports these. Small.
2. **Test the models against observed data.**
   - Predict the opentci.org raw data sets (Schnider 1998, Gepts 1987, Minto 1997, Cortínez obese and others) and report the median prediction errors.
   - This is the first check of the models against measurements rather than parameters. Medium.
3. **Independent-engine cross-checks in `tests/testthat`.**
   - rxode2 or mrgsolve in `Suggests`, skipped when absent, compared with stanpumpR's closed forms on each drug's own parameters.
   - Replay `nlmixr2data::nmtest` through the dose-table path.
   - Pin Eleveld, Schnider and Minto values against SimTIVA's and vitaldb/tivatci's code. Section 1.3 shows a reference has to be checked first. Medium.
4. **Studied-range warnings.**
   - Each model declares the age, weight, BMI and renal function its source studied, and the app says when the patient is outside them. Sources: opentiva, Tucuxi `domain/constraints`, nlmixr2lib `population`.
   - stanpumpR does this today only inside a few models: lixisenatide, the Tham insulins, exenatide. Medium; mostly data entry.
5. **Diff the shared papers against nlmixr2lib.**
   - A script comparing nlmixr2lib's values with stanpumpR's for the roughly 25 shared sources.
   - Every mismatch is a lead to check against the PDF, never a correction. Small.
6. **Eleveld propofol opioid switch.**
   - Automatic when an opioid is dosed, as every other implementation offers. A decision for Shafer.
7. **TCI realism and manual-pump aids** (SimTIVA, TivaTrainer, opentiva):
   - a plasma-overshoot cap in effect-site mode;
   - a pump maximum in mL/h from the syringe concentration;
   - bolus speed;
   - a target-to-manual infusion scheme;
   - an RSI overshoot planner;
   - rebuilding Cp/Ce from a retrospective list of targets.

   The TCI API already handles maximum rate and rounding; the app's TCI panel does not.
8. **Missing drugs, by clinical demand in these tools:**
   - Perioperative/ICU antibiotics: cefepime (Buclin 2020), piperacillin (Chen 2016), meropenem (Li 2006), amikacin, tobramycin (Hennig 2013), daptomycin (Dvorchik 2004), linezolid.
   - Vancomycin companions: Goti 2018 (dialysis) and Buelga 2005, beside Thomson, as Minto/Schnider sit beside Eleveld.
   - Transplant: tacrolimus (Monchaud 2012, a four-step Erlang absorption, for which `advanceTransitChain.R` fits).
   - Conditioning: busulfan (McCune 2014, Paci 2012).
   - Antifungal: voriconazole.
   - Perioperative: tranexamic acid.
   - Neonatal: caffeine.
   - Anesthesia models offered elsewhere: Hannivoort dexmedetomidine, Kamp ketamine and esketamine, Kim remifentanil, Masui 2022 remimazolam, Cortínez propofol, thiopental, atracurium.

   Each still needs the primary source read under `docs/adding-a-drug.md`.
9. **Closed-form steady state.**
   - "Already on maintenance" for chronic drugs, 1/(1 - e^(-lambda tau)) per mode instead of expanding schedules. Sources: linpk `R/linpk.R` around lines 440-464; rxode2lincmt `src/linCmt.h` around lines 1010-1040.
10. **Exact two-rate differences.** Replace the near-tie window in section 2 with an `expm1` form. Tiny clinically; tidy numerically.
11. **Variability, Bayesian individualisation and probability of target attainment** (TDMx, `tci`'s `bayes_update()`, PKPDmap, Tucuxi):
    - These are the largest gap between stanpumpR and the TDM tools. They need every model's omegas and residual error, which only a few stanpumpR models record today.
    - A Bayesian update from a measured level changes what the program is, and its regulatory position. That is for Shafer to decide.

## 5. The bio.tools registry (added 2026-11-06)

[bio.tools](https://bio.tools) is the ELIXIR registry of life-science software, annotated with the EDAM ontology.

**Method**
- Queried through its JSON API (`https://bio.tools/api/t/?q=...&format=json`).
- Search terms: pharmacokinetic(s), PBPK, pharmacometrics, pharmacodynamics, population pharmacokinetic, dosing, dose individualization, the topic Pharmacology, and others.
- This gave 1,873 unique entries. Filtering names and descriptions for PK terms left 42.

**What the registry covers, and what it misses**
- Its pharmacokinetics coverage is thin, and mostly drug discovery: ADME and toxicity predictors (SwissADME, Deep-PK, ProTox), DMPK databases (DruMAP, CMNPD), PBPK in MATLAB SimBiology, and PET kinetic modelling (kinfitr).
- **There is no entry for "propofol", "anesthesia", "target controlled infusion", "TCI", "NONMEM" or "therapeutic drug monitoring".**
- **stanpumpR is not registered.**
  - Registering it would be free, and would put it in the registry's EDAM-annotated search.
  - Topics would include Pharmacology, Pharmacokinetics and Anaesthesiology; the operation would be Simulation.
  - It would also be the registry's only anesthesia simulator.
  - Shafer decided to register it (2026-11-06). The entry is drafted in `docs/bio.tools/`.

The entries that matter to stanpumpR:

| Tool | What it is | Licence | Relevance |
|---|---|---|---|
| **PK-DB** (pk-db.com, Matthias König, Humboldt University Berlin; github.com/matthiaskoenig/pkdb) | A curated database of concentration-time data and PK parameters from clinical studies, each linked to its population (age, sex, weight, health, smoking), its interventions (dose, route, schedule) and its source publication. REST API and a Python client. | Code MIT. **The data are not**: the terms of use require citing PK-DB and every original study; ask that substantial reuse be arranged with König, including co-authorship where it is earned; and require a written licence for any commercial use. | **The most useful entry.** It is a source of digitised, published concentration-time data to test stanpumpR's models against. Not tried: the session's network refused the host. A reuse beyond spot checks should start with a note to König. |
| **mapbayr** (github.com/FelicienLL/mapbayr; CRAN) | Maximum-a-posteriori Bayesian estimation of individual PK parameters on top of mrgsolve models. | GPL-3 | The established R template if stanpumpR ever adds a Bayesian update (section 4, item 11). Reimplement; do not copy. |
| **PKconverter** (CRAN 1.5; Shiny at ek-lee.shinyapps.io/PKconverter) | Converts 1-, 2- and 3-compartment parameters among volumes/clearances, rate constants, coefficients/exponents and half-lives. | GPL >= 2 | **An R port of Shafer's own Convert.xls**: its roxygen cites `http://www.nonmemcourse.com/convert.xls`, with the same five ways of defining a model. Could serve as a third reference for `test-help-pk-converter.R`. Its 3-compartment roots call `acos()` without clamping the argument, which `cube()` avoids. |
| **vancomycin TDM** (pipet.shinyapps.io/vancomycin; github.com/pipetcpt/tdm-vancomycin; *Pharmaceutics* 2019;11:224) | Shiny app for Bayesian vancomycin dosing in Korean adults. A two-compartment model (V1 31.7 L; `TVCL = CLPOP*(CLCR/72)**CLPOP2`; V2 scaled by weight/60). MAP is computed by `optim(method = "L-BFGS-B")` in about 100 lines of `server.R` (lines 380-525). | bio.tools says Unlicense (public domain). The repository has no licence file. | The smallest readable example of a MAP update in R/Shiny, in the same stack as stanpumpR. Lead, not verified: it builds its omega matrix from values named `ETA1SD`/`ETA2SD`, so whether they are SDs or variances should be checked against the paper before anything is borrowed. |
| **DosOpt** (biit.cs.ut.ee/DosOpt; *Ther Drug Monit* 2018, PMID 29084032) | Bayesian vancomycin dose adjustment for **neonates** (University of Tartu). | Free web app; code not found | A neonatal vancomycin model, which stanpumpR lacks. |
| **caffsim** (github.com/asancpt/caffsim; CRAN) | Caffeine one-compartment oral model with inter-individual variability (Lee 2015, *Eur J Pediatr*, doi:10.1007/s00431-015-2581-x). CL 0.098 L/h/kg, V 0.72 L/kg, ka 4.27 /h. | GPL-3 | A caffeine source for stanpumpR, which has none. Read Lee 2015 itself. Noted in passing: its steady-state Cmax and Cmin (`R/caffPkparam.R:69-70`) use the intravenous-bolus formula for an oral model. |
| **ACEInhibPKPD** (github.com/ashleefv/ACEInhibPKPD; JOSS 2017) | MATLAB PK/PD of ACE inhibition (angiotensin II), for teaching. Fitted to **Shionoiri 1992** benazepril and Shionoiri 1988 cilazapril in hypertensive patients with normal and impaired renal function; v2.1 adds lisinopril, delapril and ramipril. | MIT | stanpumpR's benazepril (Kaiser 1989) has no renal term, and Shionoiri 1992 appears nowhere in the repository. That paper measures benazeprilat with normal and impaired renal function, so it is a candidate source for a renal covariate, under the antihypertensive registry's rules (`docs/antihypertensives.md`). |
| **Open Systems Pharmacology**, **mrgsolve**, **nlmixr2** | Already covered in section 2. | | |

**Not relevant:** the rest of the 42 are deep-learning ADME predictors, chemical and peptide databases, oncology trial-design tools, PET and perfusion imaging kinetics, a PhysiCell PK/PD plug-in, global sensitivity analysis for PBPK (pksensi), and a "quantum-biological digital twin" (PreciseDose).

## Sources

**Anesthesia simulators and TCI**
- https://github.com/luktinghin/simtiva
- https://github.com/opentiva/opentiva
- https://github.com/jarretrt/tci
- https://github.com/AnesthesiaSimulation/Python_Anesthesia_Simulator
- https://github.com/vitaldb/tivatci
- https://github.com/JMathiszig-Lee/PyTCI
- https://opentci.org

**PK engines**
- https://github.com/metrumresearchgroup/mrgsolve
- https://github.com/nlmixr2/rxode2
- https://github.com/nlmixr2/rxode2lincmt
- https://github.com/benjaminrich/linpk
- https://github.com/InsightRX/PKPDsim
- https://github.com/InsightRX/PKPDmap
- https://github.com/LAPKB/pharmsol

**Model libraries**
- https://github.com/nlmixr2/nlmixr2lib
- https://github.com/sotalya/tucuxi-drugs
- https://github.com/sotalya/tucuxi-core

**TDM and dosing tools**
- https://github.com/OliversLab/VancoSim
- https://github.com/levenc/posologyr
- https://github.com/InsightRX/clinPK
- https://clincalc.com/Aminoglycoside/
- https://www.insight-rx.com/provider/drug-modules
- https://doseme-rx.com/why-dosemerx/vancomycin-models
- https://clinpharmacol.fmhs.auckland.ac.nz/nextdose/medicines/medicines_summary.htm
- https://www.page-meeting.org/Abstracts/tdmx-a-web-application-for-therapeutic-drug-monitoring-enhanced-by-pharmacometrics/
- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9008526/ (VancoCalc)
- https://www.mdcalc.com/calc/10170

**bio.tools**
- https://bio.tools
- https://github.com/matthiaskoenig/pkdb
- https://github.com/FelicienLL/mapbayr
- https://cran.r-project.org/package=PKconverter
- https://github.com/pipetcpt/tdm-vancomycin
- http://www.biit.cs.ut.ee/DosOpt
- https://github.com/asancpt/caffsim
- https://github.com/ashleefv/ACEInhibPKPD
