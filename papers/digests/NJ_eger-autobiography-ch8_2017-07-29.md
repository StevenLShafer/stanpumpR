---
file: NJ_eger-autobiography-ch8_2017-07-29.pdf
pmid: none (unpublished manuscript)
doi: none
citation: "Eger EI II, Shafer SL. The Evolution of Pharmacokinetics. Chapter 8 of Eger's scientific autobiography. Unpublished manuscript, working draft of 29 July 2017; 37 pp., 89 references. Supplied by Shafer from his own copy, 2026-11-23."
kind: review
drugs: [nitrousOxide, isoflurane, sevoflurane, desflurane]
source_read: "partial - text layer, born-digital (no OCR needed: 37 pp., median 2331 letters a page). Every scientific section read in full, and the 89-item reference list. The closing personal material - the sections titled 'A Reflection' and 'A Diagnosis of Cancer of the Gall Bladder', and four family photograph captions naming living relatives - is deliberately NOT digested: it carries nothing pharmacokinetic. Figures are images in the PDF export and were not read."
digest_by: Claude Opus 5, 2026-11-23
checked_against: stanpumpR master 23b87a27
verification: partly-verified
open_questions:
  - "[decision] The chapter's own final model has FIVE body compartments - lungs, VRG, MG, FG and an intertissue diffusion group of about 500 min time constant carrying roughly 30% of the uptake - where stanpumpR's gas engine has the 1962 four (lungs, VRG, MG, FG, plus the circuit). Whether to add the ITDG is Shafer's call. It would matter where the chapter says it matters: the washout of a long, soluble anaesthetic."
  - "[decision] This is an unpublished manuscript. This card records only its science, but whether a card about it belongs in the public repository at all is Shafer's to decide, as is whether the chapter should be cited as a published source once it is published."
  - "[check] The draft's cyclopropane blood/gas coefficients disagree with each other: the text gives 0.6 at 1% inspired and about 0.47 at 100%, then reports Perl as assuming 0.42 for cyclopropane against 0.47 for nitrous oxide, while Rackow's quoted sentence has cyclopropane moving from 0.47 upward. The 0.42/0.47 pair reads as transposed against Rackow's own baseline. Settled by the Perl and Rackow papers (chapter refs 39, 40). It changes nothing in stanpumpR, which has no cyclopropane."
  - "[check] The chapter attributes the experimental demonstration of intertissue diffusion, kidney to perirenal fat, to its ref 49, which the reference list gives as Allott, Steward and Mapleson, Pharmacokinetics of halothane in the dog: comparison of theory and measurement in individuals, Br J Anaesth 1976;48:279-95. That title is a theory-against-measurement comparison, and I could not confirm from the reference alone that it reports the kidney-to-fat transfer. Either the citation number is wrong or the paper contains more than its title suggests; the paper would settle it."
---

# Eger 2017 (autobiography ch. 8): The evolution of inhaled-anaesthetic pharmacokinetics

A first-person account, written with Shafer as editor, of how the mathematical model of inhaled
anaesthetic uptake and distribution was built and successively corrected from the late 1950s to the
2010s. Shafer's editorial queries to Eger are still in the draft, in angle brackets; several are
substantive disagreements about the science, and they are recorded under *Caveats* because nothing
carrying one should be read as settled.

## Study
Not a study: a narrative review of the author's own programme, with 89 references. The empirical work
it recounts, in the order the chapter takes it:

- **The 1962 Papper-Kitz conference, New York**, the first on uptake and distribution; proceedings
  published 1963. Severinghaus, MacKrell and Mapleson presented closed-form approximations (refs 3,
  4, 5); Eger presented differential equations solved numerically on the Berkeley IBM, and showed the
  closed forms could not carry the effect of a gas's inspired concentration on its own uptake. Built
  on Price's 1960 analysis of thiopental distribution (ref 2). Perl argued at the same meeting, and
  beforehand, for intertissue diffusion (refs 6, 7) - the point the chapter returns to later.
- **Solubility measurements**, 16 reports 1961-1970, beginning with Larson, Eger and Severinghaus's
  Ostwald coefficients (ref 1), falling to 2-3 a decade afterwards.
- **Washin/washout with dense late sampling** (Carpenter and Eger, 1980s): enflurane, halothane,
  isoflurane and methoxyflurane, each sub-MAC in nitrous oxide to a total 1.1 MAC, given for two
  hours to nine paralysed normocapnic patients aged 39 +/- 4 (mean +/- SD), with venous partial
  pressures followed for **five to nine days**. Polyexponential fits to 36 data sets, adding
  exponents while the residual sum of squares fell significantly. **27 of the 36 were best fit with
  five compartments.** Repeated at 30 min rather than two hours with the same result (ref 44), in
  pigs in 1990 (desflurane, sevoflurane, isoflurane, halothane; ref 45), in humans in 1991 (refs 46,
  47), and in patients aged 73 +/- 3 (ref 48).
- **Metabolism**: Halsey's miniature swine (ref 57) - methoxyflurane about 50% metabolised, halothane
  about 35%, enflurane 10%, isoflurane and cyclopropane none. Then the saturation studies below.
- **Percutaneous and transmembrane loss**: the subject's arm in an airtight chamber, with the chamber
  volume, skin area and gas tension measured against simultaneous arterial sampling (refs 80-82).
- **Clinical recovery**: desflurane against sevoflurane at 2, 4 and 8 hours of 1.25 MAC (ref 52);
  children, desflurane against isoflurane (ref 53); McKay 2010 (ref 54), randomised.

## Model and parameters
The chapter fits no model of its own. Its quantitative content is the following relations and the
measurements above. Figures were not read.

```yaml
parameters:
  - {name: gas_space_expansion, value: "1/(1 - FI)", unit: fold, source: "Nitrous Oxide Can Expand Gas Spaces", note: "compliant closed space at equilibrium; FI 0.5 -> 2x, FI 0.75 -> 4x"}
  - {name: gas_space_pressure, value: "1 atm + P_anaesthetic", unit: atm, source: "same section", note: "NON-compliant space (eye bubble, skull): 50% nitrous oxide -> 1.5 atm"}
  - {name: equilibration_air_bubble, value: "almost instant", unit: time, source: "same section (ref 66)"}
  - {name: equilibration_pneumothorax, value: "a few", unit: min, source: "same section (ref 67)"}
  - {name: equilibration_intestinal_gas, value: "a few", unit: h, source: "same section (refs 67, 69)"}
  - {name: lambda_concentration_effect, value: "lambda' = lambda (1 - FI)", unit: dimensionless, source: "The Concentration and Second Gas Effects", note: "the chapter's device for carrying the concentration effect in a model that otherwise holds solubility constant; FI 1 gives lambda' 0, and FA/FI then starts at 1 and does not move"}
  - {name: fifth_compartment_time_constant, value: 500, unit: min, source: "All models are wrong", note: "approximate; carries about 30% of the anaesthetic taken up"}
  - {name: bone_marrow_share, value: "7-8", unit: "% of uptake", source: "same section", note: "the most bone marrow could explain, against the 30% observed"}
  - {name: halothane_hepatic_Km, value: 0.029, unit: "% of an atmosphere", source: "Metabolism (Cahalan, ref 61)", note: "+/- 0.003 (mean +/- SE), about 0.04 MAC, non-invasive in vivo in humans"}
  - {name: enflurane_defluorination_Km, value: 0.05, unit: MAC, source: "Metabolism (Hitt, ref 60)", note: "half Vmax, in rats"}
  - {name: halothane_hepatic_share, value: 50, unit: "% of elimination", source: "Metabolism (Sawyer, ref 58)", note: "at about 0.01% MAC; near-total at trace concentrations, negligible at anaesthetic ones"}
  - {name: enflurane_metabolised_third_MAC, value: 10, unit: "% of uptake", source: "Metabolism (Cahalan, ref 63)", note: "unchanged by a four-fold dose increase"}
  - {name: halothane_metabolised_third_MAC, value: 55, unit: "% of uptake", source: "same", note: "falls to 41% at four-fold the concentration; 54.8 +/- 1.0 without and 55.2 +/- 5.9 with 70% nitrous oxide"}
  - {name: percutaneous_loss_N2O, value: 3.6, unit: "mL/min/m2", source: "Skin and Membrane section", note: "at 70% inspired; about an order of magnitude below uptake in the first hour or two"}
  - {name: amniotic_vs_skin_diffusion, value: "10-40", unit: fold, source: "same section (ref 83)", note: "in vitro; peritoneal and pleural lie between (ref 84)"}
  - {name: blood_gas_sevoflurane, value: 0.65, unit: dimensionless, source: "Kinetics and Recovery, as the draft states it", note: "quoted for the recovery comparison, not measured here"}
  - {name: blood_gas_desflurane, value: 0.45, unit: dimensionless, source: same, note: "as above"}
  - {name: desflurane_first_hour_elimination, value: 40, unit: "% faster than sevoflurane", source: "Kinetics and Recovery (ref 52)", note: "first hour of recovery, after 2, 4 or 8 h at 1.25 MAC"}
```

The non-quantitative model statements the chapter rests on, each of which stanpumpR either uses or
does not (see *Verification*):

- FA/FI rises faster the less soluble the agent. Raising ventilation moves it little for a poorly
  soluble agent and nearly in proportion for a highly soluble one; raising cardiac output **hinders**
  the rise, with the same solubility dependence.
- Ventilation and cardiac output rising **together** speeds the rise rather than cancelling, because
  the extra flow also shortens tissue equilibration and so returns higher tensions to the lungs.
  Tested across species by body size (Wahrenbrock, ref 27, now queued): faster in a rat than a human
  than a whale. The chapter offers fever and hyperthyroidism as the clinical cases, metabolism, CO2
  production, ventilation and blood flow rising roughly in proportion.
- Redistribution matters as much as the total. A greater VRG share speeds the rise - the young
  patient, who also has a smaller FRC, against a higher MAC; a greater muscle or fat share slows it
  (exercise, weight reduction, a beta-agonist, ref 31); and in hypovolaemic shock, where cerebral
  flow is preserved at the expense of everything else, "In this reduced heart-brain circuit, FA/FI
  rises extremely fast", with a corresponding risk of cardiovascular collapse.
- Anaesthetic depression of ventilation is a **negative** feedback and so protective during
  spontaneous breathing, while depression of circulation is a **positive** feedback that can run away
  to collapse; controlled ventilation bypasses the protective loop. Surgical stimulation opposes both
  (refs 24, 25).
- Falling body temperature raises solubility in blood and every tissue, lowers metabolism and so
  ventilation and cardiac output, and lowers MAC rectilinearly (refs 32-35); the net is slower
  induction, more so the more soluble the agent.
- A V/Q mismatch opens an FA-Fa difference whose size depends on solubility; it arises from disease,
  from anaesthetic bronchodilation, and from such routine things as paralysis and controlled
  ventilation. **Endobronchial intubation** is the extreme case: one lung ventilated at twice normal,
  the other perfused and not ventilated. For a poorly soluble agent the doubled ventilation cannot
  compensate, FA being unable to exceed FI, and the arterial partial pressure is initially about half
  normal; for a highly soluble agent doubling the delivery doubles the blood partial pressure and
  compensates exactly, so the arterial rise is not delayed. Predicted from an analogue model (ref 37
  = Eger and Severinghaus, Anesthesiology 1964;25:620-6, now queued) and confirmed in dogs and people
  (ref 38 = Stoelting and Longnecker, Anesthesiology 1972;36:352-6, now queued). The chapter draws
  the oxygen analogy explicitly: poorly soluble oxygen cannot be compensated, which is why
  hypoxaemia is the risk of one-lung ventilation while hypercarbia rarely is.
- Duration and concentration govern recovery, because the slow compartments do not reach steady state
  within a clinical case; emergence relates to MAC-hours with a slope set by solubility (ref 51).
- Metabolism is **saturable**, and so matters only at trace concentrations - which is why leaving it
  out of the uptake model was safe at anaesthetic concentrations. Drug released from fat during a
  long recovery emerges at trace concentrations and is therefore more heavily metabolised.
- The rebreathing circuit's own structure changes the kinetics (refs 77, 78): three requirements for
  CO2-free rebreathing - one-way valves on both limbs, an overflow valve not between the inspiratory
  valve and the patient, and fresh gas entering between the patient and the inspiratory valve - and
  three fresh-gas inlet placements of decreasing efficiency, the most efficient equivalent to the
  Magill (Mapleson A) circuit.

## Pharmacodynamics and end points
No PK/PD model is fitted, and no ke0, EC50 or MAC value is given for any agent. The chapter records
that inhaled agents alter the EEG (refs 12, 13), that Faulconer proposed EEG waveforms as a surrogate
for effect-site concentration and that Eger demonstrated it for ether (ref 14), and credits Hull 1978
(pancuronium, Br J Anaesth 50:1113-23) and Sheiner 1979 (d-tubocurarine, Clin Pharmacol Ther
25:358-71) with working out plasma-to-effect-site equilibration, calling the effect site one of the
fundamental concepts of anaesthetic pharmacology. It notes that the effect site is not always the
brain (refs 10, 11). End points named in the recovery work: response to command, orientation to place
and date, and swallowing 20 mL of water without coughing or drooling (McKay, ref 54), which recovered
sooner after desflurane than sevoflurane, correlated with MAC-hours for sevoflurane but not
desflurane, and worsened with body mass index for sevoflurane but not desflurane.

## Covariates
None in the parameter sense. The patient factors the chapter says change the kinetics, with its
reasoning, are **age**, **body temperature**, **cardiac output and ventilation** and their
**distribution**, **metabolic rate** (fever, hyperthyroidism, surgical stimulation), **body size
across species**, and **V/Q mismatch** from disease, paralysis, controlled ventilation or
endobronchial intubation.

## Caveats
- **An unpublished working draft**, dated 29 July 2017, weeks before Eger's death; never finished and
  never peer reviewed. `screen_inreview.py` clears it, correctly - it is not a manuscript under peer
  review - but it is unpublished all the same, and the card is scoped accordingly.
- **Shafer's editorial queries are unanswered, and several are substantive.** He disputes that
  doubling the blood concentration doubles FA/FI, the ratio approaching 1 asymptotically rather than
  2; disputes that emergence is truly linear in MAC-hours, arguing it cannot be and that the cited
  2-8 h study cannot show otherwise because everything fast has equilibrated by two hours, and
  offering a monotonic relation instead; denies that increasing duration necessarily increases the
  fraction metabolised, since a large enough dose would be eliminated mostly above trace
  concentrations; questions whether a lower MAC in the young or the cold is not simply offset by
  raising FI, which would make it a pharmacodynamic statement with no kinetic consequence; asks
  whether ventilation and cardiac output are not nearly interchangeable in the equations; asks
  whether the tissue:gas coefficients must be scaled by (1 - FI) as well as the blood:gas one; and
  rejects the Mencken epigraph on principle, holding that models are successive approximations and
  never right. He also doubts whether the one recovery study quoted is representative of its
  literature, and twice asks for autobiographical detail where the text has become textbook.
- Typographical defects of a draft, not corrected here: a year missing its leading digit, a
  became/because slip, an unresolved four-to-five edit that leaves both numbers standing, one
  sentence whose two halves are identical, and - in the key equation of the last section - the
  partition-coefficient symbol itself missing from the text layer, so that the relation reads with a
  blank where lambda should be. The equation is recoverable only from Shafer's own bracketed note
  that he substituted symbols for what he could not read.
- The reference list carries no PMIDs or DOIs, and two queries say the underlying papers (the 1963
  conference volume) were unobtainable to Shafer. Hull 1978 and Sheiner 1979 are cited inline with
  full citations rather than by number, which is how a draft looks mid-edit.
- Historical agents carry much of the argument - cyclopropane, ether, methoxyflurane, halothane,
  enflurane, fluroxene, acetylene, ethylene - none of which is in stanpumpR's library. One result is
  explicitly unpublished data (soda lime degrading an experimental anaesthetic in seconds).
- A single-animal result is reported as such (fluroxene in one test swine, ref 59).

## Used by stanpumpR
The context file records no locations, because `works.json` was last rebuilt from extraction round V
(master e4145b5, 2026-10-30) and both citations post-date it; `diff_cites.py` will offer the chapter
to the next round. Found by searching the repository directly:

| Location | What it uses the chapter for |
|---|---|
| `R/endobronchial.R:9-18` | Names the chapter as THE SOURCE for the endobronchial-intubation model and quotes its Ventilation/Perfusion Inequalities section at length. Read in full 2026-11-19. |
| `R/physiology.R:401` | Quotes the reduced heart-brain circuit sentence for the cardiac-output slider's protected brain and heart share. |
| `R/physiology.R:490` | Cites the chapter for the direction of the body-temperature effect: a fall in temperature raises solubility in blood and every tissue, slowing induction most for a soluble agent, and lowers MAC. |
| `inst/app-help/models/physiology.md:218, 654` | The same two passages, for the user. |

## Verification against stanpumpR
- **Endobronchial intubation** (`R/endobronchial.R`; `ENDOBRONCHIAL_SHUNT_FRACTION` 0.5,
  `ENDOBRONCHIAL_VENTILATED_VOLUME_SHARE`): the code implements one lung ventilated and the other
  perfused but taking up no agent, with half the cardiac output as a shunt, and feeds the tissues
  `gasArterial()` = (1 - s) alveolar + s mixed venous. That is the chapter's idealisation, and the
  quoted passage is verbatim. The chapter's solubility-dependent consequences follow from the
  arterial mixing rather than being coded separately, which is the right way round. **Matches.**
- **The shock heart-brain circuit** (`PHYSIOLOGY_GAS_PROTECTED_SHARE`): the chapter gives the
  direction and the mechanism and **no number**; stanpumpR's 16% protected share comes from Williams
  and Leggett 1989. The chapter supports the model's shape, not its value. **Matches as far as it
  goes; the magnitude is not the chapter's, and the code does not claim it is.**
- **Body temperature**: direction only, again. The chapter gives no slope; stanpumpR takes its
  numbers from Zhou 2002 (solubility) and Liu 2001 (MAC), and `R/physiology.R:490` cites the chapter
  for the direction alone. **Matches.** Note that the chapter also has cardiac output and ventilation
  falling with temperature, which stanpumpR deliberately does **not** couple (Shafer, 2026-11-16: the
  clinician sets each) - a documented departure, not a discrepancy.
- **The number of compartments.** `gasStateCount()` is 2 + the tissue groups, and `gasTissueShares()`
  gives brain, muscle and fat, plus `vrgRest` only when physiology is off normal. So the engine
  carries circuit + lungs + VRG + MG + FG: the **1962 four-compartment body**. The chapter's own
  conclusion, from 27 of 36 data sets, is that **five** are needed, the fifth an intertissue
  diffusion group of about 500 min carrying some 30% of uptake, which it says others demonstrated
  experimentally. A search for intertissue diffusion across `R/`, `inst/app-help/` and `docs/` finds
  nothing. **Not implemented** - raised as a `[decision]`, not a transcription error.
- **The concentration and second gas effects**: implemented, but by GasMan's construction rather than
  the chapter's. `R/advanceClosedFormGas.R` carries `uptakeEffect` (on by default), summing uptake
  across all gases and adding it to each gas's alveolar tension as `GasDoc.cpp::Calc` does under the
  names `uptake effect` and `Correct for constant lung capacity`; the engine holds the summed uptake fixed
  within each sub-step to keep the step linear, so propagation is exact within a sub-step and first
  order in dt between them. The chapter's device, lambda' = lambda(1 - FI), is an alternative
  expression of the same physics for a model that cannot carry the volume change directly. stanpumpR
  carries it directly, so Shafer's unanswered query about scaling the tissue:gas coefficients does
  not bite here. **Matches in substance, by a different construction.**
- **Nitrous oxide expanding gas spaces** (1/(1 - FI), and 1 atm + P in a non-compliant space): not
  modelled. stanpumpR plots partial pressures and has no closed-gas-space compartment. **Not
  implemented**, and no code claims otherwise.
- **Circuit structure**: stanpumpR has the ideal circuit and a semi-closed circle kept for the GasMan
  validation, with `gasCircuitBlend()` governing rebreathing. The chapter's three structural
  requirements and its three fresh-gas inlet variants are not offered as options. **Not
  implemented**, and out of scope as the app stands.
- **Saturable metabolism of the volatile agents**: not modelled, and the chapter is the authority for
  why that is safe - metabolism is a negligible route at anaesthetic concentrations. **Consistent.**
- Nothing in the chapter contradicts a number stanpumpR holds.

Three of the chapter's references were absent from the archive and are now queued
(`stanpumpr-queue.csv`, noted as wanted by this card): **PMID 14211488** (ref 37, Eger and
Severinghaus 1964) and **PMID 5020644** (ref 38, Stoelting and Longnecker 1972), which are the
prediction and the experimental confirmation behind `R/endobronchial.R` and were cited there without
being held, and **PMID 4149271** (ref 27, Wahrenbrock 1974), the across-species test. None is free at
PMC.
