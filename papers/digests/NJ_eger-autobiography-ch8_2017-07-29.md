---
file: NJ_eger-autobiography-ch8_2017-07-29.pdf
pmid: none (a book chapter)
doi: none
citation: "Eger EI II. Autobiography of a Persistent Anesthesiologist. Edited by Shafer SL and Spitler LE. Lippincott Williams & Wilkins (Wolters Kluwer), 1 October 2021; 224 pp., ISBN 9781975169190. The archived file is the PRE-PUBLICATION DRAFT of its pharmacokinetics chapter, headed 'Chapter 8. The Evolution of Pharmacokinetics' and dated 29 July 2017 (37 pp., 89 references), supplied by Shafer from his own copy 2026-11-23. Cite the published book, not this draft."
kind: review
drugs: [nitrousOxide, isoflurane, sevoflurane, desflurane]
source_read: "partial - text layer, born-digital (no OCR needed: 37 pp., median 2331 letters a page). Every scientific section read in full (PDF pp. 3-28), and the 89-item reference list (pp. 31-36). The closing personal material - the sections titled 'A Reflection' and 'A Diagnosis of Cancer of the Gall Bladder' (p. 29 onward), and four family photograph captions naming living relatives - is deliberately NOT digested: it carries nothing pharmacokinetic. Figures are images in the PDF export and were not read. Page numbers throughout this card are one-based pages of the archived PDF; the chapter's own printed numbers run two lower."
digest_by: Claude Opus 5, 2026-11-23
checked_against: stanpumpR master 23b87a27
verification: partly-verified
open_questions:
  - "[decision] The chapter's own final model has FIVE body compartments - lungs, VRG, MG, FG and an intertissue diffusion group of about 500 min time constant carrying roughly 30% of the uptake (PDF pp. 15-16) - where stanpumpR's gas engine has the 1962 four (lungs, VRG, MG, FG, plus the circuit). Whether to add the ITDG is Shafer's call. It would matter where the chapter says it matters: the washout of a long, soluble anaesthetic."
  - "[check] The published book's chapter number and title for this material are not confirmed. The draft heads itself 'Chapter 8. The Evolution of Pharmacokinetics'; no table of contents for the published book was reachable (the publisher's page lists none, and the Ovid page refuses automated requests), so the card follows the draft's own designation. Worth settling from a copy of the book, which would also show which of the draft's editorial queries were resolved and how."
  - "[check] The draft's cyclopropane blood/gas coefficients disagree with each other (PDF pp. 14-15): the text gives 0.6 at 1% inspired and about 0.47 at 100% (p. 14), then reports Perl as assuming 0.42 for cyclopropane against 0.47 for nitrous oxide (p. 15), while Rackow's quoted sentence on the same page has cyclopropane moving from 0.47 upward. The 0.42/0.47 pair reads as transposed against Rackow's own baseline. Settled by the Perl and Rackow papers (chapter refs 39, 40). It changes nothing in stanpumpR, which has no cyclopropane."
  - "[check] What is the denominator of Halsey's 50/35/10% metabolism figures (PDF p. 21, ref 57 = Halsey, Sawyer, Eger, Bahlman and Impelman, Anesthesiology 1971;35:43-7, PMID 5088786)? The chapter states the percentages bare, with no denominator and no exposure condition. The paper's title is 'Hepatic metabolism of ...', and the companion ref 58 is titled 'Concentration dependence of hepatic halothane metabolism' (Anesthesiology 1971;34:230-4, PMID 5544623), so the figures are very unlikely to be fractions of an administered anaesthetic dose - CodeRabbit on PR #857 suggested they are hepatic extraction at low subanaesthetic concentrations, which is consistent with both titles and with the chapter's own argument, but neither paper has been read. Both are now queued; either would settle it. Until then the card states the figures as the chapter does and says the denominator is unstated."
  - "[check] The chapter attributes the experimental demonstration of intertissue diffusion, kidney to perirenal fat, to its ref 49 (PDF p. 16), which the reference list gives as Allott, Steward and Mapleson, Pharmacokinetics of halothane in the dog: comparison of theory and measurement in individuals, Br J Anaesth 1976;48:279-95. That title is a theory-against-measurement comparison, and I could not confirm from the reference alone that it reports the kidney-to-fat transfer. Either the citation number is wrong or the paper contains more than its title suggests; the paper would settle it."
---

# Eger: The evolution of inhaled-anaesthetic pharmacokinetics (autobiography ch. 8)

A first-person account of how the mathematical model of inhaled anaesthetic uptake and distribution
was built and successively corrected from the late 1950s to the 2010s. The chapter is **published**,
in *Autobiography of a Persistent Anesthesiologist* (Eger EI II, ed. Shafer SL and Spitler LE; LWW,
2021), Eger having chosen Shafer to edit it.

**What is archived is the working draft, not the published text.** It is dated 29 July 2017, weeks
before Eger's death, and Shafer's editorial queries to Eger are still in it, in angle brackets.
Several are substantive disagreements about the science; they are recorded under *Caveats*, because
nothing carrying one should be read as settled, and because the published book may have resolved any
of them in either direction. **Anything taken from here and used should be checked against, and cited
as, the published book.**

## Study
Not a study: a narrative review of the author's own programme, with 89 references (PDF pp. 31-36).
Page numbers below are the archived PDF's, on the convention set out under *Model and parameters*.
The empirical work it recounts, in the order the chapter takes it:

- **The 1962 Papper-Kitz conference, New York** (p. 4), the first on uptake and distribution;
  proceedings published 1963. Severinghaus, MacKrell and Mapleson presented closed-form
  approximations (refs 3, 4, 5); Eger presented differential equations solved numerically on the
  Berkeley IBM (p. 3), and showed the closed forms could not carry the effect of a gas's inspired
  concentration on its own uptake. Built on Price's 1960 analysis of thiopental distribution (ref 2).
  Perl argued at the same meeting, and beforehand, for intertissue diffusion (refs 6, 7) - the point
  the chapter returns to later.
- **Solubility measurements**, 16 reports 1961-1970 (p. 6), beginning with Larson, Eger and
  Severinghaus's Ostwald coefficients (ref 1), falling to 2-3 a decade afterwards.
- **Washin/washout with dense late sampling** (Carpenter and Eger, 1980s; pp. 15-16, ref 43):
  enflurane, halothane, isoflurane and methoxyflurane, each sub-MAC in nitrous oxide to a total
  1.1 MAC, given for two hours to nine paralysed normocapnic patients aged 39 +/- 4 (mean +/- SD),
  with venous partial pressures followed for **five to nine days** (p. 15). Polyexponential fits to
  36 data sets, adding exponents while the residual sum of squares fell significantly. **27 of the 36
  were best fit with five compartments** (p. 15). Repeated at 30 min rather than two hours with the
  same result (p. 16, ref 44), in pigs in 1990 (desflurane, sevoflurane, isoflurane, halothane;
  ref 45), in humans in 1991 (refs 46, 47), and in patients aged 73 +/- 3 (p. 16, ref 48).
- **Metabolism**: Halsey's miniature swine (p. 21, ref 57) - methoxyflurane about 50%, halothane
  about 35%, enflurane 10%, isoflurane and cyclopropane none. **The chapter gives no denominator and
  no exposure condition for these four figures**, so they must not be read as fractions of an
  administered anaesthetic dose: the paper is titled *Hepatic metabolism of halothane,
  methoxyflurane, cyclopropane, Ethrane and Forane in miniature swine*, and the chapter's own next
  paragraphs (p. 21) say halothane's metabolised fraction is strongly concentration-dependent -
  nearly all of it at trace concentrations, about half of elimination at 0.01% MAC, and virtually
  none at anaesthetic concentrations. See *open_questions*. Then the saturation studies (pp. 21-23),
  whose own figures do carry their conditions.
- **Percutaneous and transmembrane loss** (p. 26, refs 80-82): the subject's arm in an airtight
  chamber, with the chamber volume, skin area and gas tension measured against simultaneous arterial
  sampling.
- **Clinical recovery**: desflurane against sevoflurane at 2, 4 and 8 hours of 1.25 MAC (pp. 18-19,
  ref 52); children, desflurane against isoflurane (p. 19, ref 53); McKay 2010 (pp. 19-20, ref 54),
  randomised.

## Model and parameters
The chapter fits no model of its own, and has no parameter table: its quantitative content is stated
in the prose, as the relations below and the measurements under *Study*. Figures were not read.

**Page locators.** Every `page` below is a **one-based page of the archived PDF**
(`NJ_eger-autobiography-ch8_2017-07-29.pdf`, 37 pp.). The chapter's own printed page numbers run two
lower, the PDF opening with a cover page and a header page, so PDF p. 24 carries the running header
`The Evolution of Pharmacokinetics 22`. `source` names the section, and a bracketed number is the
chapter's own reference number, never a page.

```yaml
parameters:
  - {name: gas_space_expansion, value: "1/(1 - FI)", unit: fold, page: 24, source: "Nitrous Oxide Can Expand Gas Spaces", note: "compliant closed space at equilibrium; the chapter's worked examples, same page: FI 0.5 -> 2x, FI 0.75 -> 4x"}
  - {name: gas_space_pressure, value: "1 atm + P_anaesthetic", unit: atm, page: 24, source: "same section (ref 74)", note: "NON-compliant space (eye bubble, skull): 50% nitrous oxide -> 1.5 atm"}
  - {name: equilibration_air_bubble, value: "almost instant", unit: time, page: 24, source: "same section (ref 66)"}
  - {name: equilibration_pneumothorax, value: "a few", unit: min, page: 24, source: "same section (ref 67)"}
  - {name: equilibration_intestinal_gas, value: "a few", unit: h, page: 24, source: "same section (refs 67, 69)"}
  - {name: lambda_concentration_effect, value: "lambda' = lambda (1 - FI)", unit: dimensionless, page: 28, source: "The Concentration and Second Gas Effects", note: "the chapter's device for carrying the concentration effect in a model that otherwise holds solubility constant; FI 1 gives lambda' 0, and FA/FI then starts at 1 and does not move. The lambda symbol itself is absent from the text layer on this page - see Caveats"}
  - {name: fifth_compartment_time_constant, value: 500, unit: min, page: 16, source: "All models are wrong", note: "approximate; carries about 30% of the anaesthetic taken up"}
  - {name: bone_marrow_share, value: "7-8", unit: "% of uptake", page: 16, source: "same section", note: "the most bone marrow could explain, against the 30% observed"}
  - {name: halothane_hepatic_Km, value: 0.029, unit: "% of an atmosphere", page: 21, source: "Metabolism (Cahalan, ref 61)", note: "+/- 0.003 (mean +/- SE), about 0.04 MAC, non-invasive in vivo in humans. Shafer's query on p. 22 points out the units are not given; % as partial pressure is his reading, not the text's"}
  - {name: enflurane_defluorination_Km, value: 0.05, unit: MAC, page: 21, source: "Metabolism (Hitt, ref 60)", note: "half Vmax, in rats"}
  - {name: halothane_hepatic_share, value: 50, unit: "% of elimination", page: 21, source: "Metabolism (Sawyer, ref 58)", note: "at about 0.01% MAC; near-total at trace concentrations, negligible at anaesthetic ones"}
  - {name: enflurane_metabolised_third_MAC, value: 10, unit: "% of uptake", page: 23, source: "Metabolism (Cahalan, ref 63)", note: "unchanged by a four-fold dose increase"}
  - {name: halothane_metabolised_third_MAC, value: 55, unit: "% of uptake", page: 23, source: "same (ref 63)", note: "falls to 41% at four-fold the concentration; 54.8 +/- 1.0 without and 55.2 +/- 5.9 with 70% nitrous oxide, same page"}
  - {name: percutaneous_loss_N2O, value: 3.6, unit: "mL/min/m2", page: 26, source: "Effect of Anesthetic Loss Through Skin and Membrane (refs 80-82)", note: "at 70% inspired; about an order of magnitude below uptake in the first hour or two"}
  - {name: amniotic_vs_skin_diffusion, value: "10-40", unit: fold, page: 26, source: "same section (ref 83)", note: "in vitro; peritoneal and pleural lie between (ref 84)"}
  - {name: blood_gas_sevoflurane, value: 0.65, unit: dimensionless, page: 18, source: "Kinetics and Recovery, as the draft states it", note: "quoted for the recovery comparison, not measured here"}
  - {name: blood_gas_desflurane, value: 0.45, unit: dimensionless, page: 18, source: same, note: "as above"}
  - {name: desflurane_first_hour_elimination, value: 40, unit: "% faster than sevoflurane", page: 19, source: "Kinetics and Recovery (ref 52)", note: "first hour of recovery, after 2, 4 or 8 h at 1.25 MAC"}
```

The non-quantitative model statements the chapter rests on, each of which stanpumpR either uses or
does not (see *Verification*):

- **p. 6-7.** FA/FI rises faster the less soluble the agent. Raising ventilation moves it little for
  a poorly soluble agent and nearly in proportion for a highly soluble one; raising cardiac output
  **hinders** the rise, with the same solubility dependence (refs 15, 16).
- **p. 8-9.** Ventilation and cardiac output rising **together** speeds the rise rather than
  cancelling, because the extra flow also shortens tissue equilibration and so returns higher
  tensions to the lungs (ref 26). Tested across species by body size (p. 9, Wahrenbrock, ref 27, now
  queued): faster in a rat than a human than a whale. The chapter offers fever and hyperthyroidism as
  the clinical cases, metabolism, CO2 production, ventilation and blood flow rising roughly in
  proportion.
- **p. 9-10.** Redistribution matters as much as the total. A greater VRG share speeds the rise - the
  young patient, who also has a smaller FRC, against a higher MAC (p. 9-10, refs 26, 28, 29); a
  greater muscle or fat share slows it (p. 10: exercise, weight reduction, a beta-agonist, refs 30,
  31); and in hypovolaemic shock, where cerebral flow is preserved at the expense of everything else,
  "In this reduced heart-brain circuit, FA/FI rises extremely fast" (p. 9), with a corresponding risk
  of cardiovascular collapse.
- **p. 8.** Anaesthetic depression of ventilation is a **negative** feedback and so protective during
  spontaneous breathing (refs 17-23), while depression of circulation is a **positive** feedback that
  can run away to collapse; controlled ventilation bypasses the protective loop. Surgical stimulation
  opposes both (refs 24, 25).
- **p. 11.** Falling body temperature raises solubility in blood and every tissue, lowers metabolism
  and so ventilation and cardiac output, and lowers MAC rectilinearly (refs 32-35); the net is slower
  induction, more so the more soluble the agent.
- **p. 12-13.** A V/Q mismatch opens an FA-Fa difference whose size depends on solubility; it arises
  from disease, from anaesthetic bronchodilation, and from such routine things as paralysis and
  controlled ventilation (p. 12, ref 36). **Endobronchial intubation** is the extreme case (p. 12):
  one lung ventilated at twice normal, the other perfused and not ventilated. For a poorly soluble
  agent the doubled ventilation cannot compensate, FA being unable to exceed FI, and the arterial
  partial pressure is initially about half normal; for a highly soluble agent doubling the delivery
  doubles the blood partial pressure and compensates exactly, so the arterial rise is not delayed
  (p. 12). Predicted from an analogue model (ref 37 = Eger and Severinghaus, Anesthesiology
  1964;25:620-6, now queued) and confirmed in dogs and people (ref 38 = Stoelting and Longnecker,
  Anesthesiology 1972;36:352-6, now queued). The chapter draws the oxygen analogy explicitly (p. 13):
  poorly soluble oxygen cannot be compensated, which is why hypoxaemia is the risk of one-lung
  ventilation while hypercarbia rarely is.
- **p. 16-18.** Duration and concentration govern recovery, because the slow compartments do not
  reach steady state within a clinical case (p. 16-17); emergence relates to MAC-hours with a slope
  set by solubility (p. 17-18, refs 33, 51).
- **p. 21-22.** Metabolism is **saturable**, and so matters only at trace concentrations - which is
  why leaving it out of the uptake model was safe at anaesthetic concentrations (p. 21). Drug
  released from fat during a long recovery emerges at trace concentrations and is therefore more
  heavily metabolised (p. 22).
- **p. 14-15.** Cyclopropane violates Henry's law, binding haemoglobin saturably (p. 14), which is
  what resolved Rackow's 5% discrepancy against nitrous oxide; nitrous oxide obeys it, and Lockhart
  found isoflurane and halothane obey it in brain (p. 14, ref 41). The intertissue-diffusion
  speculation was later demonstrated by others for kidney to perirenal fat (p. 16, ref 49 - see
  *open_questions*).
- **p. 25-26.** The rebreathing circuit's own structure changes the kinetics (p. 25, refs 77, 78):
  three requirements for CO2-free rebreathing - one-way valves on both limbs, an overflow valve not
  between the inspiratory valve and the patient, and fresh gas entering between the patient and the
  inspiratory valve (p. 25) - and three fresh-gas inlet placements of decreasing efficiency (p. 26),
  the most efficient equivalent to the Magill (Mapleson A) circuit (ref 79 = Mapleson, Br J Anaesth
  1954;26:323-32).

## Pharmacodynamics and end points
No PK/PD model is fitted, and no ke0, EC50 or MAC value is given for any agent. On **p. 6** the
chapter records that inhaled agents alter the EEG (refs 12, 13), that Faulconer proposed EEG
waveforms as a surrogate for effect-site concentration and that Eger demonstrated it for ether
(ref 14), and credits Hull 1978 (pancuronium, Br J Anaesth 50:1113-23) and Sheiner 1979
(d-tubocurarine, Clin Pharmacol Ther 25:358-71) with working out plasma-to-effect-site
equilibration, calling the effect site one of the fundamental concepts of anaesthetic pharmacology;
the same page notes that the effect site is not always the brain (refs 10, 11). End points named in
the recovery work (**pp. 19-20**, McKay, ref 54): response to command, orientation to place and date,
and swallowing 20 mL of water without coughing or drooling, which recovered sooner after desflurane
than sevoflurane, correlated with MAC-hours for sevoflurane but not desflurane, and worsened with
body mass index for sevoflurane but not desflurane.

## Covariates
None in the parameter sense. The patient factors the chapter says change the kinetics, with its
reasoning, are **age**, **body temperature**, **cardiac output and ventilation** and their
**distribution**, **metabolic rate** (fever, hyperthyroidism, surgical stimulation), **body size
across species**, and **V/Q mismatch** from disease, paralysis, controlled ventilation or
endobronchial intubation.

## Caveats
- **A pre-publication draft of a published chapter.** The book appeared in 2021 (LWW); this file is
  the draft of 29 July 2017, weeks before Eger's death. So its wording is not the published wording,
  its editorial queries are not in the published book, and any passage here may have been cut,
  rewritten or corrected before print. A book chapter is also not peer-reviewed in the sense a
  journal paper is. `screen_inreview.py` clears it, correctly: it is not a manuscript under peer
  review.
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
- Typographical defects of a draft, not corrected here: a year missing its leading digit (p. 6), a
  became/because slip (p. 7), an unresolved four-to-five edit that leaves both numbers standing
  (p. 16), one sentence whose two halves are identical (p. 24), and - in the key equation of the
  last section (p. 28) - the partition-coefficient symbol itself missing from the text layer, so
  that the relation reads with a blank where lambda should be. The equation is recoverable only from
  Shafer's own bracketed note that he substituted symbols for what he could not read.
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

Five of the chapter's references were absent from the archive and are now queued
(`stanpumpr-queue.csv`, noted as wanted by this card):

| PMID | Chapter ref | Why |
|---|---|---|
| 14211488 | 37 | Eger and Severinghaus 1964, the analogue-model prediction behind `R/endobronchial.R`, cited there without being held |
| 5020644 | 38 | Stoelting and Longnecker 1972, its confirmation in dogs and people, likewise |
| 4149271 | 27 | Wahrenbrock 1974, the across-species test of proportional ventilation and cardiac output |
| 5088786 | 57 | Halsey 1971, which would settle the denominator of the 50/35/10% metabolism figures |
| 5544623 | 58 | Sawyer 1971, the concentration dependence of hepatic halothane metabolism behind them |

None is free at PMC.
