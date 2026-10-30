## Parkinson disease drugs

The drugs used to treat Parkinson's disease are listed under **Parkinson disease** in the startup menu: [levodopa](help:drugs/levodopa) (carbidopa-levodopa tablets, with a second model fitted in dyskinetic patients, [levodopaSimon](help:drugs/levodopaSimon)), [levodopa intestinal gel](help:drugs/levodopaIntestinalGel) and the entacapone gel [Lecigon](help:drugs/levodopaEntacaponeIntestinalGel), [pramipexole](help:drugs/pramipexole) and [extended-release pramipexole](help:drugs/pramipexoleER), [safinamide](help:drugs/safinamide), the COMT inhibitors [opicapone](help:drugs/opicapone) and [tolcapone](help:drugs/tolcapone), [istradefylline](help:drugs/istradefylline), [extended-release amantadine](help:drugs/amantadineER) (the Osmolex ER tablet), [subcutaneous apomorphine](help:drugs/apomorphine) for OFF episodes and, for Parkinson's disease psychosis, [pimavanserin](help:drugs/pimavanserin). [Zonisamide](help:drugs/zonisamide), approved in Japan for Parkinson's disease at 25 to 50 mg a day, stays under **Antiseizure**.

Every model is a published population model whose parameters were read in full, most of them in the FDA's own clinical pharmacology reviews. Levodopa (all four of its models) and apomorphine are drawn with an effect site, because they are the only ones with a published, fitted equilibration between plasma and effect. Every other drug is a plasma curve: for some (pramipexole, istradefylline) the trials found no concentration-effect relation to fit, and for the enzyme inhibitors the plasma concentration is not the effect at all (below).

### Timing: when the effect peaks

**Levodopa** acts within half an hour of a tablet in advanced disease and its effect peaks about 40 minutes after its plasma peak. Mao and colleagues fitted the motor response (UPDRS Part III) of 27 patients with advanced disease to their measured plasma levodopa and found an equilibration half-life of 0.39 hours (ke0 1.80 per hour); tapping speed equilibrates a little more slowly (0.59 hours) and dyskinesia at about the same rate (0.45 hours). The model draws that motor effect site, so after a 200 mg tablet the plasma peaks at about 80 minutes and the effect site at about 2 hours. **This is a property of advanced, fluctuating disease.** In early disease the response to a single dose is blunted by a long-duration response that builds and decays over days, and the short-duration response equilibrates far more slowly: the equilibration half-life falls by about 17% a year as the disease advances (Contin 1998), from about 3 hours in Hoehn and Yahr stage 1 to 45 minutes in stage 4 (Triggs 1996, as Mao quotes it). Neither was read in full; the model uses only Mao's full-text values. Simon 2016, fitted on its own plasma model in 30 patients with peak-dose dyskinesia, found an equilibration half-life of 0.51 hours for motor response and dyskinesia together, a higher half-maximal motor concentration (1.41 mcg/mL) and a much steeper response (Hill 4.26): that is [levodopaSimon](help:drugs/levodopaSimon).

With **time until threshold** turned on, levodopa's threshold is Mao's half-maximal UPDRS concentration, 0.81 mcg/mL, so the plot shows when each dose is expected to wear off. [Load the wearing-off scenario](scenario:levodopa-wearing-off) to see it.

**Apomorphine** injected under the skin acts in 7 to 14 minutes and peaks at 20 to 30 minutes (7 and 27 minutes to a clinically relevant and to the maximal response after 4 mg, Nasser 2024); the sublingual film takes about four times as long. The [model](help:drugs/apomorphine) is the FDA review's: the plasma peaks about 11 minutes after an injection and the motor effect site, equilibrating with a half-time of 7.8 minutes, at about 24. The **dopamine agonists** taken by mouth, the **MAO-B inhibitors**, amantadine and istradefylline build their effect over days to weeks of titration rather than dose by dose.

### The plasma is not always the effect

Rasagiline and selegiline inhibit MAO-B **irreversibly**: the inhibition lasts until the enzyme is resynthesised, more than a week in platelets and with a 40-day half-time in the brain (Fowler 1994), long after the drug has gone. Safinamide's inhibition is reversible, but platelet MAO-B stays inhibited for days. Opicapone binds COMT so tightly that red-cell COMT inhibition has a half-life of 62 hours after one dose and over 100 hours after repeated doses, against a plasma half-life of about an hour; its plasma curve falls to nothing within hours of each bedtime capsule while its effect on levodopa (60 to 80% more exposure) persists. Entacapone and tolcapone, by contrast, inhibit COMT only while they are present: entacapone's inhibition is gone within 8 hours. For these drugs the plasma curve shows the exposure, and the help page of each says what the effect does.

### Therapeutic ranges

Few drugs here have one. Those that are published are drawn as the band:

- **Levodopa**: 1.0 to 3.5 mcg/mL 0.5 to 1 hour after a dose, laboratory alert level 5 mcg/mL, the reference range of the AGNP consensus guideline's update 2026 (Hart 2026), the concentration expected after 100/25 mg rather than an efficacy window (the 2017 update gave 0.9 to 2.0 mcg/mL). Mao's half-maximal motor concentration is 0.81 mcg/mL, and on intestinal gel the plateau is typically 1.5 to 4 mcg/mL.
- **Amantadine**: 600 to 1,700 ng/mL 2 hours after the second dose of the day of immediate-release tablets, laboratory alert level 3,000 ng/mL (Hart 2026; the 2017 update gave 300 to 600 ng/mL at trough). Confusion and hallucinations rise with the concentration, most in the elderly and in renal impairment.
- **Tolcapone** 3 to 6 mcg/mL two hours after a dose and **entacapone** 0.4 to 1.0 mcg/mL one hour after 200 mg were the 2017 update's ranges (quoted by Müller 2024); the update 2026 lists neither, so tolcapone is drawn without a band. It removed biperiden, bromocriptine, cabergoline, pramipexole, ropinirole and rotigotine from the guideline altogether. **Pimavanserin**'s 40 to 85 ng/mL is for schizophrenia, and the guideline warns it may not apply in Parkinson's disease, so it is not drawn.
- **Apomorphine**: 3.9 to 15.2 ng/mL, the minimum effective and minimum toxic concentrations of van Laar's infusion study as the FDA review quotes them (the abstract gives 4.7 and 8.5 ng/mL), drawn as its band; half the maximal motor improvement at 10.7 ng/mL.

The AGNP values are from Table 5 of the guideline's update 2026 (Hart 2026, *Pharmacopsychiatry* 59:166-295, [doi 10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)), read in full; the 2017 values are quoted from Müller and colleagues' review of therapeutic drug monitoring in Parkinson's disease (*J Neural Transm* 2024). No range is established for pramipexole, ropinirole, rotigotine, the MAO-B inhibitors, opicapone, istradefylline or pimavanserin.

### Covariates and genotype

**Renal function** matters most for pramipexole (90% excreted unchanged; the model's clearance falls 0.74% per mL/min of creatinine clearance below 121) and amantadine (its elimination is proportional to the eGFR, so its half-life is 16 hours at an eGFR of 90 and 73 hours at 20), and both models read the Patient Profile's serum creatinine. **Body size** enters levodopa's central volume (and both of Simon's and Senek's parameters, allometrically), tolcapone's clearance and central volume through lean body weight, pramipexole's peripheral volume, safinamide's clearance and volume, and istradefylline's through lean body mass. **Age** lowers safinamide's, istradefylline's and pimavanserin's clearance; **height** raises pimavanserin's. **Sex** lowers opicapone's clearance 14% in women; women's levodopa exposure is 27 to 40% higher, mostly through their weight, which the levodopa model carries.

**Interactions the Patient Profile cannot express** are often larger than any of these: smoking lowers istradefylline exposure about 40% and raises rasagiline clearance 39%, ropinirole is a CYP1A2 substrate (ciprofloxacin +84%, smoking -38%), pimavanserin and istradefylline are CYP3A4 substrates, the COMT inhibitors raise levodopa's exposure, protein meals compete with levodopa for absorption, and food triples selegiline's. Each drug's page says which are held at the reference value.

**Genotype** has no quantified effect on the kinetics of any drug here, so none of these models reads a pharmacogenetic field. The best-studied gene is COMT: the Val158Met polymorphism does not change levodopa's own kinetics (Contin 2005), but patients with the high-activity Val/Val genotype gain more from entacapone, 62% more levodopa exposure against 34% in Met/Met (Corvol 2011). With entacapone infused in the gel (Lecigon) the fall in levodopa clearance, 36.5%, was seen in every one of 11 genotyped patients with no trend by COMT genotype, and higher DDC activity tended to a higher clearance (Senek 2020, graphical only). CYP1A2 alleles were not associated with rasagiline's kinetics (Zubiaur 2022). Selegiline is metabolised mainly by CYP2B6 in vitro, and pramipexole is secreted by OCT2, but neither has a measured genotype effect in patients. Variants of DDC, the dopamine transporter and DRD2 are associated with dose requirements and dyskinesia, not with concentrations.

### Product by product

What each product's US label and the literature say about its timing, range, covariates and genotype, whether or not a model is in the library. The labels are FDA prescribing information from Drugs@FDA; the European product information (EPAR) of the centrally authorised products (Azilect, Comtess, Inbrija, Mirapexin, Neupro, Numient, Ongentys, Stalevo, Tasmar, Xadago) is cited in `inst/extdata/parkinsonsProducts.csv` but could not be read from here.

<!-- generated: parkinsons-products-table -->

### Where each drug stands

One row per drug and candidate source model, from `inst/extdata/parkinsonsRegistry.csv`. "Needs the paper" means a model exists whose numbers could not be read here; "needs an engine feature" means a complete model was read whose input the library cannot yet carry (a patch, an inhaler, a chain of absorption compartments); "read; not offered" is a complete model kept as the record of an alternative or set aside for a stated reason.

<!-- generated: parkinsons-registry-table -->

### Two pieces of the engine built for this group

Levodopa's tablets and gel, and opicapone's capsules, are absorbed through **one transit compartment**: the dose passes through two equal first-order steps before it reaches the circulation, which makes the absorption start slowly and peak later than a single step would. The engine carries it exactly, as two parallel depots whose difference is the transit chain. The intestinal gel is a continuous **enteral infusion** into the jejunum (`mg/hr PO`), absorbed through that same chain. See [absorption](help:models/absorption).

### The papers that would unblock the rest

<!-- generated: parkinsons-to-obtain -->
