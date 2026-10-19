## Parkinson disease drugs

The drugs used to treat Parkinson's disease are listed under **Parkinson disease** in the startup menu: [levodopa](help:drugs/levodopa) (carbidopa-levodopa tablets) and [levodopa intestinal gel](help:drugs/levodopaIntestinalGel), [pramipexole](help:drugs/pramipexole) and [extended-release pramipexole](help:drugs/pramipexoleER), [safinamide](help:drugs/safinamide), [opicapone](help:drugs/opicapone), [istradefylline](help:drugs/istradefylline), [extended-release amantadine](help:drugs/amantadineER) (the Osmolex ER tablet) and, for Parkinson's disease psychosis, [pimavanserin](help:drugs/pimavanserin). [Zonisamide](help:drugs/zonisamide), approved in Japan for Parkinson's disease at 25 to 50 mg a day, stays under **Antiseizure**.

Every model is a published population model whose parameters were read in full, most of them in the FDA's own clinical pharmacology reviews. Levodopa is the only one drawn with an effect site, because it is the only one with a published, fitted equilibration between plasma and effect. Every other drug is a plasma curve: for some (pramipexole, istradefylline) the trials found no concentration-effect relation to fit, and for the enzyme inhibitors the plasma concentration is not the effect at all (below).

### Timing: when the effect peaks

**Levodopa** acts within half an hour of a tablet in advanced disease and its effect peaks about 40 minutes after its plasma peak. Mao and colleagues fitted the motor response (UPDRS Part III) of 27 patients with advanced disease to their measured plasma levodopa and found an equilibration half-life of 0.39 hours (ke0 1.80 per hour); tapping speed equilibrates a little more slowly (0.59 hours) and dyskinesia at about the same rate (0.45 hours). The model draws that motor effect site, so after a 200 mg tablet the plasma peaks at about 80 minutes and the effect site at about 2 hours. **This is a property of advanced, fluctuating disease.** In early disease the response to a single dose is blunted by a long-duration response that builds and decays over days, and the short-duration response equilibrates far more slowly: the equilibration half-life falls by about 17% a year as the disease advances (Contin 1998), from about 3 hours in Hoehn and Yahr stage 1 to 45 minutes in stage 4 (Triggs 1996, as Mao quotes it). Neither was read in full; the model uses only Mao's full-text values.

With **time until threshold** turned on, levodopa's threshold is Mao's half-maximal UPDRS concentration, 0.81 mcg/mL, so the plot shows when each dose is expected to wear off. [Load the wearing-off scenario](scenario:levodopa-wearing-off) to see it.

**Apomorphine** injected under the skin acts in 7 to 14 minutes and peaks at 20 to 30 minutes (7 and 27 minutes to a clinically relevant and to the maximal response after 4 mg, Nasser 2024); the sublingual film takes about four times as long. It is not in the library yet: see *Where each drug stands*. The **dopamine agonists** taken by mouth, the **MAO-B inhibitors**, amantadine and istradefylline build their effect over days to weeks of titration rather than dose by dose.

### The plasma is not always the effect

Rasagiline and selegiline inhibit MAO-B **irreversibly**: the inhibition lasts until the enzyme is resynthesised, more than a week in platelets and with a 40-day half-time in the brain (Fowler 1994), long after the drug has gone. Safinamide's inhibition is reversible, but platelet MAO-B stays inhibited for days. Opicapone binds COMT so tightly that red-cell COMT inhibition has a half-life of 62 hours after one dose and over 100 hours after repeated doses, against a plasma half-life of about an hour; its plasma curve falls to nothing within hours of each bedtime capsule while its effect on levodopa (60 to 80% more exposure) persists. Entacapone and tolcapone, by contrast, inhibit COMT only while they are present: entacapone's inhibition is gone within 8 hours. For these drugs the plasma curve shows the exposure, and the help page of each says what the effect does.

### Therapeutic ranges

Few drugs here have one. Those that are published are drawn as the band:

- **Levodopa**: 0.9 to 2.0 mcg/mL one hour after 250/25 mg carbidopa-levodopa, the reference range of the AGNP consensus guideline (Hiemke 2018), an expected peak rather than an efficacy window. Mao's half-maximal motor concentration is 0.81 mcg/mL, and on intestinal gel the plateau is typically 1.5 to 4 mcg/mL.
- **Amantadine**: 300 to 600 ng/mL at trough (AGNP). Confusion and hallucinations rise with the concentration, most in the elderly and in renal impairment.
- **Entacapone** 0.4 to 1.0 mcg/mL one hour after 200 mg and **tolcapone** 3 to 6 mcg/mL two hours after a dose (AGNP), although monitoring them is not recommended. **Biperiden**, not marketed in the US, 1 to 6.5 ng/mL.
- **Apomorphine**: the motor response appeared at a mean of 4.7 ng/mL (1.4 to 10.7) and dyskinesia at 8.5 ng/mL in van Laar's infusion study (an abstract).

The AGNP values are quoted from Müller and colleagues' review of therapeutic drug monitoring in Parkinson's disease (*J Neural Transm* 2024), which was read in full; the guideline itself could not be read here. No range is established for pramipexole, ropinirole, rotigotine, the MAO-B inhibitors, opicapone, istradefylline or pimavanserin.

### Covariates and genotype

**Renal function** matters most for pramipexole (90% excreted unchanged; the model's clearance falls 0.74% per mL/min of creatinine clearance below 121) and amantadine (its elimination is proportional to the eGFR, so its half-life is 16 hours at an eGFR of 90 and 73 hours at 20), and both models read the Patient Profile's serum creatinine. **Body size** enters levodopa's central volume, pramipexole's peripheral volume, safinamide's clearance and volume, and istradefylline's through lean body mass. **Age** lowers safinamide's, istradefylline's and pimavanserin's clearance; **height** raises pimavanserin's. **Sex** lowers opicapone's clearance 14% in women; women's levodopa exposure is 27 to 40% higher, mostly through their weight, which the levodopa model carries.

**Interactions the Patient Profile cannot express** are often larger than any of these: smoking lowers istradefylline exposure about 40% and raises rasagiline clearance 39%, ropinirole is a CYP1A2 substrate (ciprofloxacin +84%, smoking -38%), pimavanserin and istradefylline are CYP3A4 substrates, the COMT inhibitors raise levodopa's exposure, protein meals compete with levodopa for absorption, and food triples selegiline's. Each drug's page says which are held at the reference value.

**Genotype** has no quantified effect on the kinetics of any drug here, so none of these models reads a pharmacogenetic field. The best-studied gene is COMT: the Val158Met polymorphism does not change levodopa's own kinetics (Contin 2005), but patients with the high-activity Val/Val genotype gain more from entacapone, 62% more levodopa exposure against 34% in Met/Met (Corvol 2011). CYP1A2 alleles were not associated with rasagiline's kinetics (Zubiaur 2022). Selegiline is metabolised mainly by CYP2B6 in vitro, and pramipexole is secreted by OCT2, but neither has a measured genotype effect in patients. Variants of DDC, the dopamine transporter and DRD2 are associated with dose requirements and dyskinesia, not with concentrations.

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
