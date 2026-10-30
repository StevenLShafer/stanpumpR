### The model

Eslicarbazepine acetate (Aptiom) is a prodrug, hydrolysed almost completely on first pass to **eslicarbazepine**, the active moiety, which is what the curve shows. The parameters are from Falcao and colleagues (*CNS Drugs* 2012;26:79-91), 641 phase III patients, with absorption from the Simulations Plus AAPS 2013 poster: one compartment, apparent oral, clearance 2.43 L/h and volume 61.3 L at 70 kg (allometric), absorption 2.34/h. The half-life is about 17 hours.

### Dose basis

The dose is entered as mg of eslicarbazepine **acetate** (the product strength); eslicarbazepine is 0.858 of that by molecular weight, applied through the oral bioavailability. Once-daily dosing gives a trough near 7.4 mg/L at 800 mg (Sunkaraneni 2018), which the model reproduces.

### What is not modelled

Enzyme inducers raise clearance (Falcao and colleagues: phenytoin or phenobarbital adds about 1.4 L/h, and carbamazepine an amount that grows with its dose); this is applied only when the **Include antiseizure drug interactions** box in Graph Options is ticked (see [Antiseizure drug interactions](help:models/antiseizure-interactions)). No creatinine-clearance term was found in this cohort. There is no effect site.

### Band

3 to 35 mg/L at trough, the range of the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 40 mg/L), based on an expert guideline (Jacob and colleagues, 2016) and oxcarbazepine's well-studied range for its active metabolite. Norway's national ranges use a narrower, eslicarbazepine-specific 3 to 26 mg/L (12 to 100 µmol/L; Svendsen and colleagues, *Epilepsy Res* 2017;129:125-131, adopted by Reimers and colleagues, *Drug Des Devel Ther* 2018;12:271-280, https://doi.org/10.2147/DDDT.S154388), on the ground that eslicarbazepine is about 80% of oxcarbazepine's racemic metabolite.
