### A research model, behind an opt-in

Diamorphine (diacetylmorphine, heroin) is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or an individual clinical prediction, and it describes pharmaceutical diamorphine given by the studied routes, not street heroin, whose composition, co-exposures and routes differ.

### The model

The pharmacokinetics are Cai and colleagues' 2025 adult population model (Cai L, Zhai J, Ji B, et al. *CPT Pharmacometrics Syst Pharmacol* 2025;14(3):435-447), a mixed-effects parent → 6-monoacetylmorphine (6-MAM) → morphine cascade fitted to intramuscular (IM) and intranasal (IN) pharmaceutical diamorphine hydrochloride in ten adult male regular heroin users.

### What is plotted — parent diamorphine only

stanpumpR's closed-form engine resolves at most one metabolite level, so only the **parent diamorphine** disposition is plotted here; the 6-MAM and morphine analytes are **not modelled**. In Cai's structure the diamorphine central compartment is one-compartment: it is filled by first-order absorption and emptied only by conversion to 6-MAM. The parent plasma curve is therefore reproduced exactly from Cai's central volume (8.21 L at 70 kg) and that conversion rate constant (103 h⁻¹) used as the elimination rate, giving a very short diamorphine half-life (about 0.4 min at 70 kg). Because 6-MAM and morphine — which carry the clinical opioid effect — are not modelled, there is **no effect site** (the plot is plasma only) and no MEAC or typical-concentration band.

### Routes and bioavailability

Diamorphine is offered **intranasally and intramuscularly** only, matching the model's data. Intramuscular is the bioavailability reference (F = 1, a relative reference, not a measured absolute bioavailability); intranasal F is 0.519 relative to IM. First-order absorption uses Cai's absorption rate constant (3.04 h⁻¹). Intravenous, smoked, and foil-heated-vapour heroin are **not** represented — those follow different models (Rook and colleagues, 2006) that are not implemented.

### Covariates

Cai carries its own allometric size covariate on body weight: volumes scale linearly, clearance to the 0.75 power, and first-order rates to the −0.25 power. With *Adjust weight to fat-free mass* on, the covariate is evaluated at the patient's pharmacokinetic (fat-free-mass) weight; with it off, at total body weight, reproducing the published scaling. The model was fitted in adults; its paediatric extrapolation and age-maturation terms are not reproduced here.

### Where to be careful

This is a single-analyte, plasma-only exposure estimate of a model whose clinically relevant species (6-MAM, morphine) are deliberately omitted. Read it only as the parent diamorphine plasma time course after pharmaceutical IM or IN diamorphine, within the studied population and dose range — not as an effect, a potency, or a prediction for any individual.
