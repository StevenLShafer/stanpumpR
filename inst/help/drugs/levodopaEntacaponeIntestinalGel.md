### The model

Levodopa-entacapone-carbidopa intestinal gel (Lecigon) is pumped into the jejunum like [levodopa-carbidopa gel](help:drugs/levodopaIntestinalGel), with entacapone added to block levodopa's second route of metabolism. It is approved in Sweden and other European countries, not in the US.

The model is Senek, Nyholm and Nielsen's (*Sci Rep* 2020). Eleven patients in Uppsala with advanced disease took each gel on consecutive days, with 14 hours of infusion and sampling to 17 hours. The model has one compartment:

- **Absorption.** Effectively immediate (fixed at 50 per hour).
- **Volume.** 74.5 L for 70 kg.
- **Clearance.** 27.9 L/h for 70 kg on the levodopa-carbidopa gel, and **36.5% lower, 17.7 L/h, with entacapone**.

Both the volume and the clearance scale with weight. The motor effect site is the one used for every levodopa model in the library (Mao 2013, equilibration half-life 0.39 h). Entacapone slows levodopa's elimination, not its action, so the effect site is unchanged.

### How to dose it

Enter the morning dose and any extra doses as **mg PO** and the continuous rate as **mg/hr PO**, all as levodopa.

The lower clearance means a continuous rate gives 1.58 times (1/0.635) the steady-state levodopa it would on Duodopa in Senek's own model, against the study's levodopa-carbidopa gel arm. The authors therefore conclude that the continuous rate should be about **35% lower** when switching from levodopa-carbidopa gel. In the original study the rate was 20% lower, and levodopa concentrations climbed through the day. At steady state:

- 60 mg/h of Lecigon gives 3.4 mcg/mL;
- 39 mg/h, 35% less, gives 2.2 mcg/mL, the steady state of 60 mg/h of Duodopa in Senek's model.

The library's own [levodopa intestinal gel](help:drugs/levodopaIntestinalGel) is a different model of Duodopa (Othman and Dutta 2014: clearance 24.8 L/h, bioavailability 0.97), so beside it the ratio is smaller, about 1.44: at 60 mg/h it plateaus at 2.35 mcg/mL against Lecigon's 3.4. Comparing the two drugs on one plot therefore understates the difference Senek measured within the same patients.

### Genotype

All eleven patients were genotyped for COMT (rs4680, Val158Met) and for two DDC variants. The comparison was graphical only, because there were too few patients for a formal covariate analysis:

- **COMT.** Entacapone lowered levodopa clearance in every patient, and by a similar amount whatever their COMT genotype. This contrasts with Corvol 2011, where oral entacapone did more in high-activity Val/Val patients.
- **DDC.** Patients with higher DDC activity tended to have a higher clearance.

No genotype term is applied.

### What is not in the model

- **Early over-prediction.** The model over-predicts the first hours on Lecigon, for reasons the authors could not identify.
- **Meals.** Protein meals moved the concentrations, but no food effect could be fitted.
- **Second compartment.** A two-compartment model was unstable on these data.
- **Entacapone itself.** Entacapone is not plotted.
