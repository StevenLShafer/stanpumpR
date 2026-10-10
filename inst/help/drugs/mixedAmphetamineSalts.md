### What is plotted

The plasma concentration of **d-amphetamine**, in ng/mL, after oral Adderall, entered as the labelled strength in mg of mixed amphetamine salts. Use **mg PO** for the immediate-release tablet (Adderall) and **mg PO XR** for the extended-release capsule (Adderall XR). The frequencies (**mg PO bid**, **mg PO XR qd**) repeat the dose. l-amphetamine is not plotted; see below.

### This model is fitted here, not published

No population pharmacokinetic model of Adderall itself has been published with its parameters. This model is a typical-patient curve fitted for stanpumpR to group means published by McGough and colleagues (*J Am Acad Child Adolesc Psychiatry* 2003;42:684-691), with values from the Adderall XR and Adderall prescribing information. McGough studied 51 children with ADHD, aged 6 to 12, with a mean weight of 37.8 kg (range 22 to 73 kg) and 86 per cent boys. Each child received a single 20 mg dose of Adderall XR, sampled from 0.5 to 24 hours, and then took one of five treatments once daily for a week (Adderall 10 mg, Adderall XR 10, 20 or 30 mg, or placebo). d- and l-amphetamine were measured separately.

### Dose basis

Each Adderall strength is equal masses of four salts: dextroamphetamine saccharate, dextroamphetamine sulfate, and the racemic amphetamine aspartate and amphetamine sulfate. From their formula weights, 20 mg of salts contains 12.51 mg of amphetamine base. That matches the 12.5 mg "total amphetamine base equivalence" printed in the Adderall XR label. Of the base, 9.50 mg is d-amphetamine and 3.02 mg l-amphetamine, the labels' "3:1". So **0.4749 of the entered dose is converted to d-amphetamine**, once. That factor is shown as the bioavailability, but it is a dose basis, not an oral bioavailability.

### The model

One compartment. At 37.8 kg:

- **Apparent clearance, 10.1 L/h:** the 9.50 mg of d-amphetamine divided by McGough's area under the curve to infinity after XR 20 mg (936.7 ng·h/mL). McGough's steady-state groups give 10.4 to 12.2 L/h.
- **Apparent volume, 132 L:** from the label's 9 hour d-amphetamine half-life in children aged 6 to 12.
- **Absorption, 0.69 per hour, no lag:** fitted so that the peak after Adderall 10 mg once daily falls at McGough's 3.3 hours. The label gives about 3 hours.

### How Adderall XR is represented

The XR capsule contains two kinds of beads: about half the dose is released at once and half later. Here **each XR dose is given as half the dose at the time entered and half 4 hours later**, each absorbed as the immediate-release tablet. So Adderall XR 20 mg is exactly Adderall 10 mg twice, 4 hours apart, which is the comparison the label makes. The 4 hours was not imposed: fitting the delay to McGough's XR peak time of 6.8 hours gave 4.0 hours. The real release is gradual rather than two instantaneous pulses, so the first hour or so after each half is the least reliable part of the curve.

### How well it fits

Model against McGough's observed group means:

| | Model | Observed |
|---|---|---|
| XR 20 mg, single dose: peak | 50.5 ng/mL at 6.9 h | 48.8 ng/mL at 6.8 h |
| XR 20 mg, single dose: AUC 0-24 h | 741 ng·h/mL | 704 ng·h/mL |
| Adderall 10 mg daily, steady state: peak | 33.2 ng/mL at 3.3 h | 33.8 ng/mL at 3.3 h |
| XR 30 mg daily, steady state: peak | 91.8 ng/mL | 89.0 ng/mL |

The steady-state groups were 6 to 9 children each. A typical-patient curve is not the same as a mean of individual curves, and McGough's coefficients of variation were 28 to 56 per cent. An individual child's curve can sit far from this one.

### Covariates

McGough fitted no weight model, so the weight dependence is **borrowed** from Tsuda's pediatric d-amphetamine model ([lisdexamfetamine](help:drugs/lisdexamfetamine)): clearance scales with weight to the 0.600 and volume to the 0.776, from 37.8 kg. That rests on disposition being a property of d-amphetamine rather than of the product. It is an assumption, and the least certain part of the model away from McGough's weight range. With the [fat-free-mass switch](help:models/fat-free-mass) on, the equations see the pharmacokinetic weight; with it off, total weight.

### l-amphetamine

Not plotted. It is about a quarter of the base, its concentrations tracked d-amphetamine's almost exactly (correlation 0.997), and no behavioural effect has been shown to depend on it separately. From McGough's data and the label's 11 hour half-life in children, its apparent clearance is about 9.8 L/h and its volume about 155 L. Its curve would be about a third of d-amphetamine's height, peaking a little later.

### Not represented

- **Food:** a high-fat meal delays the XR peak by about 2.5 hours without changing the amount absorbed (label).
- **Urine pH:** acid urine speeds and alkaline urine slows the renal elimination of amphetamine.
- **CYP2D6:** not modelled.
- **Adolescents and adults:** extrapolations. The label gives longer half-lives in both (11 and 10 hours).
- **Other products:** Mydayis, Evekeo and dextroamphetamine products are different products, and entering them here would use the wrong dose basis or release.

### Effect site and typical range

None. McGough found no clear relationship between concentration and behavioural response, and no calibrated concentration-effect model exists for a classroom measure or for weekly symptom scores. No band is drawn, and nothing on this plot is a dose recommendation.
