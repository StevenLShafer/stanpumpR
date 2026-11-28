### The model

Zhang and colleagues (*Front Pharmacol* 2020;11:594562, [doi:10.3389/fphar.2020.594562](https://doi.org/10.3389/fphar.2020.594562)) fitted 236 serum concentrations from 159 hospitalised Chinese children (87 boys, 72 girls), 0.2 to 14 years old (median 3.7) and 2.9 to 69 kg (median 14.8). Nine in ten samples were steady-state troughs. One compartment describes them:

- clearance 0.0694 L/h × (1 + 2.82 × weight / 16.71 kg) × 0.882^(serum creatinine / 29.075 µmol/L)
- volume 1.39 L × 1.75^(weight / 16.71 kg)

At the median child the clearance is 0.014 L/h/kg and the volume 0.15 L/kg, the paper's values.

### When stanpumpR uses it

This is one of [two teicoplanin models](help:drugs/teicoplanin). stanpumpR uses it for children up to 16.5 years.

### Covariates

- **Weight** on clearance and volume, as fitted. With [the scaling switch](help:models/fat-free-mass) on, both read the pharmacokinetic weight.
- **Serum creatinine**, the child's own, as the source used it. A blank field is a normal creatinine for the child's age and sex.

### Where to be careful

The model was fitted to troughs. Its clearance is informed by them, but its volume, and so the size of the peaks it draws, much less so. A single compartment also cannot show teicoplanin's distribution or its long terminal phase: for a 15 kg child the half-life it draws is about 7 hours, where adults' terminal half-life is days.
