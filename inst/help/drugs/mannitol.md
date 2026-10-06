### The model

Mannitol's kinetics are from Kaneda and colleagues (*J Clin Pharmacol* 2010;50:536-543). They gave 0.5 or 1.0 g/kg over 15 minutes to 22 adults having elective craniotomy, and fitted a **three-compartment** model, the only population model of therapeutic intravenous mannitol in adults. Clearance in that study depended on the dose: 0.04 L/min in the 0.5 g/kg group and 0.07 L/min in the 1.0 g/kg group. The library uses 0.07, because it reproduces the eight-hour concentration of an independent 1 g/kg study (Rudehill and colleagues, *J Neurosurg Anesthesiol* 1993;5:4-12). With 0.04 the predicted eight-hour level is twice the one observed.

### What is plotted: serum osmolality

Mannitol is not shown as a drug concentration. The plot shows the **serum osmolality** it is predicted to produce:

osmolality = baseline + 0.555 × plasma mannitol (mOsm/L)

The baseline is the **Serum osmolality** field in the Patient Profile, 290 mOsm/kg by default. Enter the patient's measured value. One gram of mannitol is 5.49 mOsm.

The factor 0.555 is less than 1 because mannitol stays in the extracellular fluid and draws water out of cells. That water dilutes sodium, so the measured osmolality rises by only about half the mannitol concentration. The osmolal gap still equals the mannitol concentration. Rudehill measured both in the same patients: plasma mannitol peaked at 32.4 mOsm/L while osmolality rose from 292 to 310 mOsm/kg. Treating mannitol as an ideal solute (factor 1) would overpredict the peak rise by about 80%.

### Covariates

The published means are taken to describe the 70 kg reference adult and are scaled to fat-free mass. With *Adjust weight to fat-free mass* off they are used unscaled. Kaneda found that weight-based dosing gave higher than expected concentrations in obese patients, which is consistent with this scaling. The baseline osmolality affects nothing but the starting point of the curve.

### Effect site

There is none. The reduction in brain water follows the gradient between plasma and brain osmolality, not an effect-site concentration, and no ke0 has been published. Only the plasma line is drawn.

### Typical concentrations

The shaded band is 300 to 320 mOsm/kg, the usual target range during osmotherapy, with 320 the conventional ceiling. The y-axis of this panel starts just below the baseline instead of at zero.

### Where to be careful

Mannitol is cleared by glomerular filtration. Renal function is not modelled, so in renal impairment the predicted osmolality falls far too fast, while in practice mannitol accumulates. Kaneda's terminal half-life (4.4 hours) is longer than the 1.2 to 2.4 hours other studies report. The 0.555 factor comes from one study and fits the first hour best: as cells equilibrate the rise shrinks, and the osmotic diuresis concentrates the plasma. Neither intracranial pressure nor serum sodium is predicted. Infants mature their GFR over the first years of life, which is not modelled. The full validation is in `docs/mannitol.md`.
