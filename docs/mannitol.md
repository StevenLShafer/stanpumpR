# Mannitol in stanpumpR

Mannitol is the library's first osmotic agent. stanpumpR plots it as the
**serum osmolality** it is predicted to produce, not as a drug concentration:

```
osmolality(t) = baseline + 0.555 × Cp(t)
```

- `Cp(t)` is the plasma mannitol concentration in mOsm/L (1 mmol of mannitol is
  1 mOsm, because it does not dissociate; 1 g is 1000 / 182.17 = 5.49 mOsm).
- `baseline` is the patient's serum osmolality before mannitol, in mOsm/kg. It is
  the **Serum osmolality** field in the Patient Profile, 290 by default, and the
  `osmolality` argument of `getDrugPK()` and `simulateDrugsWithCovariates()`.
- 0.555 is the net rise in measured osmolality per mOsm/L of plasma mannitol
  (see *The pharmacodynamic link*, below).

Doses are entered in g, g/kg, g/hr or g/kg/hr. The shaded band is 300–320
mOsm/kg, the usual target range during osmotherapy, with 320 the conventional
ceiling. Mannitol has no effect site, so only the plasma line is drawn, and the
panel's y-axis starts just below the baseline instead of at zero.

The model is in `R/drugs_mannitol.R`; the transform is applied at the end of
`simCpCe()`.

---

## Where this came from, and what changed

The starting point was a Python prototype written with ChatGPT
(`mannitol_pkpd.py`, version 1.1, literature searched to 2026-10-06). It
reconstructed the Kaneda 2010 population means, offered both of Kaneda's
dose-group clearances, added a pediatric extension, and linked plasma mannitol
to osmolarity as `background + 1 × Cp`. Its numerical verification (mass
balance, an independent ODE solve, superposition) was sound, and stanpumpR's
closed-form solution reproduces the same three-compartment model to machine
precision. Three of its choices were changed after checking them against the
literature.

### 1. The PK model: Kaneda 2010 — confirmed

Kaneda K, Baker MT, Han TH, Weeks JB, Todd MM. *Pharmacokinetic characteristics
of bolus-administered mannitol in patients undergoing elective craniotomy.* J
Clin Pharmacol 2010;50(5):536–543. [PMID 20051588](https://pubmed.ncbi.nlm.nih.gov/20051588/),
[doi:10.1177/0091270009348973](https://doi.org/10.1177/0091270009348973).

A PubMed search for mannitol pharmacokinetic studies (titles containing
*mannitol* and *pharmacokinetic*, *kinetics* or *disposition*) found no other
population model of therapeutic intravenous mannitol in adults. The
alternatives are older two-compartment fits in a handful of subjects (Cloyd
1986, n = 4; Rudehill 1993, n = 15), studies of oral mannitol, neonatal data,
and mannitol used as a GFR marker. Kaneda is the right choice. The parameters
below were checked against the abstract and match the prototype:

| | V1 | V2 | V3 | CL1 | CL2 | CL3 |
|---|---|---|---|---|---|---|
| Kaneda population mean | 2.80 L | 8.86 L | 12.0 L | 0.04 / 0.07 L/min | 2.07 L/min | 0.16 L/min |

Kaneda found weight to be a covariate on V2 and dose on CL1. Only the abstract
was available to us, so those covariate equations, the variability and the
reference weight are not reconstructed. The prototype's documentation says the
same.

### 2. Clearance: 0.07 L/min, not 0.04 — changed

Kaneda reported clearance separately for the two dose groups: 0.04 L/min after
0.5 g/kg and 0.07 L/min after 1.0 g/kg. stanpumpR simulates linear kinetics and
cannot switch clearance on dose, so one value had to be chosen. The prototype's
example used 0.04.

An independent study settles it. Rudehill et al. gave 1 g/kg over 30 minutes to
15 neurosurgical patients and measured plasma mannitol for 8 hours.
Simulating their regimen with the Kaneda model (70 kg):

| | End of infusion | 8 hours | Terminal half-life |
|---|---|---|---|
| Rudehill, observed | 5.91 mg/mL | 0.58 mg/mL | 2.4 h |
| Kaneda, CL 0.07 L/min | 4.85 mg/mL | 0.68 mg/mL | 4.4 h |
| Kaneda, CL 0.04 L/min | 5.06 mg/mL | 1.23 mg/mL | 7.3 h |

With 0.04 the eight-hour concentration is twice the observed value. 0.07 L/min
is also closer to the clearances others report (Rudehill, 87 mL/min) and to
the glomerular filtration rate, which is how mannitol is cleared. Both settings
underpredict the end-of-infusion peak by about 15%, and both have a longer
terminal half-life than the 1.2 to 2.4 hours reported elsewhere. Kaneda sampled
for 12 hours, long enough to see a slow phase that shorter studies missed. That
part of the curve remains the least certain.

The 0.5 g/kg dose group cleared mannitol more slowly. After small or repeated
doses the model may therefore underpredict how long the osmolality stays up.

### 3. The pharmacodynamic link: 0.555, not 1 — changed

The prototype linked osmolarity to plasma mannitol as `background + φ × Cp`
with φ = 1, the ideal-solute assumption, and noted that this was not estimated
from clinical data. In patients the measured rise is much smaller than the
mannitol concentration.

Mannitol stays in the extracellular fluid and draws water out of cells. That
water dilutes sodium and every other endogenous solute, so the calculated
osmolality *falls* while the mannitol concentration rises. The osmolal gap
still equals the mannitol concentration, which is why the gap is used to
monitor mannitol, but the measured osmolality rises by less. Rudehill measured
both in the same patients. Mean plasma mannitol peaked at 5.91 mg/mL
(32.4 mOsm/L) while serum osmolality rose from 292 to 310 mOsm/kg:

```
φ = (310 − 292) / (5.91 × 1000 / 182.17) = 0.555
```

With φ = 1, the Kaneda model predicts a rise of 27 mOsm/kg after 1 g/kg over
30 minutes. With φ = 0.555 it predicts 15. Rudehill observed 18. The
ideal-solute link overpredicts the peak osmolality, so it would understate how
much mannitol can safely be given before the 320 mOsm/kg ceiling. Because φ was
fitted from paired measurements, it does not depend on the PK model.

φ is a single constant from a single study. The true relationship changes with
time: as cells equilibrate the rise should shrink, and as the osmotic diuresis
removes free water it should grow. The constant describes the first hour or so
best.

### 4. Body size: stanpumpR's fat-free-mass scaling — changed

The prototype scaled an exploratory pediatric model with total body weight,
fat-free mass supplied separately, and a renal maturation function of
postmenstrual age (O'Hanlon 2023). stanpumpR does not collect postmenstrual age,
and every model in the library must follow `docs/weight-adjustment.md`.
Mannitol therefore uses the library rule. The population means are taken to
describe the 70 kg reference adult. Volumes scale with fat-free mass relative
to the 54.5 kg reference, and clearances with that ratio to the 0.75 power. With
**Adjust weight to fat-free mass** off, the published means are used unscaled.
This matches Kaneda's own finding that weight-based dosing gave higher than
expected concentrations in obese patients.

No renal maturation is applied. In infants the model will overpredict
clearance, and it has not been validated in children.

### Not modelled

- **Renal function.** Mannitol clearance tracks GFR. stanpumpR's renal-function
  field is not yet connected to any model. In renal impairment the predicted
  osmolality falls far too fast.
- **Intracranial pressure.** No ICP model has been published that could be
  attached. Kobayashi 1994 related ICP reduction to the plasma-to-peripheral
  mannitol gradient, which is suggestive but was not fitted to a model.
- **Water and sodium.** Diuresis, the infused water, and the resulting changes
  in sodium are not modelled. Only their average net effect at the peak is
  captured, through φ.
- **Variability.** The curve is a typical-patient prediction.

---

## References

- Kaneda K et al. J Clin Pharmacol 2010;50(5):536–543. [PMID 20051588](https://pubmed.ncbi.nlm.nih.gov/20051588/) — the PK model.
- Rudehill A et al. J Neurosurg Anesthesiol 1993;5(1):4–12. [PMID 8431668](https://pubmed.ncbi.nlm.nih.gov/8431668/), [doi:10.1097/00008506-199301000-00002](https://doi.org/10.1097/00008506-199301000-00002) — the external check, and φ.
- Cloyd JC et al. J Pharmacol Exp Ther 1986;236(2):301–306. [PMID 3080582](https://pubmed.ncbi.nlm.nih.gov/3080582/) — mannitol and serum osmolality in four humans; elimination half-life 71 min.
- Kobayashi et al. Acta Neurochir Suppl 1994;60:538–540. [PMID 7976642](https://pubmed.ncbi.nlm.nih.gov/7976642/), [doi:10.1007/978-3-7091-9334-1_148](https://doi.org/10.1007/978-3-7091-9334-1_148) — mannitol gradient and ICP.
