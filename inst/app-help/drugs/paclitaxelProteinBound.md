### The model

Chen and colleagues (*J Clin Pharmacol* 2014;54:1097-1107) pooled 150 patients with solid tumours from eight studies of nab-paclitaxel (Abraxane), paclitaxel bound to albumin nanoparticles, given alone at 80 to 375 mg/m², nearly always over 30 minutes. Three compartments describe it:

- a central volume of 15.8 L, from which paclitaxel is eliminated at a rate that saturates (maximal 8.07 mg/h, half-saturated at 0.040 mcg/mL);
- a very large tissue compartment, 1650 L, which paclitaxel enters and leaves through a carrier that saturates as the plasma concentration rises (maximal 325 mg/h, half-saturated at 4.26 mcg/mL in plasma);
- a smaller tissue compartment, 75.4 L, reached at an ordinary clearance of 41.6 L/h.

Because elimination saturates, clearance falls as the dose rises: about 28 L/h per m² at 80 mg/m² and 14 at 300 mg/m², as the label reports (13 to 30). The terminal half-life is about 20 hours (the label: 13 to 27).

### Albumin

The maximal elimination rate rises with serum albumin (to the power 0.554, about 3.9 g/dL): a patient with albumin of 2 g/dL clears paclitaxel about 30% more slowly at high concentrations. stanpumpR uses the Patient Profile's **Serum albumin**, or 3.9 g/dL when it is blank. Weight, body-surface area, age, sex and kidney function had no effect in the study. Following the library's rule that every parameter is scaled to body size, since a cohort of similar sizes cannot show that size does not matter, the volumes and maximal rates are scaled to fat-free mass, anchored at the study's cohort (median 74 kg, 57 years, 60% women), so a patient like the cohort's receives the published values. With **Adjust weight to fat-free mass** unticked they are the published values for everyone.

### What is plotted

Total plasma paclitaxel, in mcg/mL. For nab-paclitaxel, Chen 2014 found that the time above 720 ng/mL (0.72 mcg/mL, 0.84 µmol/L) best predicted a fall of 50% or more in the neutrophil count; the time above 0.05 µmol/L (0.043 mcg/mL), the threshold used for solvent-based paclitaxel, was not significant. Neither is a therapeutic range, so no band is drawn.

### Where to be careful

- **Nab-paclitaxel only.** Solvent-based paclitaxel (Taxol, in Cremophor) is a different drug in stanpumpR, [paclitaxel](help:drugs/paclitaxel), with different kinetics.
- **One reading of the figure.** The paper's diagram can also be read with each direction saturated by its own side's concentration; the two readings differ by at most 3.5%.
- **The paper's units.** Table 2 prints the maximal rates in mg/h and the half-saturating concentrations in mg/L; they are micrograms. The paper's own comparison with solvent-based paclitaxel, and the label's total volume (1741 L, the sum of the three volumes), agree only in micrograms.
- **Liver.** The label reports a 22-26% lower maximal elimination rate in moderate and severe hepatic impairment, which the model does not carry.
- **Pregnancy:** no pharmacokinetic data were found for nab-paclitaxel; nothing is changed.
