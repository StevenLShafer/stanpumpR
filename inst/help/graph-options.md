The second panel in the left sidebar. These settings change how the simulation is displayed, and in two cases (the opioid-MAC interaction and Minto / Schnider) what is computed. How far the simulation runs, **Max time**, is in the Time panel; see [Time display](help:time-display).

| Option | What it does |
|---|---|
| **Show typical** | The shaded band behind each drug: the therapeutic *Range*, a narrow band at the *Mid* value, or none. See [Reading the plot](help:reading-the-plot). |
| **Normalize to** | Rescale every curve to its own peak plasma or peak effect-site value, so drugs on very different scales can be compared in shape. Only the normalized line is shown. See [Normalization](help:models/normalization). |
| **Plasma line** | Line style for the plasma concentration: none (the default), solid, dashed, dotted or dot-dash. |
| **Effect site line** | Line style for the effect-site concentration; solid by default. |
| **Y axis height** | The height of each panel, from short to tall. |
| **Time until threshold** | For each drug, how long until it would fall to its recovery threshold if delivery stopped now, drawn as a thin black line. Only available when normalization is off. See [Time until threshold](help:models/recovery). |
| **% MEAC on opioid panels** | Each opioid's effect-site concentration as a percentage of its MEAC, drawn on its own panel on one scale shared by every opioid panel. Cannot be shown with *Time until threshold*. See [MEAC: comparing opioids](help:models/meac). |
| **Peripheral compartments** | Under each drug's panel, which is the plasma (V1), a panel for each further compartment of its model: V2 and V3, or an inhaled agent's muscle (MRG) and fat (FG) groups, and the depot of a dose given by mouth, under the tongue, intramuscularly, intranasally, subcutaneously or by tissue injection. Only available when normalization is off. See [Peripheral compartments and the depot](help:models/compartments). |
| **Log Y axis** | Log-scale the concentration axis. Unavailable while *Time until threshold*, Events or Interaction are showing. |
| **Include opioid - MAC interaction** | Report MAC equivalents relative to the opioid-reduced MAC, so the same end-tidal concentration counts for more when an opioid is on board. See [Opioid reduction of MAC](help:models/opioid-mac). |
| **Minto / Schnider** | Also run the Schnider (1998) propofol and Minto (1997) remifentanil models, the models most TCI pumps use, and draw each as a second line in the drug's panel beside the library's own curve (Eleveld's, or Kim's for remifentanil in an adult with a BMI of 30 or more). Under a target-controlled infusion the second model drives a pump of its own, drawn as a second line in the TCI rate panel. *Time until threshold* stays on the library's model. Shown only while propofol or remifentanil is in the dose table. See [The Minto and Schnider models](help:models/minto-schnider). |
| **Include antiseizure drug interactions** | Apply the pharmacokinetic interactions among the antiseizure drugs: enzyme induction by carbamazepine, phenytoin and phenobarbital (building and fading over about two weeks), inhibition and albumin displacement by valproate, and carbamazepine's induction of its own metabolism. Each drug changed is drawn beside its own curve without interactions. Shown only while the dose table holds an interacting pair, or carbamazepine. See [Antiseizure drug interactions](help:models/antiseizure-interactions). |

| **Additional plots** | Panels beneath the concentration plot: MEAC, Interaction and Events. See [Additional plots](help:additional-plots). |

## Only the options that apply

An option that applies to particular drugs is shown only while one of them is in the dose table: *% MEAC on opioid panels* and the MEAC plot while an opioid is, *Include opioid - MAC interaction* while an opioid and an inhaled agent are, the Interaction plot while propofol and an opioid are, *Minto / Schnider* while propofol or remifentanil is, the Hamberg warfarin model while warfarin is, and *Include antiseizure drug interactions* while the table holds an interacting pair or carbamazepine. A hidden option keeps its setting for when it returns.

## Notes

**Normalization hides a line.** Normalizing to peak plasma shows only the plasma line; normalizing to peak effect site shows only the effect-site line. This is intentional: the two would otherwise be normalized to different peaks and could not be compared.

**Line styles and the emailed slide.** The styles you choose are the ones that appear on the emailed PowerPoint slide.
