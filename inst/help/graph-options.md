The second panel in the left sidebar. These settings change how the simulation is displayed, and in three cases (Max time, the opioid-MAC interaction and Minto / Schnider) what is computed.

| Option | What it does |
|---|---|
| **Show typical** | The shaded band behind each drug: the therapeutic *Range*, a narrow band at the *Mid* value, or none. See [Reading the plot](help:reading-the-plot). |
| **Normalize to** | Rescale every curve to its own peak plasma or peak effect-site value, so drugs on very different scales can be compared in shape. Only the normalized line is shown. See [Normalization](help:models/normalization). |
| **Max time** | How far the simulation runs. The choices follow the **Time units** in the Time card: 1 to 24 hours for minutes and hours, 2 to 365 days for days, 4 to 52 weeks for weeks (see [Time display](help:time-display)). On plots longer than a day the grid of evaluation times scales with it (see the note below). If a dose or event is near Max time or beyond it (within half an hour, for minutes and hours), the plotted range is extended to show it, in steps matching the axis, up to the unit's longest Max time. |
| **Plasma line** | Line style for the plasma concentration: none (the default), solid, dashed, dotted or dot-dash. |
| **Effect site line** | Line style for the effect-site concentration; solid by default. |
| **Y axis height** | The height of each panel, from short to tall. |
| **Time until threshold** | For each drug, how long until it would fall to its recovery threshold if delivery stopped now, drawn as a thin black line. Only available when normalization is off. See [Time until threshold](help:models/recovery). |
| **Log Y axis** | Log-scale the concentration axis. Unavailable while *Time until threshold*, Events or Interaction are showing. |
| **Include opioid - MAC interaction** | Report MAC equivalents relative to the opioid-reduced MAC, so the same end-tidal concentration counts for more when an opioid is on board. See [Opioid reduction of MAC](help:models/opioid-mac). |
| **Minto / Schnider** | Also run the Schnider (1998) propofol and Minto (1997) remifentanil models, the models most TCI pumps use, and draw each as a second line in the drug's panel beside the library's own curve (Eleveld's, or Kim's for remifentanil in an adult with a BMI of 30 or more). Under a target-controlled infusion the second model drives a pump of its own, drawn as a second line in the TCI rate panel. *Time until threshold* stays on the library's model. Enabled only while propofol or remifentanil is in the dose table. See [The Minto and Schnider models](help:models/minto-schnider). |

## Notes

**Max time and detail.** The simulation is evaluated at every dose and event time and at points that start close together after each dose and spread out after it. On plots of a day or less that spacing does not depend on Max time; on longer plots it is scaled to Max time, with no step longer than 1/500 of the plot (about 17 hours on a year-long one), and the first point after a dose comes later. A long plot therefore shows less of the fine structure after each bolus. Choose the shortest Max time that holds your question.

**Normalization hides a line.** Normalizing to peak plasma shows only the plasma line; normalizing to peak effect site shows only the effect-site line. This is intentional: the two would otherwise be normalized to different peaks and could not be compared.

**Line styles and the emailed slide.** The styles you choose are the ones that appear on the emailed PowerPoint slide.
