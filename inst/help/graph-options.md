The second panel in the left sidebar. These settings change how the simulation is displayed, and in two cases (Max time and the opioid-MAC interaction) what is computed.

| Option | What it does |
|---|---|
| **Show typical** | The shaded band behind each drug: the therapeutic *Range*, a narrow band at the *Mid* value, or none. See [Reading the plot](help:reading-the-plot). |
| **Normalize to** | Rescale every curve to its own peak plasma or peak effect-site value, so drugs on very different scales can be compared in shape. Only the normalized line is shown. See [Normalization](help:models/normalization). |
| **Max time** | How far the simulation runs: from one hour to a year. The grid of evaluation times scales with it. If a dose or event is within half an hour of Max time or beyond it, the plotted range is extended to show it, in steps matching the grid. |
| **Plasma line** | Line style for the plasma concentration: none (the default), solid, dashed, dotted or dot-dash. |
| **Effect site line** | Line style for the effect-site concentration; solid by default. |
| **Y axis height** | The height of each panel, from short to tall. |
| **Time until threshold** | For each drug, how long until it would fall to its recovery threshold if delivery stopped now, drawn as a thin black line. Only available when normalization is off. See [Time until threshold](help:models/recovery). |
| **Log Y axis** | Log-scale the concentration axis. Unavailable while *Time until threshold*, Events or Interaction are showing. |
| **Include opioid - MAC interaction** | Report MAC equivalents relative to the opioid-reduced MAC, so the same end-tidal concentration counts for more when an opioid is on board. See [Opioid reduction of MAC](help:models/opioid-mac). |

## Notes

**Max time and detail.** The simulation is evaluated on a grid whose spacing grows with Max time (ten minutes of spacing for a one-hour plot, a day for a year-long one), plus every dose and event time. A long plot therefore shows less of the fine structure after each bolus. Choose the shortest Max time that holds your question.

**Normalization hides a line.** Normalizing to peak plasma shows only the plasma line; normalizing to peak effect site shows only the effect-site line. This is intentional: the two would otherwise be normalized to different peaks and could not be compared.

**Line styles and the emailed slide.** The styles you choose are the ones that appear on the emailed PowerPoint slide.
