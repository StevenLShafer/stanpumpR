Each drug in the dose table gets its own panel, labelled with the drug's name and its concentration units. Panels are stacked, share the time axis, and are coloured with the drug's colour from the library.

## Plasma and effect site

Each drug can be drawn as two lines:

- **Plasma** concentration: what an arterial sample would contain. Hidden by default.
- **Effect site** concentration: the concentration at the site of drug action, which lags the plasma. This is usually what you care about clinically, and it is drawn solid by default.

Both are configurable under **Graph Options → Plasma line / Effect site line**, including turning either off. The lag between them, and why the effect site keeps rising after a bolus while the plasma falls, is explained under [The effect site and ke0](help:models/effect-site).

For the inhaled agents the "plasma" line is the alveolar (end-tidal) tension and the "effect site" line is the vessel-rich group (brain) tension.

## The shaded band

**Graph Options → Show typical** controls a shaded band behind each drug:

- **Range**: the usual therapeutic range for that drug, from the *Lower* and *Upper* values in the drug library.
- **Mid**: a narrow band at the typical concentration, plus or minus 5 per cent.
- **none**: no band.

The band is orientation, not a target. It is a published typical range for a typical indication, and it can be edited under [Drug Library](help:drug-library).

## The time-until-threshold line

With **Graph Options → Time until threshold** ticked, a thin black line appears on each panel, read against the labels at the panel's right-hand edge. At every moment it shows how long the concentration would take to fall to the drug's recovery threshold if delivery stopped right then. See [Time until threshold](help:models/recovery).

Each panel's labels are in whichever unit suits the longest time on that panel, whatever the x axis shows: minutes (`min`) below 2 hours, hours (`h`) below 2 days, days (`d`) below 3 weeks, and weeks (`wk`) beyond. One panel can therefore read in minutes and the next in days. The search for the threshold stops at a horizon: a day for a drug with an effect site, and a week (or the length of the plot, if that is longer) for a drug timed on its plasma, such as an antibiotic. A line that levels off at the horizon means the time is at least that long, and the hover then says "more than 24 hours" (or "more than 7 days"). The top label is rounded up to a whole number of its unit, so that level need not be the top of the panel: a week's horizon on labels running to 8 d, for example, levels off at 7 d.

## Interacting with the plot

- **Hover** over any curve for the time and concentration at that point. The time is in the **Time units** chosen in the Time card ("3.5 days"), or the time of day under **Actual time**. The concentration is read at exactly that time from the full simulation: the effect-site concentration (`Ce`) for a drug with an effect site, and the plasma concentration (`Cp`) for one without, such as an antibiotic or a prodrug. With the time-until-threshold line on, the hover adds that time too, in the same unit as the panel's labels. On the MEAC and interaction panels the hover shows the summed value or the probability, also read at the hovered time.
- **Click** anywhere on a drug's panel to add a dose of that drug at that time. A small dialog asks for the amount and unit, and the dose is applied immediately, bypassing the draft.
- **Double-click** a drug's panel to edit or delete that drug's doses in a compact table.
- On the **Events** panel, click to add an event and double-click to edit the event list.

## Panels that are not drugs

| Panel | Shown when | What it plots |
|---|---|---|
| % MEAC | *Additional Plots → MEAC* | Each opioid's effect-site concentration as a percentage of its minimum effective analgesic concentration, and their sum |
| p response | *Additional Plots → Interaction* | Probability of response to laryngoscopy from the propofol-opioid interaction surface |
| Events | *Additional Plots → Events* | Clinical events on the time axis |
| MAC equivalents | Any potent inhaled agent is running | Alveolar concentration as a multiple of the age-adjusted MAC, summed over agents |
| Oxygen | Any gas flow is running | Inspired and alveolar oxygen |

See [Additional plots](help:additional-plots).

## Axes

The y axis of each panel is linear by default; **Log Y axis** switches to logarithmic where that is meaningful. The x axis runs to **Max time** and is labelled in the **Time units** chosen in the Time card (minutes, hours, days or weeks), or with times of day under **Actual time**; see [Time display](help:time-display). The unit changes only the labels: the simulation itself is always computed in minutes. Panel height is set by the **Y axis height** slider.

## Resolution

Curves are evaluated at the time of every dose and event and on an even grid between them, then drawn as straight segments. The closed-form solution is exact at every evaluated point; only the drawing between points is an interpolation.
