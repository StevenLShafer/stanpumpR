Five steps produce a simulation. Everything else in this help is refinement.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-simulator.png" alt="The simulator: sidebar on the left, the plot in the middle, the Time card and the dose table on the right">
<figcaption>The simulator. Patient and graph settings on the left, the plot in the middle, the dose table on the right.</figcaption>
</figure>

When the app opens, a welcome dialog states what stanpumpR is and is not. **OK** dismisses it; **Take the tour** brings you to this page.

<figure class="help-figure help-figure-medium">
<img src="stanpumpr-assets/help/quick-start-welcome.png" alt="The welcome dialog, with OK and Take the tour buttons">
</figure>

## 1. Describe the patient

Open **Patient Profile** in the left sidebar. Enter age, weight, height and sex. The unit buttons beside each field switch between years and months, kilograms and pounds, inches and centimetres. These four covariates drive the pharmacokinetic models, so they change the predictions.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-patient.png" alt="The Patient Profile panel: age with yr/mo buttons, weight with kg/lb, height with in/cm, sex, CYP 2D6, and the greyed-out Pregnant and Renal Function fields">
<figcaption>The Patient Profile panel. This picture was taken before the CYP 2D6 field became active; Pregnant and Renal Function are still greyed out.</figcaption>
</figure>

*CYP 2D6* sets the metaboliser phenotype for the drugs with active metabolites, and *Adjust weight to fat-free mass* is on by default. Two further fields, *Pregnant* and *Renal Function*, are visible but disabled. See [Patient profile](help:patient-profile).

## 2. Enter the doses

The **Doses** table is to the right of the plot. In the *Drug* column, start typing a drug name and choose from the list. Enter a *Time* (minutes, or `1:30` for an hour and a half), a *Dose*, and pick *Units*.

<figure class="help-figure help-figure-medium">
<img src="stanpumpr-assets/help/quick-start-doses.png" alt="The dose table with four propofol rows: a 2 mg/kg bolus, an infusion of 150 then 100 mcg/kg/min, and a stop at 90 minutes; below it the Apply Changes, Undo and Redo buttons">
<figcaption>A propofol induction and infusion: a bolus, two infusion rates, and a row with a dose of 0 to stop the infusion. Apply Changes, Undo and Redo are beneath the table.</figcaption>
</figure>

- A mass unit (`mg`, `mcg/kg`) makes the row a **bolus**.
- A rate unit (`mcg/kg/min`, `mg/hr`) makes the row an **infusion** that runs until the next row for that drug changes it. To stop an infusion, add a row at the stop time with a dose of 0.

Right-click a row to insert or remove rows. See [The dose table](help:dose-table).

## 3. Apply

Press **Apply Changes**. Edits to the dose table do not recalculate until you do, so you can make several related edits and see them together. The button is grey when there is nothing new to apply. *Undo* and *Redo* step through your edits.

## 4. Read the plot

Each drug gets its own panel. The solid line is the **effect-site** concentration, the one that matters clinically; the plasma line is hidden by default and can be turned on under **Graph Options**. The shaded band is the drug's usual therapeutic range, from the drug library.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-plot.png" alt="The propofol panel: a dashed plasma line spiking after the bolus, a solid effect-site line held inside the shaded band by the infusion, and a thin black time-until-threshold line read against minute labels on the right">
<figcaption>The plot for the doses above, with the plasma line turned on (dashed) and <em>Time until threshold</em> ticked (the thin black line, read against the minute scale on the right).</figcaption>
</figure>

- **Hover** for the exact concentration at any time.
- **Click** to add a dose at that time (applied immediately).
- **Double-click** a drug's curve to edit or delete its doses.

See [Reading the plot](help:reading-the-plot).

## 5. Refine

**Graph Options** sets how far the simulation runs, line styles, normalization, a log axis, and the *Time until threshold* lines.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-graph-options.png" alt="The Graph Options panel: Show typical, Normalize to, Max time, plasma and effect-site line styles, Y axis height, Time until threshold, and the opioid-MAC interaction checkbox">
<figcaption>The Graph Options panel.</figcaption>
</figure>

- **Additional Plots** adds the MEAC panel for comparing opioids, the propofol-opioid interaction panel, and the clinical events timeline.
- **Suggest Dosing** (above the dose table) works backwards from a target effect-site concentration.
- The URL in your browser's address bar encodes the whole simulation; copy it to share or save your work. See [Sharing a simulation by URL](help:sharing).

## Try it

Every [teaching scenario](help:scenarios/index) is a complete simulation with a button that loads it into the simulator. The pictures above came from [Propofol induction and maintenance infusion](help:scenarios/propofol-induction-maintenance).

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-scenario-page.png" alt="A scenario page in the Help tab, with the Load into the simulator button above the patient and dose tables">
<figcaption>A scenario page. <em>Load into the simulator</em> replaces the current patient, doses and options and switches to the Simulator tab.</figcaption>
</figure>

[Load a propofol bolus](scenario:propofol-bolus) and watch the effect site lag the plasma. Then open **Graph Options** and turn the plasma line on.
