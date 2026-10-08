Five steps produce a simulation. Everything else in this help is refinement.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-simulator.png" alt="The simulator: sidebar on the left, the plot in the middle, the Time card and the dose table on the right">
<figcaption>The simulator. Patient and graph settings on the left, the plot in the middle, the dose table on the right.</figcaption>
</figure>

When the app opens it asks which drugs to display, with a box for each drug, grouped by category: hypnotics and sedatives, opioids, neuromuscular blockade, inhaled anesthetics, antibiotics, corticosteroids and others. Propofol, fentanyl, remifentanil and rocuronium start ticked; untick them to start with something else. **Start** puts each ticked drug in the dose table with a dose of 0 at time 0, ready to edit. Any drug can be added to the dose table later. On a first visit, and once a week after that, the dialog also states what stanpumpR is and is not, and **Take the tour** starts with your ticked drugs and brings you to this page.

A link to a saved simulation, or reloading the page, skips the question and opens the simulation as it was, provided its dose table has at least one drug in it. With an empty dose table (for instance, reloading before pressing **Start**), the question is asked again.

## 1. Describe the patient

Open **Patient Profile** in the left sidebar. Enter age, weight, height and sex. The unit buttons beside each field switch between years and months, kilograms and pounds, inches and centimetres. These four covariates drive the pharmacokinetic models, so they change the predictions.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-patient.png" alt="The Patient Profile panel: age with yr/mo buttons, weight with kg/lb and the ticked Adjust weight to fat-free mass box, height with in/cm, sex, CYP 2D6 set to Normal, baseline serum osmolality of 280 mOsm/kg, and the greyed-out Renal Function field">
<figcaption>The Patient Profile panel for a 40-year-old man. The greyed-out Renal Function selector shown here has since been replaced by an optional Serum creatinine field; Pregnant, disabled, appears only for women of child-bearing age.</figcaption>
</figure>

*Adjust weight to fat-free mass* is on by default. *CYP 2D6* sets the metaboliser phenotype for the drugs with active metabolites. *Baseline serum osmolality* is read only by mannitol. *Serum creatinine* is optional and used by the renally cleared drugs; left blank, a normal value is assumed. *Pregnant*, for women of child-bearing age, is visible but disabled. See [Patient profile](help:patient-profile).

## 2. Enter the doses

The **Doses** table is to the right of the plot. In the *Drug* column, start typing a drug name and choose from the list. Enter a *Time*, a *Dose*, and pick *Units*. Times are in the **Time units** chosen in the *Time* card above the table: minutes when the app opens, or hours, days or weeks for longer courses (changing the unit converts the table). With the *Actual time* display, a time like `09:30` is a clock time.

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

**Graph Options** sets how far the simulation runs (*Max time*, whose choices follow the time units), line styles, normalization, a log axis, and the *Time until threshold* lines.

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
