Five steps produce a simulation. Everything else in this help is refinement.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-simulator.png" alt="The simulator: on the left the Patient Profile, Graph Options, Time, Dose table and Email Slide panels; the plot in the middle with the Add new drug button beneath it; the dose table on the right with Apply Changes, Undo and Redo">
<figcaption>The simulator. Patient, graph and time settings on the left, the plot in the middle, the dose table on the right.</figcaption>
</figure>

When the app opens it asks which drugs to display. The drugs are grouped by category: hypnotics and sedatives, opioids, oral analgesics, neuromuscular blockade, inhaled anesthetics, antibiotics, corticosteroids, antidepressants, stimulants, local anesthetics, antiemetics, antiseizure drugs, antipsychotics, antidiabetic drugs, anticoagulants, antihypertensives, vasoactive and resuscitation drugs, Parkinson disease, and others. Each category starts collapsed, showing only its name and how many drugs it holds; click it to open its list and tick the drugs you want. A category with drugs ticked says how many, even when collapsed again. Nothing is ticked to begin with. The illicit-drug research models are never offered here. **Start** puts each ticked drug in the dose table with a dose of 0 at time 0, ready to edit; with nothing ticked, the dose table starts empty. Any drug can be added to the dose table later.

<figure class="help-figure help-figure-medium">
<img src="stanpumpr-assets/help/quick-start-welcome.png" alt="The opening dialog: the statement of what stanpumpR is, a Take the tour button, and the drug categories as collapsed sections, each with the number of drugs it holds, and a Start button at the bottom">
<figcaption>The opening dialog, as on a first visit: the statement of what stanpumpR is, then the drug categories, each collapsed until clicked.</figcaption>
</figure>

On a first visit, and once a week after that, the dialog also states what stanpumpR is and is not, and **Take the tour** starts with your ticked drugs and brings you to this page.

A link to a saved simulation, or reloading the page, skips the question and opens the simulation as it was, provided its dose table has at least one drug in it. With an empty dose table (for instance, reloading before pressing **Start**), the question is asked again.

## 1. Describe the patient

Open **Patient Profile** in the left sidebar. Enter age, weight, height and sex. The unit buttons beside each field switch between years and months, kilograms and pounds, inches and centimetres. These four covariates drive the pharmacokinetic models, so they change the predictions.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-patient.png" alt="The Patient Profile panel: age with yr/mo buttons, weight with kg/lb and the ticked Adjust weight to fat-free mass box, height with in/cm, and sex">
<figcaption>The Patient Profile panel for a 40-year-old man given propofol. Propofol reads none of the other fields, so only these are shown; a genotype, the serum creatinine or the baseline osmolality appears when a drug that reads it is in the dose table.</figcaption>
</figure>

*Adjust weight to fat-free mass* is on by default. The fields below it are shown only while a drug that reads them is in the dose table, and the question mark beside each names those drugs. *CYP 2D6* sets the metaboliser phenotype for the drugs with active metabolites, some antipsychotics and metoprolol; *CYP 2C19*, *CYP 2C9*, *CYP 2B6*, *VKORC1* and *CYP4F2* are read by the few drugs whose models depend on them. *Baseline serum osmolality* is read only by mannitol. *Serum creatinine* is optional and used by the drugs with a renal covariate; left blank, a normal value is assumed. *Pregnant*, shown for a woman of 12 to 59, applies the documented changes of late pregnancy to the drugs whose models have them. *Opioid tolerance*, shown while an opioid is dosed, raises each opioid's MEAC. See [Patient profile](help:patient-profile).

## 2. Enter the doses

The **Doses** table is in the panel to the right of the plot (the arrow at its top hides and shows it). In the *Drug* column, start typing a drug name and choose from the list. Enter a *Time*, a dose in the *Dose/Target* column (a target concentration for a TCI row), and pick *Units*. Times are in the **Time units** chosen in the *Time* panel of the left sidebar: minutes when the app opens, or hours, days or weeks for longer courses (changing the unit converts the table). With the *Actual time* display, a time like `09:30` is a clock time.

<figure class="help-figure help-figure-medium">
<img src="stanpumpr-assets/help/quick-start-doses.png" alt="The dose table, with Drug, Time, Dose/Target and Units columns, holding four propofol rows: a 2 mg/kg bolus, an infusion of 150 then 100 mcg/kg/min, and a stop at 90 minutes; below it the Apply Changes, Undo and Redo buttons">
<figcaption>A propofol induction and infusion: a bolus, two infusion rates, and a row with a dose of 0 to stop the infusion. Apply Changes, Undo and Redo are beneath the table.</figcaption>
</figure>

- A mass unit (`mg`, `mcg/kg`) makes the row a **bolus**.
- A rate unit (`mcg/kg/min`, `mg/hr`) makes the row an **infusion** that runs until the next row for that drug changes it. To stop an infusion, add a row at the stop time with a dose of 0.
- A unit with a route (`mg PO`, `mg IM`, `mg RA`) gives the dose by that route, and one ending in `qd`, `bid`, `tid`, `qid` or `q4h` (`mg PO bid`), or `qam`/`qpm` (the oral antiseizure drugs) or `qwk` (subcutaneous), repeats it on that schedule to the end of the plot.

Right-click a row to insert or remove rows. See [The dose table](help:dose-table).

## 3. Apply

Press **Apply Changes**. Edits to the dose table do not recalculate until you do, so you can make several related edits and see them together. The button is grey when there is nothing new to apply. *Undo* and *Redo* step through your edits.

## 4. Read the plot

Each drug gets its own panel. The solid line is the **effect-site** concentration, the one that matters clinically; the plasma line is hidden by default and can be turned on under **Graph Options**. A drug without an effect-site model (no time to peak effect or ke0) has no effect site, and its solid line is the plasma (or, for a few drugs, whole-blood or serum) concentration; for some drugs the time to peak effect is a clinical estimate rather than a measurement, and each drug's page says which. The shaded band is the drug's usual therapeutic range, from the drug library; what it means (a measured serum range, a response concentration, a target) differs by drug, and each drug's page says.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-plot.png" alt="The propofol panel: a dashed plasma line spiking after the bolus, a solid effect-site line held inside the shaded band by the infusion, and a thin black time-until-threshold line read against minute labels on the right">
<figcaption>The plot for the doses above, with the plasma line turned on (dashed) and <em>Time until threshold</em> ticked (the thin black line, read against the minute scale on the right).</figcaption>
</figure>

- **Hover** for the exact concentration at any time.
- **Click** to add a dose at that time (applied immediately). While an infusion, a TCI, a schedule or an inhaled agent is running, the dialog also offers **Stop**, which ends it at that time; while a scheduled dose (`mg PO bid`, `mg IM qid`, `mg SC qwk`) is running, **Skip next dose**.
- **Add new drug** (the button at the lower left of the plot) to add a drug that is not yet plotted, without going to the dose table.
- **Double-click** a drug's curve to edit or delete its doses.

See [Reading the plot](help:reading-the-plot).

## 5. Refine

The **Time** panel, in the left sidebar, sets the time units, how far the simulation runs (the *Max time* slider), where the axis starts (the *Min time* slider, for a close look late in a long plot) and whether times are elapsed or clock times.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-time.png" alt="The Time panel: Time units set to Minutes, the Max time slider at 120 min, the Min time slider at 0 min, and Time Display set to Elapsed time">
<figcaption>The Time panel.</figcaption>
</figure>

**Graph Options** sets line styles, normalization, the *Time until threshold* lines, a log axis (offered while no right-hand scale is drawn), the *Peripheral compartments* panels, and the *Minto / Schnider* switch that draws those propofol and remifentanil models beside the library's own. Options that apply only to some drugs, such as the opioid-MAC interaction and % MEAC, appear while such a drug is dosed.

<figure class="help-figure help-figure-narrow">
<img src="stanpumpr-assets/help/quick-start-graph-options.png" alt="The Graph Options panel: Show typical, Normalize to, plasma and effect-site line styles, Y axis height, Time until threshold, Peripheral compartments, the Minto / Schnider switch, and Additional plots with its Events box">
<figcaption>The Graph Options panel, for propofol alone.</figcaption>
</figure>

- **Additional plots**, at the foot of Graph Options, adds the clinical events timeline and, while the drugs they need are dosed, the MEAC panel for comparing opioids and the propofol-opioid interaction panel.
- **Suggest Dosing** (in the *Dose table* panel of the left sidebar) works backwards from a target effect-site concentration.
- The URL in your browser's address bar encodes the whole simulation; copy it to share or save your work. See [Sharing a simulation by URL](help:sharing).

## Try it

Every [teaching scenario](help:scenarios/index) is a complete simulation with a button that loads it into the simulator. The pictures above came from [Propofol induction and maintenance infusion](help:scenarios/propofol-induction-maintenance).

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-scenario-page.png" alt="A scenario page in the Help tab, with the Load into the simulator button above the patient and dose tables">
<figcaption>A scenario page. <em>Load into the simulator</em> replaces the current patient, doses and options and switches to the Simulator tab.</figcaption>
</figure>

[Load a propofol bolus](scenario:propofol-bolus) and watch the effect site lag the plasma. Then open **Graph Options** and turn the plasma line on.
