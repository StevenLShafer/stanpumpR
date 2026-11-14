Five steps produce a simulation, and a sixth lets you watch it: the Visualizer shows the drug moving between the compartments of its model. Everything else in this help is refinement.

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

*Adjust weight to fat-free mass* is on by default. The fields below it are shown only while a drug that reads them is in the dose table, and the question mark beside each names those drugs. *CYP 2D6* sets the metaboliser phenotype for the drugs with active metabolites, some antipsychotics and metoprolol; *CYP 2C19*, *CYP 2C9*, *CYP 2B6*, *VKORC1* and *CYP4F2* are read by the few drugs whose models depend on them. *Baseline serum osmolality* is read only by mannitol. *Serum creatinine* is optional and used by the drugs with a renal covariate; left blank, a normal value is assumed. *Pregnant*, shown for a woman of 12 to 59, applies the documented changes of late pregnancy to the drugs whose models have them. *Opioid tolerance*, shown while an opioid is dosed, raises the MEAC of every opioid that has one, including the active metabolites codeine and tramadol form (codeine, tramadol and buprenorphine have none of their own). See [Patient profile](help:patient-profile).

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

**Physiology** asks *what if*: four sliders change liver function, renal function, cardiac output and gastrointestinal absorption, each from a tenth of normal to ten times normal on a log scale, and the curves move at once. A drug whose clearance is mostly hepatic and high in extraction (propofol) follows cardiac output; one low in extraction (alfentanil) follows liver function; a low cardiac output also shrinks the central volume, so a bolus peaks higher, and slows the effect site a little. Under the sliders the panel names the dosed drugs they changed and any they cannot change yet. **Reset to normal** puts them back. How each slider works, and the evidence for it, is in [Physiology sliders](help:models/physiology).

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-physiology.png" alt="The Physiology panel: a line saying each slider is a multiple of normal function from a tenth to ten times on a log scale; sliders for Liver function, Renal function, Cardiac output and GI absorption, each with a question-mark tip, all at normal except Cardiac output at ×0.50; a Reset to normal link; and the note Changed: propofol">
<figcaption>The Physiology panel with cardiac output at half of normal, for a propofol bolus.</figcaption>
</figure>

- **Additional plots**, at the foot of Graph Options, adds the clinical events timeline and, while the drugs they need are dosed, the MEAC panel for comparing opioids and the propofol-opioid interaction panel.
- **Suggest Dosing** (in the *Dose table* panel of the left sidebar) works backwards from a target effect-site concentration.
- The URL in your browser's address bar encodes the whole simulation; copy it to share or save your work. See [Sharing a simulation by URL](help:sharing).

## 6. Watch the compartments

The **Visualizer** button, beside *Add new drug* under the plot, opens the plotted drug in a window of its own (with several drugs it drops up a list). It plays the plot's own simulation, from its *Min time* to its *Max time*, and shows where the drug is at each moment. Press **Play**, drag the time bar, or press **Video** to save the run as an MP4 for teaching.

It opens in the **2D view**: each compartment is a box whose fill height is the drug's concentration, on one scale for every box, and whose width is its volume, so drug can be seen running downhill from a higher level to a lower one. The arrows are the flows: red into the central compartment, green out of it, thicker for a faster flow.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-visualizer-propofol-2d.png" alt="The Visualizer's 2D view of a propofol bolus at 1 minute 36 seconds: V2 on the left, V1 with the blue effect-site bar beside it in the middle, V3 on the right, each a box filled to its concentration; green arrows carry drug from V1 into V2 and V3 and down out of it as elimination; the readout and amounts cards below">
<figcaption>Propofol 140 mg at 1 minute 36 seconds, in the 2D view: the effect site (the blue bar) has just caught up with the plasma, both at 8.24 µg/mL, while V2 and V3 take up drug and V1 eliminates it.</figcaption>
</figure>

**3D view** (or the **v** key) shows the same moment as Charles Minto's glasses: each compartment a glass whose floor area is its volume and whose liquid is as tall as the concentration. Drag to turn the scene; point at a glass or an arrow for its numbers.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-visualizer-propofol-3d.png" alt="The Visualizer's 3D view of the same moment: V1 as a tall glass in the middle with the blue effect-site ribbon beside it, V2 and V3 as wider, shallower glasses behind on either side, green arrows from V1 to each and down from its base">
<figcaption>The same moment in the 3D view.</figcaption>
</figure>

The **inhaled anesthetics** are shown the same way, as **partial pressures**: in the breathing circuit (what the patient inspires), the lungs (end-tidal), the vessel-rich group (the brain and the other well-perfused organs, the effect site, in blue), muscle and fat. A box's width, or a glass's floor area, is the compartment's capacity for the agent, its volume times how soluble the agent is in it, so fat, small in volume but very soluble, is the widest. The arrows show the fresh gas coming in, the gas vented through the pop-off valve, the breathing, and the blood carrying the agent between the lungs and each tissue.

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-visualizer-sevoflurane-2d.png" alt="The Visualizer's 2D view of sevoflurane five minutes after the vaporizer is turned off: a narrow circuit bar, then the lungs, the blue vessel-rich group, a wide muscle box and a wider fat box, each filled to its partial pressure; arrows on a blood line beneath them carry agent from the vessel-rich group and muscle back to the lungs and on into fat">
<figcaption>Sevoflurane at 2:05, five minutes after the vaporizer was turned off after two hours, in the 2D view: the lungs (0.30%) have emptied first, the vessel-rich group (0.64%) and muscle (0.92%) are giving agent back to the blood, and fat (0.07%) is still taking it up.</figcaption>
</figure>

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-visualizer-sevoflurane-3d.png" alt="The Visualizer's 3D view of the same moment: the lungs, the blue vessel-rich group, muscle and fat as glasses filled to their partial pressures, each labelled, with red arrows from the vessel-rich group and muscle into the lungs, a thin green arrow on into fat, and green arrows carrying the exhaled agent to the circuit and out">
<figcaption>The same moment in the 3D view. A gas run chooses its own starting view, in which no glass, arrow or label is hidden.</figcaption>
</figure>

[Watching a propofol bolus redistribute](help:scenarios/visualizer-propofol-bolus) and [Watching sevoflurane leave the body](help:scenarios/visualizer-sevoflurane-emergence) are the runs above, with what to look for; the other runs made to be watched are in the *Seeing the compartments* group of the [teaching scenarios](help:scenarios/index).

## Try it

Every [teaching scenario](help:scenarios/index) is a complete simulation with a button that loads it into the simulator. The pictures above came from [Propofol induction and maintenance infusion](help:scenarios/propofol-induction-maintenance).

<figure class="help-figure">
<img src="stanpumpr-assets/help/quick-start-scenario-page.png" alt="A scenario page in the Help tab, with the Load into the simulator button above the patient and dose tables">
<figcaption>A scenario page. <em>Load into the simulator</em> replaces the current patient, doses and options and switches to the Simulator tab.</figcaption>
</figure>

[Load a propofol bolus](scenario:propofol-bolus) and watch the effect site lag the plasma. Then open **Graph Options** and turn the plasma line on.
