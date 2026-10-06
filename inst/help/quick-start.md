Five steps produce a simulation. Everything else in this help is refinement.

## 1. Describe the patient

Open **Patient Profile** in the left sidebar. Enter age, weight, height and sex. The unit buttons beside each field switch between years and months, kilograms and pounds, inches and centimetres. These four covariates drive the pharmacokinetic models, so they change the predictions.

Three further fields, *Pregnant*, *CYP 2D6* and *Renal Function*, are visible but disabled: no model in the library yet responds to them. See [Patient profile](help:patient-profile).

## 2. Enter the doses

The **Doses** table is to the right of the plot. In the *Drug* column, start typing a drug name and choose from the list. Enter a *Time* (minutes, or `1:30` for an hour and a half), a *Dose*, and pick *Units*.

- A mass unit (`mg`, `mcg/kg`) makes the row a **bolus**.
- A rate unit (`mcg/kg/min`, `mg/hr`) makes the row an **infusion** that runs until the next row for that drug changes it. To stop an infusion, add a row at the stop time with a dose of 0.

Right-click a row to insert or remove rows. See [The dose table](help:dose-table).

## 3. Apply

Press **Apply Changes**. Nothing recalculates until you do, so you can make several related edits and see them together. *Undo* and *Redo* step through your edits.

## 4. Read the plot

Each drug gets its own panel. The solid line is the **effect-site** concentration, the one that matters clinically; the plasma line is hidden by default and can be turned on under **Graph Options**. The shaded band is the drug's usual therapeutic range, from the drug library.

- **Hover** for the exact concentration at any time.
- **Click** to add a dose at that time (applied immediately).
- **Double-click** a drug's curve to edit or delete its doses.

See [Reading the plot](help:reading-the-plot).

## 5. Refine

- **Graph Options** sets how far the simulation runs, line styles, normalization, a log axis, and the *Time until threshold* lines.
- **Additional Plots** adds the MEAC panel for comparing opioids, the propofol-opioid interaction panel, and the clinical events timeline.
- **Suggest Dosing** (above the dose table) works backwards from a target effect-site concentration.
- The URL in your browser's address bar encodes the whole simulation; copy it to share or save your work. See [Sharing a simulation by URL](help:sharing).

## Try it

[Load a propofol bolus](scenario:propofol-bolus) and watch the effect site lag the plasma. Then open **Graph Options** and turn the plasma line on.
