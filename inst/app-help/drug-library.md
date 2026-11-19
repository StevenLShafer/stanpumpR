Two editors under the **Settings** menu in the navigation bar change the drug library for the current session. Changes are not saved on the server, but a saved link carries them (see [Sharing](help:sharing)); reloading the page without one restores the defaults.

## Drug Library

Opens the table behind every drug. The columns are:

| Column | Meaning |
|---|---|
| Drug | The name, as it appears in the dose table |
| Concentration.Units | Usually `mcg` or `ng`: the drug is plotted in these units per mL; also `pg` (exenatide, lixisenatide), `mcU` (the insulins and vasopressin, mcU/mL), and `IU` (heparin and enoxaparin activity, IU/mL). The exceptions are not per mL: `%` for the gases, `mOsm` for mannitol (plotted as mOsm/kg), `mmol` for intravenous calcium and magnesium sulfate (the rise in the serum ion, mmol/L), `INR` for the vitamin-K antagonists (the INR they produce), `ACT` for heparin on bypass (the activated clotting time, s) and `L/min` for ventilation, a setting of the inhaled agents |
| Bolus.Units, Infusion.Units | The units a bolus and an infusion are expressed in internally; also used by Suggest Dosing |
| Default.Units | The unit a new row starts with |
| Units | The comma-separated list offered in the dose table |
| Color | The drug's colour on the plot, as a hex code |
| Lower, Upper | The therapeutic range that draws the shaded band |
| Typical | The typical concentration; the *Mid* band is ±5% of it |
| MEAC | Minimum effective analgesic concentration; 0 for non-opioids. Drives the MEAC panel and the opioid-MAC interaction |
| Class | `IV` or `gas` |

A drug in `mcg` or `ng` can be switched between the two; its range, MEAC and recovery threshold are converted with it, unless you typed a new value in the same edit, which is then read in the new units. So are the concentrations you have typed elsewhere: its targets in the dose table (a switch that would take a target, a range, the MEAC or the threshold above 1,000,000,000 is refused) and its measured concentrations under Bayesian Updating (a switch is refused while a measured concentration has no drug chosen, since it could not be said which drug's units it is in). A change of units ends the dose table's undo and redo, which would otherwise bring back a target in the old units. A metabolite the drug forms, and the drug interactions it drives, follow the switch. A unit added to the Units list is tried before it is accepted: one the drug's model cannot simulate, or one that gives no concentration (a route the model has no absorption for; for a drug plotted above a baseline, such as mannitol's osmolality, one that leaves the curve at its baseline), is refused with the reason. Only the inhaled agents take the gas units, an `Effect site target` needs a model with an effect site, and a drug plotted as an INR, an ACT, an osmolality or a molar concentration takes no target at all. A Units edit cannot remove the drug's Bolus.Units or Infusion.Units, the units Suggest Dosing writes: change those first. Nor can it remove a unit the dose table uses, applied or not: change or delete those doses first. A target-controlled infusion is offered only to a drug plotted in mcg or ng per mL whose model is solved in closed form, and a drug with saturable elimination or a transit chain, whose numbers are integrated in its model's own units, keeps its concentration units. A number is held to the limits when you change it, and so is a range, whose Lower may not be above its Upper; one you leave as it was is kept, whatever it is. The edits travel with a saved link (see [Sharing](help:sharing)).

The dialog's warning is sincere: it is intended for collaborators checking a model, and it is easy to break the session by entering something the simulation cannot use. If you do, reload the page.

If you believe a default is wrong, please write to steven.shafer@stanford.edu with the source you would cite.

The shipped values are in `inst/extdata/drugDefaults_global.csv` in the repository and are shown on each drug's page under [Drug library](help:drugs/index).

## Drug Thresholds

Edits the **recovery thresholds** alone: the concentration that *Time until threshold* counts down to for each drug, called `endCe` in the library. The dialog can also switch *Time until threshold* on.

For the inhaled agents the threshold is in per cent and is shown for the patient's age; the volatile agents' thresholds follow MAC as the age changes, so the number in the dialog is the number on the plot. A **MAC** row sets the threshold for the MAC-equivalents panel, in multiples of the age-adjusted MAC.

Enter `0` for no threshold. A blank, negative or non-numeric entry is refused: *Apply* says which drug to correct, the dialog stays open, and no threshold is changed until it is.

Edited thresholds travel with the URL, so a shared simulation reports the same times until threshold. See [Time until threshold](help:models/recovery).

## Defaults

| Drug | Lower | Upper | Typical | MEAC | Threshold | Units |
|---|---|---|---|---|---|---|
| *See each drug's page, or the drug index, for the current values.* | | | | | | |

The thresholds for the intravenous drugs default to their MEAC where they have one, and to the lower end of the typical range otherwise. The antibiotics' thresholds are the plasma concentration at which **free** drug equals the MIC for the drug's main target organism. Cefazolin's curve is unbound drug, so its threshold is the MIC itself. The others' curves are total drug, so their thresholds are higher than the MIC, by the inverse of the free fraction. See [Time until threshold](help:models/recovery).
