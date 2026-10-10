Two editors under the **Settings** menu in the navigation bar change the drug library for the current session. Changes are not saved on the server, and they are not carried in the URL except for the thresholds; reloading the page restores the defaults.

## Drug Library

Opens the table behind every drug. The columns are:

| Column | Meaning |
|---|---|
| Drug | The name, as it appears in the dose table |
| Concentration.Units | `mcg` or `ng`: the drug is plotted in these units per mL (`%` for the gases) |
| Bolus.Units, Infusion.Units | The units a bolus and an infusion are expressed in internally; also used by Suggest Dosing |
| Default.Units | The unit a new row starts with |
| Units | The comma-separated list offered in the dose table |
| Color | The drug's colour on the plot, as a hex code |
| Lower, Upper | The therapeutic range that draws the shaded band |
| Typical | The typical concentration; the *Mid* band is ±5% of it |
| MEAC | Minimum effective analgesic concentration; 0 for non-opioids. Drives the MEAC panel and the opioid-MAC interaction |
| Class | `IV` or `gas` |

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
