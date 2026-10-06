The dose table is the main way you talk to the program. It sits to the right of the plot. Each row is one thing done to the patient: a bolus, a change of infusion rate, an oral dose, a vaporizer setting.

## Columns

**Drug.** Autocompletes from the drug library and accepts only names in it. Type a few letters and choose. Removing the drug name removes the row.

**Time.** When it happens. The field is forgiving:

| You type | It means |
|---|---|
| `12` | twelve minutes |
| `1:30` | one hour thirty minutes |
| `130` | the same, read as `HH:MM` |
| `0:80` | rolls over to `1:20` |

Anything that cannot be read as a time becomes 0 rather than raising an error. If the time display is set to *Actual time*, times are clock times; see [Time display](help:time-display).

**Dose.** A number. Negative numbers and junk become 0.

**Units.** A drop-down whose contents depend on the drug in that row. The list, and the default, come from the drug library and are shown on each drug's page. The unit decides what kind of row it is:

| Unit looks like | Row type | Example |
|---|---|---|
| mass, or mass per kg | **bolus** at that time | `mg`, `mcg/kg` |
| mass per minute or hour, with or without per kg | **infusion** from that time | `mcg/kg/min`, `mg/hr` |
| mass with `PO`, `IM` or `IN` | **extravascular dose** at that time | `mg PO`, `mg IN` |
| `L/min` | fresh gas flow or ventilation setting | inhaled agents |
| `%` | vaporizer setting | inhaled agents |

## Infusions

An infusion runs from its time until the next row for that same drug changes it, or until the end of the simulation. To stop one, add a row for the same drug at the stop time with a dose of 0. A bolus row does not interrupt an infusion; the two add.

Per-kilogram units use the weight in the Patient Profile at the time of simulation, so changing the weight changes the delivered amount.

## Applying changes

Edits go into a **draft**. Nothing recalculates until you press **Apply Changes**. This is deliberate: it lets you make several related edits and simulate them together, rather than watching the plot flicker through half-finished states. The button is grey when the draft matches what is applied.

**Undo** and **Redo** step through your edit history. The history is cleared whenever the table is set by another route, because there is then no earlier draft worth returning to.

Three routes bypass the draft and apply at once: clicking the plot to add a dose, the *Edit doses* dialog opened by double-clicking a curve, and *Suggest Dosing*. Loading a teaching scenario from the help does the same.

## Adding and removing rows

Right-click in the table for the context menu: insert row above, insert row below, remove row. There are always a few blank rows at the bottom to type into, and a new one appears when the last is used. Column editing is deliberately disabled.

## Limits

At most 500 rows. Doses above 10⁹ are rejected. Rows that are incomplete (no drug, no time, no dose or no unit) are ignored by the simulation, so a half-typed row does no harm.

## Rules for the inhaled agents

Entering any gas adds a `ventilation` row if there is none, with a default minute ventilation scaled to the patient's weight. Entering nitrous oxide adds an `oxygen` row at 21 per cent of the fresh gas if there is none. Gas flows and ventilation are rounded to the nearest 0.1 L/min. See [Inhaled anesthetics](help:inhaled-agents).

## Copy and paste

The table accepts paste from a spreadsheet, one row per line with tab-separated Drug, Time, Dose and Units. Drug names must match the library exactly.
