The dose table is the main way you talk to the program. It sits to the right of the plot. Each row is one thing done to the patient: a bolus, a change of infusion rate, an oral dose, a vaporizer setting.

## Columns

**Drug.** Autocompletes from the drug library and accepts only names in it. Type a few letters and choose. Removing the drug name removes the row.

**Time.** When it happens, in the **Time units** chosen in the Time card above the table (the card header says which: *Doses (times in hours)*). The field is forgiving:

| You type | It means |
|---|---|
| `12` | twelve of the time unit: twelve minutes, or twelve days |
| `1.5` | one and a half: 90 minutes when the unit is hours |
| `130` | 130 of the unit, not 1:30 |
| `1:30` | with the *Actual time* display, the clock time 01:30 (with *Elapsed time* the table takes numbers only; an `H:MM` pasted in is read as hours and minutes) |
| `0:80` | rolls over to `01:20` |

Times are never negative. With *Actual time*, a number without a colon is counted from the procedure start. What the Time and Dose cells accept is set out under *Entries that cannot be read*, below. See [Time display](help:time-display), which also explains what happens to the table when you change the unit (it is converted, so the doses stay put).

A regimen written in days goes in most easily with the Time units set to days. Day 1 begins at time 0, so day *n* begins at *n* − 1: "1600 mg a day on days 1 and 2, then 1200 mg a day on days 3 to 7" is three rows of a once-a-day unit, 1600 at `0`, 1200 at `2`, and a stop (0) at `7`. See *Scheduled doses* below.

**Dose.** A number, never negative. The unit goes in the Units column, not here.

**Units.** A drop-down whose contents depend on the drug in that row. The list, and the default, come from the drug library and are shown on each drug's page. The unit decides what kind of row it is:

| Unit looks like | Row type | Example |
|---|---|---|
| mass, or mass per kg | **bolus** at that time | `mg`, `mcg/kg` |
| mass per minute or hour, with or without per kg | **infusion** from that time | `mcg/kg/min`, `mg/hr` |
| mass with `PO`, `IM` or `IN` | **extravascular dose** at that time | `mg PO`, `mg IN` |
| mass per day with `PO` | **oral rate**: the daily dose spread evenly over each day, from that time | `mg/day PO` (amiodarone) |
| any of the above doses followed by `qd`, `bid`, `tid` or `qid` | **scheduled dose**, repeated | `mg bid`, `mg PO tid` |
| `L/min` | fresh gas flow or ventilation setting | inhaled agents |
| `%` | vaporizer setting | inhaled agents |

## Entries that cannot be read

A Time or Dose cell takes one number, written plainly (`2.5`, `.5`, `1,000`) or in scientific notation (`1e3` is stored as 1000), or, for a time, hours and minutes (`1:30`). Spaces and quotation marks around the entry, and a leading `+`, are dropped. A blank cell in a new row becomes 0, but emptying a filled Time or Dose cell is refused: the cell keeps its value and a message asks for a number (enter `0` for no dose).

Nothing else is guessed at. An entry with a minus sign, a letter or a unit (`-5`, `5 mg`, `8:44 pm`), a second decimal point or colon (`1.2.3`, `1:2:30`), a comma that does not separate thousands (`1,5`), or a space or other mark inside the number (`8 30`, `8;30`) is not read as some other number. It is refused with a message saying why, and the cell keeps the value it had, so the table always shows the dose the simulation uses; a cell that was empty stays empty, and the row is ignored by the simulation, like any incomplete row, until it is corrected. The *Add a dose* and *Edit doses* dialogs instead say what could not be read and stay open. (Until October 2026 such entries were stripped to their digits, so `-5` became 5 and `1e3` became 13.)

## Infusions

An infusion runs from its time until the next row for that same drug changes it, or until the end of the simulation. To stop one, add a row for the same drug at the stop time with a dose of 0. A bolus row does not interrupt an infusion; the two add. An oral rate (`mg/day PO`) behaves the same way: each row sets the daily dose from its time, and `0 mg/day PO` stops it.

Per-kilogram units use the weight in the Patient Profile at the time of simulation, so changing the weight changes the delivered amount.

## Scheduled doses

For drugs given on a schedule (the analgesic opioids, the antibiotics, the steroids and mannitol), each bolus, oral, intramuscular and intranasal unit also comes with a frequency:

| Suffix | Meaning | Interval |
|---|---|---|
| `qd` | once a day | 24 hours |
| `bid` | twice a day | 12 hours |
| `tid` | three times a day | 8 hours |
| `qid` | four times a day | 6 hours |

The first dose is given at the time in the row, and the same dose is then repeated at that interval until the end of the X axis, so lengthening the axis extends the schedule. The repeats are not added to the dose table, which would fill with rows, but they are merged back in when a slide is emailed, so the exported dose table lists the full dose sequence.

- To **stop** a schedule, enter a scheduled dose of **0** for the same drug and route at the stop time, at any frequency. `0 mg PO bid` stops an oral schedule of that drug; an intravenous schedule carries on. An ordinary dose of 0 does not stop it.
- To **change** the dose or the frequency, enter a new scheduled row for the same route: it replaces the running schedule from its own time.
- Ordinary doses can be given alongside a schedule; they add.

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
