The dose table is the main way you talk to the program. It sits in a panel to the right of the plot, open when the app starts on a desktop and closed on a phone; the arrow at the top of the panel hides and shows it, so on a phone open it before entering doses, and on any screen the plot can have the whole width once the doses are in. Each row is one thing done to the patient: a bolus, a change of infusion rate, an oral dose, a vaporizer setting.

## Columns

**Drug.** Autocompletes from the drug library and accepts only names in it. Type a few letters and choose. Removing the drug name removes the row. The illicit-drug research models (diamorphine, cocaine, methamphetamine, LSD, psilocybin and MDMA) are offered only while **Show illicit drugs**, in the *Dose table* panel of the left sidebar, is ticked, and their names are shown in red.

**Time.** When it happens, in the **Time units** chosen in the *Time* panel of the left sidebar (the dose table's header says which: *Doses (times in hours)*). The field is forgiving:

| You type | It means |
|---|---|
| `12` | twelve of the time unit: twelve minutes, or twelve days |
| `1.5` | one and a half: 90 minutes when the unit is hours |
| `130` | 130 of the unit, not 1:30 |
| `1:30` | with the *Actual time* display, the clock time 01:30 (with *Elapsed time* the table takes numbers only; an `H:MM` pasted in is read as hours and minutes) |
| `0:80` | rolls over to `01:20` |

Times are never negative. With *Actual time*, a number without a colon is counted from the procedure start. What the Time and Dose cells accept is set out under *Entries that cannot be read*, below. See [Time display](help:time-display), which also explains what happens to the table when you change the unit (it is converted, so the doses stay put).

A regimen written in days goes in most easily with the Time units set to days. Day 1 begins at time 0, so day *n* begins at *n* − 1: "1600 mg a day on days 1 and 2, then 1200 mg a day on days 3 to 7" is three rows of a once-a-day unit, 1600 at `0`, 1200 at `2`, and a stop (0) at `7`. See *Scheduled doses* below.

**Dose/Target.** A number, never negative. The unit goes in the Units column, not here. The column is headed *Dose/Target* because, for a drug that offers target-controlled infusion, the number is a target concentration when the unit is *Plasma target* or *Effect site target* (see [Target-controlled infusion](help:tci)); the dialogs opened from the plot say *Dose or Target* for those drugs.

**Units.** A drop-down whose contents depend on the drug in that row. The list, and the default, come from the drug library and are shown on each drug's page. The unit decides what kind of row it is:

| Unit looks like | Row type | Example |
|---|---|---|
| mass, or mass per kg | **bolus** at that time | `mg`, `mcg/kg` |
| mass per minute or hour, with or without per kg | **infusion** from that time | `mcg/kg/min`, `mg/hr` |
| mass with `PO`, `SL`, `IM`, `IN` or `RA` (regional anesthesia), the oral ones sometimes naming a form (`tablet`, `liquid`, `ER`, `DR`, `XR`) | **extravascular dose** at that time; see [absorption](help:models/absorption) | `mg PO`, `mg IN`, `mg RA`, `mg PO liquid` |
| mass per day with `PO` | **oral rate**: the daily dose spread evenly over each day, from that time | `mg/day PO` (amiodarone) |
| `mg/hr RA` | **perineural infusion** into the tissue depot, from that time | bupivacaine, ropivacaine, lidocaine |
| `mg PE` and its rate and IM forms | **fosphenytoin**, in phenytoin sodium equivalents | `mg PE`, `mg PE/min`, `mg PE IM` (phenytoin) |
| `Plasma target`, `Effect site target` | **target-controlled infusion**: the dose is the target concentration | see [Target-controlled infusion](help:tci) |
| a bolus, oral, sublingual, intramuscular or intranasal dose followed by `qd`, `bid`, `tid`, `qid` or `q4h` | **scheduled dose**, repeated | `mg bid`, `mg PO tid` |
| `L/min` | fresh gas flow or ventilation setting | inhaled agents |
| `%` | vaporizer setting | inhaled agents |

## Entries that cannot be read

A Time or Dose cell takes one number, written plainly (`2.5`, `.5`, `1,000`) or in scientific notation (`1e3` is stored as 1000), or, for a time, hours and minutes (`1:30`). Spaces and quotation marks around the entry, and a leading `+`, are dropped. A blank cell in a new row becomes 0, but emptying a filled Time or Dose cell is refused: the cell keeps its value and a message asks for a number (enter `0` for no dose).

Nothing else is guessed at. An entry with a minus sign, a letter or a unit (`-5`, `5 mg`, `8:44 pm`), a second decimal point or colon (`1.2.3`, `1:2:30`), a comma that does not separate thousands (`1,5`), or a space or other mark inside the number (`8 30`, `8;30`) is not read as some other number. It is refused with a message saying why, and the cell keeps the value it had, so the table always shows the dose the simulation uses; a cell that was empty stays empty, and the row is ignored by the simulation, like any incomplete row, until it is corrected. The *Add a dose* and *Edit doses* dialogs instead say what could not be read and stay open. (Until October 2026 such entries were stripped to their digits, so `-5` became 5 and `1e3` became 13.)

## Infusions

An infusion runs from its time until the next row for that same drug changes it, or until the end of the simulation. To stop one, add a row for the same drug at the stop time with a dose of 0. A bolus row does not interrupt an infusion; the two add. An oral rate (`mg/day PO`) behaves the same way: each row sets the daily dose from its time, and `0 mg/day PO` stops it. So does a perineural infusion (`mg/hr RA`), which runs into the tissue depot rather than the vein.

Per-kilogram units use the weight in the Patient Profile at the time of simulation, so changing the weight changes the delivered amount.

## Scheduled doses

For drugs given on a schedule (the analgesic opioids, the oral analgesics and benzodiazepines, the antibiotics, the steroids, the antidepressants, antipsychotics, stimulants, antiseizure and antidiabetic drugs and ondansetron), their bolus, oral, sublingual, intramuscular, intranasal and subcutaneous units also come with the frequencies the drug is usually given at, from:

| Suffix | Meaning | Interval |
|---|---|---|
| `qd` | once a day | 24 hours |
| `bid` | twice a day | 12 hours |
| `tid` | three times a day | 8 hours |
| `qid` | four times a day | 6 hours |
| `q4h` | every 4 hours (offered wherever `qid` is) | 4 hours |
| `qwk` | once a week (subcutaneous units only: semaglutide, dulaglutide, tirzepatide) | 7 days |

The first dose is given at the time in the row, and the same dose is then repeated at that interval until the end of the X axis, so lengthening the axis extends the schedule. The repeats are not added to the dose table, which would fill with rows, but they are merged back in when a slide is emailed, so the exported dose table lists the full dose sequence.

- To **stop** a schedule, enter a scheduled dose of **0** for the same drug and route at the stop time, at any frequency. `0 mg PO bid` stops an oral schedule of that drug; an intravenous schedule carries on.
- To **change** the dose or the frequency, enter a new scheduled row for the same route: it replaces the running schedule from its own time.
- **Oral and sublingual schedules** end at the next oral or sublingual dose of any kind; the two routes count as one. A later oral or sublingual row for that drug, scheduled or not, in any oral form, ends the running oral or sublingual schedule from its own time: a `bid` row entered during a `tid` schedule replaces it, and an ordinary `0 mg PO` or `0 mg SL` stops it, since a new dose is a new instruction to the patient. An oral or sublingual dose entered at the same time as a schedule's first dose (a loading dose taken with it) does not end it.
- On the other routes an ordinary dose given alongside a schedule adds to it, and an ordinary dose of 0 does not stop it.

### Skip next dose

A patient who misses a tablet, or vomits it, wants to know whether to wait for the next dose or take two. So does a patient who misses an injection, a spray or a scheduled intravenous dose (`mg bid`). Click a drug's curve, or double-click it, while it has a schedule (`qd`, `bid`, `tid`, `qid`, `q4h` or `qwk`) running on any route: the dialog then offers **Skip next dose**, naming the time of the first scheduled dose at or after the time clicked. It writes the skip into the dose table by the rules above, where it can be seen and changed: a `0 mg PO` (or `0 mg SL`) row at the skipped time, or on every other route a scheduled 0 in the schedule's own unit, such as `0 mg IM qid` or `0 mg SC qwk` (an ordinary 0 does not stop those schedules), which ends the schedule, and the scheduled row again one interval later, which restarts it, unless the schedule would have ended before then. To compare the patient's options, change that restart row, for instance to a double dose taken now and the schedule resumed at its usual time. An infusion or a TCI target has no next dose to skip: it is delivered continuously, so the button is never offered for one; to end it, use **Stop**.

### Stop

An infusion, a target-controlled infusion, a schedule and an inhaled agent's vaporizer or flow setting all run until a row in the dose table ends them. Click a drug's curve, or double-click it, at a time when any of them is running for that drug, and the dialog offers **Stop**, naming the drug and the time clicked. It adds the row the rules already read as a stop, at that time, with a dose of 0: a rate of 0 in the infusion's units, a target of 0 for a TCI, a scheduled dose of 0 in the schedule's own units (which stops it on every route), or a setting of 0 for an inhaled agent. Everything running for the drug is stopped together, and the rows can be seen and changed in the dose table like any other. A single bolus or oral dose is not ongoing, so a drug given only those offers no Stop.

## Applying changes

Edits go into a **draft**. Nothing recalculates until you press **Apply Changes**. This is deliberate: it lets you make several related edits and simulate them together, rather than watching the plot flicker through half-finished states. The button is grey when the draft matches what is applied.

**Undo** and **Redo** step through your edit history. The history is cleared whenever the table is set by another route, because there is then no earlier draft worth returning to.

Four routes bypass the draft and apply at once: clicking the plot to add a dose, the *Add new drug* button under the plot (the same dialog, for a drug not yet in the table), the *Edit doses* dialog opened by double-clicking a curve, and *Suggest Dosing*. Loading a teaching scenario from the help does the same.

## Adding and removing rows

Right-click in the table for the context menu: insert row above, insert row below, remove row. There are always a few blank rows at the bottom to type into, and a new one appears when the last is used. Column editing is deliberately disabled.

## Limits

At most 500 rows. Doses above 10⁹ are rejected. Rows that are incomplete (no drug, no time, no dose or no unit) are ignored by the simulation, so a half-typed row does no harm.

## Rules for the inhaled agents

Entering any gas adds a `ventilation` row if there is none, with a default minute ventilation scaled to the patient's weight. Entering nitrous oxide adds an `oxygen` row at 21 per cent of the fresh gas if there is none. Gas flows and ventilation are rounded to the nearest 0.1 L/min. See [Inhaled anesthetics](help:inhaled-agents).

## Copy and paste

The table accepts paste from a spreadsheet, one row per line with tab-separated Drug, Time, Dose and Units. Drug names must match the library exactly.
