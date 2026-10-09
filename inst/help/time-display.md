The **Time** card above the dose table chooses how times are entered and displayed. The simulation is identical whatever you choose: the calculation itself always works in minutes.

## Time units

**Minutes**, **hours**, **days** or **weeks**. The unit does three things:

- a number typed in the dose table, or in any dialog that asks for a time, is in this unit: `1.5` is one and a half hours when the unit is hours, a day and a half when it is days;
- the plot's time axis is labelled in it;
- it decides which **Max time** choices are offered (see below).

The Doses card says which unit its times are in, for example *Doses (times in days)*, and so does every dialog that asks for a time.

Changing the unit **converts the dose table**: every time is rewritten in the new unit, so the doses stay where they were. A dose at `90` minutes becomes `1.5` hours, then `0.0625` days. A small notification says so. As when the time display changes, the switch also applies any edits you had not yet applied, and the undo history starts afresh.

A time that is not a round number in the new unit is written to ten significant figures, so converting back and forth is exact: one day is `0.1428571429` weeks, and back in days it is `1` again.

Use minutes or hours for anaesthesia and the hours around it, and days or weeks for drugs given over days to months (antibiotics, steroids, a long opioid course).

## Time Display

### Elapsed time

Everything is counted from zero, in the time unit.

Type times as numbers of the unit: the dose table takes no colon in elapsed time. An elapsed `H:MM` that reaches the table another way (pasted, or typed in the dialog that opens when you click the plot) is read as hours and minutes in any unit, so `1:30` is an hour and a half and `36:00` a day and a half; it is converted to a number of the unit when you change unit.

### Actual time

Offered for minutes and hours only: clock times address the 24 hours after the procedure start, which is too short for days or weeks. Choosing days or weeks switches the display to elapsed time.

Enter a **Procedure start** as `HH:MM` (24-hour clock). A time with a colon in the dose table is then a clock time: a dose at `09:15` with a procedure start of `08:30` is 45 minutes into the simulation. A time without a colon is still a number of the time unit, counted from the procedure start: `90` in minutes, or `1.5` in hours, is an hour and a half after it. The plot's time axis is labelled with the clock.

When you switch from clock time to elapsed time, the clock times in the table are converted to the time since the procedure start. Switching the other way converts an elapsed `H:MM` to a number of the unit (in clock mode `1:30` would mean half past one), and leaves numbers as they are. A conversion that needs the procedure start, when the box does not hold a time, is refused and the settings put back: enter the procedure start first.

The procedure start defaults to the time on your computer when the app opened.

## Max time

The length of the simulation is set under **Graph Options → Max time**. Its choices follow the time unit:

| Time units | Max time choices |
|---|---|
| Minutes, hours | 1, 2, 4, 6, 8, 12, 18 or 24 hours |
| Days | 2, 3, 4, 7, 14, 28, 56, 91, 182 or 365 days |
| Weeks | 4, 8, 13, 26, 39 or 52 weeks |

Minutes and hours offer the same lengths, so switching between them keeps Max time. Changing to another unit keeps it if the new unit offers it, otherwise takes the next longer choice, or the unit's longest when there is none longer: going from days or weeks to minutes or hours gives 24 hours, and 365 days becomes 52 weeks (364 days).

If a dose or an event falls near or after the end of the plot, the plot is lengthened to show it, but never past the unit's longest choice (24 hours, 365 days or 52 weeks). A dose beyond that brings a notification: choose a longer Max time or a larger time unit.

## Restrictions

- **Target-controlled infusions and inhaled agents** are simulated only on plots of 7 days or less. With a TCI target row or an inhaled agent in the table and a longer plot, the plot area says so instead of drawing, until you remove those rows or choose a Max time of 7 days or less.
- **Suggest Dosing** is offered when the time unit is minutes or hours.
- **A drug that acts over weeks to months**, added to a plot shorter than a week, brings a notification offering to show 365 days.
