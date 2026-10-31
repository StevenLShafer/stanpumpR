The **Time** panel of the left sidebar chooses how times are entered and displayed, how long the simulation runs (**Max time**) and where the time axis starts (**Min time**). Like the sidebar's other panels it opens when its header is clicked, and the header reads the settings out while it is closed, for example *Time (hours, Actual time)*. The simulation is identical whatever you choose: the calculation itself always works in minutes.

## Time units

**Minutes**, **hours**, **days** or **weeks**. The unit does three things:

- a number typed in the dose table, or in any dialog that asks for a time, is in this unit: `1.5` is one and a half hours when the unit is hours, a day and a half when it is days;
- the plot's time axis is labelled in it;
- it sets the range and the step of the **Max time** and **Min time** sliders, which are shown in it (see below).

The Doses card says which unit its times are in, for example *Doses (times in days)*, and so does every dialog that asks for a time.

Changing the unit **converts the dose table**: every time is rewritten in the new unit, so the doses stay where they were. A dose at `90` minutes becomes `1.5` hours, then `0.0625` days. A small notification says so. As when the time display changes, the switch also applies any edits you had not yet applied, and the undo history starts afresh.

A time that is not a round number in the new unit is written to ten significant figures, so converting back and forth is exact: one day is `0.1428571429` weeks, and back in days it is `1` again.

Use minutes or hours for anaesthesia and the hours around it, and days or weeks for drugs given over days to months (antibiotics, steroids, a long opioid course).

## Time Display

### Elapsed time

What the app opens with. Everything is counted from zero, in the time unit.

Type times as numbers of the unit: the dose table takes no colon in elapsed time. An elapsed `H:MM` that reaches the table another way (pasted, or typed in the dialog that opens when you click the plot) is read as hours and minutes in any unit, so `1:30` is an hour and a half and `36:00` a day and a half; it is converted to a number of the unit when you change unit.

### Actual time

Offered for minutes and hours only: clock times address the 24 hours after the procedure start, which is too short for days or weeks. Choosing days or weeks switches the display to elapsed time.

Enter a **Procedure start** as `HH:MM` (24-hour clock). A time with a colon in the dose table is then a clock time: a dose at `09:15` with a procedure start of `08:30` is 45 minutes into the simulation. A time without a colon is still a number of the time unit, counted from the procedure start: `90` in minutes, or `1.5` in hours, is an hour and a half after it. The plot's time axis is labelled with the clock.

When you switch from clock time to elapsed time, the clock times in the table are converted to the time since the procedure start. Switching the other way converts an elapsed `H:MM` to a number of the unit (in clock mode `1:30` would mean half past one), and leaves numbers as they are. A conversion that needs the procedure start, when the box does not hold a time, is refused and the settings put back: enter the procedure start first.

The procedure start defaults to the time on your computer when the app opened.

## Max time

The length of the simulation is set by the **Max time** slider, in the Time panel below **Time units**. It reads in the time unit, and its range and step follow it:

| Time units | Max time | Step |
|---|---|---|
| Minutes | 5 minutes to 24 hours | 5 minutes |
| Hours | 15 minutes to 24 hours | 15 minutes |
| Days | 1 to 365 days | 1 day |
| Weeks | 1 to 52 weeks | 1 week |

Drag the handle, or click it and use the arrow keys to move one step at a time. Changing the unit keeps Max time, rounded up to the new unit's step and kept within its range: 24 hours becomes 1 day, and 365 days becomes 52 weeks (364 days).

If a dose or an event falls near or after the end of the plot (within half an hour, for minutes and hours), the plot is lengthened to show it, in steps matching the axis, but never past the end of the slider (24 hours, 365 days or 52 weeks). A dose beyond that brings a notification: choose a longer Max time or a larger time unit.

## Min time

The **Min time** slider, below Max time, sets where the time axis starts, so that a stretch late in a long plot can be looked at on its own: weekly doses over 16 weeks, say, seen from week 15 to week 16. Everything is still simulated from time 0, so every earlier dose counts; only the window drawn moves. Each panel's height is scaled to what is inside the window, and a click on the plot adds a dose inside it.

Min time is always at least one step below Max time. Dragging Max time down to it pulls it down too; dragging it up to Max time pushes Max time up. Loading a teaching scenario sets it back to 0.

**Max time, Min time and detail.** The simulation is evaluated at every dose and event time and at points that start close together after each dose and spread out after it. On plots of a day or less that spacing does not depend on Max time; on longer plots it is scaled to the length of the window drawn (from Min time to Max time), with no step in the window longer than 1/500 of it (about 17 hours on a year-long plot from 0, 20 minutes in a one-week window). So a window from week 15 to week 16 is drawn as finely as a one-week plot, and narrowing the window is the way to see the fine structure after each dose on a long plot.

## Restrictions

- **Target-controlled infusions and inhaled agents** are simulated only on plots of 7 days or less. With a TCI target row or an inhaled agent in the table and a longer plot, the plot area says so instead of drawing, until you remove those rows or choose a Max time of 7 days or less.
- **Suggest Dosing** is offered when the time unit is minutes or hours.
- A drug that acts over weeks to months (amiodarone, and the weekly or daily peptides semaglutide, oral semaglutide, dulaglutide and tirzepatide), in the dose table of a plot shorter than a week, brings a notification offering to show 365 days.
