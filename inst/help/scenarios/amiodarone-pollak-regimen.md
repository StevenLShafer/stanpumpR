## What to look for

Amiodarone is given here as Pollak, Bouillon and Shafer proposed: 1600 mg/day for 2 days, 1200 for 5 days, then 1000, 800 and 600 mg/day for a week each, 400 mg/day for 62 days, and 343 mg/day from day 90 on. The *Time units* are **days** and the plot runs for a year. Each dose row is a daily oral dose given as a constant rate ("mg/day PO"), which is how the model was fitted.

The shaded band is the therapeutic window of 1.0 to 2.5 mg/L. Serum amiodarone enters it on the first day (1.19 mg/L at 24 hours) and stays between 1.2 and 1.75 mg/L for the rest of the year, ending at 1.50 mg/L. The steps down every week are there to hold the concentration while the drug's enormous peripheral volume (12,700 L for the typical patient) fills; each one is taken just as the serum level starts to drift up.

Desethylamiodarone, the active metabolite, has its own row. It rises far more slowly: 0.56 mg/L at a week, 0.95 at four weeks, 1.15 at three months and 1.34 at a year. No therapeutic range has been established for it, so its panel has no band.

The maintenance dose follows from one number. At steady state the input equals the output, so the dose rate is the target concentration times clearance: 1.5 mg/L × 229 L/day = 343 mg/day, which can be given as 400 mg/day on six days of the week.

## Try next

- Change *Time units* to **weeks** and the axis reads in weeks; the dose times convert themselves (day 2 becomes 0.2857142857 weeks) and convert back exactly when you return to days.
- Replace the regimen with a single row of 400 mg/day PO at 0. Without a loading phase serum amiodarone is only 0.56 mg/L after a week and 1.33 mg/L after three months: most of the first season is spent below the window.
- Set *Max time* to 28 days to look at the loading phase on its own.

## Background

The model and its limits: [amiodarone](help:drugs/amiodarone) and [desethylamiodarone](help:drugs/desethylamiodarone). How metabolites are simulated: [Active metabolites](help:models/metabolites).
