### One product, two drugs

Coartem tablets contain **20 mg of artemether and 120 mg of lumefantrine**. You enter Coartem in the dose table in tablets (`tablet PO`, or `tablet PO bid` for the twice-daily doses). stanpumpR then simulates and draws the two components separately, each with its own model and in its own panel:

- [artemether](help:drugs/artemether), drawn with its active metabolite [dihydroartemisinin](help:drugs/dihydroartemisinin);
- [lumefantrine](help:drugs/lumefantrine).

Both components are drawn because both are active (Steven L. Shafer, 2026-11-24). They have different roles: artemether and dihydroartemisinin clear most parasites within the first two days, and lumefantrine, which stays in the body for days, clears the rest. The label's own trial in China shows this. Artemether given alone cured 46% of patients by day 28, lumefantrine alone 90%, and Coartem 98%.

Clicking a component's panel opens the dose dialog for Coartem, because Coartem is what the dose table holds.

### The regimen

The label gives six doses over three days. The first dose is at 0 h, the second 8 h later, and then one dose each morning and evening on days 2 and 3: 0, 8, 24, 36, 48 and 60 h. The number of tablets per dose depends on body weight:

| Weight | Tablets per dose |
|---|---|
| 5 to under 15 kg | 1 |
| 15 to under 25 kg | 2 |
| 25 to under 35 kg | 3 |
| 35 kg and over | 4 |

Enter the course as six rows of `tablet PO`. Alternatively, enter the first two doses as `tablet PO` and the rest as one row of `tablet PO bid` at 24 h with a 0 at 72 h, which ends the schedule.

Coartem should be taken with food. Every patient in the source studies took it with food or milk. Fasting lowers artemether's bioavailability two- to threefold and lumefantrine's about sixteenfold, and the curves do not show this.

### The two time scales

The two drugs are measured in different units: artemether and dihydroartemisinin in ng/mL, lumefantrine in mcg/mL.

- **Artemether and dihydroartemisinin** each rise and fall within hours of each dose, so they appear as six spikes in the first three days.
- **Lumefantrine** builds up over the three days and then falls with a half-life of several days.

A plot of a week or more shows lumefantrine's fall to its day-7 concentration. The day-7 concentration is the established marker of cure (see [lumefantrine](help:drugs/lumefantrine)).

### Not modelled

- Interactions through CYP3A4 are not modelled. The label lists ketoconazole, lopinavir/ritonavir, rifampin and efavirenz, which change exposure severalfold. Ritonavir moves the two components in opposite directions: it lowers artemether and raises lumefantrine.
- Parasite clearance is not modelled.
- The QT prolongation that the label links to lumefantrine concentrations is not modelled.
