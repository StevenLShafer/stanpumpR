## What to look for

A 66-year-old, 78 kg man is started on **dabigatran etexilate 150 mg twice a day** on the day of a total hip replacement, and followed for a week. The drug is the [dabigatran after hip replacement](help:drugs/dabigatranHip) model (Trocóniz 2007), which is **not** the library's atrial-fibrillation dabigatran model — it is the post-operative one, whose point is that the first days behave differently.

- On **days 0 and 1** the doses are absorbed and cleared **slowly**: the post-operative gut is sluggish, so the absorption rate constant is only 0.022 /h (a flip-flop, with a 31-hour absorption half-life) and the apparent clearance is lower. The plasma level rises gently and the peaks and troughs are shallow.
- From **day 2** the doses given then are absorbed about four times faster and cleared faster, so each new dose produces a **sharper peak** and the curve starts to **swing** between doses rather than drifting.
- Each dose keeps the kinetics of the day it was given on, so the transition is gradual: the slow day-0 and day-1 doses are still being absorbed while the brisk day-2 doses come and go on top of them.

## Try next

- Raise the **serum creatinine** in the Patient Profile (say to 1.6 mg/dL) and apply. Creatinine clearance falls, so the day-2-to-10 apparent clearance drops and the later concentrations climb; the higher creatinine also lowers that period's absorption rate.
- Set the **age** older and apply. Age lowers the day-2-to-10 absorption rate, so the later peaks come a little later and lower.
- Switch the **fat-free-mass** setting off and on: the model has no weight term of its own, so body size enters only through the creatinine clearance and the library's size factors.

## Background

The [dabigatran after hip replacement](help:drugs/dabigatranHip) page explains the two-phase model, where the 24-hour split comes from, and why the breakpoint is taken from the start of the simulation. This is a research and teaching model: a simulated concentration is **not** a clotting assay, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.
