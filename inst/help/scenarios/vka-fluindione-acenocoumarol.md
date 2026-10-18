## What to look for

This is a **hypothetical side-by-side comparison** of the two 2012 research models, not a treatment plan — no one is given two vitamin-K antagonists together, and new fluindione starts are no longer permitted in France. A 60-year-old, 69 kg man is modelled on each of two European vitamin-K antagonists — **fluindione 20 mg once a day** and **acenocoumarol 4 mg once a day** — for four weeks, CYP2C9 wild-type (**\*1/\*1**) and VKORC1-sensitive (**AA**), the genotype set in the Patient Profile.

Each drug gets its **own INR panel**, and each panel is that one drug's prediction alone: the simulator computes each drug's INR independently, so **neither panel is the INR of taking both drugs together**. The two panels come from the same indirect-response model (Verstuyft 2012) but behave very differently, which is the point of putting them side by side.

- Neither INR jumps. Both **climb over days** as the drug inhibits the production of clotting-factor activity: the plotted quantity is 1/(a turnover pool), so it moves at the pace of that turnover (half-lives near 18 hours), not with the plasma concentration.
- **Fluindione** clears slowly, so it accumulates over the first week to a sustained INR near 2.9 — the paper's steady-state value for this genotype — with only a small daily ripple.
- **Acenocoumarol** clears quickly, so between once-daily doses its concentration rises and falls sharply and the INR **swings** day to day rather than settling at a single level. This is why the simulated acenocoumarol INR cannot be pinned to a single steady-state number the way fluindione's can.

## Try next

- Change **CYP2C9** from \*1/\*1 to a \*3 carrier (for example **\*1/\*3**) and apply. Both drugs' clearance falls (fluindione × 0.637, acenocoumarol × 0.7), their concentrations rise, and both INR lines climb higher.
- Change **VKORC1** from AA to **GG** and apply. The half-maximal concentration rises (fluindione × 1.30, acenocoumarol × 2.29), so the patient is less sensitive and both INR lines fall for the same dose.
- Remove one drug from the dose table to compare the two time courses side by side on the same axis scale.

## Background

Both drugs are described on their pages — [fluindione](help:drugs/fluindione) and [acenocoumarol](help:drugs/acenocoumarol) — and share the indirect-response INR engine. They are research and teaching models: a simulated INR is **not** a dose recommendation, a bleeding risk or a procedure-safe time. Fluindione's slowly accumulating concentration lets its simulated steady-state INR match the paper's Table IV; acenocoumarol's fast clearance makes its simulated steady-state peak about 20–30% from that table, because the paper does not state the maintenance dose or the steady-state definition behind it.
