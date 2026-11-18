## What to look for

Two rows, one drug: cenobamate **50 mg PO qam** and **200 mg PO qpm**. These are the two independent daily slots the `qam` and `qpm` units provide. The morning dose is deliberately the smaller of the two, because the antiseizure drugs are sedating and can blunt memory while the patient is awake; the larger dose is given at night, when sedation is wanted rather than avoided. This is the one regimen the morning/evening split was built for, so the slots are offered only on the oral antiseizure maintenance drugs.

The plot shows a daily rhythm — a step up each evening, a smaller step each morning — riding on a baseline that climbs for about two weeks before it levels off. Cenobamate's half-life is long (about 65 hours in the model; the label gives 50–60), so a single day's doses barely move the running level; it is the accumulation over many days that fills the compartment. The plasma line carries the curve: cenobamate is modelled plasma-only, with no effect site.

## Try next

- Change the **200 mg qpm** row to **200 mg qd**. A plain whole-day schedule is a regimen change that ends *every* running oral schedule, the morning slot included, so the 50 mg qam disappears and only the once-daily 200 mg remains. To keep the split, both rows must be the slot units.
- Add a later **100 mg qam** row a few days in. It replaces only the morning slot; the evening qpm keeps running unchanged.
- Add an ordinary **0 mg PO** row. An oral dose of 0 stops every oral schedule at once — the simplest way to end the regimen.

## Background

[The dose table](help:dose-table) explains the scheduling rules, including how `qam` and `qpm` differ from the whole-day frequencies (`qd`/`bid`/`tid`/`qid`/`q4h`) in not replacing each other. The cenobamate model and its reference parameters are on its [drug page](help:drugs/cenobamate).
