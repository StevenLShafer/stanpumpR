## Vasoactive and resuscitation drugs

The intravenous vasopressors, vasodilators, rate-control and antiarrhythmic drugs, and resuscitation drugs of intensive care are listed under **Vasoactive and resuscitation** in the startup menu: [atropine](help:drugs/atropine), [digoxin](help:drugs/digoxin), [diltiazem](help:drugs/diltiazem), [dopamine](help:drugs/dopamine), [epinephrine](help:drugs/epinephrine), [esmolol](help:drugs/esmolol), [labetalol](help:drugs/labetalol), [magnesium sulfate](help:drugs/magnesiumSulfate), [nitroglycerin](help:drugs/nitroglycerin), [norepinephrine](help:drugs/norepinephrine), [procainamide](help:drugs/procainamide), [propranolol](help:drugs/propranolol) and [verapamil](help:drugs/verapamil). [Clevidipine](help:drugs/clevidipine) was already in the library and stays under **Antihypertensives**. Every one is a **concentration model only**: no blood pressure, heart rate or rhythm is predicted, and no drug in the group has an effect site, and only digoxin, with its published therapeutic range, has a shaded band.

### Why not all of them

A drug is offered only where a published model could be read and checked. That rule removes more of this group than of any other in the library, for three reasons.

**Many are endogenous.** Epinephrine, norepinephrine, dopamine and vasopressin are made by the body, so a study measures the drug given plus the patient's own, and a model needs an endogenous production term. The engine has no constant input, so these drugs are plotted, where they are plotted at all, as the concentration **above** the patient's own, as [intravenous calcium](help:drugs/calcium) and magnesium are. For epinephrine that baseline is under 1% of an infusion's concentration; for norepinephrine it is nearer 7%.

**Several have no kinetics in the usual sense.** Nitroprusside breaks down in blood within minutes and is not itself measured; the cyanide and thiocyanate it releases have kinetics of their own but no transferable model of how much is formed. Adenosine's half-life in whole blood is a second or two, and its clearance falls as the dose rises. Sodium bicarbonate has a "bicarbonate space" that changes with the patient's own bicarbonate, not a volume and a clearance.

**Most of the rest were fitted decades ago, and only the abstract is readable here.** An abstract can fix a model: two half-lives, a clearance and a central volume determine a two-compartment model, and a fifth number, when there is one, checks it. That is how labetalol, diltiazem and nitroglycerin were built, and each drug's page gives the check. The papers obtained later were read in full, and two of them turned out not to be usable as published: nicardipine's (Cheung 1999), whose table does not form a consistent compartmental model, and methylene blue's (Walter-Sack 2009), which reports only non-compartmental summaries. But most abstracts print a clearance and a terminal half-life only, which is one or two numbers short, and a half-life is never permission to invent a volume.

### Where each drug stands

One row per drug and candidate source, from `inst/extdata/vasoactiveRegistry.csv`. "Needs the paper" means a model exists whose numbers could not be read here; "read; not offered" is a complete model of a drug already offered from another source, kept as the record of an alternative; "no usable model" means none exists that this engine could represent.

<!-- generated: vasoactive-registry-table -->

### The papers that would unblock the rest

Each of these would let a model be read and checked, or, for the three drugs built from an abstract, confirm what the abstract leaves open (the dose's salt basis, the assay, the sampling site). Most of the older *British Journal of Clinical Pharmacology* papers are free in PubMed Central as scans.

<!-- generated: vasoactive-to-obtain -->
