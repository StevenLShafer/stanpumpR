### The model

Nortriptyline's parameters come from Vos and colleagues (*Clin Pharmacokinet* 2025), a population analysis of 85 adults with major depression from a Dutch trial of genotype-guided dosing, using the structure of Kvist 2001's model 2: one compartment with a well-stirred liver, hepatic plasma flow 36 L/h and intrinsic clearance 42.3 L/h, so a clearance of 19.5 L/h and an oral bioavailability of 46%; volume 1,080 L; absorption 0.533 per hour (Kvist's, fixed: no samples were drawn during absorption). The half-life is 38.5 hours, so steady state takes about a week. No weight effect was found, so the values are scaled from the library's reference man.

### Formed from amitriptyline

Nortriptyline is also amitriptyline's active metabolite. When amitriptyline is given, the nortriptyline it forms is drawn with this model, at the rate [amitriptyline's](help:drugs/amitriptyline) source (Koh 2018) identifies relative to nortriptyline's clearance, and is added to any nortriptyline given directly.

### CYP2D6

Nortriptyline is cleared by CYP2D6, but the source fitted no genotype term: its typical patient is a mixture of the source's patients, 42% intermediate and 10% poor metabolisers. The Patient Profile's **CYP 2D6** field is not read. Healthy normal metabolisers clear it faster: after 25 mg, Jensen 2012 measured the same peak as this model but a 24-hour half-life and about 60% of its exposure. In a poor metaboliser the concentration is several times higher.

### Against the consensus guideline

The AGNP consensus (Hart 2026, Table 6) gives the concentration 24 hours after a once-daily dose as 0.804 ng/mL per mg of daily dose; this model, 0.815.

### The band

The band is the AGNP therapeutic reference range for depression (Hart 2026, Table 5), 70-170 ng/mL, 12 hours after the dose; the laboratory alert level is 300 ng/mL. The Pamelor label gives 50-150 ng/mL. Doses are entered as nortriptyline base, as the US capsules are labelled.

### Where to be careful

CYP2D6 inhibitors (fluoxetine, paroxetine, bupropion, quinidine) raise the concentration; they are not modelled. In late pregnancy women needed about 1.6 times their usual dose (Wisner 1993, a secondary quotation), which is recorded but not applied.
