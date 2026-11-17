### The model

Voriconazole's parameters come from Friberg and colleagues (*Antimicrob Agents Chemother* 2012;56:3032-3042), a population analysis of rich plasma profiles from 35 healthy adults (6 mg/kg intravenously every 12 hours on day 1, 4 mg/kg to day 7, then 200 mg by mouth), 26 adolescents and 112 children. Two compartments, with elimination both **linear** (6.16 L/h at 70 kg) and **saturable** (Michaelis-Menten, Vmax 114 mg/h and Km 1.15 mg/L), and a Vmax that **falls over the first days** of treatment: the fraction lost reaches half its maximum 2.41 hours after the first dose. Central volume 79 L, peripheral volume 103 L, intercompartmental clearance 15.5 L/h at 70 kg. Orally, bioavailability is 64%, absorbed quickly after a lag of about an hour. The saturable elimination is run by the library's numerical engine, so the curve is not superposable: doubling the dose more than doubles the concentration.

### CYP2C19

In adults the Patient Profile's **CYP 2C19** field sets how much of Vmax is lost: **poor** and **intermediate** metabolisers lose all of it, so at steady state only the linear clearance remains and the concentration is about twice a normal metaboliser's; **normal**, **rapid** and **ultrarapid** metabolisers keep 18% of it (the source did not test adults for the rapid *17 allele, so those two take the normal value). The label says poor metabolisers have on average 4-fold higher exposure than homozygous normal metabolisers; this model gives about 2 to 2.7-fold, so it may underestimate a poor metaboliser's concentration. Children aged 2 to 12 lose 75% of Vmax whatever their genotype, and children and adolescents absorb oral voriconazole more slowly, with a shorter lag, and distribute it faster.

### Body size

The source's own allometric scaling to 70 kg (clearances to the 0.75 power, volumes in proportion). With [the scaling switch](help:models/fat-free-mass) on, the weight is the fat-free-mass weight.

### The band

The band is the trough range of the ESICM/ESCMID position paper on antimicrobial monitoring (Abdul-Aziz and colleagues, *Intensive Care Med* 2020;46:1127-1153): "A Cmin target between 2 and 6 mg/L is recommended to guide voriconazole dosing", efficacy at troughs of 1-2 mg/L and above, liver and neurological toxicity at 4.5-6 mg/L and above. It is a trough range, drawn against the whole curve. Plasma protein binding is 58%.

### Where to be careful

The adults were healthy and young (22 to 55 years); the label reports higher exposure in women and in elderly men, which the model does not carry. Liver disease, and the many drugs that inhibit or induce CYP2C19, CYP2C9 and CYP3A4, are not modelled. The decline in Vmax is measured from the first dose in the table and does not reverse after a pause. Children under 2 were not studied.
