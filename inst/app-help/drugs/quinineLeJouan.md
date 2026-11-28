### The model

Le Jouan and colleagues (*Antimicrob Agents Chemother* 2005;49:3658-3662) studied 30 children with uncomplicated falciparum malaria in Yaoundé, Cameroon: 0.55 to 6.7 years (mean 2.8), 13.6 ± 3.8 kg. They took quinine 8.3 mg/kg (as base, in a syrup) every 8 hours for 5 days.

The model has one compartment with first-order absorption (0.934 /h) and no lag. Its clearance and volume are each the unbound fraction of quinine times a straight line in body weight:

- clearance = unbound fraction × 0.53 L/h/kg × weight;
- volume = unbound fraction × (57 L + 3.8 L/kg × weight).

### The unbound fraction

In acute malaria, inflammatory proteins bind quinine, and as the child recovers the unbound fraction rises, so the apparent clearance and volume rise with it. The paper let it rise by 0.001 per hour, from 0.114 at the start of treatment through 0.15 at 36 hours to 0.186 at 72 hours. stanpumpR cannot change a parameter with time since treatment began, so it fixes the unbound fraction at its 36-hour value, 0.15 (Steven Shafer's decision of 28 November 2026). For a 15 kg child that gives a clearance of 1.19 L/h and a volume of 17.1 L, between the paper's 0.91 to 1.48 L/h and 13 to 21 L over the first three days.

The half-life does not depend on the unbound fraction: 9.9 hours at 15 kg. The paper prints 9.1 hours, which its own typical values do not give (0.693 × 13 / 0.91 = 9.9).

### Who it is used for

Children up to about 12 years, by the [quinine](help:drugs/quinine) group's rule. Its weight terms are not held to the weights studied, so an older child is extrapolated along the paper's straight lines.

### Body size

The paper's own weight terms are applied. With [the scaling switch](help:models/fat-free-mass) on, the weight in them is the pharmacokinetic weight.
