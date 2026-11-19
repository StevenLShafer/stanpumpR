### The model

Flumazenil's pharmacokinetics and effect site come from Fiset and colleagues (*Clin Pharmacol Ther* 1995;58:567-582), a study in healthy men (mean 79 kg, 32 years) sedated with midazolam held constant by a target-controlled infusion, in whom flumazenil was infused at 0.5 or 1 mg/min until the EEG effect of midazolam was reversed. Arterial samples were drawn from 30 seconds into the infusion to 4 hours after it, and venous samples to 8 hours. Three compartments (two-stage means): V1 5.6 L, V2 15.9 L, V3 39.4 L; clearances 1.12, 1.79 and 0.86 L/min; half-lives 0.9, 11.4 and 63 minutes.

### The effect site

The effect site is flumazenil's own EEG reversal of midazolam. The equilibration half-life averaged 4.96 minutes at light and 3.96 minutes at deep sedation; their mean, 4.46 minutes, is used, so the effect peaks about 3.3 minutes after a bolus. The label says that 80% of the response comes within 3 minutes. The benzodiazepine it opposes is not modelled: flumazenil's half-life is shorter than midazolam's and much shorter than diazepam's or lorazepam's, which is why sedation can return as flumazenil wears off.

### Body size

The source's men averaged 79 kg and 177 cm, so the published values belong to a man of that size; with [the scaling switch](help:models/fat-free-mass) on they are scaled from his fat-free mass, volumes in proportion and clearances to the 0.75 power. With the switch off the published values are used unscaled.

### Against the label

After 1 mg over 5 minutes the label gives an AUC of 15 ng·h/mL, which this model reproduces (14.9), and a clearance of about 1 L/h/kg (this model, 0.85 L/h/kg at 79 kg). Its peak, 24 ng/mL, is less than half this model's, because the label's samples were venous and the model's early samples arterial, which gives a small central volume. Roncari and colleagues (*Br J Clin Pharmacol* 1986) measured a clearance of 15.5 mL/min/kg against this model's 14.2.

### The band

No therapeutic range is established; flumazenil is titrated to effect. The band is the label's 12-28 ng/mL, the peak plasma concentrations that "usually produce complete antagonism" after the usual sedating doses of a benzodiazepine (0.1-0.2 mg, 3-6 ng/mL, gives partial antagonism). The concentration needed depends on the benzodiazepine and how much of it is present.

### Where to be careful

The label says clearance falls to 40-60% of normal in moderate and to 25% in severe liver disease, and rises by half with food; none of this is modelled. Age, sex and renal failure have no effect according to the label. Flumazenil is dosed as itself (0.1 mg/mL); there is no salt.
