### One product, one drug drawn

Zerbaxa is ceftolozane and tazobactam. The label names each dose by the combined weight: **1.5 g of Zerbaxa is 1 g of ceftolozane and 0.5 g of tazobactam**. Enter Zerbaxa in the dose table in that weight (`g`, `mg/kg`, or a rate such as `g/hr`), as it is ordered. stanpumpR draws its active component, [ceftolozane](help:drugs/ceftolozane), at two thirds of each dose.

Tazobactam is not drawn. It is a beta-lactamase inhibitor that protects ceftolozane from some bacterial enzymes and has no useful antibacterial effect of its own. Steven L. Shafer decided on 2026-11-24 that a component of a combination product that only changes the efficacy or exposure of the other is not drawn.

Clicking the ceftolozane panel opens the dose dialog for Zerbaxa, because Zerbaxa is what the dose table holds.

### The regimen

The label gives every dose every 8 hours, as a 1-hour infusion, except for children with hospital-acquired or ventilator-associated pneumonia, whose doses run over 2 hours:

| Infection | Adults | Children (from 32 weeks' gestational age) |
|---|---|---|
| Complicated intra-abdominal infection (with metronidazole) | 1.5 g | 30 mg/kg, up to 1.5 g |
| Complicated urinary tract infection | 1.5 g | 30 mg/kg, up to 1.5 g |
| Hospital-acquired or ventilator-associated pneumonia | 3 g | 60 mg/kg (75 mg/kg from 2 years), up to 3 g |

To enter a 1-hour infusion of 1.5 g, enter `1.5 g/hr` at its start and `0 g/hr` an hour later, and repeat every 8 hours. A child's pneumonia dose over 2 hours is entered as half the dose per hour, and its 0 two hours after the start. The label reduces an adult's dose when the creatinine clearance (Cockcroft-Gault) is 50 mL/min or less, and gives a loading and maintenance dose for intermittent hemodialysis; it establishes no dose for an adult below 15 mL/min who is not on intermittent hemodialysis. For a child of 2 years or more it grades the dose by the estimated GFR, and it does not recommend Zerbaxa for a child younger than 2 years with renal impairment. The label does not establish safety and effectiveness below 32 weeks' gestational age.

### Set the infection

The model's volumes depend on the infection (see [ceftolozane](help:drugs/ceftolozane)). Enter it in the Patient Profile's **Infection** field. The default, *None (healthy)*, gives a healthy volunteer's curve. In an infected patient the central volume is 1.25 to 2.14 times larger, so the default overstates the peak.
