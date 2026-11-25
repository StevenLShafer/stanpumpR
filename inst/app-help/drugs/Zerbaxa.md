### One product, one drug drawn

Zerbaxa is ceftolozane and tazobactam. The label names each dose by the combined weight: **1.5 g of Zerbaxa is 1 g of ceftolozane and 0.5 g of tazobactam**. Enter Zerbaxa in the dose table in that weight (`g`, `mg/kg`, or a rate such as `g/hr`), as it is ordered. stanpumpR draws its active component, [ceftolozane](help:drugs/ceftolozane), at two thirds of each dose.

Tazobactam is not drawn. It is a beta-lactamase inhibitor that protects ceftolozane from some bacterial enzymes and has no useful antibacterial effect of its own. Steven L. Shafer decided on 2026-11-24 that a component of a combination product that only changes the efficacy or exposure of the other is not drawn.

Clicking the ceftolozane panel opens the dose dialog for Zerbaxa, because Zerbaxa is what the dose table holds.

### The regimen

The label gives every dose as a 1-hour infusion every 8 hours:

| Infection | Adults | Children |
|---|---|---|
| Complicated intra-abdominal infection (with metronidazole) | 1.5 g | 30 mg/kg, up to 1.5 g |
| Complicated urinary tract infection | 1.5 g | 30 mg/kg, up to 1.5 g |
| Hospital-acquired or ventilator-associated pneumonia | 3 g | 60 mg/kg (75 mg/kg from 2 years), up to 3 g |

To enter a 1-hour infusion of 1.5 g, enter `1.5 g/hr` at its start and `0 g/hr` an hour later, and repeat every 8 hours. The label reduces the dose when the creatinine clearance is 50 mL/min or less.

### Set the infection

The model's volumes depend on the infection (see [ceftolozane](help:drugs/ceftolozane)). Enter it in the Patient Profile's **Infection** field. The default, *None (healthy)*, gives a healthy volunteer's curve. In an infected patient the central volume is 1.25 to 2.14 times larger, so the default overstates the peak.
