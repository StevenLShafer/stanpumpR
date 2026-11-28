### One product, one drug drawn

Avycaz is ceftazidime and avibactam. The label names each dose by the combined weight: **2.5 g of Avycaz is 2 g of ceftazidime and 0.5 g of avibactam**. Enter Avycaz in the dose table in that weight (`g`, `mg/kg`, or a rate such as `g/hr`), as it is ordered. stanpumpR draws its active component, [ceftazidime](help:drugs/ceftazidime), at 80% of each dose.

Avibactam is not drawn. It is a beta-lactamase inhibitor that protects ceftazidime from some bacterial enzymes and has no useful antibacterial effect of its own. Steven L. Shafer decided on 2026-11-24 that a component of a combination product that only changes the efficacy or exposure of the other is not drawn.

Clicking the ceftazidime panel opens the dose dialog for Avycaz, because Avycaz is what the dose table holds.

### The regimen

With normal renal function the label gives every dose every 8 hours as a 2-hour infusion:

| Patient | Dose |
|---|---|
| Adults with a creatinine clearance above 50 mL/min | 2.5 g |
| Children 2 to under 18 years (eGFR above 50 mL/min/1.73 m²) | 62.5 mg/kg, up to 2.5 g |
| Children 6 months to under 2 years | 62.5 mg/kg |
| Infants 3 to under 6 months | 50 mg/kg |
| Infants over 28 days to under 3 months | 37.5 mg/kg |
| Neonates up to 28 days, gestational age 31 weeks or more | 25 mg/kg |

The 28-day cutoff is postnatal age for a term infant and corrected age for a preterm one (postnatal age less the weeks born before 40 weeks' gestation), so a preterm infant stays on 25 mg/kg longer.

For complicated intra-abdominal infection it is given with metronidazole. To enter a 2-hour infusion of 2.5 g, enter `1.25 g/hr` at its start and `0 g/hr` two hours later, and repeat every 8 hours. In renal impairment the label lowers the dose and lengthens the interval (all doses still over 2 hours):

| Adult creatinine clearance (Cockcroft-Gault), or child's eGFR (bedside Schwartz) from 2 years | Adults | Children 2 years and older |
|---|---|---|
| 31 to 50 mL/min (per 1.73 m² in children) | 1.25 g every 8 hours | 31.25 mg/kg, up to 1.25 g, every 8 hours |
| 16 to 30 | 0.94 g every 12 hours | 23.75 mg/kg, up to 0.94 g, every 12 hours |
| 6 to 15 | 0.94 g every 24 hours | 23.75 mg/kg, up to 0.94 g, every 24 hours |
| 5 or less | 0.94 g every 48 hours | 23.75 mg/kg, up to 0.94 g, every 48 hours |

Both ceftazidime and avibactam are removed by hemodialysis, so on dialysis days the dose is given after it. The label gives no regimen for children younger than 2 years with renal impairment.

### Set the infection

The model's clearance and central volume depend on the infection (see [ceftazidime](help:drugs/ceftazidime)). Enter it in the Patient Profile's **Infection** field. The default, *None (healthy)*, gives a healthy participant's curve. For the infections the source modelled, the central volume is 1.49 to 2.20 times larger, so the default overstates the peak; *Other* gives the healthy curve, as the source fitted no other infection.
