### One product, one drug drawn

RECARBRIO is imipenem, cilastatin and relebactam. The label names each dose by the combined weight: **1.25 g of RECARBRIO is 500 mg of imipenem, 500 mg of cilastatin and 250 mg of relebactam**. Enter RECARBRIO in the dose table in that weight (`g`, `mg/kg`, or a rate such as `g/hr`), as it is ordered. stanpumpR draws its active component, [imipenem](help:drugs/imipenem), at 0.4 of each dose.

Cilastatin and relebactam are not drawn. Cilastatin blocks imipenem's breakdown in the kidney, and relebactam is a beta-lactamase inhibitor that protects imipenem from some bacterial enzymes; neither has a useful antibacterial effect of its own. Steven L. Shafer decided on 2026-11-24 that a component of a combination product that only changes the efficacy or exposure of the other is not drawn, and on 2026-11-28 that RECARBRIO is entered as a product of its own.

Imipenem given as PRIMAXIN is entered as **imipenem**, in milligrams of imipenem, as that label states its doses. Clicking the imipenem panel opens the dose dialog for RECARBRIO when the dose table holds RECARBRIO and not imipenem.

### The regimen

The label gives adults with a creatinine clearance of 90 mL/min or more 1.25 g every 6 hours, as a 30-minute infusion. Children from 3 months to 18 years receive 37.5 mg/kg every 6 hours over 60 minutes below 30 kg, and 1.25 g every 6 hours over 30 minutes from 30 kg. Infants from birth (at least 37 weeks' postmenstrual age) to 3 months receive 37.5 mg/kg every 8 hours over 60 minutes. The label reduces the dose in renal impairment.

To enter a 30-minute infusion of 1.25 g, enter `2.5 g/hr` at its start and `0 g/hr` half an hour later, and repeat every 6 hours.

### Set the infection

The model's clearance and central volume depend on the infection (see [imipenem](help:drugs/imipenem)). Enter it in the Patient Profile's **Infection** field. Pneumonia lowers both, so a patient with pneumonia has a higher exposure than the default, *None (healthy)*, shows.
