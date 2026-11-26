### The model

ATryn is recombinant antithrombin alfa, made in transgenic goats and approved in the US in 2009 for the prevention of thromboembolism around surgery and delivery in hereditary antithrombin deficiency. It is not listed on DailyMed today; the US plasma-derived product is [Thrombate III](help:drugs/antithrombin). The two products differ: by ATryn's label, its clearance is about seven times the plasma-derived product's and its half-life about a ninth.

DeJongh and colleagues' population analysis (*Clin Appl Thromb Hemost* 2014) fitted 15 adults with hereditary deficiency given a single dose (13 women, 2 men), then checked the model against two phase 3 trials of 32 patients treated around surgery or delivery. The delivery patients cleared it faster than the model predicted, and their parameters were re-estimated. Two compartments:

| | Surgery (not pregnant) | Delivery |
|---|---|---|
| Clearance | 0.665 L/h | 1.38 L/h |
| Volume at steady state | 7.72 L | 14.3 L |
| Central volume | 3.08 L | 5.70 L |
| Intercompartmental clearance | 0.613 L/h | 0.613 L/h |

The central volume is the steady-state volume ÷ 2.51 in both groups. At 70 kg the model gives a clearance of 9.5 mL/h/kg, a steady-state volume of 110 mL/kg, a terminal half-life of 12 hours (15 hours at delivery) and a rise of 2.3 IU/dL for each IU/kg. The label's single-dose study gives 9.6 and 7.2 mL/h/kg, 126 and 156 mL/kg, 11.6 and 17.7 hours, and 2.24 and 1.94 IU/dL per IU/kg after 50 and 100 IU/kg.

### What is plotted

Antithrombin activity in **IU/dL** (% of normal): the patient's own plus the activity infused. The patient's own is the Patient Profile's **Antithrombin baseline**, shared with the plasma-derived product. Left blank, it is 44.7 IU/dL, the source population's (hereditary deficiency). Here, unlike in the plasma-derived model, the baseline does not change the volume.

The band is the label's target, 80 to 120% of normal (IU/dL).

### Covariates

- **Pregnancy**: under the Patient Profile's **Pregnant** (third trimester or term), the delivery values above: clearance × 2.08 and both volumes × 1.85. The label's dosing rule for pregnant women gives larger doses for the same reason.
- **Weight**: the source printed no body size and no covariate was retained, so its values belong to the 70 kg reference man and are scaled to the patient's fat-free mass. With the switch off they are the published values whatever the weight.
- **Heparin** was not studied as a covariate; the label warns that anticoagulants, surgery, delivery and bleeding may change the half-life.

### Doses

`IU/kg` or `IU` as a dose, and `IU/kg/hr` or `IU/hr` as an infusion. The label gives a 15-minute loading dose, then a continuous infusion: (100 − baseline) × weight ÷ 2.3 IU and ÷ 10.2 IU/hour in surgery, ÷ 1.3 and ÷ 5.4 in pregnancy, adjusted by 30% when the activity 2 hours later is outside 80–120%.

### Not modelled

The patient's own antithrombin is held constant; its production, and its consumption in surgery, delivery or by heparin, are not drawn.
