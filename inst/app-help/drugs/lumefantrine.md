### A component of Coartem

Lumefantrine is given in the United States only in [Coartem](help:drugs/Coartem). Each tablet contains 120 mg. It cannot be entered in the dose table on its own; its curve comes from the Coartem you give.

### The model

The parameters come from the lumefantrine model of Ding and colleagues (*Br J Clin Pharmacol* 2026;92:589-605, doi 10.1002/bcp.70301, Table 4). It was fitted to 1,448 concentrations from 885 patients with uncomplicated falciparum malaria in the TRACII and TACT-CV trials, aged 1.9 to 65 years and weighing 9 to 101 kg. The parameters are apparent (oral) values for a typical 45 kg patient:

- **Absorption:** the dose passes through a depot and five transit compartments, with a mean transit time of 5.43 h. Each step runs at 6/5.43 = 1.10 per hour, read as for [artemether](help:drugs/artemether).
- **Clearance:** CL/F 4.35 L/h × (weight / 45)<sup>0.75</sup>.
- **Volumes:** central 101 L and peripheral 311 L, each × weight / 45.
- **Intercompartmental clearance:** Q/F 1.65 L/h × (weight / 45)<sup>0.75</sup>.

The weight exponents were fixed, not estimated.

### The dose per kilogram

Lumefantrine's absorption is limited by the dose. In Ding's model, the bioavailability falls by 6.47% for each mg/kg of a dose above 9.8 mg/kg. stanpumpR applies this to each dose on the patient's actual weight:

- A 45 kg patient's four tablets (480 mg, 10.7 mg/kg) are absorbed at 0.94 of the reference.
- A 70 kg patient's four tablets (6.9 mg/kg) are absorbed at 1.19.

The authors say this straight-line relation "is not suitable for extrapolation beyond the dose range studied". So the dose is held within the range they studied, 4.75 to 16.0 mg/kg a dose.

Two other terms in the model act on bioavailability: parasitaemia (11.6% less per tenfold rise) and fever (12.2% less per °C). Both are held at the reference values, 10<sup>4.56</sup> parasites/µL and 37.5 °C, because the Patient Profile has neither.

### Why Ding and not the WWARN model

Kloprogge and colleagues' pooled analysis (*PLoS Med* 2018;15:e1002579) is larger: 1,347 patients. But it describes absorption with a single slow rate of 0.0386 per hour, fitted mostly to sparse samples taken on day 7. With that rate, a single dose would peak at the end of the third day. The label, and every densely sampled study, puts the peak 6 to 8 hours after the dose. Ding's transit chain, fitted with 79 densely sampled patients, gives that peak.

The two models agree on what the plot mostly shows, the clearance and the day-7 level:

- Kloprogge's clearance at Ding's reference dose is about 4.8 L/h; Ding's is 4.35.
- Both put the typical day-7 concentration near 460 ng/mL.

### Checks

For the typical 45 kg patient on the label's six doses of four tablets, this model gives:

- a day-7 concentration of 462 ng/mL, against Ding's median of 452;
- an area to 21 days of 591 h·mcg/mL, against 600;
- a peak of 7.2 mcg/mL. The label's patients averaged 5.6 to 9.0 mcg/mL after six doses.

Ding's own median peak, 15.2 mcg/mL, is twice that. It was reported only for the densely sampled patients, and the reason for the difference is not known.

### Pregnancy

With the Pregnant field ticked, the clearance is multiplied by 1.232 and the central volume by 1.281. These factors come from Juma and colleagues (*Br J Clin Pharmacol* 2026;92:911-921), the same group's model in 50 pregnant and 25 non-pregnant women with malaria in Kenya. Pregnant women had lower exposure: their day-7 concentration was 0.69 times the non-pregnant women's, and 44% of them, against 20%, were below 200 ng/mL.

### The day-7 concentration

No therapeutic range is established, and no band is drawn. The established marker is a single concentration on day 7 after the first dose: below it, the infection is more likely to return. Kloprogge lists the targets of 175, 200 and 280 ng/mL used in different studies. It is a threshold at one time, not a range.

### Not modelled

- Food. The curve is for a fed patient. Fasting lowers lumefantrine's bioavailability about sixteenfold.
- Interactions through CYP3A4. The label lists ketoconazole (lumefantrine AUC ×1.6), lopinavir/ritonavir (×2.3), rifampin (−68%) and efavirenz (−20%).
- Desbutyl-lumefantrine, whose exposure is under 1% of the parent's.
- The QT prolongation the label links to lumefantrine concentrations.
