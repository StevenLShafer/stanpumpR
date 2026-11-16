### The model

Panetta and colleagues (*Cancer Chemother Pharmacol* 2003;52:435-441) measured temozolomide in 39 infants, children and young adults with brain tumours at St. Jude Children's Research Hospital, 0.7 to 21.9 years (median 7.1), taking 145 to 200 mg/m² once daily for 5 days. Temozolomide is absorbed quickly (absorption rate 2.4 per hour) into a single volume and leaves it with a half-life of about 2 hours.

### Body size

Clearance and volume both rise with body-surface area, on a logarithmic scale: in a child of 1.1 m² (the median) clearance is 5.0 L/h and the volume 12.7 L; at an adult's 1.8 m², 10.6 L/h and 27.2 L. The equations were fitted from 0.45 to 2.1 m². Outside that range, stanpumpR holds the clearance and volume per square metre at their values at the nearer limit, so they scale in proportion to body-surface area. That is a choice made in stanpumpR, not the paper's.

### What is plotted

Plasma temozolomide, in mcg/mL, after oral doses (bioavailability taken as 1, as in the paper, so the clearance and volume are apparent values). Temozolomide breaks down without enzymes to its active form, MTIC, which is not modelled. There is no therapeutic range, so none is drawn.

### Where to be careful

- **Age is not applied.** The paper found clearance per m² rising slightly with age (about 37% from 7 to 21 years) but printed no equation combining age with body-surface area, so only body-surface area is used.
- **Lower doses, lower clearance.** The paper saw clearance 17% lower, on average, when the dose was cut from 200 to 150 mg/m², without a model of it; the curve does not show this.
- **Pregnancy:** no pharmacokinetic data were found; nothing is changed.
