### The model

Oxaliplatin comes from Bastian, Barrail and Urien (*Anticancer Drugs* 2003;14:817-824): 56 French adults with metastatic cancer, 66 courses of 50 to 130 mg/m² given over 2 or 4 hours, sampled for 28 hours. Two compartments describe the drug, with one addition that matters:

- clearance from the central compartment of 14.1 L/h in the typical man (71 kg, serum creatinine 87 µmol/L);
- a central volume of 24.9 L, a peripheral volume of 136 L and an intercompartmental clearance of 34.8 L/h;
- **a gallbladder store.** From the start of each course, drug passes from the central compartment into a store, "likely the gallbladder", at 1.78 per hour, which is three times faster than it is cleared. Two hours after the course starts (TLAG) the gallbladder empties all at once into the gut, and from there the drug returns to the blood at 0.23 per hour. After that moment nothing more is stored.

So the curve falls very steeply once the infusion stops (the store is still taking drug), then flattens or rises a little as the stored drug comes back, and then declines with a half-life of about 10 hours in men and 15 hours in women. The whole of the stored drug returns, so the area under the curve is the dose divided by the clearance, as the paper calculates it.

The model reproduces the paper's numbers. For the typical man given 230 mg over 2 hours, the peak is 1.38 mcg/mL at the end of the infusion, and the concentrations at 4, 12 and 24 hours are 0.79, 0.48 and 0.22 mcg/mL. The paper's example area, 230 mg divided by its median clearance of 11.3 L/h, is 20.35 mg·h/L.

### Two things this structure does that a user will notice

- **A longer infusion of the same dose peaks higher.** Because the gallbladder stops taking drug 2 hours after the course starts, the second half of a 4-hour infusion meets no biliary sink. The same 230 mg peaks at 1.75 mcg/mL over 4 hours against 1.38 over 2 hours. This follows from the published structure; it is not something measured.
- **Each course has its own clock.** A bolus, or an infusion from the moment its rate rises above zero until it is stopped, is one course, with its own store emptying 2 hours after it starts. Courses are simulated separately and added, which is exact because the model is linear in the dose. A change of rate during an infusion does not start a new course.

The paper estimated very large variability in the timing (282%) and the rate of return (391%), so for any one patient the secondary rise could come much earlier or later than the curve shows.

### How the gallbladder was read

Bastian and colleagues cite Plusquellec and Houin (*J Biomed Eng* 1992;14:521-526) for their equations. In Plusquellec's model the transfer into the gallbladder never stops: the gallbladder empties at chosen times, and after its last emptying it fills again and keeps what it holds. With the single emptying Bastian fitted, the gallbladder would then be a sink: the terminal half-life would be about 4 hours and only about a third of the dose would ever be seen in the plasma. Bastian's own results contradict that reading. They compute the area as dose over clearance, and their terminal half-lives (10.5 hours in men, 14.4 in women) are those of the two-compartment part alone (10.3 and 15.5 hours at the typical values). The model that matches the paper's numbers stops filling the gallbladder when it empties. Steven Shafer decided (6 November 2026) to implement the model as published, in that form.

Physiologically, bile flows continuously, and that is Shafer's own view. A continuous model is the older form of enterohepatic recirculation: Colburn and colleagues (*Drug Metab Dispos* 1979;7:100-102) described phenolphthalein in the rat by continuous biliary transfer from the central compartment, a fixed delay of 3.7 hours for transit and bacterial hydrolysis, and reabsorption, with a small fecal loss. Bastian's parameters cannot be moved into such a model: run continuously they give terminal half-lives of about 20 hours in men and 32 in women, and concentrations 30 to 55% below the fitted curve between 2.5 and 12 hours. A continuous model of oxaliplatin would have to be fitted again to data.

### What is plotted

Ultrafilterable (unbound) plasma platinum, expressed as oxaliplatin, in mcg/mL (mg/L). The assay measured platinum; the paper reports every concentration as oxaliplatin and computes the area as the dose in mg of oxaliplatin over the clearance, so doses and concentrations are on the same oxaliplatin basis (as platinum mass the values would be about half). Doses are entered in mg or as an infusion rate; a dose prescribed per m² (85 to 130 mg/m² in practice) must be multiplied by the body surface area first. The compartment panels show the peripheral concentration and, as the depot, the drug held in the gallbladder and the gut that has yet to return. There is no effect site and no therapeutic range.

Samples were taken to 28 hours. Delord and colleagues (*Cancer Chemother Pharmacol* 2003;51:127-131) excluded their 24-hour ultrafiltrate samples, judging the platinum then to be bound to amino acids rather than the active drug, so the late tail of an ultrafilterable platinum curve is not all active oxaliplatin.

### Covariates

- **Weight**, on the clearance, the central volume and the intercompartmental clearance, relative to 71 kg. With the fat-free-mass switch on, the weight used is the patient's pharmacokinetic weight.
- **Serum creatinine**, as (creatinine / 87 µmol/L)^−0.57 on clearance. A blank field is the library's assumed normal creatinine (0.9 mg/dL in men, 0.8 in women), which gives a man a clearance 5% above the paper's typical value. A creatinine below 53 µmol/L (0.6 mg/dL), the lowest studied, is taken as 53. The highest studied was 567 µmol/L.
- **Sex**: women's clearance is 0.60 times men's, a large effect with wide uncertainty (standard error 0.25).
- Age, body surface area, creatinine clearance, serum proteins and infusion length were tested and not kept.

### Where to be careful

- **The gallbladder structure is the paper's,** and its consequences (the infusion-length effect, the single emptying) are properties of the model, not of oxaliplatin.
- **Not in pregnancy:** no pregnancy pharmacokinetics are known, and nothing is adjusted.
- **Adults only. Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read on the page images of the paper's PDF. See [chemotherapy](help:models/chemotherapy) for the other models considered, including Delord 2003.
