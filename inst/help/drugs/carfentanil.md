### The pharmacokinetics here are assumed, not measured

Read this section before using the curve.

**There is no published human pharmacokinetic model of carfentanil.** That is an absence, not a thin literature. Every human study that measures carfentanil uses [¹¹C]carfentanil as a PET *radiotracer*, to quantify how much of some other drug — naloxone, nalmefene, buprenorphine — is occupying the µ-opioid receptor. Those are microdoses reporting brain binding, not carfentanil's plasma kinetics at a dose that does anything. The only in-vivo carfentanil pharmacokinetics are in rats.

This model therefore runs on **sufentanil's disposition**, on the judgement of **Shafer** that carfentanil's pharmacokinetics are likely identical to sufentanil's. The two are close congeners from the same 4-substituted phenylpiperidine series and share the physicochemistry that sets volumes and clearance in this class: high lipophilicity, high protein binding, high hepatic extraction. That is an informed assumption by a pharmacokineticist, recorded as one. It is **not a fitted carfentanil model** and must not be cited as one.

The parameters are not copied into this drug — the function calls sufentanil's and returns its vector, so the two are identical by construction and any future correction to sufentanil carries across. The underlying numbers are Gepts and colleagues (*Anesthesiology* 1995;83:1194-1204), a 70 kg adult: V1 14.3 L, V2 63.1 L, V3 261.6 L, clearance 0.92 L/min.

### What the assumption covers, and what it does not

It covers the **shape of the curve** — how fast the concentration falls after a dose, and how much accumulates during one. It does not make the two drugs interchangeable, because they differ enormously in **potency**, and potency is not a pharmacokinetic quantity.

**Shafer** puts carfentanil at roughly **20 times the potency of fentanyl**. Published estimates generally run higher, commonly around 100 times, derived from the frequently quoted figure of 10,000 times morphine; receptor-affinity ratios fall in between. Nothing in this model uses any of those numbers. Like every drug in the illicit category this one is **plasma only**, so no potency ratio enters the simulation at any point. The figure is here so that a reader can scale the curve themselves, knowing exactly which number they applied and whose it is.

That separation is the point. A potency ratio carries no kinetic information at all, so it can never stand in for a pharmacokinetic model: simulating fentanyl and relabelling the axis would assert fentanyl's half-life and fentanyl's context-sensitive decrement curve for carfentanil, which the rat data and the clinical pattern of carfentanil overdose both contradict. Borrowing a close congener's *disposition* is a different and much weaker claim, and it is the one being made here.

### What is plotted

**Venous plasma**, in ng/mL, on the assumed disposition. No effect site: sufentanil's equilibration half-time is deliberately not carried across, because the assumption that was made is about how the body handles the drug, not about how fast the receptor sees it. There is no MEAC and no shaded band.

### Route

**Intravenous only.** Gepts' parameters are absolute volumes and clearances from an intravenous study, so they predict an intravenous dose correctly. Carfentanil is also encountered insufflated and smoked; neither is offered, because no absorption model for carfentanil exists for either and stanpumpR has no pulmonary route at all. Both are recorded in the route registry rather than quietly left out.

### Where to be careful

The rat data (Bergh and colleagues, *Neuropharmacology* 2019;158:107596) found carfentanil's kinetics to be **non-linear**: the area under the curve rose faster than the dose, and the half-life lengthened as the dose rose. The authors hypothesised that impaired clearance at high doses contributes to how dangerous the drug is in people. This model is linear, as sufentanil's is, so if that non-linearity is real in humans the curve here will **under**-predict both the exposure and the duration at the doses that actually cause harm — which is the direction that matters. It is also consistent with the clinical picture of carfentanil overdose, where renarcotization and repeated naloxone dosing are reported.

Treat this curve as a structured way to think about an ultrapotent opioid's disposition, with the provenance of every part of it visible, and not as a measurement of carfentanil.
