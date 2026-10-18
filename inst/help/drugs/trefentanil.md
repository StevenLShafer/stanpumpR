### The model

Trefentanil (ANQ-3665) is a 4-phenylpiperidine µ-agonist that reached phase 1 and was never marketed. Its parameters come from **Lemmens**, Dyck, **Shafer** and **Stanski** (*Clin Pharmacol Ther* 1994;56:261-271), who gave it to healthy men with arterial sampling and the spectral edge of the EEG as a continuous measure of drug effect. That makes trefentanil one of very few fentanyl analogs with a complete, identified human model of both the pharmacokinetics and the pharmacodynamics — most of the others have neither.

The study ran in two phases, and both produced parameters. An infusion-rate-assessment phase in nine volunteers established the rate that gave a maximal EEG effect in 3 to 10 minutes, and a crossover phase in five volunteers then compared trefentanil with fentanyl and alfentanil in the same subjects. **The crossover phase is what is implemented here.** It is the set the paper's own abstract and discussion quote as trefentanil's, its kinetics and its dynamics come from the same five people, and it is the arm designed as the head-to-head against the two congeners this library already carries. The paper found no difference between the phases. The infusion-rate phase would give a somewhat larger model — central volume 4.5 L against 3.5, steady-state volume 39.3 L against 37, clearance 0.483 L/min against 0.444 — and is not implemented.

### How the compartments were obtained

The paper prints no micro-constants. For each subject it gives the **fractional coefficients** and **hybrid rate constants** of the triexponential disposition function together with the central volume, which identifies the three-compartment model completely:

> c(t)/Dose = (1/V<sub>c</sub>) × (A e<sup>−αt</sup> + B e<sup>−βt</sup> + C e<sup>−γt</sup>), with A + B + C = 1

The obvious move — inverting the printed *mean* row — is wrong, and visibly so. Averaging does not commute with a non-linear inversion, so the mean coefficients carry neither the mean volume nor the mean clearance: inverting the mean row gives a steady-state volume of 40.0 L against the paper's own reported mean of 37, a clearance of 0.482 against 0.444, and a distribution clearance of 1.31 against 1.11. Those are errors of 8%, 9% and 18% against numbers printed in the same table.

So each of the five subjects is inverted separately and the resulting volumes and clearances averaged. That reproduces **every subject's own** published steady-state volume, elimination clearance and distribution clearance to within 2% — the residual being the two significant figures the coefficients are printed to — and the averages then land on the paper's means of 37 L, 0.444 L/min and 1.11 L/min. None of those nine numbers is used to build the model, so they are a genuine check on it.

### What is plotted

**Arterial plasma**, in ng/mL. The study sampled the radial artery during and after the infusion and switched to venous sampling only from 3.5 hours onward.

### Covariates

None: the paper fits no weight, age or sex model. The five crossover subjects were men weighing 77, 88, 78, 81 and 75 kg, a mean of 79.8 kg, so the derived parameters belong to a 79.8 kg man. This library's reference patient is a **70 kg, 170 cm man**, who must receive a model's parameters unchanged, so the set is carried to 70 kg allometrically on total weight — volumes by 70/79.8, clearances by (70/79.8)<sup>0.75</sup> — before [the scaling switch](help:models/fat-free-mass) takes it to the patient. Heights were not reported, so the subjects' fat-free mass cannot be computed and this step cannot be done on the fat-free scale. Skipping it would hand a 70 kg patient the parameters of a 79.8 kg one: volumes 14% too large, clearances 10% too large.

### Effect site

Published, and unusually well measured. The equilibration half-time between arterial concentration and EEG effect was **1.20 ± 0.50 minutes**, estimated semiparametrically by collapsing the hysteresis loop between concentration and spectral edge. That is supplied directly as k<sub>e0</sub> rather than through a time to peak effect, and the time to peak effect is then solved from it, giving about 2.1 minutes at the reference patient. For comparison, the same study measured 0.60 minutes for alfentanil and 5.4 minutes for fentanyl — trefentanil equilibrates nearly as fast as alfentanil and far faster than fentanyl.

### No concentration target, and why

The study reports an EC<sub>50</sub> of **429 ± 313 ng/mL**, and it is deliberately *not* carried as a MEAC or drawn as a band. It is the effect-site concentration producing half of the maximal slowing of the EEG spectral edge, which is not a minimum effective analgesic concentration. The scale of the difference is visible in the paper's own table: it gives **fentanyl** an EEG EC<sub>50</sub> of 9.8 ng/mL, where this library carries 0.6 ng/mL as fentanyl's MEAC — sixteen-fold apart. The authors are explicit:

> a direct calibration of EEG and clinical opioid effects (analgesia and anesthesia) has not been performed to date. Therefore the predictive validity of the EEG as a surrogate effect measurement to the clinical situation cannot be uncritically assumed.

Trefentanil was never given to patients, so no analgesic target exists to put in that slot. The effect-site curve is drawn; nothing is drawn beside it.

### What the model is for

The paper's own headline is its Figure 6: the time for a 50% decrease in effect-site concentration after infusions from 10 minutes to 10 hours. Trefentanil recovers fastest at every duration, and the margin widens as the infusion lengthens — about 26 minutes after a 10-hour infusion in this implementation, against roughly 63 for alfentanil and 237 for fentanyl using this library's own models of those two drugs. That is a statement about **context-sensitive decrement**, not about elimination half-life: trefentanil's terminal half-life, 127 minutes, is *longer* than alfentanil's 114. Faster recovery comes from the combination of a higher metabolic clearance and a larger volume of distribution, which is precisely the point the paper was making about why half-life is the wrong number to reason from.

### Where to be careful

Five subjects, all healthy young men, and the implemented parameters are a **mean of individually fitted parameters** rather than a population model — there is no between-subject variability to draw on, and the five subjects' steady-state volumes ranged from 28 to 44 L and their clearances from 0.34 to 0.52 L/min. The drug never went beyond phase 1, so nothing here has been tested against a clinical endpoint in a patient. Respiration was depressed in all subjects and had to be mechanically assisted, and muscle rigidity occurred in all but the two given the lowest infusion rates, so the concentrations simulated here are anaesthetic-range concentrations in a controlled setting. Treat the curve as a disposition and EEG-effect model, which is what it was built to be.
