### The model

stanpumpR draws isavuconazole with Desai and colleagues' population model (Desai 2016, *Antimicrob Agents Chemother* 60:5483-5491). It pools 189 healthy adults from nine phase 1 studies and 232 patients with invasive fungal infections from the SECURE trial. It has two compartments. Intravenous and oral doses share every parameter, with oral bioavailability fixed at 1.

**Doses are entered as isavuconazonium sulfate**, the prodrug, as Cresemba is labelled and prescribed: 372 mg of isavuconazonium sulfate is 200 mg of isavuconazole. The curve is plasma isavuconazole. A standard regimen is 372 mg every 8 hours for six doses, then 372 mg once a day, intravenously or by mouth.

**The peripheral volume.** The paper's covariate equation for the peripheral volume does not reproduce the typical values its own Discussion gives: about 292 L in healthy subjects and about 390 L in patients. On Steven Shafer's decision of 28 November 2026, stanpumpR uses the patients' 390 L and leaves out the paper's BMI term on that volume. The paper's half-lives support 390 L: the model's terminal half-life is 138 hours, against the patients' reported mean of 130 hours.

**Race.** Desai found that Asian patients cleared isavuconazole 36% more slowly (1.51 against 2.36 L/h). The Patient Profile has no race field, so the curve uses the predominantly Caucasian clearance and will read low for an Asian patient.

### The oral absorption

Desai described oral absorption with a Weibull function. In that function the rate of absorption rises from zero to 1.08 per hour over the first two hours or so. stanpumpR's depots absorb at a constant rate, so each oral dose is split into a few portions given at fixed delays, each absorbed at 1.08 per hour, the rate the Weibull function settles to. The split was fitted to an exact solution of Desai's model. Over the whole curve it is held within 0.53% of the peak concentration, and within 0.01% at the peak itself, the same standard as hydrochlorothiazide's Weibull. The portions are a numerical stand-in for the published shape and say nothing about the capsule.

### Target concentrations

No band is drawn. No therapeutic range of isavuconazole concentrations is established, and the label gives none.

### Pregnancy

No pharmacokinetic study of isavuconazole in pregnancy was found. It is metabolised by CYP3A4, CYP3A5 and glucuronidation, and less than 1% of a dose is excreted unchanged in the urine, so the rise in filtration of pregnancy does not reach it. The rise in CYP3A activity in pregnancy could raise its clearance, but nothing quantifies that, and nothing is applied.
