### The model

Metronidazole's intravenous parameters are from da Silva Neto and colleagues (*J Antimicrob Chemother* 2021;76:3212-3219), who fitted total plasma concentrations in 20 adults receiving prophylaxis for colorectal surgery: clearance 3.22 L/h at 70 kg, volume 0.556 L per kilogram of **adjusted** body weight (Devine ideal weight plus 40 per cent of the excess), one compartment. Half-time at 70 kg is 8.4 hours.

### Oral route

The source is intravenous only. Bioavailability, 0.841, comes from Bergan and colleagues' paired-route study of conventional tablets (1984); the absorption rate constant, 1.336/h, is the only published first-order estimate found, from six volunteers given a laboratory-made 150 mg tablet. Both are cross-study additions and are flagged as such in the code. Extended-release metronidazole is a different product that this input does not describe.

### Covariates

Weight, two ways: clearance allometrically on total weight, volume on adjusted body weight. Under the default [fat-free-mass scaling](help:models/fat-free-mass) both are expressed at the reference man and scaled by the fat-free-mass factors; with the switch off the published equations run on the patient's own total and adjusted weights.

### Effect site

None; only the plasma concentration is plotted. The shaded band (4 to 25 mcg/mL) runs from the source's MIC scenario of 4 mg/L to the peak a 500 mg dose produces.

### Where to be careful

Sampling in the source began an hour after the dose, so an early distribution phase cannot be excluded and the first hour after a fast infusion is the least certain part of the curve. Hydroxymetronidazole, an active metabolite about 65 per cent as potent against the tested anaerobes, is not modelled, and neither is hepatic impairment.
