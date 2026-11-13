### The model

Amitriptyline's parameters come from Koh and colleagues (*J Clin Pharmacol* 2019;59:532-540), who modelled amitriptyline and its active metabolite nortriptyline together in 24 healthy Korean men, six in each of four CYP2C19 and CYP2D6 genotype groups, sampled to 96 hours after a single 25 mg dose. Two compartments, apparent oral parameters: clearance 92.1 L/h in a normal metaboliser of both enzymes, 61.5 L/h of it forming nortriptyline (CYP2C19) and 30.6 L/h by other pathways (CYP2D6); central volume 155 L, intercompartmental clearance 109 L/h, peripheral volume 1,670 L. The terminal half-life is about 24 hours. The source's enterohepatic recirculation, a single-dose feature worth 0.7% of the area, is left out.

### CYP2C19 and CYP2D6

The Patient Profile's **CYP 2C19** field sets the formation of nortriptyline: a poor metaboliser forms it at 17 L/h instead of 61.5, so less nortriptyline and more amitriptyline. The rapid and ultrarapid phenotypes take the normal value (the source did not test for \*17). **CYP 2D6** sets the other pathways: an intermediate metaboliser clears 18.8 L/h by them instead of 30.6 (the source's \*10/\*10 and \*10/\*5); a poor metaboliser takes the same 18.8 L/h, an extrapolation that probably overstates a true poor metaboliser's clearance, and an ultrarapid one the normal value. The CYP2D6 effect was not statistically significant in the source.

### Absorption

The source released each dose into the gut at a constant rate over 1.13 hours, after a 25-minute lag, and absorbed it with a rate constant of 0.125 per hour. Because the nortriptyline it forms is computed alongside, that release is approximated by four quarter doses spaced through it, within 1.3% of the exact curve at the peak.

### The nortriptyline it forms

The nortriptyline curve is drawn with the library's own [nortriptyline](help:drugs/nortriptyline) model, so that it adds correctly to any nortriptyline given directly. Koh's data identify only the ratio of nortriptyline formation to nortriptyline clearance (about 1 in a CYP2C19 normal metaboliser), so the formation is that ratio times the library's nortriptyline clearance. At steady state this reproduces the consensus guideline's concentration per dose of the nortriptyline formed (0.389 against 0.376 ng/mL per mg daily). After a single dose the library's nortriptyline, slower and more widely distributed than Koh's, peaks at about half the height Koh observed (5.9 against Gupta 1999's 12.0 ng/mL), with the same area.

### Dosing

Doses are amitriptyline hydrochloride, as the US tablets are labelled (10 to 150 mg). The source did not name its salt; read as the hydrochloride converted to base (× 0.884), the model reproduces an independent study's area after 75 mg (Gupta 1999: 688 against 677 ng·h/mL).

### No band

The consensus guideline's range (Hart 2026), 80-200 ng/mL 12 hours after a dose, is for amitriptyline **plus** nortriptyline, and the two are about equal at steady state, so it is not drawn on the amitriptyline curve; nortriptyline's own range is drawn on its curve. The 24-hour trough of amitriptyline alone runs below the guideline's concentration per dose (0.23 against 0.40 ng/mL per mg), its average over the day matching.

### Where to be careful

Healthy young Korean men, many carrying CYP2D6\*10; a single dose. Fluoxetine, paroxetine and bupropion (CYP2D6) and fluvoxamine (CYP2C19, CYP1A2) raise amitriptyline; none is modelled.
