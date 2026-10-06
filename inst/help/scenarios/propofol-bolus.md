## What to look for

The plasma line (dashed) peaks at once and falls steeply as propofol leaves the blood for muscle and fat. The effect-site line (solid) starts at zero, rises, and crosses the plasma line at **1.6 minutes**, the time to peak effect. After that the effect site falls more slowly than the plasma, because it is now the plasma that is lower.

Hover over the two lines at one minute. The plasma concentration is several times the effect-site concentration: a blood sample taken then would badly overstate how much drug is at the site of action.

The effect-site peak is a fraction of the plasma peak. That fraction is set by how fast the drug redistributes compared with how fast it reaches the brain, and it is why a bolus is a less efficient way to reach a given effect-site concentration than it looks.

## Try next

- Open **Graph Options** and switch *Normalize to* to *Peak effect site*, then add a fentanyl row (100 mcg at 0) and apply. Fentanyl's effect-site peak comes later (3.7 minutes); morphine's would come at 94.
- Change the dose to 1 mg/kg and apply. Everything halves; the shape, and the time to peak, do not change. That is what a linear model means.
- Replace the bolus with an infusion of 150 mcg/kg/min and see how long the effect site takes to reach what the bolus reached in two minutes.

## Background

The model is Eleveld's general-purpose propofol model with a time to peak effect from Schnider 1999; see [Propofol](help:drugs/propofol). Why the effect site lags, and how ke0 is found from tPeak, is explained under [The effect site and ke0](help:models/effect-site).
