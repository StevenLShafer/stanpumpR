## What to look for

Two target-controlled infusions for a 70 kg, 170 cm, 40-year-old man: an effect-site target of 3 mcg/mL propofol and one of 4 ng/mL remifentanil, both set at the start and both set to 0 at sixty minutes. With **Minto / Schnider** ticked, each target is run by two pumps: one on the library's Eleveld model, one on the Schnider propofol or Minto remifentanil model that a commercial pump would run. The second pump's rates are the second line in each TCI rate panel, and the concentrations its model predicts are the second line in each concentration panel.

- **Every pump hits its own target.** All four effect-site lines rise to their target without overshoot and hold there until sixty minutes. A TCI controller always succeeds against the model it was given; what the models disagree about is how much drug that takes.
- **The rate panels say how much.** The loading doses are written on the panels: 51 mg of propofol on Eleveld's model and 55.9 mg on Schnider's, 69 mcg of remifentanil on Eleveld's and 68 mcg on Minto's. The maintenance rates then differ by a few per cent, 0.103 against 0.110 mg/kg/min of propofol at thirty minutes, and over the hour the Schnider pump gives 528 mg of propofol where the Eleveld pump gives 498, the Minto pump 710 mcg of remifentanil where the Eleveld pump gives 691. The same patient, the same targets, six per cent more propofol and three per cent more remifentanil from one model than the other.
- **The plasma lines (dashed) differ more than the effect-site lines.** The Schnider pump's loading dose goes into a central volume of 4.27 L rather than 6.3, so its plasma concentration overshoots to 12.1 mcg/mL against 7.7 before the effect site catches up.
- **The time until threshold** (the thin black line, read on the right axis) is computed from the Eleveld model only: about 11 minutes at thirty minutes and 15 minutes at the end of the propofol infusion. The second model never carries one.
- **Hover** over a rate panel and the readout gives both pumps' rates, or both loading doses, at that moment.

## Try next

- Untick *Minto / Schnider*: the second pump and its lines go, and the panels show the library's model alone. Tick it again.
- Change *Effect site target* to *Plasma target* for both drugs. Each pump now fills its own central compartment and holds the plasma at the target; the loading doses shrink, and the gap between the two propofol pumps' loading doses, which is a gap in V1, is easier to read.
- Set the age to 80 and apply. The Minto model's ke0 falls with age, so the Minto pump's loading dose for the same 4 ng/mL target changes, and its effect site takes longer to reach the target; the Eleveld pump's does not change in the same way. Hover on the remifentanil rate panel at the start to read the two loading doses.
- Raise the propofol target to 5 and see both loading doses grow in proportion.
- Make the patient a 150 kg, 160 cm woman, or load [the obesity scenario](scenario:james-lean-body-mass), which does. The Schnider pump then gives nearly twice the propofol of the Eleveld pump, because the James lean body mass falls with weight in the obese.

## Background

[Target-controlled infusion](help:tci) explains the controller and the hand-off from effect-site to plasma control. [The Minto and Schnider models](help:models/minto-schnider) gives the models and says how the second pump is simulated. [The simulation-mode scenario](scenario:eleveld-vs-minto-schnider) compares the same models under a bolus and an infusion.
