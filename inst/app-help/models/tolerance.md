Many drugs lose their effect with continued exposure. **Tolerance** is the slower form, best known for the opioids: the same concentration does less, so a higher one is needed for the same effect. **Tachyphylaxis** is the faster form, common among the drugs of intensive care. Neither changes the pharmacokinetics. The concentration in the plasma and at the effect site is what the model says it is; what changes is how much effect that concentration produces.

## How stanpumpR shows it

Tick **Graph Options → Tolerance and tachyphylaxis**. The option is offered only while a drug with a published tolerance or tachyphylaxis model is drawn. Under that drug's panel (and under its peripheral compartments, if they are shown) a panel named, for example, *nitroglycerin tachyphylaxis* starts at **100%** and falls. For a tolerance it shows the share of the drug's naive **potency** that is left (*% of naive potency*); for an attenuation, the share of its naive **effect** (*% of naive effect*).

Read it as a weather report gives wind chill: *the temperature is 20 degrees, but with the wind chill it feels like 5*. Here, the concentration is what the drug panel shows, but the drug **works like** a lower one. Hovering over the panel gives both: the concentration, and the concentration it works like. At 50%, a concentration of 2 ng/mL works like 1 ng/mL did at the start.

For a loss of potency (tolerance), 100% / P is exactly that ratio: the EC50, and so the MEAC, has risen by the factor P, and C / P is the concentration that would give today's effect in a drug-naive patient. It is not the share of the effect that is left, which depends on where the concentration sits on the drug's curve: more of the effect is kept near the top of the curve, less near the bottom. At P = 1.5, with a Hill coefficient of 2.61, a concentration at the naive EC50 keeps about half its effect. For a loss of efficacy (an attenuation of the effect itself), the percentage multiplies the effect, and the "works like" concentration is exact when the effect is proportional to the concentration.

## The models

Each model is a row of the table below, taken from a published study. A row may cover a single drug, or a whole class scaled by potency (see below). A drug that no row covers has no panel: stanpumpR does not guess how quickly or how far a drug's effect fades.

**Tolerance compartment** (Porchet, Benowitz and Sheiner 1988; Ekblom and colleagues 1993). A hypothetical compartment fills from the driving concentration C, the effect site unless the source drove it from plasma, at a first-order rate k, and empties as the drug washes out:

dC<sub>t</sub>/dt = k (C − C<sub>t</sub>)

The EC50 rises by P = 1 + (C<sub>t</sub> / c50)<sup>hill</sup>, or, where the source found the tolerance saturates, by P = 1 + (P<sub>max</sub> − 1) C<sub>t</sub><sup>hill</sup> / (c50<sup>hill</sup> + C<sub>t</sub><sup>hill</sup>). Tolerance develops with exposure, with a half-time of ln 2 / k, and recovers on the same time scale after the drug is stopped.

**Competitive tolerance compartment** (Ouellet and Pollack 1995). The same compartment, but what accumulates in it competes with the drug for its receptor, as a partial agonist of high affinity would. The EC50 rises by P = (1 + (C<sub>t</sub> / c50)<sup>hill</sup>)<sup>1/hill</sup>, with the drug's own Hill coefficient as hill. Because c50 is far below the concentrations that work, tolerance comes on within hours and takes days to go, from a single rate constant: in Ouellet and Pollack's rats, morphine's effect fell to about a quarter within 12 hours, and recovery took more than two weeks.

**Attenuation** (Chow and colleagues 1985). An attenuation factor starts at 1 when the drug is first present and falls at first order towards a floor (zero unless the source found one). It does not recover in the source's data.

## From animals to patients

Tolerance cannot be studied formally in patients, so most of these models come from animals, mostly rats, and are scaled to a 70 kg adult in two ways:

- **Time.** Durations scale with body size. A mouse's heart beats faster and its life is shorter than a human's, and a whale's is longer yet. Across mammals, the heartbeat, the breath, drug half-lives and the lifespan all grow as roughly the **one-quarter power of body weight** (Lindstedt and Calder 1981; Boxenbaum 1982). A tolerance half-time measured in 0.3 kg rats is multiplied by (70 / 0.3)<sup>0.25</sup>, about 3.9, for a 70 kg adult. The table gives both values. The quarter power is well founded for times set by transport through the body, such as the effect-site delay. It is less certain for processes inside the cell, such as receptor desensitization: cultured cells run at the same rate whatever the size of the animal they came from. Where a source supports a smaller exponent, its row gives its own. For the opioids the quarter power is kept: it puts the rat's tolerance on the scale of hours to days in a 70 kg adult, as the volunteer studies that found no acute tolerance over a few hours of remifentanil (Gustorff 2002; Angst 2009) require. One volunteer study did find tolerance within 2 to 3 hours (Vinik and Kissin 1998); matching it would need an exponent near zero.
- **Potency.** The concentration that drives tolerance is set relative to the drug's own potency. The study gives it as a multiple of the studied drug's EC50 in the animals, and stanpumpR applies that multiple to each opioid's **MEAC** from the Drug Library. One rat study of morphine (Ouellet and Pollack 1995) can therefore stand for every opioid: equipotent concentrations of fentanyl, morphine or sufentanil lose the same share of their effect, on the same time course. Opioids without a MEAC (codeine, tramadol, buprenorphine) get no panel.

A drug's own model, where one exists, comes before its class's.

The tolerance compartment is computed exactly between the simulated points, reading the concentration as a straight line between them, which is how the plot draws it. Nothing is simulated again, and the panel follows the drug's own curve, whatever the route, dose or model.

<!-- generated: pd-adaptation-table -->

## What it does not do

The panel changes nothing else. The drug's band, MEAC, time until threshold, % MEAC and the interaction panels are those of a drug-naive patient. The **Opioid tolerance** field of the Patient Profile is a separate thing: it describes a patient who arrives tolerant, and raises every opioid's MEAC by a fixed factor from the start. The panel describes tolerance developing during the simulation, from 100%.

Tolerance in these models follows the drug's concentration and nothing else. Disease, other drugs, opioid-induced hyperalgesia and the patient's history are not part of them.
