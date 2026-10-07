Most clinical events on the Events panel are annotations: they mark the time axis and change nothing. A few change the pharmacokinetics of drugs whose models have parameters for them.

## How it works

A drug's model function returns not one parameter set but a named list of them: `default`, and optionally one per event. When the dose table is simulated, the event table is scanned for events the drug knows about; at each such event the amount of drug in each compartment is carried across unchanged (`convertState()`), the parameters are switched, and the simulation continues with the new rate constants and eigenvalues (`advanceClosedForm1.R`). Because it is the amount that carries, the plasma concentration is continuous across an event only if V1 is: where V1 changes, as it does on bypass, plasma steps by the ratio of the old V1 to the new. Its slope changes in any case.

The parameter sets need not have the same number of compartments. A compartment the new set adds starts empty. Drug in a compartment the new set lacks moves to its remaining peripheral compartment, or to the central compartment if it has none, so no drug is created or lost at the event.

An event with the same name as a parameter set triggers that set until the next recognised event. Events the drug does not know about are ignored for that drug.

## Which drugs respond

At present one model: **dexmedetomidine in infants** (age ≤ 1 year), from Zuppa and colleagues' study of dexmedetomidine during infant cardiac surgery. It has parameter sets for

| Event | Meaning |
|---|---|
| CPB Start | On cardiopulmonary bypass at 37 °C |
| CPB36, CPB35, CPB34, CPB33, CPB32, CPB31 | On bypass, cooled to that temperature; V1 scales with (temperature/37)^-1.6 |
| CPB End | Off bypass; clearance recovers with a maturation term |

Clearance on bypass falls to a small fraction of its pre-bypass value (74 against 1240 mL/min at 70 kg before scaling), and the volumes change, so an infusion continued unchanged through bypass produces a rising concentration. Enter the events on the Events panel (Additional Plots → Events) with an infant patient and watch the curve bend at each one.

The adult dexmedetomidine model, and every other drug, has only the `default` set and ignores all events.

## Adding event-dependent kinetics to a drug

A model function adds a parameter set for each event it recognises and lists them in its `events` vector; the names must match the event names in `inst/extdata/eventDefaults.csv` with spaces removed. See [Contributing a drug or a model](help:contributing).

## Limits

Parameters step at an event; they do not ramp. Temperature on bypass is therefore entered as a sequence of CPB temperature events rather than as a continuous cooling curve, and the clearance recovery after bypass is a step followed by the model's own maturation term, not a gradual rewarming. The events are also not patient-specific: a CPB event means the same thing for every patient of a given age and weight.
