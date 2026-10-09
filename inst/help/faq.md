## Why did nothing happen when I typed a dose?

Edits to the dose table go into a draft. Press **Apply Changes** to simulate. The button is grey until the draft differs from the applied table. Clicking the plot to add a dose is the one shortcut: that applies immediately.

## My infusion never stops

An infusion runs until the next row for the same drug changes it. Add a row for that drug at the stop time with a dose of 0 in the same rate units.

## The time I typed turned into something else

The *Time* field accepts a number of the **Time units** (`12`, `1.5`), and with the *Actual time* display a clock time with a colon (`09:30`); `130` is 130 of the unit, not 1:30. Minutes above 59 roll over, so `0:80` becomes `01:20`. Anything it cannot read becomes zero. If the time display is set to *Actual time*, a time with a colon is a clock time and a number is counted from the procedure start. Changing the Time units rewrites every time in the table in the new unit, so `90` minutes becomes `1.5` hours: the doses have not moved. See [Time display](help:time-display).

## Why can I not pick the unit I want?

The *Units* list depends on the drug in that row and comes from the drug library. Propofol offers `mg`, `mg/kg`, `mcg/kg/min` and `mg/kg/hr`; oxycodone offers only `mg PO`. Each drug's page under [Drug library](help:drugs/index) lists its units, and the library can be edited under Settings.

## Where is the plasma concentration?

Hidden by default, because the effect site is usually what matters clinically. Turn it on under **Graph Options → Plasma line**.

## The effect-site line rises after the bolus has clearly stopped. Is that a bug?

No. The effect site equilibrates with the plasma over minutes, so its concentration keeps rising while the plasma falls, until the two cross at the time of peak effect. See [The effect site and ke0](help:models/effect-site).

## Why are two drugs on such different scales?

Because they are. Propofol is in micrograms per millilitre and remifentanil in nanograms per millilitre. **Graph Options → Normalize to** rescales every curve to its own peak so shapes can be compared; the MEAC panel compares opioids on a common axis of analgesic effect. See [Normalization](help:models/normalization) and [MEAC](help:models/meac).

## What is the thin black line and the numbers at the right edge?

*Time until threshold* (Graph Options). At each moment it shows how long the concentration would take to fall to the drug's recovery threshold if delivery stopped then. See [Time until threshold](help:models/recovery).

## Why can I not turn on the log axis?

It is unavailable while *Time until threshold*, the Events panel or the Interaction panel is showing, because those panels have no meaningful log form.

## Why does the plot say "MAC equivalents" rather than MAC?

MAC is a property of an agent, the alveolar concentration at which half of patients do not move to incision. What changes during an anesthetic is the patient's alveolar concentration *as a multiple* of that MAC, summed over the agents present. See [Inhaled anesthetics](help:inhaled-agents).

## A ventilation row appeared that I did not enter

Entering any gas adds a ventilation row if there is none, because the engine needs a ventilation to carry gas to the alveoli. Entering nitrous oxide also adds an oxygen row. You can edit both. See [Inhaled anesthetics](help:inhaled-agents).

## Does the age field really stop at 90?

Yes. An age of 90 or above is protected health information under HIPAA and is entered as 90.

## Can I save my work?

Copy the URL. It encodes the patient, the doses, the events, the options and any thresholds you edited; opening it restores the simulation. See [Sharing a simulation by URL](help:sharing).

## Can I use the engine from R without the app?

Yes. `simulateDrugsWithCovariates()` and `getDrugPK()` with `simCpCe()` are exported, and two vignettes show how. See [Driving the engine from R](help:scripting).

## The Email Slide panel says email is not configured

Sending mail needs credentials in the server's `config.yml`. The public app has them; a local copy will not until you add them. See [Email a slide](help:email-slide).

## I found an error in a model

Please contact Steven Shafer (steven.shafer@stanford.edu) or open an issue on GitHub. Each drug's page says where its parameters came from, which is the place to start.
