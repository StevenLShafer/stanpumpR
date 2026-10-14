## What it is

The [target-controlled infusion](help:tci) rows in the dose table simulate a TCI pump on the screen. The TCI API offers the same controller to a real pump. It is a small web service, `/tci/v1`, that a pump's host computer calls once per update interval, typically every ten seconds, and that answers with the rate for the next interval: one drug, one patient, aimed at the plasma or effect-site concentration the clinician set. The nine drugs whose units include *Plasma target* and *Effect site target* are offered: propofol, remifentanil, fentanyl, alfentanil, sufentanil, hydromorphone, ketamine, etomidate and lidocaine.

## Why it keeps no state

The server remembers nothing between calls: no pump, no patient, no session. Every request carries the patient's covariates, the target, the pump's limits and the complete record of what the pump has delivered since the case began, and the server rebuilds the model's state from that record every time. This is the principle of the original STANPUMP, which read the volume infused back from the pump every ten seconds and brought its model forward with what had actually been given, never with what it had asked for.

Three things follow. The delivery record is the only truth: a refused rate, an occlusion, a purge or a syringe change, once recorded, make the next plan exact. Nothing is lost: a dropped request, a restarted server or a fail-over costs one interval and no accuracy, because the next request brings the whole history. And every request is a complete, reproducible account of why the pump did what it did, with no patient identifier in it, only covariates and amounts.

## What the controller sends

A JSON body naming the drug; the time on the controller's clock; the patient (weight, height, age, sex); the syringe concentration; the pump's rate units and limits (ceiling, floor, resolution); the target (plasma or effect site, in the drug's concentration units, mcg/mL or ng/mL); two timing numbers; and the history. The history is a list of events: the rate changes the pump ran, segments the pump reports as delivered ("7.9 mL between 14:01:30 and 14:10", the preferred form wherever a pump has a volume counter), boluses given by hand outside the pump, and syringe changes. A controller that may change syringes records the initial syringe as the first event too: the request's own syringe is the one on the pump now, so once any change is recorded a volume given before the first recorded syringe has no known concentration, and the request is refused rather than priced with the wrong syringe.

The two timing numbers are the controller's update interval (1 to 60 seconds) and its latency: the time from forming the request to the pump acknowledging the new rate. Ten seconds, STANPUMP's interval, is the natural choice. Intervals of 30 to 60 seconds work but are coarse for the fast drugs: once the effect site is within 5 per cent of the target the controller holds the plasma instead, and over a long interval the plasma above target pushes the effect site up to about 3 per cent over the target before it settles. The response warns `INTERVAL_LONG` above a third of the drug's time to peak effect.

## What it receives

The rate to run, in the pump's units and in mL/hr, naming any pump limit that changed it; the segment to program (the rate, for how long, how much drug); a fallback rate; the times at which the command takes effect, must stop, and the next request is due; the predicted plasma and effect-site concentrations now, at the start of the hold and at its end; the predicted peak, when the target will be reached, how long the concentration would take to fall to a lower one with the pump off, and when the syringe empties; the resolved model and its parameters, for the pump's screen and log; and any warnings (a target above the usual range, a pump that reports it is not running, a delivery total that disagrees with the history).

## The safety contract, in plain language

**The server computes; the pump decides.** The pump keeps its own hard limits and enforces them whatever the response says.

- **Every command is a bounded segment.** The response says how long the rate is to run. At that instant, `holdUntil`, the pump stops running it whether or not a new command has arrived: a hard stop.
- **Then the fallback, then nothing.** If no new command has been accepted by `holdUntil`, the pump runs the response's fallback rate, the controller's own next step computed in advance, for at most one more interval, then stops and alarms. It never holds a rate above the fallback. The reason: a loading infusion is a very high rate for a short time, and holding it for one extra ten-second interval doubles the propofol effect-site peak (a target of 3 becomes 6 mcg/mL). During loading the fallback is zero, or another capped interval when the pump's ceiling is what limits the load.
- **Latency is estimated high.** The controller declares how long its commands take to reach the pump, and the server plans from the state the patient will be in when the new rate actually starts, assuming the old rate runs until then. If the declared latency is too short, the old rate runs longer than planned. In maintenance that costs nothing measurable: less than 0.01 per cent of the target for an error of up to 3 seconds. In the loading interval the old rate is the loading rate, and the eventual peak overshoots the step in target by about the error divided by the interval: 1.5 seconds late on a 10-second interval overshoots by 15 per cent, 3 seconds by 30 per cent. Declaring 3 seconds too much costs 0.4 per cent. So the controller uses a high percentile of its measured latencies, never the mean: landing early is safe, landing late is not.
- **Apply at once, apply once.** A response is applied the moment it arrives, so the history records the true time of the change; one that arrives after the hold has ended is discarded. Any error, timeout or unreadable body is "no response", and the pump asks again with a fresh request. A new target, a manual bolus, a pause or a syringe change sends a new request immediately.
- **Target 0 stops the infusion.** A manual bolus, once in the history, holds the pump off until the concentration is back at the target.

## The same controller as the simulator

The rate is computed by the function behind the app's TCI rows, `tciRate()`, including the hand-off from effect-site to plasma control within 5 per cent of the target. With no latency, a ten-second interval and a pump without a ceiling, a closed loop of API calls reproduces the app's TCI schedule rate for rate, and the test suite pins it. A logged request can therefore be replayed in the simulator: its history, written as dose-table rows, draws the curves the controller was planning from. Two things the API has that the app's rows do not: a pump ceiling, so a loading dose can spread over several intervals without overshoot, and the latency correction. It runs each drug's library model, never the Schnider or Minto pump that the [Minto / Schnider](help:models/minto-schnider) switch adds in the app.

## Running it

The API is not part of the shinyapps.io deployment, and shinyapps.io cannot host it: a pump controller belongs on a server the institution controls, behind TLS and authentication, with an audit log. It starts from any R installation that has the `stanpumpR` and `plumber` packages:

```sh
Rscript -e 'stanpumpR::run_tci_api(host = "127.0.0.1", port = 8000)'
```

or is deployed to Posit Connect from the package's `inst/api` directory. `plumber` is an optional dependency: the simulator never loads it. Those [driving the engine from R](help:scripting) can call `tciNextRate()` directly on a parsed request body, without a server. The developer reference, `docs/tci-api.md` in the repository, lists every field, unit token, bound and error code; the server also publishes an OpenAPI document at `/tci/v1/openapi.json`.
