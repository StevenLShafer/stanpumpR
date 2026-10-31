# stanpumpR — The TCI API (version 1)

Developer reference for the stateless HTTP API that hands a target-controlled
infusion (TCI) pump its next rate. Drafted by Claude Code, 2026-10-12, at the
request of Steven L. Shafer, from the design specification that was reviewed
for pump integration, the TCI algorithm, API safety and repository fit.
Companion code: `R/tci-api.R` (the computation), `R/tci-api-server.R` (the
Plumber router), `R/tci.R` (the controller the app already runs),
`R/getDrugPK.R` (covariates → closed-form coefficients), and the original
`STANPUMP.C` (`infuse()`, `model()`, `process_rate()`, `look_ahead()`).

This document is normative for the API's behaviour. Field names, unit tokens,
error codes, statuses and bounds are implemented exactly as written here; the
tests in `tests/testthat/test-tci-api.R` and `test-tci-api-http.R` hold the
code to it.

**Reference client.** [FauxTCI](https://stevenlshafer.github.io/FauxTCI/)
(source: [github.com/StevenLShafer/FauxTCI](https://github.com/StevenLShafer/FauxTCI))
is a simulated syringe pump that runs in the browser and implements the §7
contract against this API: set the pump's limits, pick propofol or
remifentanil, enter a target and press Start, and it shows every request it
sends and every response it receives as the case runs. Its controller is a
JavaScript port of `tciNextRate()`, validated against fixtures that its
`test/fixtures/generate.R` produces from this package (the drug models to
1e-10 relative, the responses to 1e-8, closed-loop traces to 1e-6); its
Controller switch calls a real `run_tci_api()` server instead when one is
reachable. It is the working example of a pump built to this document.

## 1. Purpose and principles

A **stateless HTTP API** that a TCI controller (the host computer driving a
syringe pump, or the pump itself) calls once per update interval to obtain the
**next infusion rate** for **one drug in one patient**, from the controller's
own record of what the pump has delivered. The server computes the rate that
reaches and then holds a target effect-site (or plasma) concentration,
**accounting for the delay** between the request and the moment the new rate
is actually running on the pump.

### 1.1 Stateless, with the delivery record as the only truth

The server keeps no state about any pump, patient or session. Every request
carries everything needed to answer it, and the model state is **re-derived
from the delivery record on every request**. This is the STANPUMP principle:
`infuse()` read the volume infused back from the pump every ten seconds and
brought the model forward with what had actually been delivered, never with
what it had asked for.

1. **The delivery record is authoritative.** The server plans from what the
   pump reports it delivered. A refused rate, a purge, an occlusion, a syringe
   change: if the controller records them (§4.2), the next plan is exact. The
   record is only as good as what the pump reports; the API gives the
   controller the means to report delivered volume, not just commanded rates.
2. **Nothing is lost.** A dropped request, a server restart, a fail-over: the
   next request carries the whole history and the answer is exact.
3. **Every request is a complete, reproducible record** of why the pump did
   what it did. The body names the drug, so a stored body replays on its own,
   in R (`tciNextRate(body)`) or in the stanpumpR simulator.
4. **No direct identifiers.** The request holds covariates and amounts.
   `requestId` is an opaque string the client chooses and must not encode a
   medical record number, name or anything that maps to a patient. The
   combination of covariates and timestamps may still be classified as
   protected under the integrator's rules; the API does not claim otherwise.

### 1.2 The same controller as the app

The rate is computed by `tciRate()` from `R/tci.R`, the function behind the
app's `Plasma target` / `Effect site target` rows (Shafer & Gregg 1992, as in
STANPUMP, including the hand-off to plasma control within 5% of the target).
With zero latency, a 10 s interval and an unlimited pump, a closed loop of API
calls reproduces the app's `tciSchedule()` rate for rate (a test pins this). A
logged request can therefore be replayed in the app.

## 2. Scope

* One drug per request, from the drugs whose library row offers target units
  (`isTciUnit()` on `getDrugDefaults(drug)$Units[[1]]`): today propofol,
  remifentanil, fentanyl, alfentanil, sufentanil, hydromorphone, oxycodone
  (added October 2026, issue #131), ketamine, etomidate, lidocaine. A two-channel pump calls twice.
* Intravenous delivery only (boluses and infusions), the default PK set of the
  drug (no TCI drug has event-dependent kinetics).
* Not in v1: Bayesian update from measured concentrations; multi-drug
  interaction; pump-specific drivers; a signed state checkpoint for very long
  cases (§4.2 explains how a client compacts its history instead).

## 3. Endpoints

All under `/tci/v1`. JSON in, JSON out, UTF-8. `{drug}` is the exact library
id as listed by `/tci/v1/drugs` (lower case, e.g. `propofol`).

| Method | Path | Purpose |
|---|---|---|
| `GET`  | `/tci/v1/health` | Liveness: `apiVersion`, package version, commit, server time, number of TCI drugs. |
| `GET`  | `/tci/v1/drugs` | The TCI drugs with their units, typical target range, hard target ceiling, tPeak, citation; plus the accepted unit tokens (§4.4) and bounds (§4.3). |
| `GET`  | `/tci/v1/drugs/{drug}/model?weight=&height=&age=&sex=&adjustToFFM=` | The model resolved for a patient (the `model` block of §6.1), for the pump to display and log. |
| `POST` | `/tci/v1/drugs/{drug}/rate` | **The controller.** Body §4, response §6. |

The OpenAPI 3 document is served at `/tci/v1/openapi.json`; the interactive
documentation (`/__docs__/`) is served only when the router is built with
`docs = TRUE` (§8), never in production.

### 3.1 Versioning

Within `/tci/v1` the server only **adds**: optional request fields, response
fields, warning codes, drugs. It never changes a meaning, a unit token, a
default, a bound or a status. Clients ignore response fields and warning codes
they do not know; the server **rejects request fields it does not know**
(`UNKNOWN_FIELD`), so a typo can never silently fall back to a default.
Breaking changes go to `/tci/v2`. Every body, including errors and `/health`,
carries `apiVersion` (`"1.0"`; the minor part is bumped on additions).

## 4. The `rate` request

```json
{
  "requestId": "ch1-000193",
  "drug": "propofol",
  "now": "2026-10-11T14:31:58.500Z",
  "patient": { "weight": 70, "height": 170, "age": 50, "sex": "male" },
  "infusate": { "concentration": 10, "units": "mg/mL" },
  "pump": {
    "rateUnits": "mL/hr",
    "maxRate": 1200,
    "minRate": 0.1,
    "rateResolution": 0.1,
    "currentRate": 40.4,
    "state": "running",
    "volumeInfused": { "value": 26.95, "units": "mL" },
    "syringeVolumeRemaining": { "value": 28.5, "units": "mL" }
  },
  "target": { "site": "effect", "value": 3.0, "units": "mcg/mL" },
  "timing": { "latency": 1.5, "interval": 10 },
  "history": [
    { "time": "2026-10-11T13:59:30.000Z", "infusate": { "concentration": 10, "units": "mg/mL" } },
    { "time": "2026-10-11T13:59:40.000Z", "bolus": 20, "units": "mg", "source": "manual" },
    { "time": "2026-10-11T14:00:00.000Z", "rate": 995.1, "units": "mL/hr" },
    { "time": "2026-10-11T14:00:10.000Z", "rate": 0, "units": "mL/hr" },
    { "from": "2026-10-11T14:01:30.000Z", "to": "2026-10-11T14:10:00.000Z", "delivered": 7.90, "units": "mL" },
    { "from": "2026-10-11T14:10:00.000Z", "to": "2026-10-11T14:20:00.000Z", "delivered": 7.88, "units": "mL" },
    { "from": "2026-10-11T14:20:00.000Z", "to": "2026-10-11T14:25:00.000Z", "delivered": 3.60, "units": "mL" },
    { "from": "2026-10-11T14:25:00.000Z", "to": "2026-10-11T14:30:00.000Z", "delivered": 3.46, "units": "mL" },
    { "from": "2026-10-11T14:30:00.000Z", "to": "2026-10-11T14:31:50.000Z", "delivered": 1.24, "units": "mL" },
    { "time": "2026-10-11T14:31:50.000Z", "rate": 40.4, "units": "mL/hr" }
  ],
  "lookAhead": { "concentration": 1.0 },
  "options": { "adjustToFFM": true }
}
```

The body is a real record, and §6.1 is the server's answer to it: a closed loop
of these requests every 10 s from 13:59:58.500Z (the first rate acknowledged at
14:00:00.000Z), 1% propofol to 3 mcg/mL effect site, a 20 mg hand bolus 20 s
before the pump started, the controller recording the initial syringe, the
loading rate and the zero that followed it as they happened and merging the
maintenance delivery into five segments (§4.2). Thirty-two minutes in, the
plasma holds the target and the pump runs 40.4 mL/hr.

### 4.1 Fields

| Field | Required | Meaning |
|---|---|---|
| `requestId` | no | Opaque string, ≤ 128 printable ASCII characters, echoed in every response including errors. Should increase monotonically per pump channel (§7). |
| `drug` | no | If present must equal the path drug (else 400 `DRUG_MISMATCH`). Include it so a stored body is self-describing. |
| `now` | yes | The controller's clock at the moment the request is formed. RFC 3339 string with a mandatory `Z` or offset and optional fractional seconds. **All computation uses this clock**; the server's clock is only reported. A JSON number here is `INVALID_TYPE` (seconds-versus-milliseconds errors are not survivable). |
| `patient.weight` | yes | kg, `MIN_WEIGHT`–`MAX_WEIGHT` (0.1–500). |
| `patient.height` | yes | cm, `MIN_HEIGHT`–`MAX_HEIGHT` (10–200). |
| `patient.age` | yes | years, decimal allowed, `MIN_AGE`–`MAX_AGE` (0–90). |
| `patient.sex` | yes | `"male"` or `"female"` (`SEX_VALUES`), nothing else. |
| `infusate.concentration`, `infusate.units` | if any `mL` quantity appears anywhere | The syringe now on the pump: concentration `≥ TCI_API_MIN_INFUSATE` (0.001, in the infusate's own units: 0.001 mg/mL or 0.001 mcg/mL; less is 422 `INVALID_VALUE`, in the request and in a history infusate event alike), units `mg/mL` or `mcg/mL`. Must equal the last `infusate` history event if there is one (`INFUSATE_MISMATCH`); with infusate events in the history it prices only the `mL` quantities at or after the first of them (§4.2). |
| `pump.rateUnits` | yes | Units of `maxRate`, `minRate`, `rateResolution`, `currentRate` **and of the response's `rate.value` and `fallback.rate`**. A rate token from §4.4. `mL/hr` needs `infusate`. |
| `pump.maxRate` | yes | The pump's ceiling, `> 0`, ≤ 1e5. |
| `pump.minRate` | no | `[0, maxRate)`. A computed rate below it is sent as 0 (STANPUMP, Harvard 22). |
| `pump.rateResolution` | no | `(0, maxRate/10]`. The rate is rounded **down** to a multiple of it (§5.4). |
| `pump.currentRate` | no | The rate the pump reports running at `now`. Cross-check only: the history is authoritative (§4.2). Differs from the history's running rate by more than `max(rateResolution, 1e-6·rate)` → warning `RATE_MISMATCH`. |
| `pump.state` | no, default `running` | `running`, `paused`, `alarm`, `occlusion`, `syringeEmpty`, `purging`, `standby`. Not `running`: the latency projection (§5.2) runs at rate 0, `RATE_MISMATCH` is not raised, and the response carries warning `PUMP_NOT_RUNNING`. The plan is still computed; the pump decides whether to run it. |
| `pump.volumeInfused` | no | `{ value ≥ 0, units: mL / mg / mcg }`: the pump's totaliser since the first history event. Compared with the integral of the history (§5.1); beyond `max(2%, 0.5 mL-equivalent)` → warning `DELIVERY_MISMATCH`, both numbers in `delivered`. |
| `pump.syringeVolumeRemaining` | no | `{ value ≥ 0, units: mL }` (needs `infusate`). Warning `SYRINGE_LOW` when the planned segment plus the fallback segment exceed it; `prediction.syringeEmptyAt` is reported. |
| `target.site` | yes | `"effect"` or `"plasma"`. |
| `target.value` | yes | `≥ 0`. **0 stops the infusion** (rate 0, `controller.plan` = `off`). Above the library's `Upper` → warning `TARGET_HIGH`; above `5 × Upper` → 422 `TARGET_OUT_OF_RANGE`. |
| `target.units` | yes | The drug's concentration units exactly: `mcg/mL` for the mcg/mL drugs (propofol, ketamine, etomidate, lidocaine), `ng/mL` for the ng/mL drugs (the opioids). Any other token, including `mg/L`, is `UNIT_UNKNOWN`; the right token for the wrong drug is `UNIT_MISMATCH`. Never converted. |
| `timing.latency` | **yes** | Seconds from `now` to the instant the pump **acknowledges** the new rate, which is the instant the controller will record as that segment's `time` in later histories. `[0, interval)`; `≥ interval` is 422 `LATENCY_INVALID`; `> interval/2` is warning `LATENCY_LONG`. Use a high percentile (e.g. the 95th) of observed form-to-acknowledge times, not the mean: landing early is safe, landing late is not (§5.5). No default. |
| `timing.interval` | **yes** | Seconds the new rate will be held: the controller's update interval. `[TCI_API_MIN_INTERVAL, TCI_API_MAX_INTERVAL]` = `[1, 60]`. Above `tPeak·60/3` for the drug → warning `INTERVAL_LONG`. No default. |
| `history` | yes (may be empty) | Everything delivered so far: §4.2. At most `TCI_API_MAX_HISTORY` (20000) events, else 413 `HISTORY_TOO_LONG`. |
| `lookAhead.concentration` | no | `> 0`, in `target.units`. The response reports the decrement time to it (§5.6). |
| `options.adjustToFFM` | no, default `true` | The app's fat-free-mass switch, passed to `getDrugPK()`. Propofol and remifentanil carry their own size covariate and ignore it; the other TCI drugs scale by `pkSizeFactors()`. Echoed in `model.adjustToFFM`. |

### 4.2 The delivery history

An array of events, each one of four kinds. Times are RFC 3339 strings on the
same clock as `now`.

| Event | Fields | Meaning |
|---|---|---|
| Rate change | `{ "time", "rate", "units" }` | The pump's rate from `time` until the next rate change, or until `now` if it is the last one. `rate ≥ 0`, a rate token. The rate before the first rate change is 0. |
| Delivered segment | `{ "from", "to", "delivered", "units" }` | The pump reports it delivered this amount between `from` and `to` (`to > from`, `delivered ≥ 0`, units `mL`, `mg` or `mcg`). Modelled as the constant rate `delivered / (to − from)`. This is STANPUMP's `save_infusion()` record and the preferred form wherever the pump has a volume counter. **Over its span a delivered segment overrides the rate changes**; delivered segments may not overlap each other (`HISTORY_OVERLAP`). |
| Bolus | `{ "time", "bolus", "units" }` | Drug given **outside the pump** (a hand-pushed syringe, another line), treated as instantaneous. `bolus > 0`, units `mg`, `mcg`, `mg/kg`, `mcg/kg`, `mL`. Two boluses at one instant are summed. |
| Infusate change | `{ "time", "infusate": { "concentration", "units" } }` | A syringe change to a different concentration, in force from `time`. Once the history records any syringe, every `mL` quantity (a `mL/hr` rate, a `mL` bolus, a `mL` segment's start) must lie at or after the first recorded one: the request's `infusate` is the syringe on the pump **now** (it must equal the last such event), so a `mL` quantity before the first recorded syringe has no known concentration and is 422 `INFUSATE_REQUIRED`, with the pointer at its `units`. A controller that may change syringes therefore records the initial syringe as an infusate event at the start. With no infusate event in the history the request's `infusate` applies throughout. |

Every event may carry `"source"` (`"tci"`, `"manual"`, `"bolus"`, any string
≤ 32 chars) for the audit trail; the computation ignores it.

Rules:

* **A bolus the pump delivers is not a `bolus` event.** A pump bolus (its
  bolus key, or the loading infusion this API prescribes) is a high rate for a
  short time; the controller reports it as the rate change or delivered
  segment the pump ran. `bolus` is only for drug that did not pass through
  the pump.
* The server sorts events by time (`from` for delivered segments). At one
  instant: infusate change, then boluses, then the rate change. Two rate
  changes, or two infusate changes, at the same instant are 422
  `HISTORY_INVALID` (the order of two objects in an array is not evidence of
  which was later).
* An event up to 2 s after `now` is clamped to `now` with warning
  `HISTORY_CLOCK_SKEW`; later than that it is 422 `HISTORY_IN_FUTURE`: the
  history is the past, not a plan. Events earlier than 1970 are
  `TIME_INVALID`.
* All times in one request come from **one clock**, the controller's. If that
  clock steps (NTP, DST, an operator), the controller re-bases its stored
  history before the next request. A monotonic clock, converted to RFC 3339
  once at request time, is the recommended source.
* `pump.currentRate` never overrides the record. The running rate at `now` is
  whatever the history says (the last rate change, or 0).
* Mass units need no `infusate`; any `mL` or `mL/hr` quantity does
  (`INFUSATE_REQUIRED`). The §4 example records the initial syringe as its
  first event, as a controller that may change syringes must, so a later
  change can be recorded without re-pricing anything; had the syringe been
  changed during the case, the history would carry the change and the
  request's `infusate` would be the new syringe.
* No limit on span. A long case is kept compact by the controller: segments
  older than 30 minutes may be merged into one delivered segment per minute
  (the fast eigenvalue has decayed to nothing by then, so the merge changes
  the state by far less than the pump's own resolution); the controller keeps
  the full record locally for audit. Within the last half hour the merge must
  be finer where the rate was changing: the §4 example carries its
  maintenance as five segments that shorten toward `now` (ten minutes down to
  two), and its response is within 0.3% of the state the full ten-second
  record gives. Merging that half hour into one segment would misplace the
  falling maintenance rate (61 to 40 mL/hr) by enough to put the reconstructed
  effect site 10% above the target, because the plasma at `now` weights the
  last few minutes of delivery far more than the first.

### 4.3 Types and bounds

* Every numeric field is a JSON number: a string (`"70"`), boolean or object
  where a number is expected is 400 `INVALID_TYPE`. Values must be finite
  after parsing (`1e400` parses to `Inf` → 422 `INVALID_VALUE`).
* `null` is treated as absent (`MISSING_FIELD` for a required field).
* Duplicate keys anywhere in the body are 400 `DUPLICATE_KEY` (jsonlite keeps
  both; the server walks the parsed list). Unknown keys anywhere are 400
  `UNKNOWN_FIELD`.
* Strings are compared exactly: case-sensitive, ASCII, no surrounding
  whitespace.
* Bounds are listed with their fields in §4.1; violations are 422
  `INVALID_VALUE` with the pointer.
* The body should be sent with `Content-Type: application/json`. The API's
  constants live in `R/constants.R` (`TCI_API_*`) and are published by
  `GET /tci/v1/drugs` under `limits`.

### 4.4 Unit tokens

One closed, case-sensitive list, using the stanpumpR library's spellings
(`hr`, `mcg`, `mL`). Anything else, including `mL/h`, `ug`, `µg`, `μg`, `ml`,
`mg/L`, is 422 `UNIT_UNKNOWN` with a message naming the nearest accepted token.

| Where | Tokens |
|---|---|
| Rates (`pump.rateUnits`, rate changes) | `mL/hr`, `mg/hr`, `mg/min`, `mcg/hr`, `mcg/min`, `mg/kg/hr`, `mg/kg/min`, `mcg/kg/hr`, `mcg/kg/min` |
| Amounts (boluses, delivered segments, `volumeInfused`) | `mL`, `mg`, `mcg`; boluses also `mg/kg`, `mcg/kg` |
| Concentrations (`target.units`, `lookAhead`) | `mcg/mL`, `ng/mL` (the drug decides which) |
| Infusate | `mg/mL`, `mcg/mL` |
| Volumes (`syringeVolumeRemaining`) | `mL` |

`/kg` units multiply by `patient.weight`. Inside the server everything is in
minutes and the drug's base mass unit (mg for mcg/mL drugs, mcg for ng/mL
drugs), as in `simCpCe()`, so a rate is base/min and a concentration base/L.

## 5. The computation

### 5.1 State from the history

The model state is seven amplitudes, `p[1..3]` (plasma) and `e[1..4]` (effect
site), one per exponential, with `Cp = sum(p)`, `Ce = sum(e)` (`tciKinetics()`
supplies the eigenvalues and coefficients). The server builds the
piecewise-constant rate function from the rate changes, overrides it on each
delivered segment's span, then walks the timeline from the first event to
`now`: over a segment of length `dt` at rate `R`,
`p ← p·exp(−λ dt) + pinf·R·(1 − exp(−λ dt))`, likewise `e` with the four
effect-site eigenvalues; a bolus `A` adds `pbolus·A`, `ebolus·A`. This is
`tciSchedule()`'s `advance()` and STANPUMP's "bring model forward". The
integral of the rate function plus the boluses is `delivered.fromHistory`.
The implementation does not step through the timeline: the system is linear,
so it sums each piece's and each bolus's closed-form contribution to the seven
amplitudes (superposition), which equals the recurrence to rounding (test
group 1 pins the equivalence) and costs one vectorised step whatever the
length of the history.

### 5.2 Latency projection

The server advances the state through `latency` at the running rate (0 if
`pump.state` is not `running`): the pump keeps running the old rate until the
new command is acknowledged. The result is the state at
`effectiveAt = now + latency`, where the plan starts. This is STANPUMP's
"preliminary calculation".

### 5.3 The plan

`tciRate(p, e, CT, site, interval/60, K)` on the state at `effectiveAt`:

* **Effect site.** The largest rate, held for `interval`, whose effect-site
  trajectory afterwards touches the target without overshoot (Shafer & Gregg,
  Appendix A). The search grid runs from the end of the interval to
  `interval + tPeak + 10 s`, because the peak after an infusion lasting `dt`
  lies up to `tPeak` after the infusion **ends**. `tciRate()` builds the grid
  from `dt` (`tau <- seq(dt, dt + K$tPeak + 10/60, by = 1/60)`, deduplicated,
  with `dt` first). Before this change the grid was fixed at `tPeak + 10 s`
  from the **start** of the interval, which is right for the app's 10 s step
  and wrong beyond about 30 s (propofol, one plan from zero then pump off:
  +0.0% at 10 s, +3.5% at 60 s, +21.6% at 120 s). At 10 s the new grid
  contains the old one and only adds the points beyond `tPeak + 10 s`, where,
  for any state produced by non-negative inputs, the effect site is already
  falling and the quotient `(CT − B)/E` is increasing, so the minimum and
  hence the schedule are unchanged (verified by `test-tci.R` and the API's
  equivalence test).
* **Hand-off.** Within `TCI_PLASMA_SWITCH` (5%) of the target the plasma is
  held instead; at steady state Cp = Ce.
* **Hand-off at long intervals.** The hand-off happens on the rising limb,
  before the peak, and the plasma plan lands Cp on the target only at the
  **end** of the interval, so over a long interval the plasma above target
  meanwhile pushes Ce over. One plan then pump off touches the target exactly
  at every interval (that is the grid), but the closed loop at 30 and 60 s
  overshoots the target by 1–3% for propofol, remifentanil and alfentanil,
  and by up to 4.2% for etomidate, before it settles (propofol at 60 s: +1.6%
  with a 1.5 s latency asking every interval, +2.7% with no latency asking at
  `nextRequestAt`; remifentanil +2.4% at 60 s and alfentanil +2.0% at 30 s;
  etomidate +1.4% at 30 s and +4.2% at 60 s, its 1.9 min peak being the slowest
  of the fast drugs against a 60 s interval; fentanyl, sufentanil,
  hydromorphone, oxycodone, ketamine and lidocaine stay within 1%), against
  ≤ 0.2% at 1–10 s. This is the Shafer–Gregg algorithm at an interval it was
  not designed for, not the search grid, and it is why `INTERVAL_LONG` warns
  above `tPeak/3`; STANPUMP's 10 s is the recommended interval. §8.2 test 5
  pins the measured excursion.
* **Plasma.** The rate that brings the plasma to the target at the end of the
  interval (Bailey & Shafer 1991); 0 if the plasma decayed to the end of the
  interval is already above it.
* **Target 0.** Rate 0, plan `off`.

### 5.4 The pump's limits

The server converts base/min to `pump.rateUnits` (through `infusate` for
`mL/hr`). Then, in order: clamp to `maxRate`; below `minRate` → 0; round
**down** to a multiple of `rateResolution` (a deficit is recovered by the next
plan, an excess is not). `rate.limitedBy` names each limit that changed the
value.

When `maxRate` caps a loading infusion the pump runs at its ceiling for this
interval and the next request plans again from the state actually reached. The
loading dose spreads over as many intervals as the ceiling requires, without
overshoot, because every plan is "the largest rate that does not overshoot".
(Verified: a 300 mL/hr cap on 1% propofol gives five intervals at the cap, one
partial, then off, peak Ce +0.08% of a 3 mcg/mL target.)

### 5.5 Timing and the delay

Let `t0 = now`, `L = latency`, `I = interval`.

* The command runs from `t1 = t0 + L` (`effectiveAt`) to `t2 = t1 + I`
  (`holdUntil`). **`holdUntil` is a hard stop** (§5.7, §7).
* `nextRequestAt = t2 − L`, so the next command is acknowledged as this hold
  ends. Exception (STANPUMP's rule): when the commanded rate is 0 and the
  controlled concentration is predicted to reach the target before
  `t2 + I` (the effect-site peak, or the plasma decaying to target),
  `nextRequestAt = max(t0, thatTime − L)`, so the maintenance infusion starts
  at the peak rather than up to an interval late. The peak counts only if it
  reaches the target (within the 1% band) and lies after `effectiveAt`: a
  plan zeroed by a pump floor whose pump-off peak stays below the band keeps
  the usual time. `nextRequestAt` is the **latest** time to ask; the
  controller asks at once after any local event (§7). When `rate.value` is 0
  it may lie beyond `holdUntil + fallback.maxSeconds`: the hold and the
  fallback are then both rate 0, and the pump's stop at that point is the
  planned state, not an alarm condition (§7, rule 4).
* `timing.now` is the request's `now` **verbatim**, the same string whatever
  its offset or precision, so a controller matches a response to its request
  by comparing the two (§7, rule 1). `effectiveAt`, `holdUntil`,
  `nextRequestAt` and the server's own times are formatted in UTC with
  milliseconds.
* **The cost of a wrong latency is one-sided.** Underestimating by δ lets the
  old rate run δ longer. In maintenance that is negligible (verified ≤ 0.01%
  of the target for δ up to 3 s). In the **loading interval** the old rate is
  the loading rate, and the eventual effect-site peak overshoots by about
  δ/I of the step in target: +15% for δ = 1.5 s on a 10 s interval, +30% for
  3 s. Overestimating by 3 s costs +0.4%. Hence the high-percentile estimate,
  and hence the hard stop: the pump itself bounds every segment to
  `segment.holdSeconds` so that a late **next** command can never extend a
  loading rate.
* The self-correction argument: whatever the actual latency, the next
  request's history records when the rate really changed, so the next state
  is exact. Errors do not accumulate.
* The client splits network from compute with `server.computeSeconds`; the
  server's absolute times are on another clock and are not used for anything
  else. `|server.receivedAt − now| > 30 s` is reported as warning
  `CLOCK_SKEW`.

### 5.6 Predictions

All from the **commanded** (clamped, rounded) rate, never the ideal.

* `state.now`, `state.effectiveAt`, `state.holdUntil`: `cp`, `ce` at `t0`,
  `t1`, `t2`, in `target.units`; `state.now` also carries the seven
  amplitudes `p`, `e` (base/L) for audit.
* `prediction.peakCe`, `prediction.peakAt`: with the pump off after `t2`, the
  maximum of Ce over `[t2, t2 + tPeak + 10 s]` on a 1 s grid (the touching
  point when the plan was not capped). `null` for a drug without an effect
  site.
* `prediction.onTarget`: the controlled concentration (Ce for an effect
  target, Cp for a plasma target) at `t2` is within 1% of the target;
  `prediction.deviation`: its signed fractional error at `t2`.
* `prediction.targetReachedAt`: the server runs the same controller forward
  from `t2` in closed loop (same interval and limits, zero latency) for up to
  `TCI_API_LOOKAHEAD_MAX` seconds and reports the first time the controlled
  concentration is within 1% of the target; `null` if never within the
  horizon; equals `holdUntil` when already on target. Three shortcuts keep
  this to a few `tciRate()` calls rather than one per interval of the horizon
  (30000 at a 1 s interval, a second of compute): the pump at its ceiling
  from `t2` on bounds every closed loop from above, so when even that
  trajectory never enters the band the answer is `null` at once (a ceiling
  below the maintenance requirement); while the controller itself chooses
  rate 0 (the effect site rising to its peak, a plasma decaying from above the
  target) the pump-off trajectory is followed in closed form to the step
  before anything the plan reads changes, which gives the same times as
  stepping; and after `TCI_API_LOOKAHEAD_EXACT_STEPS` (300) plans the step
  grows by a tenth per plan up to `TCI_API_LOOKAHEAD_MAX_STEP` (60 s), as
  `tciSchedule()` does once a maintenance infusion has settled, so the time
  reported for a slow, asymptotic approach (a ceiling just above the
  maintenance requirement) is on that coarser grid.
* `prediction.lookAhead` (only when requested): STANPUMP's `look_ahead()`.
  From `now`, **with the pump off from `now`**, the time for the controlled
  concentration to fall to `lookAhead.concentration`: `{ site, concentration,
  units, decrementSeconds, horizonSeconds: 30000, beyondHorizon }`.
  `decrementSeconds` is 0 if already below, `null` with `beyondHorizon: true`
  if not reached within the horizon. Solved by a root find on the closed form
  (bracket on a coarse grid, then `uniroot`), not a 1 s grid. The latency does
  not enter: this is the number a clinician means by "if I stop the pump now".
* `prediction.syringeEmptyAt` (only with `syringeVolumeRemaining`): when the
  syringe empties at the commanded rate; `null` if the commanded rate is 0,
  unless the rate still running during the latency empties the syringe before
  `effectiveAt`, in which case that time.

### 5.7 The segment and the fallback

The response describes the command as a **bounded segment** the pump can
program natively (rate for a time, or a volume to be infused at a rate), and
what to run if the next command does not arrive:

* `segment.holdSeconds` = `interval`; `segment.amount` = commanded rate ×
  interval in the drug's mass unit; `segment.volumeMl` likewise in mL when
  `infusate` is given.
* `fallback.rate`: the controller's own next step, STANPUMP's preliminary
  calculation applied once more: the server advances the state through the
  hold at the commanded rate, calls `tciRate()` again with the same limits,
  rounding and units. During a loading interval this is 0 (the plan is "off
  until the peak") or, under a cap, another capped interval, which the plan
  already allows for; in maintenance it is the next maintenance rate.
  `fallback.maxSeconds` = `interval`: after that, with no accepted command,
  the pump stops.
* Verified why this matters: holding the uncapped propofol loading rate for a
  second 10 s interval gives a peak Ce of 5.99 against a target of 3.0
  (+100%); with a 1200 mL/hr cap, +41%.

## 6. The `rate` response

### 6.1 Example

```json
{
  "apiVersion": "1.0",
  "requestId": "ch1-000193",
  "drug": "propofol",
  "target": { "site": "effect", "value": 3.0, "units": "mcg/mL" },
  "controller": { "plan": "plasma", "phase": "maintenance" },
  "rate": {
    "value": 39.2, "units": "mL/hr",
    "mlPerHour": 39.2,
    "massPerMinute": 6.533, "massUnits": "mg",
    "ideal": 39.20, "limitedBy": ["rateResolution"]
  },
  "segment": { "holdSeconds": 10, "amount": 1.089, "amountUnits": "mg", "volumeMl": 0.1089 },
  "fallback": { "rate": 40.2, "units": "mL/hr", "maxSeconds": 10 },
  "timing": {
    "now": "2026-10-11T14:31:58.500Z",
    "effectiveAt": "2026-10-11T14:32:00.000Z",
    "holdUntil": "2026-10-11T14:32:10.000Z",
    "nextRequestAt": "2026-10-11T14:32:08.500Z",
    "latency": 1.5, "interval": 10
  },
  "state": {
    "units": "mcg/mL",
    "now":         { "cp": 3.005, "ce": 3.007, "p": [1.627, 1.186, 0.1918], "e": [18.93, 1.266, 0.1927, -17.38] },
    "effectiveAt": { "cp": 3.005, "ce": 3.007 },
    "holdUntil":   { "cp": 3.000, "ce": 3.006 }
  },
  "prediction": {
    "units": "mcg/mL",
    "peakCe": 3.006, "peakAt": "2026-10-11T14:32:10.000Z",
    "onTarget": true, "deviation": 0.002028,
    "targetReachedAt": "2026-10-11T14:32:10.000Z",
    "lookAhead": { "site": "effect", "concentration": 1.0, "units": "mcg/mL",
                   "decrementSeconds": 636.4, "horizonSeconds": 30000, "beyondHorizon": false },
    "syringeEmptyAt": "2026-10-11T15:15:35.801Z"
  },
  "delivered": { "fromHistory": 269.4, "reported": 269.5, "units": "mg" },
  "model": {
    "drug": "propofol",
    "reference": "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959. https://pubmed.ncbi.nlm.nih.gov/29661412/",
    "concentrationUnits": "mcg/mL", "massUnits": "mg",
    "tPeak": 1.6, "adjustToFFM": true,
    "parameters": { "v1": 6.283, "v2": 20.17, "v3": 136.7, "cl1": 1.551, "cl2": 1.516, "cl3": 0.6673,
                    "k10": 0.2469, "k12": 0.2413, "k13": 0.1062, "k21": 0.07516, "k31": 0.00488,
                    "lambda_1": 0.6280, "lambda_2": 0.04306, "lambda_3": 0.003349, "ke0": 0.6871 },
    "hash": "md5:d7e1ee61f1de895385daa3903be4d7e5"
  },
  "algorithm": { "name": "shafer-gregg-1992", "plasmaSwitch": 0.05, "tauGridSeconds": 1 },
  "warnings": [],
  "server": {
    "package": "stanpumpR", "version": "0.0.1.9002", "commit": null,
    "receivedAt": "2026-10-11T14:31:58.662Z", "respondedAt": "2026-10-11T14:31:58.681Z",
    "computeSeconds": 0.019
  }
}
```

Numbers are rounded to four significant digits here; the server sends full
double precision (§6.2), `model.hash` is the MD5 of the parameters rounded
to ten significant digits (the same on every platform; the value shown is
illustrative), and `server.*` is the server's own clock. The test "the documented
example request produces the documented response" in `test-tci-api.R` runs
the §4 body and checks this response.

### 6.2 Field notes

* `controller.plan`: `effectSite`, `plasma` (including the hand-off during an
  effect-site target), `off` (target 0). `target.site` is echoed unchanged so
  a pump never displays "plasma TCI" to a clinician who set an effect-site
  target.
* `controller.phase`, for the pump's screen: `stopped` (target 0);
  `aboveTarget` (rate 0 and the controlled concentration at `effectiveAt` is
  above target); `waitingForPeak` (rate 0 otherwise); `loading` (rate > 0 and
  the controlled concentration at `effectiveAt` is below `(1 − 0.05) ×`
  target); `maintenance` (rate > 0 otherwise).
* `rate.value` is in `pump.rateUnits`; `rate.mlPerHour` is present whenever
  `infusate` is given, because pumps are programmed in mL/hr whatever units
  the client prefers; `rate.ideal` is the unclamped, unrounded rate in
  `pump.rateUnits`; `rate.limitedBy` ⊆ `["maxRate", "minRate", "rateResolution"]`.
* Numbers are serialized at **full double precision** and scalars unboxed
  (§8); nothing is rounded for display.
* `model.hash` is the MD5 of the serialized `parameters`, rounded to ten
  significant digits first so that the same model hashes identically on every
  platform (libm implementations differ in the last binary digit), so two
  responses from the same model compare by one string. `server.commit` is the
  `RemoteSha` of the installed package when it was installed from GitHub,
  else `null`.
* `GET /tci/v1/drugs/{drug}/model` returns exactly the `model` block.

### 6.3 Errors

```json
{
  "apiVersion": "1.0",
  "requestId": "ch1-000412",
  "errors": [
    { "code": "UNIT_MISMATCH", "message": "target.units must be mcg/mL for propofol", "pointer": "/target/units" }
  ],
  "server": { "package": "stanpumpR", "version": "0.0.1.9002", "receivedAt": "...", "respondedAt": "..." }
}
```

Pointers are RFC 6901 JSON Pointers (`/history/2/units`). Validation stops at
the first failure in dependency order; a response carries one or more errors.

| Status | Codes |
|---|---|
| 400 | `INVALID_JSON`, `INVALID_TYPE`, `MISSING_FIELD`, `UNKNOWN_FIELD`, `DUPLICATE_KEY`, `DRUG_MISMATCH` |
| 404 | `UNKNOWN_DRUG`, `NOT_FOUND` (no such path) |
| 405 | `METHOD_NOT_ALLOWED` |
| 413 | `HISTORY_TOO_LONG` |
| 422 | `DRUG_NOT_TCI`, `INVALID_VALUE`, `UNIT_UNKNOWN`, `UNIT_MISMATCH`, `INFUSATE_REQUIRED`, `INFUSATE_MISMATCH`, `TIME_INVALID`, `HISTORY_INVALID`, `HISTORY_OVERLAP`, `HISTORY_IN_FUTURE`, `LATENCY_INVALID`, `TARGET_OUT_OF_RANGE` |
| 500 | `INTERNAL` |

`INTERNAL` is also what the server answers when its own check of the response
fails: before a response leaves, every number in `rate`, `segment`,
`fallback`, `state`, `prediction` and `delivered` must be finite
(`tciApiCheckFinite()`). The validation bounds every input, so a `NaN` or an
`Inf` there is a fault of the server's arithmetic, never of the request, and
no pump is handed a number it cannot run.

Warnings (200, in `warnings[]`, same `{code, message, pointer}` shape):
`RATE_MISMATCH`, `DELIVERY_MISMATCH`, `PUMP_NOT_RUNNING`, `SYRINGE_LOW`,
`HISTORY_CLOCK_SKEW`, `CLOCK_SKEW`, `LATENCY_LONG`, `INTERVAL_LONG`,
`TARGET_HIGH`.

## 7. Safety contract for integrators

**The server computes; the pump decides.** The pump enforces its own hard
limits (rate, dose, occlusion, syringe) whatever the response says, and clamps
or alarms if a response exceeds them; `rate.limitedBy` is informational.

Applying a response:

1. Before applying, check `apiVersion`, `requestId`, `drug`, `target`,
   `rate.units` and `timing.now` (the request's `now`, echoed verbatim:
   compare the strings) against the request. Any mismatch (a
   crossed response from another channel is a realistic failure): discard,
   log, treat as no response.
2. Apply an accepted response **the moment it arrives**, never later; the
   history then records the true change time. A response that arrives after
   `effectiveAt` is late: apply it if before `holdUntil`, discard it after
   and ask again.
3. Apply a response only if it answers the most recent request sent on that
   channel and no newer response has been applied (`requestId` increases
   monotonically per channel).
4. Program each command as a **bounded segment**: `rate.value` for
   `segment.holdSeconds`, or `segment.volumeMl` at `rate.value`. At
   `holdUntil` with no newer accepted command, run `fallback.rate` for at most
   `fallback.maxSeconds`, then stop and alarm. **Never extend a hold at a rate
   above `fallback.rate`.** A command of rate 0 is held and falls back at 0,
   so the stop that follows is the planned state, and `nextRequestAt` (which
   may lie beyond `holdUntil + fallback.maxSeconds` while the effect site
   rises to its peak) says when to ask.
5. Any non-200 status, unparsable or truncated body, timeout or TLS failure is
   "no response": rule 4 applies. Do not resend the same body; form a new
   request with the current `now` and history. Never retry faster than once
   per second.
6. On any local event (new target, manual bolus, pause or resume, alarm,
   syringe change) send a new request at once and treat any outstanding
   response as stale. If a new target is below `prediction.peakCe`, the pump
   may stop immediately without waiting for the server; the next response
   restarts it.
7. Exactly one controller per pump channel; the API cannot arbitrate between
   two.
8. Target 0 stops the infusion. A manual bolus is reported in the history and
   the controller gives nothing until the concentration is back at target.
9. All times in a request come from one monotonic-derived clock; a clock step
   invalidates the stored history until re-based.

### 7.1 Deployment requirements

* TLS 1.2+ off loopback; mutual TLS or a bearer token at the ingress; no
  cookies, CORS disabled, `/__docs__/` off (`docs = FALSE`).
* A structured one-line audit log per request: `receivedAt`, `requestId`,
  `drug`, SHA-256 of the body, `rate.value`, `controller`, warning codes,
  `computeSeconds`. The pump retains the full request/response pairs as the
  device record.
* No rate limiting on the controller path (a 429 is a missed cycle); if an
  ingress imposes one it returns 429 with `Retry-After`, which the pump treats
  as no response.
* Plumber serves one request at a time per process: run several workers
  behind a load balancer sized for the fleet. A typical request computes in
  5 to 10 ms; one with 20000 history events in about 70 ms on a 4-core
  machine (the validation is vectorised over the events for this reason, and
  the look-ahead is bounded so that a target the pump's ceiling can never
  reach is decided at once rather than stepped for 30000 s). `test-tci-api.R`
  guards both against regression with a 0.5 s budget, skipped on CI, whose
  shared runners cannot keep a wall-clock promise.
* shinyapps.io does not host Plumber APIs and is not a place for a pump
  controller in any case. Serve with
  `Rscript -e 'stanpumpR::run_tci_api(host = "127.0.0.1", port = 8000)'`
  behind the ingress, in a container, or on Posit Connect with
  `rsconnect::deployAPI(system.file("api", package = "stanpumpR"))`
  (`inst/api/entrypoint.R` returns `stanpumpR::tciApiRouter()`). There is no
  CI deploy of the API.

## 8. Implementation

### 8.1 Files

| File | Contents |
|---|---|
| `R/constants.R` | The block `# TCI API (tci-api.R)`: `TCI_API_VERSION <- "1.0"`, `TCI_API_MIN_INTERVAL <- 1`, `TCI_API_MAX_INTERVAL <- 60`, `TCI_API_MAX_HISTORY <- 20000L`, `TCI_API_LOOKAHEAD_MAX <- 30000` (s, STANPUMP's horizon), `TCI_API_LOOKAHEAD_EXACT_STEPS <- 300L` and `TCI_API_LOOKAHEAD_MAX_STEP <- 60` (s; §5.6), `TCI_API_CLOCK_SKEW <- 2` (s, history tolerance), `TCI_API_SERVER_SKEW <- 30` (s), `TCI_API_MAX_RATE <- 1e5`, `TCI_API_TARGET_CEILING <- 5` (× Upper), `TCI_API_ON_TARGET <- 0.01`, `TCI_API_DELIVERY_TOLERANCE <- 0.02`, `TCI_API_DELIVERY_TOLERANCE_ML <- 0.5`, `TCI_API_MIN_INFUSATE <- 1e-3` (the least syringe concentration, in its own units), `TCI_API_PK_CACHE_SIZE <- 256L` (distinct covariate sets whose PK `tciApiPK()` keeps), the unit token vectors (`TCI_API_RATE_UNITS`, `TCI_API_AMOUNT_UNITS`, `TCI_API_BOLUS_UNITS`, `TCI_API_INFUSATE_UNITS`), `TCI_API_PUMP_STATES`, `TCI_API_TIME_PATTERN` (the RFC 3339 form with every field within its range, also in the OpenAPI document). |
| `R/tci.R` | The controller the app runs, shared with the API. Its one change for the API: the effect-site search grid depends on `dt`. `tciKinetics()` keeps `tPeak`, and `tciRate()` builds `tau <- seq(dt, dt + K$tPeak + 10/60, by = 1/60)`, deduplicated, with `dt` first. The change is documented in the file header, and `tests/testthat/test-tci.R` passes unchanged. |
| `R/tci-api.R` | Pure R, no Shiny, no plumber. Exported: `tciNextRate(request, drug = request$drug, receivedAt = Sys.time())`, which takes the parsed body (a list as `jsonlite::fromJSON(txt, simplifyVector = FALSE)` gives) and returns the response as a list ready for `jsonlite::toJSON(auto_unbox = TRUE, digits = NA, null = "null")`; `tciApiDrugs()` (the `/drugs` body); `tciApiModel(drug, weight, height, age, sex, adjustToFFM = TRUE)` (the `model` block); `tciApiHealth()`. Internal: `tciApiValidate()` (walks the body for duplicate and unknown keys, types, bounds and units; signals `tci_api_error`), `tciApiValidateHistory()` (every check a vectorised comparison over one row per field, no loop over the events: §7.1), `tciApiHistory()` (sort, override, integrate → segments and boluses in base units), `tciStatesFromHistory()`, `tciApiPlan()`, the prediction helpers `tciApiPeak()`, `tciApiDecrement()` and `tciApiTargetReached()`, `tciApiCheckFinite()` (every number a pump acts on is finite, else 500 `INTERNAL`), `tciApiPK()` (the PK per covariate set, in an environment of at most `TCI_API_PK_CACHE_SIZE` entries, oldest evicted), the unit helper `tciApiToBase()`, and the time helpers `tciApiParseTime()` (a strict RFC 3339 regular expression, `TCI_API_TIME_PATTERN`, then `lubridate::fast_strptime()`) and `tciApiFormatTime()` (`%Y-%m-%dT%H:%M:%OS3Z`). Errors are a classed condition `tci_api_error` with fields `status`, `code`, `message`, `pointer`, raised with `tciApiStop(status, code, message, pointer)`. Warnings accumulate in a collector and are returned in the body. `tciNextRate()` is deterministic given its arguments. |
| `R/tci-api-server.R` | `tciApiRouter(docs = FALSE)`: the pure builder, programmatic plumber routes (`plumber::pr()`, `pr_get()`, `pr_post()`), **first** `pr_set_serializer(plumber::serializer_unboxed_json(digits = NA, null = "null"))`, the `/rate` route with `parsers = list(json = list(simplifyVector = FALSE))`, `pr_set_error()` mapping a `tci_api_error` to its status and body, a JSON parse error from plumber's body parser (message matching `lexical error|parse error|premature EOF|invalid`) to 400 `INVALID_JSON`, a failure inside the controller (marked by stage in the handler, class `tci_api_internal`, whatever its message) and anything else to 500 `INTERNAL`; `pr_set_404()` → 404 `NOT_FOUND`; the OpenAPI document from `inst/api/openapi.json` via `pr_set_api_spec()`, also served at `/tci/v1/openapi.json`; mounted so paths are `/tci/v1/...`. `run_tci_api(host = "127.0.0.1", port = 8000, docs = FALSE)` = `plumber::pr_run(tciApiRouter(docs), host, port)`. Both start with `if (!requireNamespace("plumber", quietly = TRUE)) stop(...)`, use `plumber::` prefixes only, and share nothing with `run_app()` (no `config.yml`, no `.sprglobals`, no ggplot theme). |
| `inst/api/entrypoint.R` | `stanpumpR::tciApiRouter()` (for `rsconnect::deployAPI()`). |
| `inst/api/openapi.json` | Hand-written OpenAPI 3.0 document matching this reference (schemas for request, response, error). |
| `DESCRIPTION` | `Suggests:` lists `plumber`. **renv.lock is deliberately unchanged**: plumber is never loaded by the Shiny app, the deploy installs Imports only, and R-CMD-check installs Suggests. |
| `tests/testthat/test-tci-api.R` | Core tests, no HTTP (§8.2). |
| `tests/testthat/test-tci-api-http.R` | Router tests (§8.3), `skip_if_not_installed("plumber")`, entirely in-process with `pr$call()` on a hand-built Rook request environment (`REQUEST_METHOD`, `PATH_INFO`, `QUERY_STRING`, `HTTP_CONTENT_TYPE`, `rook.input` with `read`, `read_lines`, `rewind`, `SERVER_NAME`, `HTTP_HOST`); the helper `rookRequest(method, path, body = NULL, query = "")` lives in `tests/testthat/helpers.R`. No subprocess, no network. |
| `docs/tci-api.md` | This document. |
| `inst/help/tci-api.md` | The in-app help page ("The TCI API", section "The project", registered after "scripting" and "simulation-api" in `helpStaticPages()`): what the API is, the request and response in brief, the safety contract, how to run it. It links `help:tci` and `help:scripting` and is linked from both. |
| `inst/help/repository.md`, `inst/help/in-development.md`, `docs/architecture.md`, `AGENTS.md` | File-map entries, the "Recently landed" bullet, the "App features" bullet and component-catalog lines, and the AGENTS.md paragraph naming the tests. AGENTS.md's "Adding an R package" rule records that a package used only by an optional entry point (plumber for the TCI API) goes in `Suggests`, is guarded with `requireNamespace()`, and is not snapshotted into renv.lock. |

### 8.2 Tests (core, `test-tci-api.R`)

1. **Equivalence with the app.** Latency 0, `interval = TCI_INTERVAL * 60`,
   `maxRate` huge (1e5 in `mg/min`), no `minRate`/`rateResolution`, rate
   units `mg/min`, maximum 60 min (and 120): a closed loop that replays each
   commanded rate into the history as a rate change reproduces
   `tciSchedule()`'s `rates$Rate` with `expect_equal(tolerance = 1e-10)`
   (`tciSchedule()` keeps a fixed 10 s step while `maximum ≤ 333 min`). For a
   capped pump, the comparison is against `tciSchedule(..., maxRate = cap)`.
2. **Engine cross-check.** The same loop's delivery, written as a dose table
   in library units (`mg/min` rows), simulated by `simCpCe()`, agrees with
   `state.now` at the end (Cp and Ce within 1e-6 relative).
3. **Latency.** With `latency` 1.5 s and the simulated pump switching exactly
   at `effectiveAt`, the loop reaches the target without overshoot
   (`max Ce ≤ 1.002 × CT`) and holds within 0.5% after `2 × tPeak`. With
   actual latency 0.5 s shorter than declared, likewise. With actual latency
   1.5 s longer than declared on an unlimited pump the peak overshoots by
   about 15%, which the test pins as documentation of §5.5.
4. **Fallback.** Dropping every response during loading and running
   `fallback.rate` for `fallback.maxSeconds` then stopping never overshoots
   the target by more than 1%; the old rule (holding the loading rate one more
   interval) is shown to overshoot by > 50%.
5. **Interval.** One plan then pump off touches the target at every interval
   from 1 to 120 s (what the grid fix buys; before it, 60 s overshot by 3.5%
   for propofol). For every TCI drug the closed loop from zero reaches the
   target with `max Ce ≤ 1.002 × CT` at `interval ∈ {1, 5, 10}` s; at 30 and
   60 s it reaches the target within the hand-off's excursion
   (`max Ce ≤ 1.03 × CT`, or 1.045 for etomidate at 60 s, §5.3), and the
   measured figures at 60 s, propofol's +1.56% (asking every interval) and
   etomidate's +4.2%, are pinned so that a change in either direction is
   noticed.
6. **Cap and resolution.** `maxRate` 1200 and 300 mL/hr of 1% propofol:
   loading spreads over several intervals, peak Ce ≤ 1.002 × CT, each
   capped interval has `"maxRate"` in `limitedBy`; `rateResolution` 0.1
   never rounds up; `minRate` sends 0 below it.
7. **History forms.** Delivered segments, a mid-case infusate change, a
   hand bolus, `pump.state` not running, `volumeInfused` mismatch, and the
   clock-skew clamp each behave as specified; delivered segments override
   rate changes over their span; two rate changes at one instant are
   rejected.
8. **Validation.** Every error code in §6.3 is raised by a minimal request,
   with its status and pointer; every warning code appears when it should.
   Duplicate keys, unknown fields, string numbers, `1e400`, `null`, epoch
   numbers for times, `mL/h`, `µg`, `mg/L`, the wrong concentration scale.
   The syringe floor (`TCI_API_MIN_INFUSATE`) refuses `1e-305` in the
   request and `0.0001` in a history infusate event, each with its pointer;
   `tciApiCheckFinite()` refuses a response carrying `NaN` or `Inf` anywhere
   a pump acts on as 500 `INTERNAL`, also through `tciNextRate()` with a
   prediction helper mocked to misbehave.
9. **Every TCI drug** reaches its typical target (`PK$typical`) without
   overshoot and `tciApiDrugs()` lists exactly the TCI drugs. At
   `MIN_WEIGHT` (0.1 kg, 50 cm, age 0) no commanded or fallback rate lies in
   (0, 1e-6) and the first response's `fallback.rate` is exactly 0 (the zero
   snap in `tciApiPlan()`, a millionth of the target). The PK cache holds at
   most `TCI_API_PK_CACHE_SIZE` entries after 300 distinct weights, evicts the
   oldest, and a hit returns the identical object.
10. **Predictions.** `prediction.peakAt` equals the time the target is
    touched; `targetReachedAt` for a capped load is later than for an
    unlimited one; `lookAhead` matches a direct closed-form decay from the
    state at `now`; `syringeEmptyAt` arithmetic.
11. **Determinism.** Two calls with the same body give identical bodies apart
    from `server.*`. The §4 body, embedded as an R list, gives the §6.1
    response (plan `plasma`, phase `maintenance`, `rate.value` within 0.05
    mL/hr, `state.now` within 1e-3, no warnings), and the OpenAPI document's
    two examples carry the same body and the same key tree as the real
    output.

### 8.3 Tests (router, `test-tci-api-http.R`)

`/health`, `/drugs`, `/drugs/propofol/model`, a full `/rate` round trip whose
body equals `tciNextRate()`'s result for the same request (apart from
`server.*`), each error status with the §6.3 shape (including malformed JSON →
400 `INVALID_JSON`, unknown path → 404 `NOT_FOUND`, GET on `/rate` → 405),
full-precision serialization (`0.000123456789` round-trips unchanged, scalars
unboxed), and `/tci/v1/openapi.json` parses and lists the five paths (the
four endpoints and `/tci/v1/openapi.json` itself).

## 9. Design decisions

Decisions taken in the design review, recorded so they are not reopened by
accident:

* `tciRate()`'s search grid depends on the interval (a blocker from three of
  the four review lenses). The interval is capped at 60 s, with a per-drug
  warning above tPeak/3.
* The fail-safe is a server-supplied `fallback` plus a pump-side hard stop at
  `holdUntil`; "hold the current rate one more interval" was shown to double
  a loading dose.
* `latency` and `interval` are required; latency is defined to the pump's
  acknowledgement and must be estimated high; `latency ≥ interval` is an
  error.
* Times are RFC 3339 strings only; a 2 s clock-skew tolerance replaces a
  strict future rejection; the clock is the controller's.
* The history has delivered segments (preferred) and infusate-change events;
  `pump.volumeInfused` cross-checks the integral; `pump.currentRate` is an
  optional cross-check and never overrides the record; pump boluses are
  segments, not `bolus` events.
* `pump.state` and `pump.syringeVolumeRemaining` are in; `lastCommand` and a
  signed checkpoint are deferred (the history makes a lost response visible
  on its own; the compaction guidance in §4.2 covers long cases).
* Unit tokens use the library's spellings (`mL/hr`), one closed list, no
  aliases; `mL/hr` is always reported beside the pump's units.
* Rounding is always down. Contradictory input (two rate changes or two
  infusate changes at one instant, duplicate keys, unknown fields) is an
  error. A `mL` quantity before the first recorded syringe is refused rather
  than priced with the request's syringe (§4.2).
* Traceability: `model.parameters` + `hash`, `algorithm`, `server.commit`,
  `drug` in the body, `requestId` echoed in errors; `errors[]` with JSON
  pointers; 422 for domain errors.
* Full-precision unboxed serialization; `simplifyVector = FALSE` parser;
  plumber errors mapped to the API's shape; programmatic routes with a
  hand-written OpenAPI document; docs off by default.
* Covariate limits stay the package's (`MIN_*`/`MAX_*`): widening them (a
  92-year-old is an ordinary TCI patient; `MAX_AGE` is 90) is an app-wide
  decision for the maintainer, flagged in the PR.

## 10. Running the API

Plumber is in `Suggests`, not `Imports`: the Shiny app never loads it, the
shinyapps.io deploy does not install it, and `renv.lock` does not pin it.
Install it once (`install.packages("plumber")`) on the machine that serves the
API. `tciApiRouter()` and `run_tci_api()` both stop with a clear message when
it is missing; everything in `R/tci-api.R`, including `tciNextRate()`, works
without it.

Locally, on the loopback interface:

```sh
Rscript -e 'stanpumpR::run_tci_api(host = "127.0.0.1", port = 8000)'
```

Then `GET http://127.0.0.1:8000/tci/v1/health` answers, and the OpenAPI
document is at `http://127.0.0.1:8000/tci/v1/openapi.json`. Pass
`docs = TRUE` only on a development machine; it serves the interactive
documentation at `/__docs__/`, which §7.1 keeps off in production.

On Posit Connect, deploy the package's `inst/api` directory, whose
`entrypoint.R` returns the router:

```r
rsconnect::deployAPI(system.file("api", package = "stanpumpR"))
```

In either case the ingress supplies TLS, authentication and the audit log
(§7.1); the router itself has no cookies, no CORS and no state.

## 11. Client examples

Each example POSTs the §4 request body, saved as `request.json`, and reads
`rate.value` (in `pump.rateUnits`, here mL/hr) and `timing.nextRequestAt`
(the latest time to ask again). The example body's `now` is a fixed instant,
so a live server answers it with the `CLOCK_SKEW` warning; a controller sends
its current clock.

curl, reading the two fields with `jq`:

```sh
curl -sS -X POST 'http://127.0.0.1:8000/tci/v1/drugs/propofol/rate' \
  -H 'Content-Type: application/json' \
  --data @request.json |
  jq '{rate: .rate.value, units: .rate.units, nextRequestAt: .timing.nextRequestAt, warnings: [.warnings[].code]}'
```

R, with httr2 (`req_body_json()` serializes with `auto_unbox = TRUE` and
`digits = NA`, which keeps the numbers exact; `req_error()` is told to return
an error body rather than throw, so the `errors[]` array can be read):

```r
library(httr2)
body <- jsonlite::fromJSON("request.json", simplifyVector = FALSE)
resp <- request("http://127.0.0.1:8000/tci/v1/drugs/propofol/rate") |>
  req_body_json(body) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()
out <- resp_body_json(resp)
if (resp_status(resp) == 200) {
  out$rate$value            # in pump.rateUnits (mL/hr here)
  out$timing$nextRequestAt  # RFC 3339, on the controller's clock
} else {
  out$errors                # list of {code, message, pointer}
}
```

Without a server, the same body goes straight to the exported function:

```r
out <- stanpumpR::tciNextRate(body)
out$rate$value
out$timing$nextRequestAt
```

Python, with `requests`:

```python
import json
import requests

with open("request.json") as f:
    body = json.load(f)

r = requests.post(
    "http://127.0.0.1:8000/tci/v1/drugs/propofol/rate",
    json=body,
    timeout=2.0,
)
out = r.json()
if r.status_code == 200:
    print(out["rate"]["value"], out["rate"]["units"])
    print(out["timing"]["nextRequestAt"])
else:
    print(r.status_code, [e["code"] for e in out["errors"]])
```

A controller then applies the response under the rules of §7: check it
against the request, program `rate.value` as a segment of
`segment.holdSeconds`, fall back to `fallback.rate` for at most
`fallback.maxSeconds` if nothing newer has arrived by `holdUntil`, and ask
again no later than `nextRequestAt`, or at once on any local event. A
complete controller that does all of this, in JavaScript, is
[FauxTCI](https://github.com/StevenLShafer/FauxTCI) (`js/pump.js` holds the
pump's state machine, `js/transport.js` the HTTP client).
