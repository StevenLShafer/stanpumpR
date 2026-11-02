# stanpumpR Simulation API

The stanpumpR Simulation API is an HTTP service for pharmacokinetic simulation.
You send it three things:

- a dose table
- the patient's covariates
- how long to simulate

It returns the predicted plasma and effect-site concentration of every drug
over that time. The results come back as JSON, CSV, Excel (xlsx) or XML, so
any language or tool can read them.

The predictions come from the same pharmacokinetic and pharmacodynamic models
as the stanpumpR web application at [stanpumpr.io](https://stanpumpr.io), with
the app's default settings (the antiseizure drug interactions on; see
`options`). The models are covariate-adjusted models drawn from the published
literature, and they cover more than 100 drugs. They are not all of one
structure: most are linear one-, two- or three-compartment models solved in
closed form, a few are solved numerically (saturable, Michaelis-Menten
elimination or absorption) or by matrix exponential (transit-chain
absorption). Each drug's structure, source and population are on its page in
the app's Help tab. They include
intravenous anesthetics and sedatives, opioids, neuromuscular blocking and
reversal agents, local anesthetics, oral analgesics, antibiotics,
corticosteroids, antiemetics, antiseizure drugs, antidepressants,
antipsychotics and stimulants. `GET /drugs` returns the current list.

> **Intended use.** stanpumpR is for teaching and research. It is not a medical
> device and has not been cleared by any regulatory agency. Its predictions are
> population-typical values for a patient with the given covariates. They do
> not account for an individual's variability and do not replace clinical
> judgement.

---

## At a glance

| Topic | Summary |
|---|---|
| Style | REST over HTTP. Requests are JSON. Each request is independent. |
| Endpoints | `POST /simulate`, `GET /drugs`, `GET /health` |
| Response formats | JSON (default), CSV, Excel (xlsx), XML |
| State | Stateless. The server stores nothing between requests. |
| Patient data | No identifiers are needed: a request needs only weight, height, age and sex, plus optional laboratory and pharmacogenetic covariates. The service does not refuse an extra field (it is named in `warnings`), so a caller must leave identifiers out. |
| Units | Times can be sent in minutes, hours, days or weeks, and weight, height and age in common units. Every response is in minutes, kg, cm and years. |
| Versioning | `apiVersion` is `"1"`. Within version 1, fields are only added, never changed or removed. The version governs the shape of requests and responses, not the predictions: the models are revised as the literature advances, so a result is tied to a model build by `meta.build` (see [Running the service](#running-the-service)). |
| Source | Open source (MIT): [github.com/StevenLShafer/stanpumpR](https://github.com/StevenLShafer/stanpumpR) |

---

## Base URL

The API is part of the stanpumpR R package. It runs wherever the package's
service is hosted:

- on a server you operate
- on Posit Connect
- in a Docker container

This documentation is published at stanpumpr.io, but the API service itself
isn't hosted there.

In this document, `{BASE_URL}` stands for the address of the server you use,
for example `https://api.example.org`. To run a server on your own computer,
see [Running the service](#running-the-service). Its address is
`http://127.0.0.1:8000`.

---

## Quick start

This request gives a 70 kg, 170 cm, 50-year-old man a 100 mcg bolus of
fentanyl. It returns the plasma and effect-site concentrations every 5 minutes
for 10 minutes.

```bash
curl -X POST "{BASE_URL}/simulate" \
     -H "Content-Type: application/json" \
     -d '{
           "patient":  {"weight": 70, "height": 170, "age": 50, "sex": "male"},
           "duration": 10,
           "doses":    [{"drug": "fentanyl", "time": 0, "dose": 100, "units": "mcg"}],
           "output":   {"interval": 5}
         }'
```

The response (JSON):

```json
{
  "meta": {
    "apiVersion": "1",
    "stanpumpRVersion": "0.0.1.9002",
    "build": "f0fe568de3e4074020e138bcef7a86225c8213bd",
    "generated": "2026-10-12T22:44:35Z",
    "timeUnit": "minutes",
    "duration": 10,
    "grid": "requested",
    "antiseizureInteractions": true,
    "reference": "https://stanpumpr.io"
  },
  "patient": {
    "weight": 70, "weightUnit": "kg", "height": 170, "heightUnit": "cm",
    "age": 50, "ageUnit": "years", "sex": "male", "pregnant": false, "adjustToFFM": true,
    "osmolality": 280, "creatinine": null, "gestationalAge": null,
    "cyp2d6": "normal", "cyp2c19": "normal", "cyp2c9": "*1/*1", "cyp2b6": "normal",
    "vkorc1": "AA", "cyp4f2": "CC", "opioidTolerance": "none"
  },
  "drugs": [
    {
      "Drug": "fentanyl", "Dosed": true, "ConcentrationUnits": "ng/mL", "MEAC": 0.6,
      "PeakPlasma": 8.264457841, "TimePeakPlasma": 0,
      "PeakEffectSite": 2.244197188, "TimePeakEffectSite": 3.402395028,
      "RecoveryThreshold": 0.6
    }
  ],
  "doses": [
    {"Drug": "fentanyl", "Time": 0, "Dose": 100, "Units": "mcg", "Simulated": true}
  ],
  "events": [],
  "timeseries": [
    {"Drug": "fentanyl", "Time": 0,  "Plasma": 8.264457841,  "EffectSite": 0,           "ConcentrationUnits": "ng/mL"},
    {"Drug": "fentanyl", "Time": 5,  "Plasma": 1.554401881,  "EffectSite": 2.141025469, "ConcentrationUnits": "ng/mL"},
    {"Drug": "fentanyl", "Time": 10, "Plasma": 0.7994632409, "EffectSite": 1.37732736,  "ConcentrationUnits": "ng/mL"}
  ],
  "warnings": []
}
```

The same request from Python, with the time series returned as CSV:

```python
import io, requests, pandas as pd

request = {
    "patient":  {"weight": 70, "height": 170, "age": 50, "sex": "male"},
    "duration": 10,
    "doses":    [{"drug": "fentanyl", "time": 0, "dose": 100, "units": "mcg"}],
    "output":   {"interval": 5},
}
r = requests.post("{BASE_URL}/simulate", params={"format": "csv"}, json=request)
r.raise_for_status()
df = pd.read_csv(io.StringIO(r.text))   # columns: Drug, Time, Plasma, EffectSite, ConcentrationUnits
```

---

## Endpoints

| Method | Path | Returns |
|---|---|---|
| `POST` | `/simulate` | A simulation (see [The request](#the-request) and [The response](#the-response)). Query parameters: `format` (`json`, `csv`, `xlsx`, `xml`) and `layout` (`long`, `wide`). |
| `GET` | `/drugs` | The drugs a request can dose. For each drug: its category, its concentration units and the dose units it accepts (`Units`, always an array in JSON; comma-separated in CSV). An active metabolite that cannot be dosed itself, such as `desmetramadol`, is not listed; it appears in a result as a `drugs` row with `Dosed` false. `?format=csv` returns CSV. `?allowIllicitDrugs=true` adds the opt-in illicit-drug research models. |
| `GET` | `/health` | `{"status": "ok", "apiVersion": "1", "stanpumpRVersion": "...", "build": "<commit>"}` (`build` is `null` when the installation did not record its commit) |
| `GET` | `/__docs__/` | Interactive (Swagger) documentation for the running server. |

### Choosing the response format

The first of these that is present sets the format:

1. the `format` query parameter (`/simulate?format=csv`)
2. `output.format` in the request body
3. the `Accept` header: `application/json`, `text/csv`, `application/xml`, or
   `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` for xlsx

If none is given, the response is JSON. The layout (`long` or `wide`) follows
the same rule: the `layout` query parameter comes first, then `output.layout`
in the body. A `format` or `layout` at the top level of the body is not one of
these: it is ignored and named in `warnings`.

---

## The request

A request is a JSON object, sent as `application/json` (another declared
media type is refused with 415). Field names ignore case and any `_`, space or
`.`, so `weightUnit`, `weight_unit` and `WeightUnit` are the same field. A
field given more than once, in any of those spellings, is refused (400), so
the service can never pick one of two values. A field the service does not
know is ignored and named in the response's `warnings`, at every level (the
request, `patient`, `output`, `options` and the columns of `events`), so a
misspelt covariate is not defaulted in silence. The columns of `doses` are
stricter: one the service does not read (a `stopTime` on an infusion, say) is
refused (400), because ignoring it would simulate a different regimen from the
one the client meant. Set `options.unknownDoseFields` to `"warn"` to have such
a column ignored and named in `warnings` instead. `patient`, `output` and `options` must be JSON objects, and
every value, including each cell of `doses` and `events`, a single value (not
an empty array or an object); anything else is refused (400).

```jsonc
{
  "patient": {
    "weight": 70,            // required
    "height": 170,           // required
    "age": 50,               // required
    "sex": "male",           // required: "male" | "female"
    "pregnant": false,       // late-pregnancy kinetics for the drugs that read it (default false);
                             //   true needs sex "female"
    "weightUnit": "kg",      // "kg" (default) | "lb"
    "heightUnit": "cm",      // "cm" (default) | "in"
    "ageUnit": "years",      // "years" (default) | "months"
    "adjustToFFM": true,     // scale each model to fat-free mass (default) rather than total weight
    "creatinine": null,      // serum creatinine, mg/dL; omit or null for the normal value for age and sex
    "gestationalAge": null,  // gestational age at birth, weeks; omit or null for a term birth (neonatal models)
    "osmolality": 280,       // baseline serum osmolality, mOsm/kg (read only by mannitol)
    "cyp2d6": "normal",      // poor | intermediate | normal | ultrarapid
    "cyp2c19": "normal",     // poor | intermediate | normal | rapid | ultrarapid
    "cyp2c9": "*1/*1",       // *1/*1 | *1/*2 | *1/*3 | *2/*2 | *2/*3 | *3/*3
    "cyp2b6": "normal",      // poor | intermediate | normal | rapid | ultrarapid
    "vkorc1": "AA",          // GG | GA | AA
    "cyp4f2": "CC",          // CC | CT | TT
    "opioidTolerance": "none" // none | mild | somewhat | high: raises each opioid's MEAC and recovery threshold
  },

  "duration": 6,             // required: length of the simulation, in timeUnit
  "timeUnit": "hours",       // minutes (default) | hours | days | weeks
                             // applies to duration, dose and event times, and output.interval / output.times

  "doses": [                 // required: one row per dose or rate change
    {"drug": "propofol", "time": 0,      "dose": 2,   "units": "mg/kg"},
    {"drug": "propofol", "time": 0,      "dose": 100, "units": "mcg/kg/min"},
    {"drug": "propofol", "time": 1,      "dose": 0,   "units": "mcg/kg/min"},
    {"drug": "fentanyl", "time": "1:30", "dose": 50,  "units": "mcg"}
  ],

  "events": [                // optional: clinical events; some change the pharmacokinetics
    {"time": 0.5, "event": "Intubation"}
  ],

  "output": {                // optional
    "interval": 0.25,        // grid step in timeUnit, or "native" (see "The time grid")
    "times": [0, 0.5, 1, 2], // or explicit times (at least one); overrides interval
    "recovery": false,       // add a Recovery column: minutes until the drug falls below its threshold
    "format": "json",        // json | csv | xlsx | xml
    "layout": "long"         // csv and xlsx: long | wide
  },

  "options": {
    "allowIllicitDrugs": false,      // allow the opt-in "Illicit drugs" research models
    "antiseizureInteractions": true, // apply the interactions among the antiseizure drugs (default true, as in the app)
    "unknownDoseFields": "reject"    // a dose column the service does not read: "reject" (default, 400) | "warn"
  }
}
```

### Patient covariates

| Field | Required | Allowed values |
|---|---|---|
| `weight` | yes | 0.1–500 kg (after conversion from `lb`) |
| `height` | yes | 10–200 cm (after conversion from `in`) |
| `age` | yes | 0–90 years (after conversion from `months`) |
| `sex` | yes | `male`, `female` |
| `pregnant` | no | `false` (default) or `true`, which needs `sex` `female`. Only the drugs with a late-pregnancy model read it. |
| `creatinine` | no | 0.2–15 mg/dL. If it is omitted, each renally cleared model assumes the normal value for the patient's age and sex. |
| `gestationalAge` | no | 22–44 weeks, the gestational age at birth. If it is omitted, the birth is taken as at term (40 weeks). Only the neonatal models (`dobutamineNeonatal`) read it, with the age, as the postmenstrual age. |
| `osmolality` | no | 200–400 mOsm/kg. The default is 280. |
| `cyp2d6`, `cyp2c19`, `cyp2c9`, `cyp2b6` | no | The values listed above. The default is the normal phenotype (`*1/*1` for CYP2C9). Only the models that use a phenotype read it. |
| `vkorc1` | no | The VKORC1 -1639G>A genotype, read by the three vitamin-K antagonists: `warfarin`, `fluindione` and `acenocoumarol`. Unlike the phenotypes above, the default is chosen for sensitivity rather than for frequency: `AA` is the genotype that gives the **largest** INR rise, so that an unentered genotype errs toward underdosing. Pass the patient's own genotype when it is known. |
| `cyp4f2` | no | The CYP4F2 rs2108622 genotype, read by `warfarin` alone. Its default, `CC`, is that model's most sensitive genotype, by the same rule as `vkorc1`. |
| `opioidTolerance` | no | `none` (default), `mild`, `somewhat` or `high`: multiplies each opioid's MEAC, and the recovery threshold of an opioid with a MEAC, by 1, 1.5, 2 or 3, as the app's Patient Profile does. It does not change any concentration. |
| `adjustToFFM` | no | `true` (default) or `false` |

### The dose table

`doses` can be an array of row objects, as in the example above. It can also
be an object of equal-length columns:
`{"drug": [...], "time": [...], "dose": [...], "units": [...]}`.

- **Drug.** Use the drug's name as it appears in `GET /drugs` (for example
  `propofol`, `fentanyl`, `remifentanil`).
- **Units.** Each drug accepts only its own units, which `GET /drugs` lists.
  - A **bolus** is a mass (`mg`, `mcg/kg`).
  - An **infusion** is a rate (`mcg/kg/min`, `mg/hr`). It runs until the next
    rate for the same drug. Send a rate of 0 to stop it.
  - **Extravascular doses** carry their route: oral (`mg PO`), sublingual
    (`mg SL`), intramuscular (`mg IM`), intranasal (`mcg IN`), subcutaneous
    (`mg SC`, `IU SC`, `U SC`, and the insulin pump rate `U/hr SC`) and
    regional anesthesia (`mg RA`).
  - **Activity and salt units.** The heparins are dosed in IU and the insulins
    and vasopressin in units (`U`); calcium's units name the salt
    (`g chloride`, `mg/min gluconate`).
- **Scheduled doses.** A unit ending in `qd`, `bid`, `tid`, `qid` or `q4h`
  (`mg PO bid`) repeats the dose until the end of the simulation; the oral
  antiseizure drugs also take `qam` and `qpm` (independent morning and evening
  slots), and the subcutaneous units `qwk` (weekly, `mg SC qwk`). A scheduled
  dose of 0 stops the sequence.
- **Target-controlled infusion.** The units `Plasma target` and
  `Effect site target` ask for a target-controlled infusion (Shafer & Gregg,
  1992). `dose` is then the target concentration, in the drug's concentration
  units. The infusion rates the controller computes are returned in `doses`.
  Targets are accepted only when the simulation lasts a week or less.
- **Time.** A time is a number in `timeUnit`, or an elapsed time written
  `"H:MM"`, read as H hours plus MM minutes whatever `timeUnit` is: `"1:60"`
  is 120 minutes and `"99:99"` is 6,039, and either part may be left out
  (`":30"`, `"1:"`). Doses at or after `duration` are ignored: the response
  warns about them and marks them `Simulated: false` in `doses`.

### Events

`events` is optional, with the same two shapes as `doses` and the columns
`time` and `event`. The event names are: `Start`, `Timeout`, `Induction`,
`Intubation`, `Extubation`, `Emergence`, `End`, `CPB Start`, `CPB36`, `CPB35`,
`CPB34`, `CPB33`, `CPB32`, `CPB31`, `CPB End`, `Clamp On`, `Clamp Off`,
`Tourniquet On`, `Tourniquet Off` and `Other`. Today only the cardiopulmonary
bypass events change a model: dexmedetomidine's model for infants (a year old
or younger) switches its parameters at `CPB Start`, at each bypass temperature
(`CPB36` to `CPB31`, degrees Celsius) and at `CPB End`. Every other event is a
label, returned in `events` and changing no prediction.

### Limits

| Item | Limit |
|---|---|
| Dose rows | 500 |
| Repeats the scheduled doses expand to over `duration` | 8,000 (a `q4h` row over a year is 2,190) |
| Events | 100 |
| Duration | more than 0, up to 365 days |
| Rows per drug in the time series | 20,000, on a requested grid and on the native one |
| Rows in the whole time series, every drug and metabolite | 500,000 |
| Request body | 1 MB, however the service is started. Send it with a `Content-Length` header: a chunked body (`Transfer-Encoding: chunked`) is refused with 413 however small it is. |

A request over a limit is refused before it is simulated where the limit can
be known in advance (400); the native grid and the rows a metabolite adds are
known only afterwards, and a result over the limit is then refused with 422.

Inhaled anesthetics are not available through the API yet.

---

## The response

What each format carries differs — see [Formats](#formats) below: JSON and XML
carry the whole response described in this section, CSV carries only the time
series, and xlsx spreads the same parts across its sheets. All times are in
**minutes**, weight is in kg, height in cm and age in years, whatever units
the request used.

| Part | Contents |
|---|---|
| `meta` | `apiVersion`, `stanpumpRVersion`, `build` (the commit the service was built from, or `null`), `generated` (UTC time stamp), `timeUnit` (`"minutes"`), `duration` (minutes), `grid` (`"requested"` or `"native"`), `antiseizureInteractions` (whether the interactions were applied) |
| `patient` | The covariates as simulated, after unit conversion and defaults. `creatinine` and `gestationalAge` are `null` when the request left them out: each renal model then assumes the normal serum creatinine for the patient's age and sex (0.9 mg/dL for an adult man, 0.8 for a woman), and the neonatal models a term birth (40 weeks) |
| `drugs` | One row per drug, described below |
| `doses` | The full dose sequence simulated, described below |
| `events` | The events, with times in minutes |
| `timeseries` | The concentrations over time, described below |
| `warnings` | Notes about the simulation, such as ignored doses |

Each row of `drugs` has these columns:

- `Drug`
- `Dosed`: `false` for an active metabolite that was formed but not given
  directly, such as desmetramadol from tramadol
- `ConcentrationUnits`
- `MEAC`: the minimum effective analgesic concentration, or `null` if the drug
  has none
- `PeakPlasma` and `TimePeakPlasma`
- `PeakEffectSite` and `TimePeakEffectSite`: the largest value on the drug's
  own simulated time line (with the output times on it), which is dense after
  every dose however long the simulation is. It is a sampled maximum: an
  output time that falls closer to the true peak can raise it slightly (by
  0.13% for a fentanyl bolus with an output time at the exact peak)
- `RecoveryThreshold`: the concentration `Recovery` counts down to, in
  `ConcentrationUnits` (for an opioid, after `opioidTolerance`), or `null` if
  the drug has none

`doses` lists the rows you sent, plus two kinds of rows the server generated:

- the infusion rates computed for each concentration target
- the repeats of each scheduled dose

Each row has `Drug`, `Time`, `Dose`, `Units` and `Simulated`, which is `false`
for a row at or after `duration`, which the simulation ignored.

Each row of `timeseries` has `Drug`, `Time`, `Plasma`, `EffectSite` and
`ConcentrationUnits`, plus `Recovery` if you requested it.

A few values need explaining:

- The plasma and effect-site concentrations of a drug are in the same units,
  given by `ConcentrationUnits` (for example `ng/mL`, `mcg/mL`).
- `EffectSite` is `null` (empty in CSV and XML) for a prodrug that has no
  effect site of its own. Tramadol is one: its effect appears in the
  `desmetramadol` rows.
- `Recovery` is the time, in minutes, until the drug's concentration falls
  below `RecoveryThreshold` if all delivery stopped at that moment: the
  effect-site concentration for a drug with an effect site, the plasma
  concentration for one without. It is `null` where it could not be computed.
- For mannitol, `Plasma` is the serum osmolality in mOsm/kg.
- For calcium, `Plasma` is the rise in ionized calcium above the patient's
  baseline, in mmol/L (`ConcentrationUnits` is `mmol/L`). Its dose units name
  the salt (`g chloride`, `mg/min gluconate`); a request with a bare `g` is
  refused.

### The time grid

By default, every drug is reported on one regular grid. The step is the
smallest of 0.1, 0.25, 0.5, 1, 2, 5, 10, 15, 30 or 60 minutes (and so on) that
keeps each drug to 1000 steps (1001 rows, both ends included) or fewer. That
gives these steps:

- 0.1 minute for a simulation of up to 100 minutes
- 1 minute for up to about 16 hours

Set `output.interval` or `output.times` to choose your own times. The
`output.times` are returned sorted, with duplicates removed, so match the rows
by `Time` rather than by their position in the request.

The values at the requested times are calculated at those exact times, not
interpolated between other points. With `output.interval = "native"`, the
response instead uses each drug's own adaptive time grid. That grid is dense
after every dose, but its times differ from drug to drug.

### Formats

- **JSON** contains the whole response. Each table is an array of row objects,
  and missing values are `null`.
- **CSV** contains the time series only, as one table.
  - With `layout=long` (the default) there is one row per drug and time.
  - With `layout=wide` there is one row per time and one column per drug and
    site (`propofol Plasma`, `propofol EffectSite`, ...). The wide layout needs
    a common time grid, so it can't be combined with `"native"`.
  - JSON and XML always carry the long time series; `layout` does not apply
    to them.
- **xlsx** is a workbook with these sheets:
  - *Time Series* (long or wide)
  - *Drugs*
  - *Doses*
  - *Events*, if there are any
  - *Patient*
  - *About*, which holds `meta` and the warnings
- **XML** contains the whole response, with one element per row and the
  columns as attributes:

  ```xml
  <stanpumpR>
    <meta apiVersion="1" timeUnit="minutes" duration="10" .../>
    <patient weight="70" weightUnit="kg" .../>
    <drugs><drug Drug="fentanyl" ConcentrationUnits="ng/mL" MEAC="0.6" .../></drugs>
    <doses><dose Drug="fentanyl" Time="0" Dose="100" Units="mcg" Simulated="true"/></doses>
    <events/>
    <timeseries><point Drug="fentanyl" Time="0" Plasma="8.264457841" EffectSite="0" .../></timeseries>
    <warnings/>
  </stanpumpR>
  ```

---

## Errors

An error response has a JSON body of the form `{"error": "..."}`, whatever
format was requested (except 413, below).

| Status | Meaning |
|---|---|
| 400 | The request is invalid. The message names the field and, for a table, the row. For example: ``"`doses` row 3: 'mg PO' is not a unit of propofol. Its units are: mg, mg/kg, ..."`` |
| 413 | The body is larger than 1 MB, or was sent chunked (see Limits). This one comes from the HTTP server before the service reads the request, so its body is the plain text `Maximum upload size exceeded`, not JSON. |
| 415 | The body was declared as something other than `application/json`. |
| 422 | The request is valid, but a drug model could not simulate it, or the result is larger than the service returns (see Limits). The message includes the model's own error text, which can be specific to the patient or dose (not a generic message). |
| 500 | A server error the service did not expect and could not attribute to the request. Details are written to the server log, not sent to the client. |

---

## Running the service

The service is part of the stanpumpR R package. Install the package and the
`plumber` package, then start the server:

```r
if (.Platform$OS.type == "windows") {
  # Windows: the ready-made package (x64 and ARM), rebuilt on every change
  zip <- file.path(tempdir(), "stanpumpR.zip")
  download.file("https://github.com/StevenLShafer/stanpumpR/releases/download/windows-binary/stanpumpR.zip",
                zip, mode = "wb")
  install.packages(zip, repos = NULL, type = "win.binary")
} else {
  # macOS or Linux: from GitHub
  install.packages("remotes")
  remotes::install_github("StevenLShafer/stanpumpR")
}

install.packages("plumber")
stanpumpR::run_api(host = "127.0.0.1", port = 8000)
```

On Windows, use the ready-made package rather than installing from source: on
Windows 11, Smart App Control can stop a source install with nothing but
`lazy loading failed`. The
[release page](https://github.com/StevenLShafer/stanpumpR/releases/tag/windows-binary)
lists the CRAN packages stanpumpR needs.

The service is defined in `inst/plumber/plumber.R` in the package, so it can
also be hosted with `plumber::pr()` on Posit Connect or in a Docker image
(`rstudio/plumber`).

`run_api()` accepts connections from the local computer only. Pass
`host = "0.0.0.0"` to accept connections from other machines. Before you
expose the service, put it behind a reverse proxy that provides TLS and
authentication.

The service itself limits what one request may ask for (see Limits) but not
how many requests arrive or how long they run. The largest permitted requests
take seconds of computation each (a year of several scheduled drugs with
`output.recovery` true, for example), and a native-grid request is refused for
its size only after it has been simulated. So the deployment should set a
request rate, a timeout, a number of concurrent workers and a memory limit.

The installation commands above follow the latest code, which changes as the
models are revised. A deployment that must give the same answer tomorrow as
today should install one commit (`remotes::install_github("StevenLShafer/stanpumpR@<commit>")`
with a locked set of dependencies), check that `/health` reports that commit as
`build`, and keep each request with its response.

---

## Using the API from R

R users don't need the HTTP service. They can call the same functions
directly:

```r
library(stanpumpR)

result <- apiSimulate(list(
  patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
  duration = 2, timeUnit = "hours",
  doses = data.frame(
    Drug  = c("propofol", "propofol",   "propofol",   "remifentanil"),
    Time  = c(0,          0,            1,            0),
    Dose  = c(2,          100,          0,            0.15),
    Units = c("mg/kg",    "mcg/kg/min", "mcg/kg/min", "mcg/kg/min")
  ),
  output = list(interval = 1/60)              # one row per minute
))

head(result$timeseries)                       # Drug, Time, Plasma, EffectSite, ConcentrationUnits
result$drugs                                  # one row per drug: units, MEAC, peaks
apiWriteResult(result, "simulation.xlsx")     # or .csv, .json, .xml
```

| Function | Purpose |
|---|---|
| `apiSimulate(request)` | Takes a request as a list (the same fields as the JSON) and returns the response as a list of data frames |
| `apiFormatResult(result, format, layout)` | Serializes the result as JSON, CSV, xlsx or XML |
| `apiWriteResult(result, path)` | Writes the result to a file. The file extension picks the format. |
| `apiDrugCatalog()` | Returns the table that `GET /drugs` sends |
| `run_api()` | Starts the HTTP service |

---

## Questions and source code

stanpumpR is open source under the MIT license. The source code, issue tracker
and contact details for the maintainer are at
[github.com/StevenLShafer/stanpumpR](https://github.com/StevenLShafer/stanpumpR).
