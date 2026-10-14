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
as the stanpumpR web application at [stanpumpr.io](https://stanpumpr.io). The
models are closed-form, three-compartment, covariate-adjusted models drawn from
the published literature, and they cover more than 100 drugs. They include
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
| Patient data | No identifiers. A request needs only weight, height, age and sex, plus optional laboratory and pharmacogenetic covariates. |
| Units | Times can be sent in minutes, hours, days or weeks, and weight, height and age in common units. Every response is in minutes, kg, cm and years. |
| Versioning | `apiVersion` is `"1"`. Within version 1, fields are only added, never changed or removed. |
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
    "generated": "2026-10-12T22:44:35Z",
    "timeUnit": "minutes",
    "duration": 10,
    "grid": "requested",
    "reference": "https://stanpumpr.io"
  },
  "patient": {
    "weight": 70, "weightUnit": "kg", "height": 170, "heightUnit": "cm",
    "age": 50, "ageUnit": "years", "sex": "male", "pregnant": false, "adjustToFFM": true,
    "osmolality": 280, "creatinine": null,
    "cyp2d6": "normal", "cyp2c19": "normal", "cyp2c9": "*1/*1"
  },
  "drugs": [
    {
      "Drug": "fentanyl", "Dosed": true, "ConcentrationUnits": "ng/mL", "MEAC": 0.6,
      "PeakPlasma": 8.264457841, "TimePeakPlasma": 0,
      "PeakEffectSite": 2.184310272, "TimePeakEffectSite": 3.53716025
    }
  ],
  "doses": [
    {"Drug": "fentanyl", "Time": 0, "Dose": 100, "Units": "mcg"}
  ],
  "events": [],
  "timeseries": [
    {"Drug": "fentanyl", "Time": 0,  "Plasma": 8.264457841,  "EffectSite": 0,           "ConcentrationUnits": "ng/mL"},
    {"Drug": "fentanyl", "Time": 5,  "Plasma": 1.554401881,  "EffectSite": 2.099509718, "ConcentrationUnits": "ng/mL"},
    {"Drug": "fentanyl", "Time": 10, "Plasma": 0.7994632409, "EffectSite": 1.384793615, "ConcentrationUnits": "ng/mL"}
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
| `GET` | `/drugs` | The drugs the API can simulate. For each drug: its category, its concentration units and the dose units it accepts. `?format=csv` returns CSV. `?allowIllicitDrugs=true` adds the opt-in illicit-drug research models. |
| `GET` | `/health` | `{"status": "ok", "apiVersion": "1", "stanpumpRVersion": "..."}` |
| `GET` | `/__docs__/` | Interactive (Swagger) documentation for the running server. |

### Choosing the response format

The first of these that is present sets the format:

1. the `format` query parameter (`/simulate?format=csv`)
2. `output.format` in the request body
3. the `Accept` header: `application/json`, `text/csv`, `application/xml`, or
   `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` for xlsx

If none is given, the response is JSON. The layout (`long` or `wide`) follows
the same rule: the `layout` query parameter comes first, then `output.layout`
in the body.

---

## The request

A request is a JSON object. Field names ignore case and any `_`, space or
`.`, so `weightUnit`, `weight_unit` and `WeightUnit` are the same field.

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
    "osmolality": 280,       // baseline serum osmolality, mOsm/kg (read only by mannitol)
    "cyp2d6": "normal",      // poor | intermediate | normal | ultrarapid
    "cyp2c19": "normal",     // poor | intermediate | normal | rapid | ultrarapid
    "cyp2c9": "*1/*1"        // *1/*1 | *1/*2 | *1/*3 | *2/*2 | *2/*3 | *3/*3
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
    "times": [0, 0.5, 1, 2], // or explicit times; overrides interval
    "recovery": false,       // add a Recovery column: minutes until the drug falls below its threshold
    "format": "json",        // json | csv | xlsx | xml
    "layout": "long"         // csv and xlsx: long | wide
  },

  "options": {
    "allowIllicitDrugs": false   // allow the opt-in "Illicit drugs" research models
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
| `osmolality` | no | 200–400 mOsm/kg. The default is 280. |
| `cyp2d6`, `cyp2c19`, `cyp2c9` | no | The values listed above. The default is the normal phenotype (`*1/*1` for CYP2C9). Only the models that use a phenotype read it. |
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
    (`mg SL`), intramuscular (`mg IM`), intranasal (`mcg IN`) and regional
    anesthesia (`mg RA`).
- **Scheduled doses.** A unit ending in `qd`, `bid`, `tid` or `qid`
  (`mg PO bid`) repeats the dose until the end of the simulation. A scheduled
  dose of 0 stops the sequence.
- **Target-controlled infusion.** The units `Plasma target` and
  `Effect site target` ask for a target-controlled infusion (Shafer & Gregg,
  1992). `dose` is then the target concentration, in the drug's concentration
  units. The infusion rates the controller computes are returned in `doses`.
  Targets are accepted only when the simulation lasts a week or less.
- **Time.** A time is a number in `timeUnit`, or an elapsed time written
  `"H:MM"`. Doses at or after `duration` are ignored, and the response warns
  about them.

### Limits

| Item | Limit |
|---|---|
| Dose rows | 500 |
| Events | 100 |
| Duration | more than 0, up to 365 days |
| Rows per drug in the time series | 20,000 |
| Request body | 1 MB, when the server is started with `run_api()` |

Inhaled anesthetics are not available through the API yet.

---

## The response

Every format carries the same content. All times are in **minutes**, weight is
in kg, height in cm and age in years, whatever units the request used.

| Part | Contents |
|---|---|
| `meta` | `apiVersion`, `stanpumpRVersion`, `generated` (UTC time stamp), `timeUnit` (`"minutes"`), `duration` (minutes), `grid` (`"requested"` or `"native"`) |
| `patient` | The covariates as simulated, after unit conversion and defaults |
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
- `PeakEffectSite` and `TimePeakEffectSite`

`doses` lists the rows you sent, plus two kinds of rows the server generated:

- the infusion rates computed for each concentration target
- the repeats of each scheduled dose

Each row of `timeseries` has `Drug`, `Time`, `Plasma`, `EffectSite` and
`ConcentrationUnits`, plus `Recovery` if you requested it.

A few values need explaining:

- The plasma and effect-site concentrations of a drug are in the same units,
  given by `ConcentrationUnits` (for example `ng/mL`, `mcg/mL`).
- `EffectSite` is `null` (empty in CSV and XML) for a prodrug that has no
  effect site of its own. Tramadol is one: its effect appears in the
  `desmetramadol` rows.
- For mannitol, `Plasma` is the serum osmolality in mOsm/kg.
- For calcium, `Plasma` is the rise in ionized calcium above the patient's
  baseline, in mmol/L (`ConcentrationUnits` is `mmol/L`). Its dose units name
  the salt (`g chloride`, `mg/min gluconate`); a request with a bare `g` is
  refused.

### The time grid

By default, every drug is reported on one regular grid. The step is the
smallest of 0.1, 0.25, 0.5, 1, 2, 5, 10, 15, 30 or 60 minutes (and so on) that
keeps each drug to 1000 rows or fewer. That gives these steps:

- 0.1 minute for a simulation of up to 100 minutes
- 1 minute for up to about 16 hours

Set `output.interval` or `output.times` to choose your own times.

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
    <doses><dose Drug="fentanyl" Time="0" Dose="100" Units="mcg"/></doses>
    <events/>
    <timeseries><point Drug="fentanyl" Time="0" Plasma="8.264457841" EffectSite="0" .../></timeseries>
    <warnings/>
  </stanpumpR>
  ```

---

## Errors

An error response has a JSON body of the form `{"error": "..."}`, whatever
format was requested.

| Status | Meaning |
|---|---|
| 400 | The request is invalid. The message names the field and, for a table, the row. For example: ``"`doses` row 3: 'mg PO' is not a unit of propofol. Its units are: mg, mg/kg, ..."`` |
| 422 | The request is valid, but a drug model could not simulate it. |
| 500 | Unexpected server error. Details are written to the server log, not sent to the client. |

---

## Running the service

The service is part of the stanpumpR R package. Install the package and the
`plumber` package, then start the server:

```r
# install.packages("remotes"); remotes::install_github("StevenLShafer/stanpumpR")
# install.packages("plumber")
stanpumpR::run_api(host = "127.0.0.1", port = 8000)
```

The service is defined in `inst/plumber/plumber.R` in the package, so it can
also be hosted with `plumber::pr()` on Posit Connect or in a Docker image
(`rstudio/plumber`).

`run_api()` accepts connections from the local computer only. Pass
`host = "0.0.0.0"` to accept connections from other machines. Before you
expose the service, put it behind a reverse proxy that provides TLS and
authentication.

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
