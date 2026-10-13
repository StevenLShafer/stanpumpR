# The stanpumpR simulation API

Send a dose table, the patient's covariates and how long to simulate. Get back
the predicted plasma and effect-site concentrations of every drug, as JSON,
CSV, Excel or XML.

The API has three layers. Each one works without the layers above it:

| Layer | Use it from | Entry point |
|---|---|---|
| R function | R scripts, RStudio, Shiny | `apiSimulate(request)` → `apiFormatResult(result, format)` / `apiWriteResult(result, path)` |
| HTTP service | any language (Python, MATLAB, Excel Power Query, curl, ...) | `stanpumpR::run_api()` → `POST /simulate` |
| Simulation core | R, when you need every internal series | `simulateDrugsWithCovariates()` |

The API returns the same numbers the app plots. It runs
`simulateDrugsWithCovariates()`, the Shiny-free entry point the tests and the
vignettes also use (`R/api.R`).

---

## Quick start

### From R

```r
library(stanpumpR)

result <- apiSimulate(list(
  patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
  duration = 2, timeUnit = "hours",
  doses = data.frame(
    Drug  = c("propofol", "propofol", "propofol", "remifentanil"),
    Time  = c(0,          0,          1,          0),
    Dose  = c(2,          100,        0,          0.15),
    Units = c("mg/kg",    "mcg/kg/min", "mcg/kg/min", "mcg/kg/min")
  ),
  output = list(interval = 1/60)        # one row per minute (in hours)
))

head(result$timeseries)                 # Drug, Time, Plasma, EffectSite, ConcentrationUnits
result$drugs                            # one row per drug: units, MEAC, peaks
apiWriteResult(result, "simulation.xlsx")   # or .csv / .json / .xml
```

### As an HTTP service

Start the service. It needs the `plumber` package:

```r
stanpumpR::run_api(port = 8000)          # interactive docs at http://127.0.0.1:8000/__docs__/
```

Then call it:

```bash
curl -X POST "http://127.0.0.1:8000/simulate?format=csv" \
     -H "Content-Type: application/json" \
     -d '{
           "patient":  {"weight": 70, "height": 170, "age": 50, "sex": "male"},
           "duration": 60,
           "doses": [
             {"drug": "propofol", "time": 0, "dose": 2,   "units": "mg/kg"},
             {"drug": "fentanyl", "time": 0, "dose": 100, "units": "mcg"}
           ]
         }'
```

Python:

```python
import io, requests, pandas as pd

request = {
    "patient":  {"weight": 70, "height": 170, "age": 50, "sex": "male"},
    "duration": 60,
    "doses": [{"drug": "fentanyl", "time": 0, "dose": 100, "units": "mcg"}],
    "output": {"interval": 1},
}
r = requests.post("http://127.0.0.1:8000/simulate", params={"format": "csv"}, json=request)
r.raise_for_status()
df = pd.read_csv(io.StringIO(r.text))
```

R, from another session or machine:

```r
library(httr2)
resp <- request("http://127.0.0.1:8000/simulate") |>
  req_url_query(format = "json") |>
  req_body_json(list(
    patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
    duration = 60,
    doses    = list(list(drug = "fentanyl", time = 0, dose = 100, units = "mcg"))
  )) |>
  req_perform()
result <- resp_body_json(resp, simplifyVector = TRUE)
```

---

## Endpoints

| Method | Path | Returns |
|---|---|---|
| `POST` | `/simulate` | The simulation result (see [The result](#the-result)). Query parameters: `format` (`json`, `csv`, `xlsx`, `xml`) and `layout` (`long`, `wide`). |
| `GET` | `/drugs` | The drugs the API can simulate, with each drug's category, concentration units and allowed dose units. `?format=csv` for CSV. `?allowIllicitDrugs=true` adds the opt-in illicit-drug models. |
| `GET` | `/health` | `{"status": "ok", "apiVersion": "1", "stanpumpRVersion": ...}` |
| `GET` | `/__docs__/` | Interactive (Swagger) documentation. |

**Choosing the format.** The first of these that is present wins:

1. the `?format=` query parameter
2. `output.format` in the request body
3. the `Accept` header (`application/json`, `text/csv`, `application/xml`,
   or the xlsx MIME type)

If none is given, the result is JSON.

**Errors.** A bad request gets HTTP **400** and a body of the form
`{"error": "..."}`. The message names the field and, for a table, the row,
for example ``"`doses` row 3: 'mg PO' is not a unit of propofol. Its units
are: mg, mg/kg, ..."``. A request that is valid but fails inside a drug model
gets **422**. An unexpected failure gets **500**. The detail of a 500 goes to
the server log, not to the client.

---

## The request

A JSON object, or a named list in R. Field names ignore case and any `_`,
space or `.`, so `effectSite`, `effect_site` and `EffectSite` are the same
field.

```jsonc
{
  "patient": {
    "weight": 70,            // required
    "height": 170,           // required
    "age": 50,               // required
    "sex": "male",           // required: "male" | "female"
    "pregnant": false,       // late-pregnancy kinetics for the drugs that read it (default false);
                             //   true needs sex "female"; inst/help/models/pregnancy.md
    "weightUnit": "kg",      // "kg" (default) | "lb"
    "heightUnit": "cm",      // "cm" (default) | "in"
    "ageUnit": "years",      // "years" (default) | "months"
    "adjustToFFM": true,     // scale to fat-free mass (default true); docs/weight-adjustment.md
    "creatinine": null,      // serum creatinine, mg/dL; null = assumed normal for age and sex
    "osmolality": 280,       // baseline serum osmolality, mOsm/kg (mannitol only)
    "cyp2d6": "normal",      // poor | intermediate | normal | ultrarapid
    "cyp2c19": "normal",     // poor | intermediate | normal | rapid | ultrarapid
    "cyp2c9": "*1/*1",       // *1/*1 | *1/*2 | *1/*3 | *2/*2 | *2/*3 | *3/*3
    "cyp2b6": "normal"       // poor | intermediate | normal | rapid | ultrarapid
  },

  "duration": 6,             // required: length of the simulation, in timeUnit
  "timeUnit": "hours",       // minutes (default) | hours | days | weeks
                             //   applies to duration, dose and event times, output.interval/times

  "doses": [                 // required: the dose table, one row per dose or rate change
    {"drug": "propofol", "time": 0,  "dose": 2,   "units": "mg/kg"},
    {"drug": "propofol", "time": 0,  "dose": 100, "units": "mcg/kg/min"},
    {"drug": "propofol", "time": 1,  "dose": 0,   "units": "mcg/kg/min"},
    {"drug": "fentanyl", "time": "1:30", "dose": 50, "units": "mcg"}
  ],

  "events": [                // optional: clinical events (some change PK, e.g. CPB)
    {"time": 0.5, "event": "Intubation"}
  ],

  "output": {                // optional
    "interval": 0.25,        // grid step in timeUnit, or "native" (see below)
    "times": [0, 0.5, 1, 2], // or explicit times; overrides interval
    "recovery": false,       // add a Recovery column (time until the drug's threshold, minutes)
    "format": "json",        // json | csv | xlsx | xml
    "layout": "long"         // csv/xlsx: long | wide
  },

  "options": {
    "allowIllicitDrugs": false   // the opt-in "Illicit drugs" research models (R/illicit-drugs.R)
  }
}
```

**The dose table** follows the rules of the app's dose table:

- `doses` can be an array of row objects, as above. It can also be an object
  of equal-length columns: `{"drug": [...], "time": [...], "dose": [...],
  "units": [...]}`. In R it can be a data frame with columns `Drug`, `Time`,
  `Dose` and `Units`.
- `units` must be one of that drug's units. `GET /drugs` or
  `apiDrugCatalog()` lists them. A bolus is a mass (`mg`, `mcg/kg`). An
  infusion is a rate (`mcg/kg/min`), which runs until the next rate for the
  same drug; set the rate to 0 to stop it. Oral, sublingual, intramuscular,
  intranasal and regional doses carry their route (`mg PO`, `mcg IN`,
  `mg RA`).
- `mg PO bid`-style units (`qd`/`bid`/`tid`/`qid`) repeat the dose to the
  end of the simulation. The rules are in `R/scheduled.R`. A scheduled dose
  of 0 stops the sequence.
- `Plasma target` and `Effect site target` make a target-controlled
  infusion. The dose is the target concentration, in the drug's
  concentration units. Targets are allowed only when the simulation is a week
  or shorter.
- A time is a number in `timeUnit` or an elapsed `"H:MM"`. Doses at or after
  `duration` are ignored, and a warning says so.
- Limits: at most 500 dose rows and 100 events. The duration can be up to
  365 days.
- Inhaled agents are not supported yet. They have no mass dose and use a
  separate engine (`simulateGases()`).

---

## The result

The result has the same content in every format. Every time is in
**minutes**, weight is in kg, height in cm and age in years, whatever units
the request used. A client therefore never needs to know how a request was
written in order to read its answer.

| Part | Contents |
|---|---|
| `meta` | `apiVersion`, `stanpumpRVersion`, `generated` (UTC), `timeUnit` (`"minutes"`), `duration` (minutes), `grid` (`"requested"` or `"native"`) |
| `patient` | The covariates as simulated, after unit conversion and defaults |
| `drugs` | One row per simulated drug: `Drug`, `Dosed` (false for an active metabolite that was formed but not given, e.g. desmetramadol from tramadol), `ConcentrationUnits`, `MEAC` (null if the drug has none), `PeakPlasma`, `TimePeakPlasma`, `PeakEffectSite`, `TimePeakEffectSite` |
| `doses` | The full dose sequence actually simulated: your rows, plus the infusion rates a TCI target produced and the repeats of each scheduled dose. Time is in minutes. |
| `events` | The events, with Time in minutes |
| `timeseries` | `Drug`, `Time`, `Plasma`, `EffectSite`, `ConcentrationUnits`, and `Recovery` if requested |
| `warnings` | Notes on the simulation, such as ignored doses |

`EffectSite` is null (or empty in CSV and XML) for a prodrug that has no
effect site of its own. For example, tramadol's effect appears in the
`desmetramadol` rows. For mannitol, `Plasma` is the serum osmolality in
mOsm/kg. For calcium chloride and calcium gluconate, `Plasma` is the change
in ionized calcium above the patient's baseline, in mmol/L (`ConcentrationUnits`
is `mmol/L`).

### The time grid

By default every drug is reported on one regular grid. The grid uses the
smallest of 0.1, 0.25, 0.5, 1, 2, 5, 10, 15, 30 or 60 minutes (and so on)
that keeps each drug to 1000 rows or fewer. That gives 0.1 minute for an hour
and 1 minute for up to 16 hours. Set `output.interval` or `output.times` to
choose your own times. The limit is 20,000 rows per drug.

The values on the grid are **simulated at those times, not interpolated**.
The requested times are added to every engine's time line
(`withOutputTimes()` in `R/simulationTimeGrid.R`). The closed-form solutions
are exact at any point of that line. `output.interval = "native"` returns
each drug's own adaptive time line instead. That line is dense after every
dose, but its times differ from drug to drug.

### Formats

- **json**: the whole result. Each table is an array of row objects. Missing
  values are `null`.
- **csv**: the time series only, as one table. With `layout=long` (the
  default) there is one row per drug and time. With `layout=wide` there is
  one row per time and a column per drug and site (`propofol Plasma`,
  `propofol EffectSite`, ...). `wide` needs a common grid, so it cannot be
  used with `"native"`.
- **xlsx**: a workbook with the sheets *Time Series* (long or wide),
  *Drugs*, *Doses*, *Events* (if there are any), *Patient* and *About*
  (`meta` and warnings).
- **xml**: the whole result, with one element per row and the columns as
  attributes:

  ```xml
  <stanpumpR>
    <meta apiVersion="1" timeUnit="minutes" duration="60" .../>
    <patient weight="70" weightUnit="kg" .../>
    <drugs><drug Drug="fentanyl" ConcentrationUnits="ng/mL" MEAC="0.6" .../></drugs>
    <doses><dose Drug="fentanyl" Time="0" Dose="100" Units="mcg"/></doses>
    <events/>
    <timeseries><point Drug="fentanyl" Time="0" Plasma="8.26" EffectSite="0" .../></timeseries>
    <warnings/>
  </stanpumpR>
  ```

Which format is easiest depends on the client. JSON keeps everything in one
response and every language can parse it. CSV `wide` loads straight into
Excel, pandas or `read.csv()` when you only need the curves. xlsx is the
format to hand to a person.

---

## Deployment notes

- The service is `inst/plumber/plumber.R`. It only maps HTTP onto
  `apiSimulate()` and `apiFormatResult()`, so you can also host it with
  `plumber::pr()` on Posit Connect or in a Docker image
  (`rstudio/plumber`). It is not deployed with the Shiny app.
- `run_api()` listens on `127.0.0.1` by default. Pass
  `host = "0.0.0.0"` to accept connections from other machines. Put it
  behind a reverse proxy with TLS and authentication before you expose it.
  Request bodies are limited to 1 MB.
- The API is versioned (`meta.apiVersion`, `/health`). A change that would
  break existing clients bumps the version.
- stanpumpR is for teaching and research. It is not a medical device. Its
  predictions are typical values for a population and do not replace
  clinical judgement.
