Debug mode shows a log of what the program is doing and a profiler of how long each step takes. It is off by default in production and on in the test deployments of pull requests.

## Turning it on

Add `?debug=1` to the URL (or `&debug=1` if the URL already has a query string) and reload. A **Debug Section** accordion appears beneath the plot.

| Value | Meaning |
|---|---|
| `debug=0` | off |
| `debug=1` | normal: the main steps and the tables |
| `debug=2` | verbose: every reactive, including the hover handler |

The level can be changed from the **Debug level** selector in the panel.

The level stays in the address bar, ahead of the saved simulation, so reloading the page keeps it; choosing **Off** takes it out. The link sent with an emailed slide never carries it.

## The log

The left column is a running log written by `outputComments()` calls throughout the server. It shows the dose table after cleaning, the pharmacokinetic parameters calculated for each drug, the events applied, and the state restored from a URL. It is the first place to look when a simulation does not do what you expected.

## The profiler

The right column lists reactives that took longer than a threshold (100 ms by default) with their timing. Use it to see which step a slow simulation is spending its time in. The time-until-threshold lines and the gas engine's washout calculations are the expensive ones.

## For developers

`outputComments()` is the logging function; it writes to the panel when called inside a Shiny session with debug on, and to the console when `ECHO_OUTPUT_COMMENTS` is set. `profileCode()` wraps a reactive for the profiler. See [What is in the repository](help:repository).
