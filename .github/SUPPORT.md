# Getting help with stanpumpR

stanpumpR is a research and teaching tool. Its curves are what published models
predict for a typical patient; it is not clinical decision support.

- **How to use the app, or what a model assumes:** start with the **Help** tab in the
  app. Every drug has its own page giving the source of its model, its parameters at
  reference patients, and its limits.
- **A question about a model or its source:** contact Steven Shafer
  (steven.shafer@stanford.edu).
- **Something that looks wrong:** [open an
  issue](https://github.com/StevenLShafer/stanpumpR/issues/new). First [search the
  existing issues](https://github.com/StevenLShafer/stanpumpR/issues?q=is%3Aissue),
  open and closed, in case it has been reported or fixed.

## What makes a report useful

The address bar of the app *is* the simulation: it encodes the patient, the doses, the
events and the graph options. Copy it and paste it into the issue, and we can see
exactly what you saw. Then say what you expected and what appeared instead.

A GitHub issue is public. The link carries only what was entered in the app (the patient's
covariates, such as age, weight and sex, and the doses), but do not add anything that
identifies a patient.
