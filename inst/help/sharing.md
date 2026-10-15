The URL in your browser's address bar is the simulation. Every change you make is encoded into it as you go, so copying the URL at any moment captures the patient, the doses, the events, the graph options and any recovery thresholds you have edited.

## To save or share a simulation

Copy the address bar and paste it into an email, a document, or a teaching handout. Whoever opens it sees the same patient, doses, events, options and thresholds you had; edits to the rest of the Drug Library (concentrations, units, colours, ranges) are not carried, as listed below. The URL is long, because it carries the whole state. A URL that carries doses opens straight onto the simulation, without the startup drug menu.

## What is included

- Patient covariates and their units
- The applied dose table (not an unapplied draft)
- The event table
- Graph options and additional plots
- Edited recovery thresholds, including the MAC threshold
- The time units, time display mode and procedure start

## What is not included

- Edits to the Drug Library other than the thresholds
- Which help page you were reading
- Anything typed into the Email Slide panel

## The URL in an emailed slide

The email that carries the PowerPoint slide includes the same URL as a link, so a simulation can be reopened months later and reconstructed. See [Email a slide](help:email-slide).

## Linking to a scenario or a help page

The URL above carries a whole simulation. Two shorter links open the app on something it already knows:

| Link | Opens |
|---|---|
| `https://stanpumpr.io/?scenario=propofol-bolus` | the simulator with that [teaching scenario](help:scenarios/index) loaded, as its **Load** button does; the startup drug menu is skipped |
| `https://stanpumpr.io/?help=models/effect-site` | the Help tab on that page, with the default drugs in the simulator behind it |
| `https://stanpumpr.io/?help=scenarios/propofol-bolus` | a scenario's help page, with its Load button |
| `https://stanpumpr.io/?scenario=propofol-bolus&help=scenarios/propofol-bolus` | both: the scenario loaded, and the help open on its page |

The scenario id is the last part of the scenario's page address in this help, and every scenario page shows its own link under the Load button. A help page's id is its file name: `tci`, `models/effect-site`, `drugs/propofol`. A copy of the app served elsewhere takes the same parameters after its own address.

Once the link has opened, the address bar is rewritten into the ordinary long URL, so a reload keeps what was loaded, and your edits are bookmarked as usual.

Put a run of these links on the slides of a lecture, each opening in a new browser tab, and the talk steps from a slide to a live simulation and back without touching the dose table.

## Teaching

Every [teaching scenario](help:scenarios/index) is a simulation you can load, modify, and then share by URL. Build a worked example, copy its URL into your lecture notes, and students open the exact state.
