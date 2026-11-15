The URL in your browser's address bar is the simulation. Every change you make is encoded into it as you go, so copying the URL at any moment captures the patient, the doses, the events, the graph options and any recovery thresholds you have edited.

## To save or share a simulation

Copy the address bar and paste it into an email, a document, or a teaching handout. Whoever opens it sees the same patient, doses, events, options, thresholds and Drug Library edits you had. The URL is long, because it carries the whole state. A URL that carries doses opens straight onto the simulation, without the startup drug menu. There is a limit: a session with a great many drugs in its dose table can make a URL longer than the server accepts (about 8,000 characters), and such a link would not reopen. The app then says so and leaves the address bar at the plain app address; simulate fewer drugs to get a link again.

A link is plain text that anyone can edit, so the app reads its dose and event tables as it reads what you type: an entry it cannot read (an unknown drug, a unit the drug does not offer, an unreadable time or dose, an unknown event) is left out, and the app says so. Check the tables before relying on a link that did not come from you.

The URL also carries the patient's covariates and regimen. The app does not send it to other sites when you follow a link out of it, but anyone you give the URL to can read it, as can anything that records your browser history.

## What is included

- Patient covariates and their units
- The applied dose table (not an unapplied draft)
- The event table
- Graph options and additional plots
- Edited recovery thresholds, including the MAC threshold
- Edits to the Drug Library (concentration units, units, colours, ranges and MEAC), only the rows you changed. Since 2026-11-15: a link made before then opened in the standard library, so a drug switched to ng/mL had its targets and measured levels, typed in ng/mL, read a thousandfold high. A link's edits are held to the Drug Library dialog's own rules, drug by drug: a drug whose edit breaks them opens with its standard entry and the app says so, and that drug's targets, thresholds and measured concentrations, saved in the link's units, are converted to the standard units (since 2026-11-15: until then one such drug dropped every drug's edits, and a target saved as 3000 ng/mL was read as 3000 mcg/mL).
- The time units, time display mode and procedure start

## What is not included

- Which help page you were reading
- Anything typed into the Email Slide panel

## The URL in an emailed slide

The email that carries the PowerPoint slide includes the same URL as a link, so a simulation can be reopened months later and reconstructed. See [Email a slide](help:email-slide).

## Linking to a scenario or a help page

The URL above carries a whole simulation. Two shorter links open the app on something it already knows:

| Link | Opens |
|---|---|
| `https://stanpumpr.io/?scenario=propofol-bolus` | the simulator with that [teaching scenario](help:scenarios/index) loaded, as its **Load** button does; the startup drug menu is skipped |
| `https://stanpumpr.io/?help=models/effect-site` | the Help tab on that page, with an empty simulator behind it |
| `https://stanpumpr.io/?help=scenarios/propofol-bolus` | a scenario's help page, with its Load button |
| `https://stanpumpr.io/?scenario=propofol-bolus&help=scenarios/propofol-bolus` | both: the scenario loaded, and the help open on its page |

The scenario id is the last part of the scenario's page address in this help, and every scenario page shows its own link under the Load button. A help page's id is its file name: `tci`, `models/effect-site`, `drugs/propofol`. A copy of the app served elsewhere takes the same parameters after its own address.

Once the link has opened, the address bar is rewritten into the ordinary long URL, so a reload keeps what was loaded, and your edits are bookmarked as usual.

Put a run of these links on the slides of a lecture, each opening in a new browser tab, and the talk steps from a slide to a live simulation and back without touching the dose table.

## Teaching

Every [teaching scenario](help:scenarios/index) is a simulation you can load, modify, and then share by URL. Build a worked example, copy its URL into your lecture notes, and students open the exact state.
