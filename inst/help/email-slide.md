The **Email Slide** panel at the bottom of the sidebar mails you the current simulation as a PowerPoint slide.

## Using it

1. Enter a recipient email address.
2. Optionally add a comment. Tick the box confirming that the comment contains no protected health information.
3. Press **Send**.

The message carries:

- a PowerPoint file: the whole plot on the first slide as an editable vector graphic, with the title and the date, then one slide per panel (each drug, a TCI pump rate, the % MEAC sum, the interaction), so that a simulation with many drugs is still readable;
- a PNG of the plot;
- an Excel workbook with the patient's covariates, the dose table (with any TCI rates and scheduled repeats merged in), the simulated concentrations and the pharmacokinetic parameters used for every drug.

The body of the email describes the patient and links to the URL that reconstructs the simulation (see [Sharing a simulation by URL](help:sharing)).

Every time in the workbook is in minutes. When the plot is shown in hours, days or weeks (**Time units**, in the Time panel), each time column is followed by the same times in that unit, for example `Time (days)`, and the Covariates sheet and the email say which unit the plot was in and how long it ran.

A session may send at most 25 emails, to limit abuse.

## When it is not available

Sending mail needs an email account configured on the server. If the panel says "Email is not configured", the server has none. The public app is configured; a copy you run yourself will not be until you fill in `email_username` and `email_password` in `config.yml` (a Gmail address and an app password). See [What is in the repository](help:repository).

## Privacy

Sending the slide does not require a patient's name or record identifier. But it sends more than "what is on the screen": the email body links to a bookmark URL that reconstructs the whole patient and dose-table state, and the attached Excel workbook includes the patient's covariates, the full dose table, the simulated concentrations and the pharmacokinetic parameters used for every drug — all of it, not only what is visible in the current view. That content, and the comment you type, travel through your email provider and the recipient's inbox like any other message. Please do not enter patient identifiers in the comment, and treat the email itself as carrying patient-specific information subject to your own institution's email and privacy policies.
