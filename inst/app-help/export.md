The **Export** panel at the bottom of the sidebar saves the current simulation as files: each one downloaded on its own, or all three emailed. Until November 2026 it was the **Email Slide** panel, and email was the only way to get the files.

## The files

- **PowerPoint (.pptx)**: the whole plot on the first slide as an editable vector graphic, with the title and the date, then one slide per panel (each drug, a TCI pump rate, the % MEAC sum, the interaction), so that a simulation with many drugs is still readable.
- **Spreadsheet (.xlsx)**: an Excel workbook with the patient's covariates, the dose table (with any TCI rates and scheduled repeats merged in), the simulated concentrations, the results as plotted, and the pharmacokinetic parameters used for every drug.
- **Image (.png)**: the whole plot as the app draws it, at twice its size on the screen in pixels, so that it stays sharp in a document or on a slide.

Every time in the workbook is in minutes. When the plot is shown in hours, days or weeks (**Time units**, in the Time panel), each time column is followed by the same times in that unit, for example `Time (days)`, and the Covariates sheet (and the email) say which unit the plot was in and how long it ran.

## Downloading

Press **PowerPoint**, **Spreadsheet** or **Image**. Each downloads one file, named by the date and time it was made (for example `stanpumpR.261115-142233.pptx`; the server's clock, which may not be in your time zone). The PowerPoint takes a few seconds with many drugs, since it draws every panel. With no plot to export (no dose entered, or a patient field the simulation cannot read) the app says why, and your browser lists the download as failed.

A download needs nothing configured on the server, so it works in a copy you run yourself.

## Emailing

1. Enter a recipient email address.
2. Optionally add a comment. Tick the box confirming that the comment contains no protected health information.
3. Press **Send**.

The message carries all three files. Its body describes the patient and links to the URL that reconstructs the simulation (see [Sharing a simulation by URL](help:sharing)), which a downloaded file does not.

A session may send at most 25 emails, to limit abuse. The server as a whole sends at most 30 an hour, and at most 5 an hour to any one address; past either limit it asks you to try again later. Downloads are not counted.

### When email is not available

Sending mail needs an email account configured on the server. If the panel says "Email is not configured", the server has none; the downloads still work. The public app is configured; a copy you run yourself will not be until you fill in `email_username` and `email_password` in `config.yml` (a Gmail address and an app password). See [What is in the repository](help:repository).

## Privacy

Exporting does not require a patient's name or record identifier. But the files hold more than "what is on the screen": the workbook includes the patient's covariates, the full dose table, the simulated concentrations and the pharmacokinetic parameters used for every drug — all of it, not only what is visible in the current view — and an email's body links to a bookmark URL that reconstructs the whole patient and dose-table state. A downloaded file is yours to keep or pass on; an email, and the comment you type, travel through your email provider and the recipient's inbox like any other message. Please do not enter patient identifiers in the comment, and treat the files and the email as carrying patient-specific information subject to your own institution's privacy policies.
