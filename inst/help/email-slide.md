The **Email Slide** panel at the bottom of the sidebar mails you the current simulation as a PowerPoint slide.

## Using it

1. Enter a recipient email address.
2. Optionally add a comment. Tick the box confirming that the comment contains no protected health information.
3. Press **Send**.

The message carries:

- a PowerPoint file with one slide: the plot as an editable vector graphic, the title and date, and the URL that reconstructs the simulation (see [Sharing a simulation by URL](help:sharing));
- a PNG of the plot;
- an Excel workbook with the dose table and the pharmacokinetic parameters used for every drug.

A session may send at most 25 emails, to limit abuse.

## When it is not available

Sending mail needs an email account configured on the server. If the panel says "Email is not configured", the server has none. The public app is configured; a copy you run yourself will not be until you fill in `email_username` and `email_password` in `config.yml` (a Gmail address and an app password). See [What is in the repository](help:repository).

## Privacy

stanpumpR does not collect protected health information. The emailed slide contains only what is on the screen and what you type. Please do not enter patient identifiers in the comment.
