# Privacy and PHI assessment

This is an engineering assessment, not a legal determination that a deployment is HIPAA compliant.

## Structured simulation data

stanpumpR does not request a patient name, medical-record number, date of birth, address,
telephone number, encounter number, account number, or patient email address. It processes age,
sex, height, weight, pregnancy/renal/pharmacogenomic selections, medication regimens, clinical
events, and simulated concentrations. These are health-related data but are not direct identifiers
by themselves. Ages entered as 90 or older are visibly changed to 89 before calculation, export,
and bookmark persistence; the exact entered age is excluded from saved state.

## Paths by which PHI can still enter

- **Comments:** The optional email comment is free text. Its placeholder says not to enter PHI,
  but a user can still type a name, MRN, date, or other identifier.
- **Recipient:** The address is used transiently for SMTP and excluded from bookmarks and logs. If
  it is a patient's address, the mail system links that identifier to the simulation and attachments.
- **External correlation:** An unusual combination of demographics, regimen, procedure timing,
  user identity, IP address, and outside knowledge could identify an individual.

The strongest no-PHI boundary would require removing free-text comments and prohibiting delivery
to patient addresses. The current design instead warns users, minimizes persistence, and assumes
deployment and mail systems are operated with safeguards appropriate for possible PHI.

## Storage and transmission

- Bookmarks (URL by default, or opaque server-stored IDs where a host supports them) exclude
  recipient, comments, exact age, and transient UI/debug fields; ages 90+ are normalized to 89
  before persistence. Dose, event, and normalized simulation data remain in saved state. Because
  the excluded and normalized fields never enter saved state, the URL contains no confidential
  data. Restored URL state is decoded as JSON (no `unserialize`/`eval`) and re-validated
  server-side.
- Local bookmark directories are excluded from Git, package builds, and deployment bundles.
- Email exports use a private temporary directory and are deleted on success or failure. The mail
  provider may retain sent messages and attachments under its own policies.
- Email uses authenticated SMTP with mandatory TLS when enabled. Transport failures do not expose
  or log SMTP exception details.
- PPTX, XLSX, PNG, UI, subject, and body identify the output as a simulation, not a patient record.

## Operational requirements

Connect Cloud visibility/authentication, bookmark retention, mail-provider agreements, audit-log
access, incident response, and organizational policy remain deployment responsibilities. Never
log simulation tables, bookmark tokens, recipient addresses, comments, credentials, or message
contents in production.
