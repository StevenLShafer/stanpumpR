# Security deployment baseline

stanpumpR handles patient covariates and medication regimens. A clinical deployment must
therefore treat all Shiny inputs and saved-state links as untrusted and potentially sensitive.

## Secure defaults

- `bookmark_mode: url` (the default) encodes saved-simulation state directly in a shareable
  link and works on hosts without server-side state storage (e.g. shinyapps.io, which errors
  with "server is not configured for saving sessions to disk" under `server`). The URL carries no
  confidential data: recipient, comments, and exact age are excluded from bookmarks and ages
  90+ are normalized to 89 before persistence. Restored URL state is decoded as JSON by Shiny
  (no `unserialize`/`eval`) and re-validated by the server-side validators below, so a crafted
  link cannot inject code or unknown drugs. Use `server` (opaque server-stored IDs) only on a
  host that supports disk-backed bookmarks such as Posit Connect Cloud; use `disable` where
  saved-state sharing is unnecessary.
- `allow_url_debug: false` prevents users from enabling diagnostic output through the URL.
- `email_enabled: false` disables sending while leaving the email panel visible with a deployment
  status notice and PHI warning. If email is enabled, use an approved SMTP relay. Recipient
  addresses are validated but are not restricted by domain, logged, or stored in bookmarks.
- Server-side validators cap table sizes and reject unknown drugs, routes, events, non-finite
  doses, overlong text, and forged plot dimensions.
- Ages of 90 years or older are visibly normalized to 89 before simulation, export, or bookmark
  persistence. Email comments warn users not to enter PHI. The email panel remains visible when
  sending is disabled so that its privacy guidance and deployment status are apparent.

## Required reverse-proxy controls

The hosting or reverse-proxy layer must provide authentication, authorization, TLS, idle and
absolute session expiry, request/body limits, websocket limits, and audit logging. It must also
send these response headers; a HTML `<meta>` element is not sufficient for all directives:

```
Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self' data:; connect-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Content-Type-Options: nosniff
Referrer-Policy: no-referrer
Permissions-Policy: camera=(), microphone=(), geolocation=()
```

The inline-script exceptions remain necessary for the current Shiny/htmlwidgets stack. They
make strict input validation and the updated Handsontable dependency security-critical.

## Handsontable licensing

stanpumpR overrides the vulnerable Handsontable 6.2.2 embedded by `rhandsontable` with version
10.0.0. The upstream license files are shipped in `inst/www/handsontable-10.0.0/`. Hospitals
and other commercial users must obtain and configure an appropriate `handsontable_license_key`;
the default `non-commercial-and-evaluation` key must not be used where its terms do not apply.

## Email

Generated PPTX, XLSX, and PNG files are written to a private, unique temporary directory and
deleted with `on.exit()` whether sending succeeds or fails. A hospital deployment should still
prefer its approved internal mail relay and enforce identity-aware rate limiting outside Shiny.
Recipient addresses are used transiently for delivery and are not included in bookmarks or
application debug logs. Exported artifacts are labeled as simulations, not patient records.
Generated local bookmark directories are excluded from Git, package builds, and deployment
bundles. SMTP exception details are neither logged nor displayed to users because transport
exceptions can include account or infrastructure information.
Supply `STANPUMPR_EMAIL_USERNAME` and `STANPUMPR_EMAIL_PASSWORD` through the process environment
or the hosting platform's secret manager. The application deliberately does not read these
credentials from `config.yml`. For local development, put them in the user-level `~/.Renviron`
rather than a project file. Do not commit them or print them in logs. If a credential has ever
appeared in source control or a deployment bundle, revoke it before enabling email.

Posit Connect Cloud supplies these values as encrypted content variables. Do not work around a
hosting limitation by generating a plaintext `config.yml`, `.Renviron`, or other secret file in
a deployment bundle.

## Privacy boundary

See `privacy-and-phi.md` for the field inventory, PHI entry paths, email implications, bookmark
considerations, and the limits of what application code alone can claim about HIPAA compliance.

## CI/CD

GitHub Actions are pinned to immutable commits. Production installation is pinned to the
workflow's triggering commit. Fork preview deployments use separate `PR_SHINY_*` credentials;
those credentials must belong to an isolated preview account with no production access.

See `security-hardening-changelog.md` for the complete implementation and verification record.
