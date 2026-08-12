# Security deployment baseline

stanpumpR handles patient covariates and medication regimens. A clinical deployment must
therefore treat all Shiny inputs and saved-state links as untrusted and potentially sensitive.

## Secure defaults

- `bookmark_mode: server` stores simulation state on the server and places only an opaque ID
  in the URL. Use `disable` where sharing is unnecessary. Do not use `url` for clinical data.
- `allow_url_debug: false` prevents users from enabling diagnostic output through the URL.
- `email_enabled: false` removes the outbound-email interface. If email is institutionally
  approved, configure `email_allowed_domains` and an approved SMTP relay before enabling it.
- Server-side validators cap table sizes and reject unknown drugs, routes, events, non-finite
  doses, overlong text, and forged plot dimensions.

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
Supply SMTP credentials at deployment time through the platform's secret manager; do not commit
them or leave them in a developer `config.yml`. If a credential has ever appeared in plaintext,
revoke it before enabling email.

## CI/CD

GitHub Actions are pinned to immutable commits. Production installation is pinned to the
workflow's triggering commit. Fork preview deployments use separate `PR_SHINY_*` credentials;
those credentials must belong to an isolated preview account with no production access.
