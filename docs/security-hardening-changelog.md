# Security hardening implementation record

This record consolidates the work on `codex/security-hardening`. Operating requirements are in
`security-deployment.md`; privacy boundaries are in `privacy-and-phi.md`; Connect Cloud procedures
are in `connect-cloud-deployment.md`.

## Implemented controls

- Added server validation for covariates, dose/event/target tables, drugs, routes, units, finite
  values, row counts, text lengths, simulation duration, plot dimensions, and restored bookmarks.
- Defaulted to URL bookmarking (works on hosts without disk-backed state, e.g. shinyapps.io);
  kept confidential data out of the URL (recipient/comments/exact age excluded, ages 90+
  normalized) and re-validated restored state server-side. Shiny decodes URL state as JSON via
  `safeFromJSON` (no `unserialize`/`eval`), and `validateDoseTableInput` gates the sole
  `eval(call(drug,...))` sink, so a crafted link cannot inject code. `server` remains available
  for hosts with state storage (Posit Connect Cloud). Restricted production/URL debugging.
- Handsontable: the app uses the MIT-licensed 6.2.2 build bundled with `rhandsontable` (an earlier
  10.0.0 override was reverted for licensing reasons — 7.0+ requires a commercial license). 6.2.2's
  client-side XSS exposure (< 8.2.0; CVE-2021-23446) is mitigated by the `hookSanitize` /
  `hookFilterKeys` grid hooks and the server-side validators, not by a version bump.
- Added defense-in-depth CSP and documented required hosting-layer security headers.
- Pinned GitHub Actions, pinned production installation, and isolated preview credentials.
- Disabled email by default and retained a server-side send guard and per-session limit.
- Validated recipient syntax/length and rejected header injection while allowing any valid domain.
- Read SMTP credentials only from `STANPUMPR_EMAIL_*` environment variables.
- Replaced Java-dependent `mailR`/`rJava` with curl SMTP, MIME attachments, and forced TLS.
- Generated exports in private temporary directories with guaranteed cleanup; escaped HTML and
  bookmark attributes; concealed transport errors; and avoided recipient logging.
- Visibly normalized ages 90+ to 89 before calculation, export, and persistence.
- Excluded recipient, comments, exact age, debug controls, and transient UI state from bookmarks.
- Added the PHI warning and labeled the UI and exports as simulations, not patient records.
- Excluded credentials, local configuration, and bookmark data from Git/build/deployment bundles.
- Allowed startup without `config.yml`, made the legacy production workflow manual-only, and added
  Connect Cloud variables, manifest tooling, instructions, and an acceptance checklist.

## Verification record

- Commit `93f902a` passed 102 tests and workflow YAML parsing.
- Later tests cover age normalization, valid unrestricted recipients, HTML escaping, MIME
  attachments, header injection, and sanitized failures.
- Changed R/test files parse, `renv.lock` is valid JSON, dependency declarations agree, and
  `git diff --check` passes.
- The full `devtools::test()` suite passes on the working tree (77 tests, 0 failures).
- The Handsontable 10.0.0 override was subsequently reverted: the app now serves the 6.2.2 build
  bundled with `rhandsontable`, and the override code (`secureRHandsontable()`,
  `handsontablePatchedDependency()`), the vendored `inst/www/handsontable-10.0.0/` assets, the
  `handsontable_license_key` config, and the `test-shiny-utils.R` regression guard were removed.
  Residual 6.2.2 XSS risk is mitigated by the retained grid hooks and server-side validators.
- The live Connect Cloud acceptance test remains required before release. Generate and commit
  `manifest.json` from the exact GitHub revision.

## Residual risks and decisions

- Free-text comments can contain PHI despite the warning.
- A patient recipient address can link identity to simulation data in the external mail system.
- Bookmark retention/access depend on Connect Cloud configuration.
- Per-session rate limiting requires platform-level reinforcement.
- CSP permits inline script/style execution required by the current Shiny/htmlwidgets stack.
- Code hardening alone cannot establish HIPAA compliance; contracts, access controls, retention,
  monitoring, incident response, and institutional policy are also required.
