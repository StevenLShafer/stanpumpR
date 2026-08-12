# Security hardening implementation record

This record consolidates the work on `codex/security-hardening`. Operating requirements are in
`security-deployment.md`; privacy boundaries are in `privacy-and-phi.md`; Connect Cloud procedures
are in `connect-cloud-deployment.md`.

## Implemented controls

- Added server validation for covariates, dose/event/target tables, drugs, routes, units, finite
  values, row counts, text lengths, simulation duration, plot dimensions, and restored bookmarks.
- Made opaque server bookmarks the secure default and restricted production/URL debugging.
- Replaced vulnerable Handsontable 6.2.2 assets with 10.0.0 and documented licensing.
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
- The complete updated `devtools::test()` suite and live Connect Cloud acceptance test remain
  required before release. Generate and commit `manifest.json` from the exact GitHub revision.

## Residual risks and decisions

- Free-text comments can contain PHI despite the warning.
- A patient recipient address can link identity to simulation data in the external mail system.
- Bookmark retention/access depend on Connect Cloud configuration.
- Per-session rate limiting requires platform-level reinforcement.
- CSP permits inline script/style execution required by the current Shiny/htmlwidgets stack.
- Code hardening alone cannot establish HIPAA compliance; contracts, access controls, retention,
  monitoring, incident response, and institutional policy are also required.
