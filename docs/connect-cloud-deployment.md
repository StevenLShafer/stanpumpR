# Posit Connect Cloud deployment

Connect Cloud publishes this Shiny application directly from GitHub. It does not use
`renv.lock` to build the deployed R library; a committed `manifest.json` is required.

## Before connecting the repository

1. Commit and push the application changes to the intended production branch.
2. Install that exact public GitHub revision locally so the manifest records `stanpumpR` as a
   GitHub dependency rather than an unavailable local package:

   ```r
   remotes::install_github("StevenLShafer/stanpumpR@<commit-sha>", upgrade = "never")
   ```

3. Run the complete test suite, then generate the Connect Cloud manifest from the repository root:

   ```r
   source("tools/write-connect-manifest.R")
   ```

4. Confirm that the `stanpumpR` entry in `manifest.json` identifies the expected GitHub commit.
   Review and commit the manifest. Regenerate it whenever `DESCRIPTION`, `renv.lock`, or
   an application dependency changes.
5. In Connect Cloud, select `app.R` as the primary file and the production branch as the
   publishing branch.

The app starts with secure built-in defaults when the ignored local `config.yml` is absent.

## Connect Cloud variables

Configure these encrypted variables on the content item:

- `STANPUMPR_EMAIL_USERNAME`: Gmail/Google Workspace sender address.
- `STANPUMPR_EMAIL_PASSWORD`: Google app-specific password.
- `STANPUMPR_EMAIL_ENABLED`: `true` to enable sending.

Optional non-secret overrides are `STANPUMPR_EMAIL_SMTP_HOST`,
`STANPUMPR_EMAIL_SMTP_PORT`, and `STANPUMPR_EMAIL_SMTP_SSL`. Gmail defaults are
`smtp.gmail.com`, `587`, and `true`; port 587 uses mandatory STARTTLS.

Never place variable values in GitHub, `config.yml`, `.Renviron`, `manifest.json`, or build logs.

## Content settings

- Use the production branch and enable automatic publish on push only after a successful manual
  deployment.
- Configure the custom domain in Connect Cloud; TLS certificates are managed by the platform.
- Keep production debug disabled and URL debug activation disabled.
- Use server-side bookmarks. Connect Cloud supports Shiny bookmarkable state, but bookmark
  retention and access should be verified as part of the production acceptance test.
- Review content visibility and authentication before making the application public.

## Acceptance test

After each deployment, verify app startup, age normalization, dose/event validation, bookmark
restore, export generation, email delivery, temporary-file cleanup, and absence of credentials
or recipient addresses in application logs.
