# stanpumpR in the bio.tools registry

`stanpumpr.biotools.json` is stanpumpR's entry for [bio.tools](https://bio.tools), the ELIXIR registry of life-science software. It is written in [biotoolsSchema](https://biotools.readthedocs.io/en/latest/api_usage_guide.html), with EDAM terms taken from entries already in the registry. Keep it in step with the README when the library or the routes change, and resubmit it.

## Registering or updating the entry

You need a bio.tools account (bio.tools, **Sign up**). The tool ID `stanpumpr` was free on 2026-11-06.

**By the web form.** Log in, choose **Add content**, and open the JSON view of the new entry. Paste this file, check it and **Save**.

**By the API.**
1. Get a token:
   ```
   POST https://bio.tools/api/rest-auth/login/   {"username": ..., "password": ...}
   ```
2. Check the entry before you send it, with `Authorization: Token <key>`:
   ```
   POST https://bio.tools/api/tool/validate/
   ```
3. Send the entry:
   - a first registration is `POST https://bio.tools/api/tool/`;
   - a later update is `PUT https://bio.tools/api/tool/stanpumpr/`.
