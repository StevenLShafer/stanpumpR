---
file: NJ_fda-stic_web_2026-10-12.pdf
pmid: none
doi: none
citation: "U.S. FDA. Antibacterial Susceptibility Test Interpretive Criteria (STIC) web page, https://www.fda.gov/drugs/development-resources/antibacterial-susceptibility-test-interpretive-criteria, printed 10/12/2026 (11 pages)."
kind: other
drugs: [clindamycin, gentamicin]
source_read: text layer, all 11 pages
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# FDA STIC web page (printed 2026-10-12): recognised breakpoint standards

## Study
Regulatory web listing, not a study. States that, unless a drug's row lists exceptions or additions, FDA fully
recognises CLSI M100, 36th ed., 2026 (plus M45 3rd ed. 2015, M24 3rd ed. 2018, M24S 2nd ed. 2023, M43-A 2011).
Recognition of STIC does not extend to epidemiological cutoff values (ECVs); it does include surrogate tests within
the recognised standards. A table lists each antibacterial, route, whether its STIC are in the recognised CLSI
standard, and whether FDA has exceptions or additions (linked to separate drug pages).

## Model and parameters
No breakpoint values appear on this page. Relevant rows:

| Drug | Route | STIC in recognised CLSI standard | Exceptions or additions |
|---|---|---|---|
| Clindamycin | Injection, Oral | Yes | Yes (linked page) |
| Gentamicin | Injection | Yes | Yes (linked page) |

```yaml
parameters: []
```

## Pharmacodynamics and end points
None (no MIC values on the page). Not an opioid document.

## Covariates
None.

## Caveats
The linked exception pages for clindamycin and gentamicin (where FDA can add to or depart from CLSI M100) were not
printed, so whether FDA modifies the staphylococcal clindamycin or the Enterobacterales gentamicin breakpoints cannot
be told from this archive copy. The page gives no history (when a breakpoint changed or was recognised).

## Used by stanpumpR
R/drugs_clindamycin.R:42-43, R/antibioticThresholds.R:75-77 (clindamycin, "CLSI M100 / FDA STIC, Staphylococcus spp.
S <= 0.5, I 1-2, R >= 4 mg/L"); R/drugs_gentamicin.R:54-56, R/antibioticThresholds.R:78-80 (gentamicin
Enterobacterales S <= 2 mg/L, "CLSI M100 (from 2023; FDA-recognised)").

## Verification against stanpumpR
- That FDA recognises CLSI M100 STIC for clindamycin and gentamicin: **matches** (both rows "Yes"; M100 36th ed. 2026).
- The numeric breakpoints (clindamycin staphylococci S <= 0.5, I 1-2, R >= 4; gentamicin Enterobacterales S <= 2):
  **cannot verify** from this document; they are in CLSI M100, which is not archived. Both drugs carry FDA
  "exceptions or additions" pages that were not captured; these should be checked before calling the values
  "FDA-recognised".
- "from 2023, lowered from 4" (R/drugs_gentamicin.R:54-55; antibioticThresholds.R:79): **cannot verify** here; the page
  carries no dates of change.
