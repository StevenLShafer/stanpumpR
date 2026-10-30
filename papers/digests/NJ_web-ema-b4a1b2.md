---
file: NJ_web-ema-b4a1b2.pdf
pmid: null
doi: null
citation: "European Medicines Agency, Human Medicines Evaluation Division. List of nationally authorised medicinal products. Active substance: nadroparin. Procedure no. PSUSA/00002104/201803. EMA/34222/2019, dated 12 December 2018; archived 2026-10-21."
kind: other
drugs: []
source_read: "text layer, all 30 PDF pages (cover p1, product table p2-30); PDF p2 checked on a rendered image to confirm the table columns, which the text layer interleaves"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# EMA 2018: list of nationally authorised nadroparin products (PSUSA/00002104/201803)

Nadroparin is not a stanpumpR drug (registry-only), so `drugs` is empty.

## Study
Not a study and not a label: an EMA administrative list attached to a periodic safety update single assessment (PSUSA).
Cover page: EMA/34222/2019, 12 December 2018, active substance nadroparin (PDF p1). The document is what its URL says.

The table (PDF p2-30) has five columns: product name in the authorisation country, MRP/DCP number (mostly "not
available"), national authorisation number, marketing authorisation holder (MAH), and member state. Products are
nadroparin calcium solutions for injection labelled in anti-Xa IU per volume, under the names Fraxiparine / Fraxiparina,
Fraxiparine Forte, Fraxodi, Seleparina, Seledie and "Nadroparin calcium Aspen", in strengths from 2,850 IU/0.3 mL to
19,000 IU/mL (e.g. Fraxiparine 7 600 U.I. Axa/0,8 ml for S.C. injection, FR; PDF p2). MAHs seen: Aspen Pharma Trading
Limited (most rows) and Italfarmaco S.p.A. (Seleparina / Seledie, Italy). Member states in the table include BE, BG, CY,
CZ, DE, EE, ES, FR, GR, HR, HU, IE, IT, LT, LU, LV, NL, PL, PT, RO, SI, SK and AT (text-layer tally, approximate).

## Model and parameters
None. The document contains no pharmacokinetic, pharmacodynamic or dosing information.

```yaml
parameters: []  # administrative product list; no PK or PD values
```

## Pharmacodynamics and end points
None. No anti-Xa targets, no dosing, no MEAC (not an opioid).

## Covariates
None.

## Caveats
- Establishes only that nadroparin calcium products were nationally authorised in many EU member states in 2018 and
  that they are dosed in anti-Xa IU for subcutaneous injection. It is not the product information (SmPC), which would
  carry the PK.
- The list is from 2018; current authorisations may differ.

## Used by stanpumpR
- `inst/extdata/anticoagulantRegistry.csv:23` (from `data-raw/anticoagulantRegistry.R:151-155`): row `nadroparin`,
  LMWH, SC, anti-Xa activity (IU/mL), LMWH anti-Xa, `pending`, no drug; note "No drug-specific dynamic fit; do not
  alias another LMWH."

## Verification against stanpumpR
- Class LMWH, route SC, potency in anti-Xa IU: **matches** (product names and strengths on PDF p2-30).
- "No drug-specific dynamic fit": **cannot verify** from this document, which has no PK at all; it supports the
  existence and EU jurisdiction of nadroparin, nothing about the availability of a model.
- "Do not alias another LMWH": a project policy, not checkable here.
</content>
</invoke>
