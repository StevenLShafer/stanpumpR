---
file: NJ_fda-gabapentinoids-2019_web_2026-10-12.pdf
pmid: none
doi: none
citation: "US Food and Drug Administration. Neurontin, Gralise, Horizant (gabapentin) and Lyrica, Lyrica CR (pregabalin): Drug Safety Communication - Serious Breathing Problems. Posted 12/19/2019. Web page printed 2026-10-12. https://www.fda.gov/safety/medical-product-safety-information/neurontin-gralise-horizant-gabapentin-and-lyrica-lyrica-cr-pregabalin-drug-safety-communication"
kind: guideline
drugs: [pregabalin, gabapentin]
source_read: text layer (2 pages, read in full)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
revised: 2026-10-14 (parameters block added)
---

# FDA 2019: gabapentinoids and serious breathing problems

## Study
Not a study: a two-page FDA MedWatch summary of the 19 December 2019 Drug Safety Communication on gabapentin
and pregabalin. Audience: patients, health professionals, pharmacy. It links to the full communication,
which is not in this file.

## Model and parameters
None. No doses, concentrations or PK.

```yaml
parameters: []   # a safety communication: no doses, concentrations or PK
```

## Pharmacodynamics and end points
- Issue: serious breathing difficulty may occur in patients with respiratory risk factors: opioids and other
  CNS depressants, conditions that reduce lung function such as COPD; the elderly are at higher risk.
- Actions: new respiratory-depression warnings required in the gabapentinoid labels; manufacturers required to
  run trials of abuse potential, particularly with opioids.
- Advice: start gabapentinoids at the lowest dose and monitor for respiratory depression and sedation when
  co-prescribed with an opioid or other CNS depressant (e.g. a benzodiazepine). Symptoms listed: confusion,
  dizziness, extreme sleepiness, slowed or shallow breathing, unresponsiveness, bluish skin.
- No incidence, case counts or concentration thresholds on this page. Not an opioid paper; no MEAC/ke0.

## Covariates
Risk groups only: opioid/CNS-depressant co-use, lung disease (COPD), older age.

## Caveats
Printed summary page; the underlying case series and literature review are in the linked full communication,
not archived here.

## Used by stanpumpR
- inst/help/drugs/pregabalin.md:27 — pregabalin adds to opioid sedation and respiratory depression,
  particularly in the elderly and in lung disease (FDA safety communication, December 2019).

## Verification against stanpumpR
- pregabalin.md:27: the page names opioid/CNS-depressant co-use, lung disease (COPD) and the elderly, and
  advises monitoring for respiratory depression and sedation; posted 12/19/2019. **matches**.
