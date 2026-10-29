---
file: PMID_39030897_letter.pdf
pmid: 39030897
doi: 10.1002/bcp.70552
citation: "Krutsch K. Manufacturer-documented infrared interference from SSRIs: a confound in breast milk fat and pharmacokinetic studies (letter re: Monfort 2024 and Weisskopf 2020). Br J Clin Pharmacol 2026;92:2423-2424."
kind: other
drugs: [sertraline, escitalopram]
source_read: "text layer, both pages, including Table 1"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 7d8b50b
verification: not-applicable
---

# Krutsch 2026: letter on infrared milk-fat interference in the SSRI-Breast Milk popPK models

## Study
A letter to the editor about Monfort 2024 (sertraline, PMID_39030897) and Weisskopf 2020 (escitalopram, Br J Clin
Pharmacol 86:1642-1653). Both models found breast-milk fat content to be a significant covariate on the milk-to-plasma
ratio (MPR). Both measured fat with the Miris Human Milk Analyser, a mid-infrared device. The manufacturer's FDA
interference testing documents a negative fat bias from sertraline and citalopram; escitalopram was not tested, and is
presumed to behave like citalopram. The letter argues that the fat-MPR effect sizes may therefore be confounded. It
states that the primary conclusion, low infant exposure, rests on LC-MS/MS drug measurements and is unaffected.

## Model and parameters
```yaml
parameters:
  - {name: sertraline fat-MPR effect, value: "+95% MPR per doubling of fat", unit: none, source: "Table 1 (quoting Monfort 2024)", note: "the effect the letter says may be confounded"}
  - {name: escitalopram fat-MPR effect, value: "+28% / +18% MPR per doubling of fat", unit: none, source: "Table 1 (quoting Weisskopf 2020)", note: "parent / metabolite"}
```

## Pharmacodynamics and end points
None.

## Covariates
Milk fat (Miris infrared measurement): the covariate in question.

## Caveats
A letter: it reports the interference tests and infers a possible confound, but presents no reanalysis. The test
concentrations in Table 1 (sertraline 50-150, citalopram 250-750 ug/L) are the manufacturer's, not the study's.

## Used by stanpumpR
Not cited. stanpumpR models no breast-milk concentrations (`git grep -i milk` of the sertraline and escitalopram
models and help pages at 7d8b50b: no hits). Monfort 2024 supplies no number to the repo; see its card.

## Verification against stanpumpR
Nothing to verify: no repo claim depends on milk fat or MPR. **not-applicable**.
