---
file: BK_9781108565011-ch16-ventilation-perfusion-zones-in-the-lung.pdf
pmid: none
doi: 10.1017/9781108565011.019
citation: 'Chapter 16, Ventilation-Perfusion Zones in the Lung, Section 2 Respiratory Physiology, printed pp. 71-73. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: other
drugs: []
source_read: text layer only, all 3 PDF pages (printed 71-73). The chapter has no numbers the repo uses; Figure 16.1 was not rendered.
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Textbook chapter 16: West zones of the lung

## Study
Textbook chapter, qualitative physiology. No study, population or assay.

## Model and parameters
No pharmacokinetic model.
```yaml
parameters: []   # qualitative; no numeric model parameters
```
Content (PDF pp.1-2, printed pp.71-72): West zones in the upright lung. Zone 3 (base): Pa > Pv > PA, continuous flow. Zone 2 (middle): Pa > PA > Pv, flow in systole only, raised V/Q and alveolar dead space. Zone 1 (apex): PA > Pa > Pv, no flow, high dead space; absent in normal lungs but possible with low pulmonary arterial pressure (haemorrhagic shock) or high alveolar pressure (positive-pressure ventilation, high intrinsic PEEP). Normal alveolar pressure is 0 +/- 2 cmH2O. A pulmonary artery catheter wedge pressure is valid only in zone 3; a PCWP rise of more than 25% on a 10 cmH2O PEEP increase suggests the tip is not in zone 3 (printed p.73). The chapter notes the gravitational model is supplemented by structural (artery-bronchiole) explanations (its ch. 15).

## Pharmacodynamics and end points
None. No drug, ke0, MEAC or potency ratio. Opioid audit: not an opioid paper.

## Covariates
None quantified.

## Caveats
Qualitative only; no data.

## Used by stanpumpR
Not cited (`cited_in_repo: false`, no locations).

## Verification against stanpumpR
Not applicable: nothing quantitative to compare. The gas engine uses a single alveolar compartment with a fixed dead-space fraction (`R/gasProperties.R:381`) and no V/Q zones, so West-zone physiology lies outside its model.
