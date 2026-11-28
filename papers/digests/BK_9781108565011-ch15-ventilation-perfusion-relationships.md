---file: BK_9781108565011-ch15-ventilation-perfusion-relationships.pdf
citation: 'Chapter 15, Ventilation-Perfusion Relationships, Section 2 Respiratory Physiology, printed pp. 68-70. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer (all 3 PDF pages = printed pp. 68-70); no page images rendered, none needed as no number in the chapter is used by stanpumpR (Figure 15.1 plot not read)
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Book chapter 15 (Cambridge respiratory physiology text): Ventilation-Perfusion Relationships

## Study
Short teaching chapter (question-and-answer format) on gravity's effect on perfusion and ventilation, the V/Q ratio, causes of V/Q mismatch, management of hypoxaemia and the pulmonary embolism example. No drug content, no tables; one figure (Fig 15.1, PDF p.2, printed p.69) plotting perfusion, ventilation and V/Q against rib number.

## Model and parameters
None.
```yaml
parameters: []  # narrative chapter; no PK parameters
```

Physiology figures stated (PDF p.1, printed p.68): apex-to-base distance 30 cm = 30 cmH2O (22 mmHg); mean pulmonary artery pressure about 15 mmHg; perfusion 5 L/min, alveolar ventilation 4 L/min, so global V/Q 0.8; VE about 5 L/min with about one fifth dead space. Regional V/Q about 0.6 at bases, > 3 at apices (PDF p.2). Hypoxaemia defined as PaO2 < 8 kPa (PDF p.2).

## Pharmacodynamics and end points
No drug pharmacology. Opioid audit: not an opioid paper; no MEAC, ke0, tPeak or potency ratio.

## Covariates
Body position, exercise (cardiac output) and regional gravity effects on perfusion; no equations.

## Caveats
Textbook summary; the chapter itself notes the gravitational model does not explain all V/Q heterogeneity (PDF p.3, printed p.70).

## Used by stanpumpR
Nothing: no locations, drugs or uses in the context file; `cited_in_repo` is false. A case-insensitive search of the master checkout for the book DOI and chapter title found no match.

## Verification against stanpumpR
Not applicable: no repo location cites this file and it holds no quantity stanpumpR uses.
