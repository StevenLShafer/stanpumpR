---file: BK_9781108565011-ch26-anaesthesia-and-the-lung.pdf
citation: 'Chapter 26, Anaesthesia and the Lung, Section 2 Respiratory Physiology, printed pp. 107-110. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer (all 4 PDF pages = printed pp. 107-110); no page images rendered, none needed as the chapter contains no tables or numbers used by stanpumpR
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Book chapter 26 (Cambridge respiratory physiology text): Anaesthesia and the Lung

## Study
Short teaching chapter (question-and-answer format) on respiratory effects of anaesthetic drugs, effects of general anaesthesia on the lung, and postoperative pulmonary function. No data, no PK or PD tables. Book title and authors are not stated in the PDF text (only the DOI prefix and the Cambridge Core footer).

## Model and parameters
None.
```yaml
parameters: []  # narrative chapter; no PK parameters
```

## Pharmacodynamics and end points
Qualitative only (PDF p.1, printed p.107):
- Volatile agents: higher respiratory rate with lower tidal volume; blunted hypercapnic ventilatory response, ranked enflurane > desflurane > isoflurane > sevoflurane > halothane; N2O no effect.
- IV agents: initial stimulation then depression; apnoea especially with propofol; propofol depresses laryngeal reflexes and is thought to abolish the peripheral chemoreceptor response to hypoxaemia; ketamine preserves airway reflexes and ventilation.
- Opioids (audit): depress the respiratory centre (lower RR, blunted hypercapnic response), antitussive, histamine release with morphine, rarely chest wall rigidity after rapid IV strong opioid. No MEAC / analgesic concentration, no ke0 / t½ke0 / tPeak, and no equianalgesic or potency ratio is given anywhere in the chapter.
- Benzodiazepines: lower RR, blunted hypercapnic response. Neuromuscular blockers: paralysis; atracurium histamine release; suxamethonium/mivacurium prolonged block with low plasma cholinesterase.
- Only numbers: physiological PEEP 3-5 cmH2O (PDF p.2), low tidal volume < 8 mL/kg reduces postoperative pulmonary complications (PDF p.3), adding 20% nitrogen to fresh gas to splint alveoli (PDF p.3). None are drug PK/PD.

## Covariates
None.

## Caveats
Textbook summary; the ventilatory-depression ranking of volatile agents is stated without a source or numbers.

## Used by stanpumpR
Nothing: the context file lists no locations, drugs or uses, and `cited_in_repo` is false. A case-insensitive search of the master checkout for the book DOI and chapter title found no match.

## Verification against stanpumpR
Not applicable: no repo location cites this file and it contains no quantity stanpumpR uses.
