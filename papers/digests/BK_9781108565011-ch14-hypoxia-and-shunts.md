---file: BK_9781108565011-ch14-hypoxia-and-shunts.pdf
doi: 10.1017/9781108565011.017
citation: 'Chapter 14, Hypoxia and Shunts, Section 2 Respiratory Physiology, printed pp. 63-67. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer, all 5 pages; Figure 14.2 axes and the shunt-equation typography (garbled in the text layer) not checked on rendered images; no number from this chapter is used by the code as a parameter.
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
open_questions:
- '[source] RESOLVED 2026-11-27. The book''s title and authors are not printed on the archived chapter pages, which carry only the chapter header and the Cambridge Core download line. They were supplied from the publisher''s own catalogue entry for ISBN 9781108565011: Chambers D, Huang C, Matthews G, Basic Physiology for Anaesthetists, 2nd edition, Cambridge University Press, 31 July 2019, doi 10.1017/9781108565011. Every chapter card of this book now carries that citation.'
---

# Book chapter 14: Hypoxia and shunts (respiratory physiology primer)

## Study
A textbook question-and-answer chapter (PDF pp.1-5, printed pp.63-67). No data, no drug, no model. Content, as facts:
- Four kinds of hypoxia: hypoxaemic, anaemic, stagnant, cytotoxic; hypoxaemia classified as hypoventilation, diffusion limitation, shunt, V/Q mismatch. Hb saturation falls steeply when PaO2 is below 8 kPa (p.63).
- Shunt: normal physiological shunt fraction 2-5%, about half anatomical (bronchial, Thebesian veins) and half functional (p.64). Pathological shunts intracardiac, via large vessels, or intrapulmonary (the commonest), the last including one-lung ventilation / bronchial obstruction (p.64).
- Shunt-type hypoxaemia responds poorly to supplemental O2 (only extra dissolved O2); mild V/Q mismatch responds; a V/Q near zero behaves like shunt (p.65). With a shunt PaCO2 usually stays normal because alveolar ventilation rises (p.65).
- Shunt equation (p.65-66): Qs/Qt = (CcO2 - CaO2) / (CcO2 - CvO2), contents in mL O2/100 mL blood; derived from an O2 mass balance on the pulmonary vein (p.67).
- O2 content: CaO2 = [Hb] x 1.34 x SaO2 + PaO2 x 0.023, with PaO2 in kPa. End-capillary content assumes saturation 100% and PO2 = PAO2.
- Worked example (p.66): Hb 15 g/dL, PaO2 13.0 kPa, SaO2 99%, PvO2 5.3 kPa, SvO2 75%, PaCO2 5.3 kPa, PB 101 kPa, PH2O 6.3 kPa, R 0.8, FiO2 0.21: PAO2 = 13.3 kPa; CaO2 20.2, CvO2 15.2, CcO2 20.4 mL/100 mL; shunt 4% (derived by the book; checks to (20.4-20.2)/(20.4-15.2) = 3.8%).
- Figure 14.2 (p.67): PaO2 against FiO2 for shunt fractions 0 to 50%; the curve flattens as shunt rises (axes only in the text layer; curve values not read).

## Model and parameters
No pharmacokinetic model.
```yaml
parameters: []   # physiology textbook chapter; no PK parameters
```

## Pharmacodynamics and end points
None. No drug, MEAC, ke0 or potency ratio. Not an opioid; opioid audit not applicable.

## Covariates
None.

## Caveats
A teaching text: normal values are given as ranges or single illustrative numbers, with no stated population. Pressures are in kPa, not mmHg (the dissolved-O2 coefficient 0.023 mL/100 mL/kPa, derived: 0.003 per mmHg x 7.5 = 0.0225).

## Used by stanpumpR
The context file records no location and `cited_in_repo` is false; a search of the checkout for the file stem and chapter title found nothing. The chapter is background for the endobronchial / shunt oxygen model (R/arterialOxygen.R, R/endobronchial.R).

## Verification against stanpumpR
Nothing cites this chapter, so there is no citation to verify. As an independent check on R/arterialOxygen.R (not a citation check): the code sets Cc'O2 = 1.34 Hb S(PAO2) + 0.003 PAO2 (mL/dL, PAO2 in mmHg; lines 23-28), 1.34 mL/g Hb, and CaO2 = (1 - s) Cc'O2 + s CvO2 (line 32). That is the book's shunt equation rearranged (CaO2 = (1-s)CcO2 + s CvO2, from the book's balance Qt CaO2 = (Qt-Qs) CcO2 + Qs CvO2, p.66-67), with the same 1.34 and a dissolved-O2 term equal within rounding (0.003 x 7.5 = 0.0225 vs the book's 0.023). The code's normal baseline shunt and the endobronchial shunt of 0.5 (R/arterialOxygen.R:29-31) are the repo's own choices; the book gives only 2-5% as the normal physiological shunt and does not give a one-lung-ventilation shunt fraction. No discrepancy found; verdict not-applicable because the repo does not rely on this file.
