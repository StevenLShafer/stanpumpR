---
file: BK_9781108565011-ch22-control-of-ventilation.pdf
pmid: null
doi: 10.1017/9781108565011.025
citation: 'Chapter 22, Control of Ventilation, Section 2 Respiratory Physiology, printed pp. 93-96. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: other
drugs:
- opioids
- anaesthetic agents
- doxapram
source_read: text layer (all 4 PDF pages; Figure 22.1 axis labels scrambled in the text layer, no figure numbers used)
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Control of ventilation (textbook chapter 22)

## Study
Four-page textbook chapter (printed pp. 93-96, PDF pages 1-4). No data or PK. Qualitative physiology of ventilatory control with a short drug section.

## Model and parameters
```yaml
parameters: []   # no PK model; physiology values are listed below
```
Values stated in text:
- Peripheral chemoreceptors respond to PaO2 (not content), PaCO2, acidaemia (carotid bodies only) and hypotension; they account for 20% of the response to hypercapnoea, central chemoreceptors the rest; peripheral response time 1-3 s (printed p.93).
- Central chemoreceptors respond to CSF pH only, not to hypoxaemia (p.94).
- Hypoxic drive rises sharply when PaO2 falls below 8 kPa; enhanced at PaCO2 = 8 kPa; normocapnia taken as 5.3 kPa (p.95, Figure 22.1a).
- CO2 response linear over PaCO2 5-10 kPa, falling at profound hypercapnoea (animal data) (p.96, Figure 22.1b).
- Acidaemia threshold pH < 7.35 (p.93).

## Pharmacodynamics and end points
Opioid audit: no MEAC or analgesic concentration, no ke0 / t½ke0 / tPeak, no hysteresis and no equianalgesic or potency ratio is given. Qualitative only (p.96): opioids depress the medullary respiratory centre, blunting the ventilatory response to hypoxaemia and hypercapnoea and shifting the CO2-response curve rightward (Figure 22.1b), with no magnitude and no drug, dose or concentration. Anaesthetic agents likewise shift the CO2 curve rightward and depress central and peripheral chemoreceptors. The chapter says opioids do not affect the peripheral or central chemoreceptors' own response to hypoxaemia or hypercapnoea. Doxapram stimulates peripheral chemoreceptors, mechanism unclear.

## Covariates
None quantified. Chronic hypercapnoea (a COPD subset) loses CO2 sensitivity through CSF bicarbonate compensation.

## Caveats
Teaching summary with no primary data. It points to Pattinson 2008 (Br J Anaesth 100:747-58) for opioids and respiration.

## Used by stanpumpR
Not cited anywhere in the repo (context locations empty).

## Verification against stanpumpR
No repo citation, so nothing to compare. The qualitative statements (opioid rightward shift of the CO2 response) carry no numbers that a model could pin. Verdict: not-applicable.
