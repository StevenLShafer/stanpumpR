---file: BK_9781108565011-ch25-ventilatory-failure.pdf
pmid: none
doi: 10.1017/9781108565011.028
citation: 'Chapter 25, Ventilatory Failure, Section 2 Respiratory Physiology, printed pp. 104-106. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer, all 3 PDF pages (no page images rendered; the card records no number the repo uses)
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Ventilatory Failure (textbook chapter 25) 

## Study
Textbook question-and-answer chapter on respiratory physiology (PDF pp. 1-3 = printed pp. 104-106). No data, no drug, no PK or PD study. The last page is a reading list (Roussos 2003; Lumb, Nunn's Applied Respiratory Physiology 8th ed; Mehta 2006; Thavasothy 2002; Garfield 2001).

## Model and parameters
None.
```yaml
parameters: []  # textbook chapter, no pharmacokinetic parameters
```

## Pharmacodynamics and end points
No ke0, EC50, MEAC or potency ratio. Definitions only (printed p. 104, PDF p. 1): type 1 respiratory failure is PaO2 < 8.0 kPa with normal or low PaCO2; type 2 is PaO2 < 8.0 kPa with PaCO2 > 6.0 kPa. Alveolar ventilation (not minute ventilation) is inversely related to PaCO2. Opioids and general anaesthesia are named as causes of respiratory-centre depression, with no concentration or dose given. Other content: myasthenic crisis, chronic hypercapnoea in COPD, COPD exacerbation and non-invasive ventilation (PDF pp. 2-3).

## Covariates
None.

## Caveats
Teaching text, no primary data. Its only link to the library is terminology.

## Used by stanpumpR
The context file lists no locations and `cited_in_repo` is false. A text search of the checkout finds the phrase "ventilatory failure" in `AGENTS.md:146` (a wording rule: respiratory failure is ventilatory failure, and doxapram stimulates ventilation, not respiration) and in `R/drugs_doxapram.R:92` and `inst/app-help/drugs/doxapram.md:17`. Those two files cite Robson & Prescott's patients "in ventilatory failure", which is a different paper.

## Verification against stanpumpR
Nothing in the repo takes a number from this chapter. The chapter's usage (ventilation, alveolar ventilation) is consistent with the AGENTS.md terminology note, but the note is not cited to it. Verdict: not-applicable.
