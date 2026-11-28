---file: BK_9781108565011-ch08-oxygen-transport.pdf
pmid: none
doi: 10.1017/9781108565011.011
citation: 'Chapter 8, Oxygen Transport, Section 2 Respiratory Physiology, printed pp. 30-36. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer (PDF pages 1-7); no page images rendered
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Textbook ch 8 (2019): Oxygen transport

## Study
Teaching chapter on blood O2 carriage, the oxyhaemoglobin dissociation curve, Hb variants, MetHb, COHb, cyanide and myoglobin. No data set, no methods. PDF pages 1-7 = printed pp 30-36. The text layer was not checked against page images. The chapter cites Nunn's Applied Respiratory Physiology (8th ed, 2016), Dunn 2016 and others as further reading.

## Model and parameters
Typical adult figures given as teaching values. They are not fitted and carry no SE.
```yaml
parameters:
  - {name: Huefner_constant, value: 1.34, unit: "mL O2 / g Hb", source: "p30 (PDF p1)", note: "37 C typical adult blood"}
  - {name: O2_solubility, value: 0.023, unit: "mL O2 / dL / kPa", source: "p30 (PDF p1)", note: "in the content equation"}
  - {name: Hb_typical, value: 15, unit: "g/dL", source: "p30 (PDF p1)", note: worked example}
  - {name: CaO2, value: 19.8, unit: "mL/100 mL", source: "p30", note: "SaO2 97 %, PaO2 13.0 kPa; derived in text from equation"}
  - {name: CvO2, value: 15.2, unit: "mL/100 mL", source: "p30", note: "SvO2 75 %, PvO2 5.3 kPa"}
  - {name: dissolved_fraction, value: 2, unit: "% of carried O2", source: "p30", note: "98 % bound to Hb"}
  - {name: blood_volume_70kg, value: 5, unit: L, source: "p30", note: "holds about 850 mL O2"}
  - {name: myoglobin_O2_store, value: 250, unit: mL, source: "p30", note: "70 kg man"}
  - {name: lung_O2_store_air, value: 450, unit: mL, source: "p30", note: "FRC, breathing air"}
  - {name: total_O2_store, value: 1550, unit: mL, source: "p30", note: "70 kg man at sea level"}
  - {name: resting_VO2, value: 250, unit: "mL/min", source: "p30-31", note: adult}
  - {name: COHb_normal, value: "<2", unit: "%", source: "p35", note: "heavy smokers up to 9 %"}
  - {name: CO_affinity_vs_O2, value: 250, unit: "x", source: "p35"}
  - {name: MetHb_normal, value: "<1", unit: "% of Hb", source: "p35"}
```
Content equation (p30): O2 content per 100 mL = 1.34 x [Hb] x SaO2/100 + 0.023 x PO2, with Hb in g/dL and PO2 in kPa. Other values: fourth O2 binds 300x more easily than the first (p32). RBC lifespan 100-120 days, 10-20 days in sickle cell disease (p34). Stored blood 2,3-DPG is effectively zero after 1-2 weeks (p33). Cyanosis needs at least 5 g/dL deoxyHb (p36). MetHb makes pulse oximeters read 85 % (p35).

No P50 numeric value is printed (Fig 8.2 shows only a marked axis; text gives none). Axis marks in Fig 8.2: 3.5, 5, 5.3, 10, 13.3, 15, 20 kPa. Curve-shift factors (Fig 8.3, p33): right shift with raised PCO2, acidosis, 2,3-DPG, exercise, temperature, HbS. Left shift with the reverse, plus COHb, MetHb, HbF.

## Pharmacodynamics and end points
Not a drug paper. No MEAC, ke0, tPeak or potency ratio. Not an opioid paper.

## Covariates
None quantified (Hb concentration and temperature enter the worked equation or are named as shift factors only).

## Caveats
Secondary textbook source with rounded teaching values; arithmetic in the worked example is internally consistent (19.50 + 0.30 = 19.8; 15.08 + 0.12 = 15.2). No primary data, no P50 value, no temperature or pH equations. The chapter itself says the molecular mechanism of cooperativity is debated (footnote p32).

## Used by stanpumpR
The context file lists no locations (`cited_in_repo: false`, no drugs).

## Verification against stanpumpR
Nothing cites this file. For orientation only: `docs/gas-engine-expansion-mathematics.md` lines 25-27 set VO2 = 3.5 mL/kg/min x weight (about 245 mL/min at 70 kg, against the chapter's 250 mL/min), consistent. The same doc takes dissociation from Dash 2016 or Severinghaus, not from this chapter, and gives P50 20.6 mmHg at 37 C (line 185); this chapter prints no P50 to compare. Verdict: not-applicable (no citing location).
