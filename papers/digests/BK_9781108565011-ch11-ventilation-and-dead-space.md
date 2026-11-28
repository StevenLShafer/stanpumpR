---
file: BK_9781108565011-ch11-ventilation-and-dead-space.pdf
pmid: none
doi: 10.1017/9781108565011.014
citation: "Chapter 11, Ventilation and Dead Space. In: Section 2 Respiratory Physiology, Cambridge University Press (ISBN 9781108565011), printed pp 45-49; textbook chapter, authors not named in the PDF."
kind: review
drugs: []
source_read: "text layer, all 5 pages; PDF pages 1 and 4 (printed 45 and 48) rendered and read, confirming 150 mL, 2 mL/kg, 7 mL/kg, VD/VT 0.2-0.35 and the Bohr equation"
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
open_questions:
  - "[decision] The gas engine's dead-space fraction of 0.3 of minute ventilation (R/gasProperties.R:385) is attributed to Nunn and Hill 1960 in docs/gas-engine-expansion-mathematics.md:35. This chapter gives VD/VT 0.2-0.35 (PDF p4, printed 48), so 0.3 is inside it, but it is not the repo's source. Whether to record it as corroboration is for Shafer."
---

# Ventilation and Dead Space (textbook chapter 11)

## Study
Teaching chapter, no original data. PDF pages 1-5 = printed 45-49. Topics: anatomical, alveolar and physiological dead space; minute vs alveolar ventilation; the PaCO2 equation; mechanical dead space; Fowler and Bohr methods.

## Model and parameters
No pharmacokinetic model. Definitions and values:
- VT = VA + VD; VE = VT x RR; alveolar ventilation = (VT - VD) x RR; dead-space ventilation = VD x RR; VE = alveolar + dead-space ventilation (PDF p2, printed 46).
- VD(Phys) = VD(Anat) + VD(Alv) (PDF p1).
- Typical anatomical dead space 150 mL for a 70 kg man, about 2 mL/kg, around a third of VT 7 mL/kg (PDF p1; derived: 2/7 = 0.29).
- PaCO2 = K x VCO2 / VA(dot); K = 0.015 when PaCO2 is in kPa, VCO2 in mL/min, VA in L/min (PDF p2-3, printed 46-47).
- Bohr, simplified: VD/VT = (PaCO2 - PETCO2)/PaCO2; normal 0.2-0.35 in tidal breathing (PDF p4, printed 48).
- Worked example (PDF p3): VT 500 mL x 15/min vs VT 250 mL x 30/min, both VE 7500 mL/min, VD 150 mL, giving alveolar ventilation 5250 vs 3000 mL/min.
- Fowler's single-breath N2 washout gives VD(Anat) and closing capacity (Figure 11.2, PDF p4).

```yaml
parameters:
  - {name: VD_anat_70kg, value: 150, unit: mL, source: "PDF p1 (printed 45)", note: typical}
  - {name: VD_anat_per_kg, value: 2, unit: mL/kg, source: "PDF p1 (printed 45)", note: approximate}
  - {name: VT_per_kg, value: 7, unit: mL/kg, source: "PDF p1 (printed 45)", note: normal}
  - {name: VD_over_VT_normal, value: "0.2-0.35", unit: ratio, source: "PDF p4 (printed 48)", note: tidal breathing}
  - {name: K_PaCO2_kPa, value: 0.015, unit: "kPa*L/mL", source: "PDF p2-3", note: unit correction factor}
```

## Pharmacodynamics and end points
None. Not an opioid paper: no MEAC, ke0, tPeak or potency ratio.

## Covariates
Anatomical dead space rises with patient size and lung volume; it falls supine and with bronchoconstriction. Alveolar dead space rises with upright posture, low pulmonary artery pressure, PEEP or positive-pressure ventilation, embolus and COPD (qualitative, PDF p1-2).

## Caveats
Secondary teaching source; no citations for the values on these pages. Equipment (mechanical) dead space is discussed qualitatively only.

## Used by stanpumpR
Context lists no locations and `cited_in_repo` is false. The library does model dead space for gases: `GAS_DEAD_SPACE_FRACTION <- 0.3` of minute ventilation, with alveolar ventilation = MV x (1 - deadSpace) (R/gasProperties.R:385, 464-468; R/advanceClosedFormGas.R:856-863).

## Verification against stanpumpR
Independent consistency check only, not a citation. The repo's 0.3 is a fraction of minute ventilation, which equals VD/VT at a fixed respiratory rate. It lies inside the chapter's normal 0.2-0.35 and agrees with 2 mL/kg over 7 mL/kg (0.29). The chapter's VD is physiological (anatomical plus alveolar), the same total-dead-space notion the repo uses. The repo attributes 0.3 to Nunn and Hill 1960, not this chapter. No discrepancy; verdict not-applicable because nothing cites the file.
