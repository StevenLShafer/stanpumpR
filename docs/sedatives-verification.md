# Six oral and intravenous sedatives: source verification

Steven L. Shafer asked for models of six sedatives (alprazolam, diazepam,
lorazepam, clonazepam, zolpidem, temazepam) based on a specification written by
ChatGPT, and for its models and references to be checked before anything was
implemented. This records what was checked, against what, and what was built.
Drafted with Claude Code, 2026-10-09.

**How it was checked.**
- Every citation was looked up in PubMed. The title, authors, journal, volume,
  pages and DOI were compared with the specification.
- Every number was compared with the paper's full text where it could be read:
  PMC, or PDFs Dr Shafer supplied: Swart 2004, DeVane 1993, Kruizinga 2022,
  dos Santos 2009, and Berlin and Dahlström 1975. Otherwise it was compared
  with the abstract.
- Each implemented model was then run against published concentration
  measurements. Its engine output was compared with an independent
  matrix-exponential solution, which agreed to rounding error.

**Outcome.**
- Four drugs are implemented: lorazepam, alprazolam, clonazepam and zolpidem.
- Diazepam and temazepam are not implemented yet. The specification's models
  for them fail in adults, and the adult sources are behind paywalls (see the
  end of this document).

## What the specification got right and wrong

| Drug | Specification's claim | Verdict |
|---|---|---|
| alprazolam | DeVane 1993: CL 0.05 L/h/kg, V 0.7 L/kg, ka 1.1/h, women +59%, over 60 −23%, multiple illness −26% | **Correct** (full paper: equations 1-3, Tables II and IV). The parameters are apparent (oral). |
| alprazolam | Burkat 2023 receptor table | Plausible but unverifiable: two of three rows reproduce the abstract's potentiation at 59 nM. Not used: it is a receptor end point. |
| diazepam | McCann 2025, Table 3: V1 58.2 L, with the prose saying 52.8 L | **The prose's 52.8 L is the consistent one.** It matches the abstract's "26% increase" over 42 L. The table could not be read. The "IIV CV" values appear to be variances. |
| diazepam | McCann as the parent model | **Wrong population for this library.** At 70 kg it predicts a 10 mg oral peak of 90-145 ng/mL; adults reach 290-400 ng/mL at about 1 h. Its oral ka and F are fixed assumptions. |
| lorazepam | Gonzalez 2017 equations | **Correct** (abstract). Paediatric only. |
| lorazepam | Swart 2004, Table 4: V1 0.743 L, Vss 156 − 2.07 (age − 58) L, Q 36.3 L/h; abstract conflicts | **Correct, including the conflict** (PDF). The abstract misprints Vss as 56 and Q as 10 L/h. A central volume of 0.74 L, from long-infusion data, puts a 2 mg bolus at 2700 ng/mL, so the model is unusable for bolus dosing. |
| lorazepam | Barr 2001 C50s for Ramsay ≥ 2-6: 34, 51, 104, 152, 188 ng/mL | **Correct** (abstract). Plasma, steady state. |
| lorazepam | Blin 1999: CL/F 5.4 L/h, Vss/F 111.6 L, EC50 12.2-15.3 ng/mL | EC50 correct; the others plausible, from a search excerpt. |
| clonazepam | Kruizinga 2022, Table 2: every value | **Correct** (PDF). Doses were 0.5 and 1 mg of oral solution, sampled to 48 h. No lag. Not used: it fits tablets less well (below). |
| clonazepam | dos Santos 2009: two compartments with a lag; sigmoid Emax with tolerance; numbers unavailable | **Obtained** (PDF). Table 1 and Table 2 read in full. This is the model implemented. |
| clonazepam | Berlin and Dahlström 1975: IV compartments | **Obtained** (PDF). Sampling began at 10 min, so the IV central volume is not identified (48-241 L). Oral F averaged 0.98. |
| zolpidem | Kim 2026, Tables 2-3: PK and DSST PD | **Correct** (PMC full text). The 10 mg dose is tartrate, as labelled. |
| zolpidem | Cha 2024: CL/F 16.9, V/F 61.7, ka 5.41, lag 0.394 h | **Correct**. It is the same trial, from digitized curves of 23 of the 30 subjects. |
| zolpidem | label: t½ 2.6 h | **2.6 h is the 5 mg value**; 10 mg is 2.5 h. |
| temazepam | Ochs 1984: V 1.45 L/kg, CL 2.33 mL/min/kg, t½ 8.6 h | **Correct** (20 mg oral). The specification is also right that this one-compartment reduction cannot reach the label's 865 ng/mL peak. |
| temazepam | label: 30 mg mean peak 865 ng/mL, biphasic decline | **Correct**: range 666-982 ng/mL at a mean of 1.5 h; distribution t½ 0.4-0.6 h; terminal t½ 3.5-18.4 h. |

## What was built

**Zolpidem** — Kim 2026.
- Model: one compartment, apparent CL/F 18.0 L/h and V/F 64.0 L, oral only.
- Absorption: the transit chain (MTT 0.25 h, NN 19.4, ka 11.7/h) is
  represented as a 0.25 h lag before the published ka. Integrated numerically
  against the transit model, the two agree within 0.2 ng/mL from 45 min on, and
  the peaks differ by 0.6%.
- No effect site: the published effects are direct.
- Threshold: 50 ng/mL (FDA 2013 driving warning).
- Validation: 10 mg peaks at 143 ng/mL (label 121, 58-272; Greenblatt 2006
  about 140), half-life 2.46 h (label 2.5).

**Clonazepam** — dos Santos 2009, Table 1, in place of the specification's
Kruizinga 2022.
- Model: two compartments with a 0.369 h lag, apparent, from 4 mg tablets in
  23 men sampled to 72 h. Oral only.
- Why not Kruizinga: for 2 mg tablets (observed peak 13-17 ng/mL at 1-4 h,
  half-life 30-43 h), dos Santos gives 12.5 ng/mL at 2.0 h and 42 h.
  Kruizinga's solution model gives 9.9 ng/mL at 1.6 h and 57 h, because its
  sampling stopped at 48 h.
- No effect site: dos Santos found the effect direct, peaking with plasma.
- Why there is no IV route: neither study identifies the first half hour after
  an injection. Berlin's IV sampling began at 10 min; 27 ng/mL has been
  measured 2 min after 0.5 mg IV.

**Alprazolam** — DeVane 1993.
- Model: as published, oral only.
- Effect site: ke0 0.144/min, from the IV EEG study of Venkatakrishnan 2005
  (equilibration t½ 4.8 min).
- Validation: 1 mg peaks at 16.9 ng/mL (observed 12-22), but at 2.7 h against
  0.7-1.8 h, because ka came from steady-state samples.
- **Decision for Dr Shafer:** DeVane's +59% clearance in women is disputed. The
  Greenblatt and Wright review says most studies find no sex effect. It is
  implemented as published and flagged on the drug's page.

**Lorazepam** — two compartments derived from the Nielsen-Kudsk 1983 means.
- Disposition: the specification's two models were rejected (above). V1 0.59
  L/kg, CL 62.2 mL/kg/h, t½α 0.31 h, t½β 14.1 h give V2 and Q exactly.
- Routes: oral and IM absorption from Greenblatt 1982, a five-route crossover.
- Effect site: ke0 from Greenblatt 2000 (EEG, t½ke0 8.8 min), carried as a
  26.3 min time to peak effect.
- Band and threshold: Barr 2001's C50s, 34-104 ng/mL; threshold 51 (Ramsay 3).
- Validation against the label: 4 mg IV gives 74 ng/mL at 15 min (label about
  70); 2 mg PO peaks at 19.7 (about 20); 4 mg IM peaks at 53 (about 48).

## Not yet implemented, and what would unblock them

- **Diazepam.** Mould et al., *Clin Pharmacol Ther* 1995;58:35-43, the source
  of the library's midazolam model, fitted diazepam in the same 12 volunteers
  with an effect site. Its parameters are not in the abstract. Bührer 1990
  gives an arterial equilibration half-life of 1.6 min; Hung 1996 gives IM
  bioavailability 1.0 and tmax 34 min; Divoll 1983 gives oral F 0.94.
  Nordiazepam formation is 53% of the dose (Greenblatt 1988).
- **Temazepam.** van Steveninck et al., *Clin Pharmacol Ther*
  1994;55:535-545 and 546-555, give IV kinetics and EEG/saccadic PD in
  volunteers. No accessible paper reports a temazepam ke0.
- **Barr 2001** (lorazepam ICU PK, ke0 and sedation model) would replace the
  derived lorazepam disposition. **Venkatakrishnan 2005** would allow IV
  alprazolam (no product exists). IV clonazepam would need a study sampled in
  the first minutes after injection; Berlin and Dahlström 1975 is not one.
