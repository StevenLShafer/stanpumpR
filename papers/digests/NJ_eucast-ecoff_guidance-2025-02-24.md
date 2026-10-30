---
file: NJ_eucast-ecoff_guidance-2025-02-24.pdf
pmid: none
doi: none
citation: EUCAST. Guidance Document on Cephalosporins for Staphylococcus aureus Infection. 24 February 2025 (22 pages, with appendix "General consultation on Staphylococcus spp. and Cephalosporins"). https://www.eucast.org/fileadmin/src/media/PDFs/EUCAST_files/Guidance_documents/Cephalosporins_for_Staphylococcus_aureus_Infections_20250224.pdf
kind: guideline
drugs: [cefazolin, clindamycin]
source_read: text layer, 22 pages, all read (MIC distribution table p 11 and the PTA tables pp 13-14 read from the layout text; the PTA columns are partly scrambled and only the headers were relied on)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# EUCAST 2025: cephalosporins for S. aureus infection (guidance and consultation)

## Study
EUCAST guidance on whether MSSA susceptibility to cephalosporins can be inferred from cefoxitin (or mecA/mecC
negativity). Combines a literature review of clinical outcomes (mostly SSTI and MSSA bacteraemia) with a PK/PD review
and Monte Carlo probability of target attainment (RiskAMP in Excel; dosages from Breakpoint Tables v11.0) for the
IV agents cefazolin, cefepime, cefotaxime, ceftriaxone and cefuroxime.

## Model and parameters
EUCAST S. aureus MIC distributions and ECOFFs (appendix p 11 table; agent headings pp 13-15):
```yaml
parameters:
  - {name: ECOFF_cefazolin_S_aureus, value: 2, unit: mg/L, source: "p 11 table '(T)ECOFF'; p 13 heading 'Cefazolin [ECOFF = 2]'"}
  - {name: ECOFF_cefepime, value: 8, unit: mg/L, source: "p 11, p 14"}
  - {name: ECOFF_cefotaxime, value: 4, unit: mg/L, source: "p 11"}
  - {name: ECOFF_ceftriaxone, value: 8, unit: mg/L, source: "p 11 '(8)', tentative"}
  - {name: ECOFF_cefuroxime, value: 4, unit: mg/L, source: "p 11; text p 3"}
  - {name: cefazolin_PB, value: "91.6 +/- 6.7", unit: "%", source: "p 13", note: "used in the simulations"}
  - {name: cefazolin_Vd_volunteers, value: "6.94 +/- 2.2", unit: L, source: "p 13", note: "t1/2beta 1.45 +/- 0.15 h"}
  - {name: cefazolin_Vd_patients, value: "13.01 +/- 4.4", unit: L, source: "p 13", note: "t1/2beta 1.8 +/- 0.38 h"}
  - {name: PD_target, value: "25 / 30 / 35", unit: "% fT>MIC", source: "p 12", note: "stasis / 1-log / 2-log kill (cefotaxime, ceftriaxone, animal); Zelenitsky in vitro 55/75/100% for cefazolin and ceftriaxone"}
```
Recommendations (p 4): MSSA susceptibility may be inferred for cefazolin at 2 g x 3, cefuroxime iv 1.5 g x 3,
cefotaxime 2 g x 3-4, ceftriaxone 2 g x 2 or 4 g x 1 (preferably follow-on only), cefepime 2 g x 3; and for oral
cefalexin, cefadroxil, cefaclor, cefpodoxime and cefuroxime in less severe infections or oral follow-on. Cefalexin
trials used 500 mg x 2-4 (adults), 25 mg/kg x 2 (children). The cefazolin inoculum effect is noted (54.5% of 77
patients in one study, associated with higher 30-day mortality).

## Pharmacodynamics and end points
Antibacterial PK/PD only: fT>MIC targets above; PTA for cefazolin at the ECOFF of 2 mg/L is high (99-100%) with
2 g x 3 (pp 13-14, read loosely because of column scrambling). Not opioid-related; no MEAC, ke0 or potency ratio.

## Covariates
None (volunteer vs patient PK sets only).

## Caveats
Guidance, not breakpoints. The PTA tables' layout text is partly scrambled. The document is dated 24 February 2025;
the repo cites a 2026 edition (EUCAST lists an April 2026 revision that was not archived). It contains **no
clindamycin ECOFF**; clindamycin appears only as a clinical comparator.

## Used by stanpumpR
- R/drugs_cefazolin.R:58-59 - threshold 2 mg/L unbound, "the EUCAST S. aureus ECOFF and the MSSA MIC90".
- R/drugs_cefazolin.R:132-133 - band upper 2 mg/L, "the S. aureus ECOFF and MSSA MIC90".
- inst/help/drugs/cefazolin.md:7 - 2 mg/L, "the upper end of the MIC distribution of wild-type S. aureus (its
  epidemiological cut-off)".
- R/antibioticThresholds.R:63 - "EUCAST S. aureus cefazolin ECOFF 2 mg/L (EUCAST guidance on cephalosporins for
  S. aureus infections, 2026)".
- R/drugs_clindamycin.R:45 - "the EUCAST staphylococcal breakpoint and the S. aureus ECOFF are 0.25 mg/L".

## Verification against stanpumpR
- Cefazolin S. aureus ECOFF 2 mg/L (drugs_cefazolin.R:58-59, :132-133; cefazolin.md:7; antibioticThresholds.R:63):
  p 11 and p 13 give ECOFF = 2. **matches.** Applying it to unbound concentration is the repo's choice (an MIC is a
  free-drug concentration), consistent with this document's use of protein binding in its PTA.
- antibioticThresholds.R:63 cites the guidance as "2026"; the archived document is dated 24 February 2025.
  **discrepancy (citation date only)**: either archive the 2026 revision or cite 2025.
- Clindamycin S. aureus ECOFF 0.25 mg/L (drugs_clindamycin.R:45): not in this document. **cannot verify** here (the
  EUCAST staphylococcal clinical breakpoint 0.25 mg/L is confirmed by the v16.1 breakpoint tables, p 39; the ECOFF
  would come from the EUCAST MIC-distribution website).
- cefazolin.md:7 "susceptibility is inferred from oxacillin or cefoxitin": this document says cefoxitin (or
  mecA/mecC); oxacillin is the CLSI route. **matches** in substance.
