# Pregnancy pharmacokinetics in stanpumpR

*Claude Code, 2026-10-19, at the request of Steven L. Shafer: review the literature for
the pharmacokinetic changes of pregnancy for every drug model, incorporate them, require
them of every future model, and list the literature to obtain from the Stanford library.*

## Where things are

| What | Where |
|---|---|
| One row per drug: finding, what is applied, factors, design, n, confidence, sources, what was read, papers to obtain | `inst/extdata/pregnancyRegistry.csv`, built by `data-raw/pregnancyRegistry.R` |
| The flag, the registry reader, the filtration rule | `R/pregnancy.R` (`pregnancyFindings()`, `pregnancyGfrFactor()`, `pregnancyReference()`) |
| Each applied factor, its source and arithmetic | the `PREGNANCY` section of `R/drugs_<drug>.R` (insulins: `R/insulinTham.R`) |
| What the user sees | Help → Models → Pregnancy (`inst/help/models/pregnancy.md` + generated table), and a Pregnancy section on every drug's page (`helpDrugPregnancyHTML()`) |
| Enforcement | `tests/testthat/test-pregnancy.R`: every drug has a row; exactly the `applied` drugs name `pregnant`; the registry's `cl_factor`, `v_factor`, `f_factor` are what each model does; the flag off changes nothing |
| The rule for new models | `AGENTS.md` (Key Rules) and `docs/adding-a-drug.md` ("Pregnancy (required for every model)") |

## How the review was done

The library's first pregnancy adjustments (2026-10-13) followed one review, Ansari, Carvalho,
Shafer and Flood, *Anesth Analg* 2016;122:786-804, and changed eight drugs. On 2026-10-19 the
literature was searched again for each of the 182 drugs a user can dose (every row of
`drugDefaults_global.csv` with a Category, including the four inhaled anesthetics), in nine
classes, through PubMed (abstracts) and PubMed Central (full text). Each number in a
registry `finding` was read in that session from the text named in `evidence_read`
(110 rows from abstracts, 35 from full text); statements known only from memory are marked so
and their papers are on the list below. The search preferred, in order: a population model
with a pregnancy or gestational-age covariate; a paired study of the same women during
pregnancy and after it; a comparison with nonpregnant women; a therapeutic-drug-monitoring
series; a PBPK prediction; a review.

## The rules applied

- **The flag is late pregnancy** (third trimester and term). Where a source reports
  trimesters, the third-trimester value is applied.
- **Applied** only when the change was *measured* in women with useful strength, or derived
  from such a measurement by arithmetic the drug's header gives (midazolam's IV factor from
  Hebert's oral data through Quinney's mechanism; metoprolol's split of CL/F into clearance and
  first-pass; phenytoin's Km and Vmax and valproate's total CL/F from the rise in free
  fraction). A direction alone, a handful of women, no comparator, or a PBPK prediction alone
  is recorded as `unquantified` and not applied (lorazepam, risperidone, haloperidol, aspirin,
  fentanyl, codeine, tramadol, the neuromuscular blockers, heparin, phenobarbital).
- **Filtration.** A drug cleared mostly unchanged by filtration whose model reads renal function
  takes the 50% rise in GFR on that renal term (cefazolin, gentamicin, vancomycin, sugammadex,
  digoxin, fondaparinux, mannitol, hydrochlorothiazide, procainamide), unless drug-specific data
  contradict it (gabapentin's one peripartum study) or the model's structure caps it
  (pregabalin). **New in this change:** the rise is applied only when the serum creatinine is
  blank (`pregnancyGfrFactor()`); a measured pregnant creatinine (about 0.5 to 0.6 mg/dL) already
  carries it, and multiplying again counted it twice. This changes cefazolin, gentamicin and
  vancomycin for a user who enters a creatinine.
- **Drug-specific data win over the general GFR rise** where both exist: levetiracetam,
  lacosamide, topiramate, metformin and enoxaparin apply their measured factor to the whole
  clearance and not through their renal terms, which would undershoot (levetiracetam) or count
  filtration twice (metformin).
- **Models fitted in pregnant women** (calcium, magnesium sulfate, oxytocin) are
  `source_pregnant`: the flag does nothing.
- **Total concentrations.** The library plots total concentration. For phenytoin and valproate
  the factors reproduce the measured fall in TOTAL concentration, which is mostly a rise in free
  fraction; their pages say the unbound concentration falls far less.

## The drugs adjusted

The registry is authoritative; this table was generated from it on 2026-10-19 (factors on the
elimination clearance or CL/F, the volume(s) the drug's `applied` text names, oral
bioavailability, and the renal-function estimate; phenytoin's are on Km and Vmax).

| Drug | CL | V | F | GFR term | First source | Confidence |
|---|---|---|---|---|---|---|
| remifentanil | 2.000 |  |  |  | Kan 1998 | moderate |
| morphine | 1.700 |  |  |  | Gerdin 1990 | high |
| methadone | 1.714 |  |  |  | Bogen 2012 | high |
| midazolam | 1.450 |  |  |  | Hebert 2008 | moderate |
| oxycodone | 1.462 |  |  |  | Kokki 2012 | moderate |
| buprenorphine | 1.830 |  |  |  | Bastian 2017 | moderate |
| acetaminophen | 1.400 | 1.14 |  |  | Allegaert 2015 | moderate |
| ketorolac | 1.500 | 1.50 |  |  | Kulo 2012 | moderate |
| cefazolin |  |  |  | 1.5 | Philipson 1987 | moderate |
| cefalexin | 1.500 |  |  |  | Philipson 1987 | low |
| vancomycin |  |  |  | 1.5 | Goyal 2022 | moderate |
| gentamicin |  |  |  | 1.5 | Groen 2023 | low |
| dexamethasone | 2.000 | 2.00 |  |  | Tsuei 1980 | moderate |
| sugammadex |  |  |  | 1.5 | Hodge 2026 | moderate |
| mannitol |  |  |  | 1.5 | Deferm 2021 | low |
| citalopram | 1.320 |  |  |  | Westin 2017 | moderate |
| sertraline | 0.600 |  |  |  | Westin 2017 | moderate |
| paroxetine | 2.000 |  |  |  | Westin 2017 | moderate |
| quetiapine | 4.200 |  |  |  | Westin 2017 | moderate |
| aripiprazole | 2.100 |  |  |  | Westin 2017 | moderate |
| phenytoin |  |  |  |  | Tomson 1994 | moderate |
| valproate | 1.600 |  |  |  | Nau 1986 | low |
| topiramate | 1.600 |  |  |  | Ohman 2009 | moderate |
| lacosamide | 1.500 |  |  |  | Pennell 2022 | moderate |
| lamotrigine | 2.300 |  |  |  | Karanam 2025 | high |
| zonisamide | 1.600 |  |  |  | Reimers 2018 | moderate |
| levetiracetam | 1.600 |  |  |  | Tomson 2007 | moderate |
| eslicarbazepine | 1.600 |  |  |  | Christensen 2006 | low |
| insulinRegular | 1.190 |  |  |  | Catalano 1998 | low |
| insulinRegularU500 | 1.190 |  |  |  | Catalano 1998 | low |
| insulinNPH | 1.190 |  |  |  | Catalano 1998 | low |
| insulinHumanMix70 | 1.190 |  |  |  | Catalano 1998 | low |
| insulinLispro | 1.190 |  |  |  | Catalano 1998 | low |
| insulinLisproMix75 | 1.190 |  |  |  | Catalano 1998 | low |
| insulinLisproMix50 | 1.190 |  |  |  | Catalano 1998 | low |
| insulinGlargine | 1.190 |  |  |  | Catalano 1998 | low |
| metformin | 1.270 | 1.33 |  |  | Liao 2020 | moderate |
| glyburide | 1.900 | 1.57 |  |  | Hebert 2009 | moderate |
| enoxaparin | 1.500 | 1.40 |  |  | Lebaudy 2008 | high |
| fondaparinux |  |  |  | 1.5 | Wu 2025 | low |
| hydrochlorothiazide |  |  |  | 1.5 | character(0) | low |
| procainamide |  |  |  | 1.5 | character(0) | low |
| digoxin |  |  |  | 1.5 | Hebert 2008 | moderate |
| digoxinIV | 1.350 |  |  |  | Hebert 2008 | low |
| metoprolol | 1.500 |  | 0.5 |  | Ryu 2016 | moderate |

Not adjusted: 18 `unquantified`, 19 `unchanged`, 88 `unknown` (mostly drugs contraindicated or
rarely used in pregnancy: the DOACs and vitamin K antagonists, the GLP-1, SGLT2 and DPP-4
drugs, the illicit-drug models, most vasoactive infusions), 3 `source_pregnant` and 9
`not_applicable` (pediatric models, azilsartan, and the inhaled anesthetics).

## Changes to existing behaviour

- **Dexamethasone** now doubles both volumes as well as the clearance (Tsuei 1980: clearance
  559 vs 243 mL/min with an unchanged terminal half-life); doubling the clearance alone halved
  the half-life, against the only intravenous data in pregnant women.
- **Midazolam** and **buprenorphine**, described by Ansari 2016 only in direction, are now
  adjusted from paired studies (Hebert 2008; Zhu/Caritis 2024).
- **Acetaminophen**, "unchanged" in Ansari 2016 (Rayburn 1986, six women, oral), now has
  clearance × 1.4 from the larger intravenous studies at delivery (Beleyn 2014, Kulo 2012-2013,
  Allegaert 2015) and Miners 1986.
- **The filtration rise** on cefazolin, gentamicin and vancomycin is now skipped when a creatinine
  is entered (above).

## Possible extensions

- **Trimester.** Many sources (MONEAD for the antiseizure drugs; Karanam 2025's lamotrigine model,
  whose clearance rises with a half-maximal time of 12 weeks; Fischer 2014's oral labetalol;
  Westin 2017's antidepressants and antipsychotics) give the time course. A gestational-age
  field in place of the flag would let those models use it.
- **Pharmacodynamics.** MAC falls by about 28% in pregnancy and the first hours after delivery
  (isoflurane, Gin and Chan 1994); the gas pages record it, and the gas engine does not apply it.
- **A general CYP3A rise** (about 35-38%, Tracy 2005) could be applied to fentanyl and the other
  CYP3A substrates without drug data; it is recorded, not applied.
- **Oral labetalol.** The library's labetalol is intravenous, which is unchanged in pregnancy
  (Rubin 1983). Fischer 2014's population model of ORAL labetalol across pregnancy (CL/F × 1.6
  and V/F × 1.9 at 40 weeks) would be the model to add for an oral route.

## Literature to obtain from the Stanford library

These are the papers whose full text, tables or supplements would firm up a number: the
PubMed abstract lacked it, the PubMed Central copy came back without its tables, or the paper is
paywalled. The first list matters most, since each would check a factor the library applies.

### Papers that would firm up a factor the library applies

1. Allegaert K et al. Am J Obstet Gynecol 2009;200:170.e1-7 [PMID 19006783](https://pubmed.ncbi.nlm.nih.gov/19006783/) · [doi:10.1016/j.ajog.2008.08.067](https://doi.org/10.1016/j.ajog.2008.08.067)  
   *Drugs:* cefazolin. *Why:* Abstract says clearance and volume are 'increased during pregnancy' but gives no non-pregnant reference; the discussion gives the comparator.

2. Allegaert K et al. BMC Anesthesiol 2015;15:163 (Table 2) [PMID 26566962](https://pubmed.ncbi.nlm.nih.gov/26566962/) · [doi:10.1186/s12871-015-0144-3](https://doi.org/10.1186/s12871-015-0144-3)  
   *Drugs:* acetaminophen. *Why:* Open access, but the tool returned the text without Table 2: typical CL to sulfate, unchanged-drug CL and the 'at delivery' factor on central volume are needed to build a full delivery/nonpregnant ratio for total CL and V.

3. Catalano PM, Drago NM, Amini SB. Longitudinal changes in pancreatic beta-cell function and metabolic clearance rate of insulin in pregnant women with normal and abnormal glucose tolerance. Diabetes Care 1998;21:403-408 [PMID 9540023](https://pubmed.ncbi.nlm.nih.gov/9540023/) · [doi:10.2337/diacare.21.3.403](https://doi.org/10.2337/diacare.21.3.403)  
   *Drugs:* insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70, insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine. *Why:* Abstract gives MCR per m^2 only; full text needed for body surface area/weight at each visit to convert to absolute MCR (absolute late/pregravid ratio is likely above 1.19 because BSA rises).

4. Charles B, Norris R, Xiao X, Hague W. Population pharmacokinetics of metformin in late pregnancy. Ther Drug Monit 2006;28:67-72 [PMID 16418696](https://pubmed.ncbi.nlm.nih.gov/16418696/) · [doi:10.1097/01.ftd.0000184161.52573.0e](https://doi.org/10.1097/01.ftd.0000184161.52573.0e)  
   *Drugs:* metformin. *Why:* Third-trimester popPK (CL/F 28 L/h/70 kg) that concluded no change; read to reconcile with Liao/Eyal.

5. Delavenne X, et al. (POP-A-RIX). Thromb Haemost 2010;104:252-260 and any obstetric fondaparinux PK series (no PMID found; cited from another paper)  
   *Drugs:* fondaparinux. *Why:* No fondaparinux pregnancy PK ratio was found; a dedicated pregnant-vs-nonpregnant anti-Xa PK study (if one exists) would be needed to replace the GFR-analogy estimate.

6. Duan et al. J Pharm Sci 2024;113:1385-1394 [PMID 38176454](https://pubmed.ncbi.nlm.nih.gov/38176454/) · [doi:10.1016/j.xphs.2023.12.025](https://doi.org/10.1016/j.xphs.2023.12.025)  
   *Drugs:* levetiracetam. *Why:* popPK with pregnancy as a covariate on CL/F; the coefficient is not in the abstract.

7. Duan et al. Neurology 2026;107:e218456 [PMID 42685297](https://pubmed.ncbi.nlm.nih.gov/42685297/) · [doi:10.1212/WNL.0000000000218456](https://doi.org/10.1212/WNL.0000000000218456)  
   *Drugs:* levetiracetam. *Why:* Largest prospective cohort (228 pregnant women); per-trimester C/D tables for every ASM including lacosamide and the oxcarbazepine metabolite.

8. Elkomy MH, Sultan P, Drover DR, Epshtein E, Galinkin JL, Carvalho B. Antimicrob Agents Chemother 2014;58:3504-13 (and erratum 2015;59:3694, PMID 25977453) [PMID 24733461](https://pubmed.ncbi.nlm.nih.gov/24733461/) · [doi:10.1128/AAC.02613-13](https://doi.org/10.1128/AAC.02613-13)  
   *Drugs:* cefazolin. *Why:* PMC copy returned no text; the full text gives the model structure, how pre/post delivery clearance was parameterised and the free fraction assumed.

9. Eyal S, Easterling TR, Carr D, et al. Pharmacokinetics of metformin during pregnancy. Drug Metab Dispos 2010;38:833-840 [PMID 20118196](https://pubmed.ncbi.nlm.nih.gov/20118196/) · [doi:10.1124/dmd.109.031245](https://doi.org/10.1124/dmd.109.031245)  
   *Drugs:* metformin. *Why:* Full text tables give CL/F and V/F by trimester vs postpartum (same women); the abstract gives only renal clearance.

10. Fukushima Y, Yamamoto Y, Yamazaki E, et al. Change in the pharmacokinetics of lacosamide before, during, and after pregnancy. Seizure 2021;88:12-14 [PMID 33774498](https://pubmed.ncbi.nlm.nih.gov/33774498/) · [doi:10.1016/j.seizure.2021.03.011](https://doi.org/10.1016/j.seizure.2021.03.011)  
   *Drugs:* lacosamide. *Why:* No abstract in PubMed; a before/during/after case with CYP2C19 genotype.

11. Goudie RJB, Lunn D, Hovorka R, Murphy HR. Pharmacokinetics of insulin aspart in pregnant women with type 1 diabetes: every day is different. Diabetes Care 2014;37:e121-e122 [PMID 24855162](https://pubmed.ncbi.nlm.nih.gov/24855162/) · [doi:10.2337/dc13-2535](https://doi.org/10.2337/dc13-2535)  
   *Drugs:* insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70, insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine. *Why:* Letter (no abstract) reporting aspart PK in early vs late pregnancy (time-to-peak and clearance) in the same women; would firm up the absorption-delay factor.

12. Hebert MF, Easterling TR, Kirby B, et al. Clin Pharmacol Ther 2008;84:248-253 [PMID 18288078](https://pubmed.ncbi.nlm.nih.gov/18288078/) · [doi:10.1038/clpt.2008.1](https://doi.org/10.1038/clpt.2008.1)  
   *Drugs:* digoxin, digoxinIV, midazolam. *Why:* digoxin: The abstract gives unbound renal and secretory clearance only. The full text should give total CL/F, AUC, fu and creatinine clearance in pregnancy vs postpartum, which would confirm the 1.5x total-renal assumption and say whether CL/F changed. digoxinIV: Total clearance, fraction renal and fu in pregnancy, to replace the 1.35 composed from renal clearance and Iisalo's nonrenal fraction. midazolam: Total (not unbound) CL/F, V/F and fu by period, and individual paired ratios; abstract gives only unbound CL/F.

13. Hebert MF, Ma X, Naraharisetti SB, et al. Are we optimizing gestational diabetes treatment with glyburide? The pharmacologic basis for better clinical practice. Clin Pharmacol Ther 2009;85:607-614 [PMID 19295505](https://pubmed.ncbi.nlm.nih.gov/19295505/) · [doi:10.1038/clpt.2009.5](https://doi.org/10.1038/clpt.2009.5)  
   *Drugs:* glyburide. *Why:* The PMC text omitted Tables 2-3 (individual NCA and compartmental parameters including MAT and unbound CL/F); needed to confirm the volume ratio and whether absorption changed.

14. Hodge AT et al. Int J Obstet Anesth 2026;66:104874 [PMID 41747364](https://pubmed.ncbi.nlm.nih.gov/41747364/) · [doi:10.1016/j.ijoa.2026.104874](https://doi.org/10.1016/j.ijoa.2026.104874)  
   *Drugs:* rocuronium, sugammadex. *Why:* rocuronium: Full two-compartment rocuronium parameter set (V1, V2, Q) and which 'volume of distribution' the abstract reports; would allow a direct comparison with a non-pregnant model. sugammadex: Full parameter set (Vc, Vp, Q), serum creatinine/CrCl of the women and whether 13.9 L is Vc or Vss; needed to confirm the GFR-only adjustment and the volume comparison.

15. Hogstedt S, Lindberg B, Peng DR, Regardh CG, Rane A. Pregnancy-induced increase in metoprolol metabolism. Clin Pharmacol Ther 1985;37:688-692 [PMID 4006368](https://pubmed.ncbi.nlm.nih.gov/4006368/) · [doi:10.1038/clpt.1985.114](https://doi.org/10.1038/clpt.1985.114)  
   *Drugs:* metoprolol. *Why:* The only study with IV and oral metoprolol in the same pregnant women. Its tables give systemic CL, F and Vss in pregnancy vs postpartum, which would replace the well-stirred split (cl1 x1.5, F x0.5) with measured ratios.

16. Koerner M, Yerby M, Friel P, McCormick K. Valproic acid disposition and protein binding in pregnancy. Ther Drug Monit 1989;11:228-230 (no PMID found; cited from another paper)  
   *Drugs:* valproate. *Why:* Cited from memory (not retrieved this session): a direct study of valproate total and unbound disposition across pregnancy; PMID not verified.

17. Krzyzanski W et al. J Pharmacokinet Pharmacodyn 2026 (cohort population analysis, dexamethasone in parturients) [PMID 42406183](https://pubmed.ncbi.nlm.nih.gov/42406183/) · [doi:10.1007/s10928-026-10048-5](https://doi.org/10.1007/s10928-026-10048-5)  
   *Drugs:* dexamethasone. *Why:* PMC13337934 exists but was not retrieved; gives one-compartment CL and V for dexamethasone in parturient women at birth that could replace the volume inference.

18. Kulo A et al. Bosn J Basic Med Sci 2017;17:54-60 [PMID 27968707](https://pubmed.ncbi.nlm.nih.gov/27968707/) · [doi:10.17305/bjbms.2016.1515](https://doi.org/10.17305/bjbms.2016.1515)  
   *Drugs:* ketorolac. *Why:* Open-access PMC5341779 but tool returned no body text: S- and R-ketorolac CL and Vss at delivery vs postpartum/controls, to give separate factors for the S and R systems.

19. Kulo A et al. Br J Clin Pharmacol 2013;75:850-60 [PMID 22845052](https://pubmed.ncbi.nlm.nih.gov/22845052/) · [doi:10.1111/j.1365-2125.2012.04402.x](https://doi.org/10.1111/j.1365-2125.2012.04402.x)  
   *Drugs:* acetaminophen. *Why:* PMC record returned no body text; popPK volumes at delivery vs postpartum.

20. Kulo A et al. Int J Obstet Anesth 2012;21:334-8 [PMID 22901775](https://pubmed.ncbi.nlm.nih.gov/22901775/) · [doi:10.1016/j.ijoa.2012.06.001](https://doi.org/10.1016/j.ijoa.2012.06.001)  
   *Drugs:* ketorolac. *Why:* Absolute CL (L/h) and V (L) by group, weights, to confirm the BSA-normalized ratios.

21. Kulo A et al. Weight, pregnancy and oral contraceptives affect intravenous paracetamol clearance in young women. Eur Rev Med Pharmacol Sci 2014 [PMID 24668698](https://pubmed.ncbi.nlm.nih.gov/24668698/)  
   *Drugs:* acetaminophen. *Why:* Pooled n=73; abstract says CL and distribution volume higher at delivery but gives no numbers for V.

22. Lazebnik N et al. Postgrad Med J 1985;61:979-81 [PMID 4070116](https://pubmed.ncbi.nlm.nih.gov/4070116/) · [doi:10.1136/pgmj.61.721.979](https://doi.org/10.1136/pgmj.61.721.979)  
   *Drugs:* gentamicin. *Why:* Free PMC scan exists (PMC2418464) but the API returned no text; the half-lives of pregnant vs non-pregnant women would give a second within-study ratio.

23. Li Y, et al. Br J Clin Pharmacol 2023;89:1152-1161 [PMID 36260320](https://pubmed.ncbi.nlm.nih.gov/36260320/) · [doi:10.1111/bcp.15572](https://doi.org/10.1111/bcp.15572)  
   *Drugs:* levetiracetam. *Why:* A popPK model fitted only in pregnant women with a trimester covariate on CL/F; Table of final estimates needed for a trimester-specific factor (abstract gives only CL/F 3.82 L/h).

24. Liao MZ, Flood Nichols SK, Ahmed M, et al. Effects of pregnancy on the pharmacokinetics of metformin. Drug Metab Dispos 2020;48:264-271 [PMID 31980499](https://pubmed.ncbi.nlm.nih.gov/31980499/) · [doi:10.1124/dmd.119.088435](https://doi.org/10.1124/dmd.119.088435)  
   *Drugs:* metformin. *Why:* PMC7076518 exists but the tool returned no body: need gestational age at study, the CL/F and V/F means (L/h, L) at 500 mg, and the estimated F, to confirm the 1.27/1.33 ratios and decide whether F should be modelled separately.

25. Luxford AM, Kellaway GS. Pharmacokinetics of digoxin in pregnancy. Eur J Clin Pharmacol 1983;25:117-121 [PMID 6617713](https://pubmed.ncbi.nlm.nih.gov/6617713/) · [doi:10.1007/BF00544027](https://doi.org/10.1007/BF00544027)  
   *Drugs:* digoxin. *Why:* The abstract has no numbers. Its finding of higher serum digoxin in pregnancy despite higher renal clearance needs the tables, and possibly an assay explanation (cross-reacting endogenous digoxin-like substances).

26. Miners JO et al. Br J Clin Pharmacol 1986;22:359-62 [PMID 3768250](https://pubmed.ncbi.nlm.nih.gov/3768250/) · [doi:10.1111/j.1365-2125.1986.tb02901.x](https://doi.org/10.1111/j.1365-2125.1986.tb02901.x)  
   *Drugs:* acetaminophen. *Why:* Gestational age, n, and absolute CL/F and V/F (PMC scan, text not returned).

27. Monfort A et al. Br J Clin Pharmacol 2024;90(11):2849-2860 (ConcePTION perinatal sertraline popPK, 38 women) [PMID 39030897](https://pubmed.ncbi.nlm.nih.gov/39030897/) · [doi:10.1111/bcp.16177](https://doi.org/10.1111/bcp.16177)  
   *Drugs:* sertraline. *Why:* Perinatal popPK model; abstract reports CL reduced 42% in CYP2C19 poor metabolizers but gives no explicit gestational-age covariate on CL. Full text/supplement needed to see whether a quantitative pregnancy (not genotype) clearance effect was estimated that could be mapped directly to cl1.

28. Murphy HR et al. (Cambridge) closed-loop insulin delivery in type 1 diabetes pregnancy, Diabetes Care ~2011 - the source of the ~30 min gestational delay in aspart time-to-peak cited by Goudie 2015 (no PMID found; cited from another paper)  
   *Drugs:* insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70, insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine. *Why:* Original early-vs-late pregnancy aspart time-to-peak data (citation known only from Goudie's reference to it; PMID not retrieved in this session).

29. Pennell PB, Karanam A, Meador KJ, et al. Antiseizure Medication Concentrations During Pregnancy: Results From the MONEAD Study. JAMA Neurol 2022;79:370-379 (PMC8845026) [PMID 35157004](https://pubmed.ncbi.nlm.nih.gov/35157004/) · [doi:10.1001/jamaneurol.2021.5487](https://doi.org/10.1001/jamaneurol.2021.5487)  
   *Drugs:* carbamazepine, lacosamide, zonisamide, levetiracetam. *Why:* PMC full text returned empty through the tool; need trimester-by-trimester dose-normalized concentrations (Table 2 / eTables) for every ASM, because the abstract gives only the 'up to' (maximal) decrease without saying which trimester; also the carbamazepine figure in the abstract is internally inconsistent (11.56 -> 7.97 is a 31% fall, not the stated 17.3%).

30. Petrenaite V, Ohman I, Thi Nguyen Larsen P, et al. Pregnancy-induced changes in the pharmacokinetics of oxcarbazepine/MHD (Epilepsia/Epilepsy Res 2018, as named in the task brief) (no PMID found; cited from another paper)  
   *Drugs:* eslicarbazepine. *Why:* Named in the brief from memory; not located this session. A prospective study of MHD (with enantiomers) during pregnancy would firm up the surrogate factor.

31. Philipson A, Stiernstedt G, Ehrnebo M. Clin Pharmacokinet 1987;12:136-44 [PMID 3829560](https://pubmed.ncbi.nlm.nih.gov/3829560/) · [doi:10.2165/00003088-198712020-00004](https://doi.org/10.2165/00003088-198712020-00004)  
   *Drugs:* cefazolin, cefalexin. *Why:* cefazolin: Paired during/after pregnancy study; tables give CL, V and the serum-albumin/unbound fraction for cefazolin, which would let the TOTAL clearance ratio (1.45) be split into its unbound-clearance and binding parts for this unbound model. cefalexin: Same paper as for cefazolin: the cephradine tables (oral and IV, paired) are the only direct basis for an oral cephalosporin factor.

32. Polepally AR, et al. Ann Clin Transl Neurol 2014;1:99-106 (PMC4038031) [PMID 24883336](https://pubmed.ncbi.nlm.nih.gov/24883336/) · [doi:10.1002/acn3.29](https://doi.org/10.1002/acn3.29)  
   *Drugs:* lamotrigine. *Why:* Open access (PMC4038031) but only the abstract was read here; its model equations would give the gestational-week slope (0.118 L/h/week) form for a time-varying option.

33. Popovic J, Grujic Z, Sabo A. J Clin Pharm Ther 2007;32:595-602 [PMID 18021337](https://pubmed.ncbi.nlm.nih.gov/18021337/) · [doi:10.1111/j.1365-2710.2007.00864.x](https://doi.org/10.1111/j.1365-2710.2007.00864.x)  
   *Drugs:* gentamicin. *Why:* Primary table of gentamicin (and ceftriaxone, cefazolin) CL, V, k in caesarean vs non-pregnant women; only a secondary summary was read.

34. Reimers A, et al. Epilepsy Res 2018;144:25-29 [PMID 29751353](https://pubmed.ncbi.nlm.nih.gov/29751353/) · [doi:10.1016/j.eplepsyres.2018.05.002](https://doi.org/10.1016/j.eplepsyres.2018.05.002)  
   *Drugs:* zonisamide. *Why:* Mean (not nadir) C/D by trimester; the abstract gives only the minimum.

35. Tomson T, Lindbom U, Ekqvist B, Sundqvist A. Disposition of carbamazepine and phenytoin in pregnancy. Epilepsia 1994;35:131-135 [PMID 8112235](https://pubmed.ncbi.nlm.nih.gov/8112235/) · [doi:10.1111/j.1528-1157.1994.tb02922.x](https://doi.org/10.1111/j.1528-1157.1994.tb02922.x)  
   *Drugs:* phenytoin, carbamazepine. *Why:* phenytoin: Gives the total and unbound phenytoin clearances per trimester; the abstract gives direction only. Would confirm the Vmax factor (unbound clearance rise) directly. carbamazepine: Trimester values of total and unbound carbamazepine clearance (abstract says 'slightly decreased' only).

36. Tomson T, Lindbom U, Ekqvist B, Sundqvist A. Epilepsy and pregnancy: a prospective study of seizure control in relation to free and total plasma concentrations of carbamazepine and phenytoin. Epilepsia 1994;35:122-130 [PMID 8112234](https://pubmed.ncbi.nlm.nih.gov/8112234/) · [doi:10.1111/j.1528-1157.1994.tb02921.x](https://doi.org/10.1111/j.1528-1157.1994.tb02921.x)  
   *Drugs:* phenytoin. *Why:* Number of phenytoin pregnancies, the per-trimester free and total values with SDs, and the measured free fractions (to check the 2.1-fold fu rise derived here from the ratio of means).

37. Tsuei SE et al. Clin Pharmacol Ther 1980;28:88-98 [PMID 7389259](https://pubmed.ncbi.nlm.nih.gov/7389259/) · [doi:10.1038/clpt.1980.136](https://doi.org/10.1038/clpt.1980.136)  
   *Drugs:* dexamethasone. *Why:* Only the abstract was read. The full text gives V (Vss or Varea) for pregnant and non-pregnant women and the source of the non-pregnant comparator (probably part I of the series), needed to confirm the volume factor of about 2 and whether the 2.3x clearance ratio is like-for-like (blood/plasma ratio 0.92 vs 0.81 differs).

38. Voinescu PE, Park S, Chen LQ, et al. Antiepileptic drug clearances during pregnancy and clinical implications for women with epilepsy. Neurology 2018;91:e1228-e1236 (PMC6161546) [PMID 30185446](https://pubmed.ncbi.nlm.nih.gov/30185446/) · [doi:10.1212/WNL.0000000000006240](https://doi.org/10.1212/WNL.0000000000006240)  
   *Drugs:* phenytoin, valproate, topiramate. *Why:* PMC full text returned empty through the tool; its tables give trimester clearance ratios for phenytoin (n=7) and valproate (n=5), which are not in the abstract, and the third-trimester (not just maximal) ratios for levetiracetam and topiramate.

39. Wilson CM et al. Anaesthesia 1987;42:1057-1062 [PMID 3688386](https://pubmed.ncbi.nlm.nih.gov/3688386/) · [doi:10.1111/j.1365-2044.1987.tb05168.x](https://doi.org/10.1111/j.1365-2044.1987.tb05168.x)  
   *Drugs:* midazolam. *Why:* Only IV comparison of pregnant (labour, caesarean) vs non-pregnant midazolam; abstract gives direction (lower clearance in labour) but no numbers; contradicts the CYP3A direction.

40. Yerby MS, Friel PN, McCormick K, et al. Pharmacokinetics of anticonvulsants in pregnancy: alterations in plasma protein binding. Epilepsy Res 1990;5:223-228 [PMID 2384078](https://pubmed.ncbi.nlm.nih.gov/2384078/) · [doi:10.1016/0920-1211(90)90042-t](https://doi.org/10.1016/0920-1211(90)90042-t)  
   *Drugs:* phenytoin, phenobarbital. *Why:* phenytoin: Free fraction of phenytoin by trimester (abstract says only that it rose significantly). phenobarbital: Total and free phenobarbital by trimester.

41. Yerby MS, Friel PN, McCormick K. Antiepileptic drug disposition during pregnancy. Neurology 1992;42(4 Suppl 5):12-16 [PMID 1574166](https://pubmed.ncbi.nlm.nih.gov/1574166/)  
   *Drugs:* valproate. *Why:* Total and free valproate concentrations per trimester with doses; would replace the derived 1.6 with a measured total-clearance ratio.

### Papers that could turn a recorded finding into an applied factor, or confirm that none is needed

1. Ali-Melkkila T et al. Anaesthesia 1990;45:634-637 [PMID 2400072](https://pubmed.ncbi.nlm.nih.gov/2400072/) · [doi:10.1111/j.1365-2044.1990.tb14385.x](https://doi.org/10.1111/j.1365-2044.1990.tb14385.x)  
   *Drugs:* glycopyrrolate. *Why:* Clearance and volume in parturients (abstract gives only Cmax, Tmax, t1/2 without units, AUC); needed for any comparison with Bartels 2013.

2. Any S-/R-warfarin enantiomer pharmacokinetic study in pregnant women (CYP2C9 substrate; CYP2C9 activity is reported to rise in pregnancy) (no PMID found; cited from another paper)  
   *Drugs:* warfarin. *Why:* To separate a true clearance change from the PD clotting-factor changes; none was retrievable in this session.

3. Bartels PA, Hanff LM, Mathot RA, et al. BJOG 2007;114:230-233 [PMID 17166219](https://pubmed.ncbi.nlm.nih.gov/17166219/) · [doi:10.1111/j.1471-0528.2006.01200.x](https://doi.org/10.1111/j.1471-0528.2006.01200.x)  
   *Drugs:* nicardipine. *Why:* Maternal plasma concentrations at known infusion rates (1-6.5 mg/h) in 10 preeclamptic women, for an apparent clearance.

4. Brookfield KF, Su F, Elkomy MH, Drover DR, Lyell DJ, Carvalho B. Pharmacokinetics and placental transfer of magnesium sulfate in pregnant women. Am J Obstet Gynecol 2016;214:737.e1-9 [PMID 26767791](https://pubmed.ncbi.nlm.nih.gov/26767791/) · [doi:10.1016/j.ajog.2015.12.060](https://doi.org/10.1016/j.ajog.2015.12.060)  
   *Drugs:* magnesiumSulfate. *Why:* Gives the non-preeclamptic (tocolysis/neuroprotection) pregnant clearance and its covariates. It would let the help page say how a normotensive parturient differs from the model's preeclamptic population, and whether that difference is explained by serum creatinine.

5. Chen et al. J Clin Pharmacol 2026;66:e70205 [PMID 42130310](https://pubmed.ncbi.nlm.nih.gov/42130310/) · [doi:10.1002/jcph.70205](https://doi.org/10.1002/jcph.70205)  
   *Drugs:* ceftriaxone. *Why:* Gives which observed pregnancy data the PBPK model was calibrated to and the predicted CL and V separately; would decide whether CL x 1.2 is worth applying.

6. Dailey PA et al. Anesthesiology 1984;60:569-574 [PMID 6145374](https://pubmed.ncbi.nlm.nih.gov/6145374/) · [doi:10.1097/00000542-198406000-00009](https://doi.org/10.1097/00000542-198406000-00009)  
   *Drugs:* vecuronium. *Why:* Volumes and individual curves at caesarean; could be simulated against the Vega-Villa model over 0-4 h to see whether early concentrations differ.

7. Dam M, Christiansen J, Munck O, Mygind KI. Antiepileptic drugs: metabolism in pregnancy. Clin Pharmacokinet 1979;4:53-62 [PMID 421411](https://pubmed.ncbi.nlm.nih.gov/421411/) · [doi:10.2165/00003088-197904010-00005](https://doi.org/10.2165/00003088-197904010-00005)  
   *Drugs:* phenobarbital. *Why:* Per-patient phenobarbital clearance during and after pregnancy; needed to turn the direction into a third-trimester factor.

8. Gin T et al. Br J Anaesth 1990;64:148-153 [PMID 2138487](https://pubmed.ncbi.nlm.nih.gov/2138487/) · [doi:10.1093/bja/64.2.148](https://doi.org/10.1093/bja/64.2.148)  
   *Drugs:* propofol. *Why:* Compartmental fit of the control group and the timing of samples around delivery (blood loss confound).

9. Gin T et al. Br J Anaesth 1991;67:49-53 [PMID 1859759](https://pubmed.ncbi.nlm.nih.gov/1859759/) · [doi:10.1093/bja/67.1.49](https://doi.org/10.1093/bja/67.1.49)  
   *Drugs:* propofol. *Why:* Postpartum-comparator study: individual data and blood vs plasma basis needed to judge whether 0.93 is a real null.

10. Gin T, Chan MT. Anesthesiology 1994;81:829-832 [PMID 7943833](https://pubmed.ncbi.nlm.nih.gov/7943833/) · [doi:10.1097/00000542-199410000-00009](https://doi.org/10.1097/00000542-199410000-00009)  
   *Drugs:* isoflurane, sevoflurane, desflurane, nitrousOxide. *Why:* Human first-trimester isoflurane MAC; check the methods (tetanic stimulus vs skin incision) before applying to a MAC40 defined by incision.

11. Leutritz AL et al. Br J Clin Pharmacol 2023;89(2):737-750 [PMID 36103361](https://pubmed.ncbi.nlm.nih.gov/36103361/) · [doi:10.1111/bcp.15533](https://doi.org/10.1111/bcp.15533)  
   *Drugs:* duloxetine. *Why:* Full text may contain a quantitative trimester-by-trimester dose-adjusted concentration ratio for duloxetine that would let a cl1 factor be derived; the abstract gives direction only.

12. Matsumura H, Takagi K, Seki H, et al. Hypertens Pregnancy 2014;33:93-101 [PMID 24131296](https://pubmed.ncbi.nlm.nih.gov/24131296/) · [doi:10.3109/10641955.2013.846367](https://doi.org/10.3109/10641955.2013.846367)  
   *Drugs:* nicardipine. *Why:* The full text should give the dose/maternal-plasma-concentration regression for continuous IV nicardipine in 18 preeclamptic women. Its slope is 1/CL and could be compared directly with the model's clearance.

13. Moore RG, McBride WG. Eur J Clin Pharmacol 1978;13:275-284 [PMID 668785](https://pubmed.ncbi.nlm.nih.gov/668785/) · [doi:10.1007/BF00716363](https://doi.org/10.1007/BF00716363)  
   *Drugs:* diazepam. *Why:* Only term disposition study of total diazepam; no abstract in PubMed; needed to check total CL and Vd against non-pregnant values.

14. Muller AE et al. Antimicrob Agents Chemother 2010;54:2175-81 [PMID 20176904](https://pubmed.ncbi.nlm.nih.gov/20176904/) · [doi:10.1128/AAC.01017-09](https://doi.org/10.1128/AAC.01017-09)  
   *Drugs:* clindamycin. *Why:* PMC copy (PMC2863651) returned no text; the full paper gives the per-kg values, the body weights and the comparison the authors drew, needed to judge whether 10 L/h reflects pregnancy or the sampling design.

15. Ngan Kee WD, Khaw KS, Tan PE, Ng FF, Karmakar MK. Anesthesiology 2009;111:506-512 [PMID 19672175](https://pubmed.ncbi.nlm.nih.gov/19672175/) · [doi:10.1097/ALN.0b013e3181b160a3](https://doi.org/10.1097/ALN.0b013e3181b160a3)  
   *Drugs:* phenylephrine. *Why:* The full text has maternal arterial phenylephrine concentrations at delivery together with the infusion doses. An apparent steady-state clearance (rate/Css) could be derived from it and compared with Hengstmann's 2.0 L/min in non-pregnant volunteers. This is the only route to a parturient number.

16. O'Hare MF, et al. (propranolol pharmacokinetics in pregnancy, oral; named in the brief). Citation not retrieved in this session. (no PMID found; cited from another paper)  
   *Drugs:* propranolol. *Why:* An oral study would show whether first-pass (CL/F) changes as for metoprolol. That does not affect this IV model, but would matter for an oral propranolol. The paper could not be located by PubMed search here.

17. Pacheco LD et al. Am J Perinatol 2007;24:79-82 [PMID 17260328](https://pubmed.ncbi.nlm.nih.gov/17260328/) · [doi:10.1055/s-2006-958162](https://doi.org/10.1055/s-2006-958162)  
   *Drugs:* hydrocortisone. *Why:* Narrative review of corticosteroid PK in pregnancy; its reference list may point to older tracer studies of cortisol metabolic clearance in pregnancy (Migeon/Beitins era) that PubMed searches did not surface with abstracts.

18. Palahniuk RJ, Shnider SM, Eger EI 2nd. Anesthesiology 1974;41:82-83 [PMID 4857789](https://pubmed.ncbi.nlm.nih.gov/4857789/) · [doi:10.1097/00000542-197407000-00021](https://doi.org/10.1097/00000542-197407000-00021)  
   *Drugs:* isoflurane, sevoflurane, desflurane, nitrousOxide. *Why:* Original term-pregnancy MAC reduction (ewes: halothane, methoxyflurane, isoflurane); no abstract in PubMed, figures known only from memory (about 25-40%).

19. Papini O et al. J Pharm Biomed Anal 2005;40:397-403 [PMID 16143486](https://pubmed.ncbi.nlm.nih.gov/16143486/) · [doi:10.1016/j.jpba.2005.07.021](https://doi.org/10.1016/j.jpba.2005.07.021)  
   *Drugs:* lorazepam. *Why:* Actual maternal weights (the 77.9 kg used here is back-calculated), the non-pregnant reference values the authors compared against, and whether CL/F is per kg of pregnant weight.

20. Pihlajamaki K, Kanto J, Lindberg R, Karanko M, Kiilholma P. Br J Anaesth 1990;64:556-62 [PMID 2354094](https://pubmed.ncbi.nlm.nih.gov/2354094/) · [doi:10.1093/bja/64.5.556](https://doi.org/10.1093/bja/64.5.556)  
   *Drugs:* bupivacaine. *Why:* Only the abstract was read; the tables give clearance/F and V/F for pregnant vs non-pregnant women, which would show whether total clearance is in fact unchanged.

21. Ridd MJ et al. Clin Pharmacol Ther 1989;45:506-512 [PMID 2721106](https://pubmed.ncbi.nlm.nih.gov/2721106/) · [doi:10.1038/clpt.1989.65](https://doi.org/10.1038/clpt.1989.65)  
   *Drugs:* diazepam. *Why:* Total-plasma clearance, Vss and free fraction values (abstract gives only free clearance).

22. Rubin PC, Butters L, Kelman AW, Fitzsimons C, Reid JL. Br J Clin Pharmacol 1983;15:465-470 (free in PMC: PMC1427790) [PMID 6849783](https://pubmed.ncbi.nlm.nih.gov/6849783/) · [doi:10.1111/j.1365-2125.1983.tb01531.x](https://doi.org/10.1111/j.1365-2125.1983.tb01531.x)  
   *Drugs:* labetalol. *Why:* Only the abstract was read. The full text (free in PMC) would give per-kg values and the half-lives, to confirm that per-kg IV clearance is also unchanged, since the model scales by weight.

23. Shanmugalingam R et al. Am J Obstet Gynecol 2019;221:255.e1-255.e9 [PMID 31051121](https://pubmed.ncbi.nlm.nih.gov/31051121/) · [doi:10.1016/j.ajog.2019.04.027](https://doi.org/10.1016/j.ajog.2019.04.027)  
   *Drugs:* aspirin. *Why:* Gestational age at sampling, V/F of salicylate, whether the 44% is CL/F of SA, and the enteric-coated arm (the library's model is enteric-coated low-dose aspirin).

24. Wang X et al. J Matern Fetal Neonatal Med 2011;24:444-8 (PMC3033962) [PMID 20608802](https://pubmed.ncbi.nlm.nih.gov/20608802/) · [doi:10.3109/14767058.2010.497573](https://doi.org/10.3109/14767058.2010.497573)  
   *Drugs:* metronidazole. *Why:* Free in PMC (not retrieved this session); the tables give CL/F in L/h by trimester, which could be set against da Silva Neto's 3.22 L/h for a numeric check.
