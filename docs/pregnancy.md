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
  from such a measurement by arithmetic the drug's header gives (a group set against the
  library's own model at its age and weight, for propofol and midazolam; metoprolol's split of CL/F into clearance and
  first-pass; phenytoin's Km and Vmax and valproate's total CL/F from the rise in free
  fraction). A direction alone, a handful of women, no comparator, or a PBPK prediction alone
  is recorded as `unquantified` and not applied (lorazepam, risperidone, haloperidol, aspirin,
  fentanyl, codeine, tramadol, rocuronium, cisatracurium, heparin).
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
| propofol | 1.280 |  |  |  | Gin 1990 | moderate |
| remifentanil | 2.000 |  |  |  | Kan 1998 | moderate |
| morphine | 1.700 |  |  |  | Gerdin 1990 | high |
| methadone | 1.714 |  |  |  | Bogen 2012 | high |
| midazolam | 1.280 |  |  |  | Hebert 2008 | moderate |
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
| glycopyrrolate | 1.900 |  |  |  | Ali-Melkkila 1990 | low |
| mannitol |  |  |  | 1.5 | Deferm 2021 | low |
| citalopram | 1.320 |  |  |  | Westin 2017 | moderate |
| sertraline | 0.600 |  |  |  | Westin 2017 | moderate |
| paroxetine | 2.000 |  |  |  | Westin 2017 | moderate |
| quetiapine | 4.200 |  |  |  | Westin 2017 | moderate |
| aripiprazole | 2.100 |  |  |  | Westin 2017 | moderate |
| phenytoin |  |  |  |  | Tomson 1994 | moderate |
| valproate | 1.600 |  |  |  | Nau 1986 | low |
| phenobarbital | 1.250 |  |  |  | Dam 1979 | moderate |
| topiramate | 1.600 |  |  |  | Ohman 2009 | moderate |
| lacosamide | 1.500 |  |  |  | Pennell 2022 | moderate |
| lamotrigine | 2.300 |  |  |  | Karanam 2025 | high |
| zonisamide | 1.600 |  |  |  | Reimers 2018 | moderate |
| levetiracetam | 1.600 |  |  |  | Tomson 2007 | moderate |
| eslicarbazepine | 1.600 |  |  |  | Christensen 2006 | low |
| insulinRegular | 1.250 |  |  |  | Catalano 1998 | low |
| insulinRegularU500 | 1.250 |  |  |  | Catalano 1998 | low |
| insulinNPH | 1.250 |  |  |  | Catalano 1998 | low |
| insulinHumanMix70 | 1.250 |  |  |  | Catalano 1998 | low |
| insulinLispro | 1.250 |  |  |  | Catalano 1998 | low |
| insulinLisproMix75 | 1.250 |  |  |  | Catalano 1998 | low |
| insulinLisproMix50 | 1.250 |  |  |  | Catalano 1998 | low |
| insulinGlargine | 1.250 |  |  |  | Catalano 1998 | low |
| metformin | 1.270 | 1.33 |  |  | Liao 2020 | moderate |
| glyburide | 1.900 | 1.57 |  |  | Hebert 2009 | moderate |
| enoxaparin | 1.500 | 1.40 |  |  | Lebaudy 2008 | high |
| fondaparinux |  |  |  | 1.5 | Wu 2025 | low |
| hydrochlorothiazide |  |  |  | 1.5 | Ansari 2016 | low |
| procainamide |  |  |  | 1.5 | Ansari 2016 | low |
| digoxin |  |  |  | 1.5 | Hebert 2008 | moderate |
| digoxinIV | 1.420 |  |  |  | Hebert 2008 | low |
| metoprolol | 1.500 | 1.50 | 0.5 |  | Ryu 2016 | moderate |
| vasopressin | 5.075 |  |  |  | Davison 1993 | moderate |

Not adjusted: 16 `unquantified`, 19 `unchanged`, 87 `unknown` (mostly drugs contraindicated or
rarely used in pregnancy: the DOACs and vitamin K antagonists, the GLP-1, SGLT2 and DPP-4
drugs, the illicit-drug models, most vasoactive infusions), 3 `source_pregnant` and 9
`not_applicable` (pediatric models, azilsartan, and the inhaled anesthetics).

## Changes to existing behaviour

- **Dexamethasone** now doubles both volumes as well as the clearance (Tsuei 1980: clearance
  559 vs 243 mL/min with an unchanged terminal half-life); doubling the clearance alone halved
  the half-life, against the only intravenous data in pregnant women.
- **Midazolam** and **buprenorphine**, described by Ansari 2016 only in direction, are now
  adjusted: midazolam × 1.28 from Wilson 1987's intravenous groups (Hebert 2008's paired oral data
  give 1.45 through a well-stirred liver), buprenorphine from Zhu/Caritis 2024.
- **Propofol** (and its Schnider companion), "unchanged" in Ansari 2016, now × 1.28 from Gin 1990
  and 1991 set against the Eleveld model, which reproduces their nonpregnant women.
- **Glycopyrrolate** × 1.9 (Ali-Melkkilä 1990, against the same laboratory's nonpregnant patients).
- **Metoprolol** now also scales both volumes and Q by 1.5, so its half-lives are unchanged, as Hogstedt
  1985 measured; the bioavailability factor 0.5 is now measured, not derived.
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

1. Allegaert K et al. BMC Anesthesiol 2015;15:163 (Table 2) [PMID 26566962](https://pubmed.ncbi.nlm.nih.gov/26566962/) · [doi:10.1186/s12871-015-0144-3](https://doi.org/10.1186/s12871-015-0144-3)  
   *Drugs:* acetaminophen. *Why:* Open access, but the tool returned the text without Table 2: typical CL to sulfate, unchanged-drug CL and the 'at delivery' factor on central volume are needed to build a full delivery/nonpregnant ratio for total CL and V.

2. Delavenne X, et al. (POP-A-RIX). Thromb Haemost 2010;104:252-260 and any obstetric fondaparinux PK series (no PMID found; cited from another paper)  
   *Drugs:* fondaparinux. *Why:* No fondaparinux pregnancy PK ratio was found; a dedicated pregnant-vs-nonpregnant anti-Xa PK study (if one exists) would be needed to replace the GFR-analogy estimate.

3. Duan et al. J Pharm Sci 2024;113:1385-1394 [PMID 38176454](https://pubmed.ncbi.nlm.nih.gov/38176454/) · [doi:10.1016/j.xphs.2023.12.025](https://doi.org/10.1016/j.xphs.2023.12.025)  
   *Drugs:* levetiracetam. *Why:* popPK with pregnancy as a covariate on CL/F; the coefficient is not in the abstract.

4. Duan et al. Neurology 2026;107:e218456 [PMID 42685297](https://pubmed.ncbi.nlm.nih.gov/42685297/) · [doi:10.1212/WNL.0000000000218456](https://doi.org/10.1212/WNL.0000000000218456)  
   *Drugs:* levetiracetam. *Why:* Largest prospective cohort (228 pregnant women); per-trimester C/D tables for every ASM including lacosamide and the oxcarbazepine metabolite.

5. Fukushima Y, Yamamoto Y, Yamazaki E, et al. Change in the pharmacokinetics of lacosamide before, during, and after pregnancy. Seizure 2021;88:12-14 [PMID 33774498](https://pubmed.ncbi.nlm.nih.gov/33774498/) · [doi:10.1016/j.seizure.2021.03.011](https://doi.org/10.1016/j.seizure.2021.03.011)  
   *Drugs:* lacosamide. *Why:* No abstract in PubMed; a before/during/after case with CYP2C19 genotype.

6. Goudie RJB, Lunn D, Hovorka R, Murphy HR. Pharmacokinetics of insulin aspart in pregnant women with type 1 diabetes: every day is different. Diabetes Care 2014;37:e121-e122 [PMID 24855162](https://pubmed.ncbi.nlm.nih.gov/24855162/) · [doi:10.2337/dc13-2535](https://doi.org/10.2337/dc13-2535)  
   *Drugs:* insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70, insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine. *Why:* Letter (no abstract) reporting aspart PK in early vs late pregnancy (time-to-peak and clearance) in the same women; would firm up the absorption-delay factor.

7. Hodge AT et al. Int J Obstet Anesth 2026;66:104874 [PMID 41747364](https://pubmed.ncbi.nlm.nih.gov/41747364/) · [doi:10.1016/j.ijoa.2026.104874](https://doi.org/10.1016/j.ijoa.2026.104874)  
   *Drugs:* rocuronium, sugammadex. *Why:* rocuronium: Full two-compartment rocuronium parameter set (V1, V2, Q) and which 'volume of distribution' the abstract reports; would allow a direct comparison with a non-pregnant model. sugammadex: Full parameter set (Vc, Vp, Q), serum creatinine/CrCl of the women and whether 13.9 L is Vc or Vss; needed to confirm the GFR-only adjustment and the volume comparison.

8. Koerner M, Yerby M, Friel P, McCormick K. Valproic acid disposition and protein binding in pregnancy. Ther Drug Monit 1989;11:228-230 (no PMID found; cited from another paper)  
   *Drugs:* valproate. *Why:* Cited from memory (not retrieved this session): a direct study of valproate total and unbound disposition across pregnancy; PMID not verified.

9. Krzyzanski W et al. J Pharmacokinet Pharmacodyn 2026 (cohort population analysis, dexamethasone in parturients) [PMID 42406183](https://pubmed.ncbi.nlm.nih.gov/42406183/) · [doi:10.1007/s10928-026-10048-5](https://doi.org/10.1007/s10928-026-10048-5)  
   *Drugs:* dexamethasone. *Why:* PMC13337934 exists but was not retrieved; gives one-compartment CL and V for dexamethasone in parturient women at birth that could replace the volume inference.

10. Kulo A et al. Bosn J Basic Med Sci 2017;17:54-60 [PMID 27968707](https://pubmed.ncbi.nlm.nih.gov/27968707/) · [doi:10.17305/bjbms.2016.1515](https://doi.org/10.17305/bjbms.2016.1515)  
   *Drugs:* ketorolac. *Why:* Open-access PMC5341779 but tool returned no body text: S- and R-ketorolac CL and Vss at delivery vs postpartum/controls, to give separate factors for the S and R systems.

11. Li Y, et al. Br J Clin Pharmacol 2023;89:1152-1161 [PMID 36260320](https://pubmed.ncbi.nlm.nih.gov/36260320/) · [doi:10.1111/bcp.15572](https://doi.org/10.1111/bcp.15572)  
   *Drugs:* levetiracetam. *Why:* A popPK model fitted only in pregnant women with a trimester covariate on CL/F; Table of final estimates needed for a trimester-specific factor (abstract gives only CL/F 3.82 L/h).

12. Liao MZ, Flood Nichols SK, Ahmed M, et al. Effects of pregnancy on the pharmacokinetics of metformin. Drug Metab Dispos 2020;48:264-271 [PMID 31980499](https://pubmed.ncbi.nlm.nih.gov/31980499/) · [doi:10.1124/dmd.119.088435](https://doi.org/10.1124/dmd.119.088435)  
   *Drugs:* metformin. *Why:* PMC7076518 exists but the tool returned no body: need gestational age at study, the CL/F and V/F means (L/h, L) at 500 mg, and the estimated F, to confirm the 1.27/1.33 ratios and decide whether F should be modelled separately.

13. Monfort A et al. Br J Clin Pharmacol 2024;90(11):2849-2860 (ConcePTION perinatal sertraline popPK, 38 women) [PMID 39030897](https://pubmed.ncbi.nlm.nih.gov/39030897/) · [doi:10.1111/bcp.16177](https://doi.org/10.1111/bcp.16177)  
   *Drugs:* sertraline. *Why:* Perinatal popPK model; abstract reports CL reduced 42% in CYP2C19 poor metabolizers but gives no explicit gestational-age covariate on CL. Full text/supplement needed to see whether a quantitative pregnancy (not genotype) clearance effect was estimated that could be mapped directly to cl1.

14. Murphy HR et al. (Cambridge) closed-loop insulin delivery in type 1 diabetes pregnancy, Diabetes Care ~2011 - the source of the ~30 min gestational delay in aspart time-to-peak cited by Goudie 2015 (no PMID found; cited from another paper)  
   *Drugs:* insulinRegular, insulinRegularU500, insulinNPH, insulinHumanMix70, insulinLispro, insulinLisproMix75, insulinLisproMix50, insulinGlargine. *Why:* Original early-vs-late pregnancy aspart time-to-peak data (citation known only from Goudie's reference to it; PMID not retrieved in this session).

15. Pennell PB, Karanam A, Meador KJ, et al. Antiseizure Medication Concentrations During Pregnancy: Results From the MONEAD Study. JAMA Neurol 2022;79:370-379 (PMC8845026) [PMID 35157004](https://pubmed.ncbi.nlm.nih.gov/35157004/) · [doi:10.1001/jamaneurol.2021.5487](https://doi.org/10.1001/jamaneurol.2021.5487)  
   *Drugs:* carbamazepine, lacosamide, zonisamide, levetiracetam. *Why:* PMC full text returned empty through the tool; need trimester-by-trimester dose-normalized concentrations (Table 2 / eTables) for every ASM, because the abstract gives only the 'up to' (maximal) decrease without saying which trimester; also the carbamazepine figure in the abstract is internally inconsistent (11.56 -> 7.97 is a 31% fall, not the stated 17.3%).

16. Petrenaite V, Ohman I, Thi Nguyen Larsen P, et al. Pregnancy-induced changes in the pharmacokinetics of oxcarbazepine/MHD (Epilepsia/Epilepsy Res 2018, as named in the task brief) (no PMID found; cited from another paper)  
   *Drugs:* eslicarbazepine. *Why:* Named in the brief from memory; not located this session. A prospective study of MHD (with enantiomers) during pregnancy would firm up the surrogate factor.

17. Polepally AR, et al. Ann Clin Transl Neurol 2014;1:99-106 (PMC4038031) [PMID 24883336](https://pubmed.ncbi.nlm.nih.gov/24883336/) · [doi:10.1002/acn3.29](https://doi.org/10.1002/acn3.29)  
   *Drugs:* lamotrigine. *Why:* Open access (PMC4038031) but only the abstract was read here; its model equations would give the gestational-week slope (0.118 L/h/week) form for a time-varying option.

18. Popovic J, Grujic Z, Sabo A. J Clin Pharm Ther 2007;32:595-602 [PMID 18021337](https://pubmed.ncbi.nlm.nih.gov/18021337/) · [doi:10.1111/j.1365-2710.2007.00864.x](https://doi.org/10.1111/j.1365-2710.2007.00864.x)  
   *Drugs:* gentamicin. *Why:* Primary table of gentamicin (and ceftriaxone, cefazolin) CL, V, k in caesarean vs non-pregnant women; only a secondary summary was read.

19. Reimers A, et al. Epilepsy Res 2018;144:25-29 [PMID 29751353](https://pubmed.ncbi.nlm.nih.gov/29751353/) · [doi:10.1016/j.eplepsyres.2018.05.002](https://doi.org/10.1016/j.eplepsyres.2018.05.002)  
   *Drugs:* zonisamide. *Why:* Mean (not nadir) C/D by trimester; the abstract gives only the minimum.

20. Tomson T, Lindbom U, Ekqvist B, Sundqvist A. Disposition of carbamazepine and phenytoin in pregnancy. Epilepsia 1994;35:131-135 [PMID 8112235](https://pubmed.ncbi.nlm.nih.gov/8112235/) · [doi:10.1111/j.1528-1157.1994.tb02922.x](https://doi.org/10.1111/j.1528-1157.1994.tb02922.x)  
   *Drugs:* phenytoin, carbamazepine. *Why:* phenytoin: Gives the total and unbound phenytoin clearances per trimester; the abstract gives direction only. Would confirm the Vmax factor (unbound clearance rise) directly. carbamazepine: Trimester values of total and unbound carbamazepine clearance (abstract says 'slightly decreased' only).

21. Voinescu PE, Park S, Chen LQ, et al. Antiepileptic drug clearances during pregnancy and clinical implications for women with epilepsy. Neurology 2018;91:e1228-e1236 (PMC6161546) [PMID 30185446](https://pubmed.ncbi.nlm.nih.gov/30185446/) · [doi:10.1212/WNL.0000000000006240](https://doi.org/10.1212/WNL.0000000000006240)  
   *Drugs:* phenytoin, valproate, topiramate. *Why:* PMC full text returned empty through the tool; its tables give trimester clearance ratios for phenytoin (n=7) and valproate (n=5), which are not in the abstract, and the third-trimester (not just maximal) ratios for levetiracetam and topiramate.

22. Yerby MS, Friel PN, McCormick K. Antiepileptic drug disposition during pregnancy. Neurology 1992;42(4 Suppl 5):12-16 [PMID 1574166](https://pubmed.ncbi.nlm.nih.gov/1574166/)  
   *Drugs:* valproate. *Why:* Total and free valproate concentrations per trimester with doses; would replace the derived 1.6 with a measured total-clearance ratio.

### Papers that could turn a recorded finding into an applied factor, or confirm that none is needed

1. Any S-/R-warfarin enantiomer pharmacokinetic study in pregnant women (CYP2C9 substrate; CYP2C9 activity is reported to rise in pregnancy) (no PMID found; cited from another paper)  
   *Drugs:* warfarin. *Why:* To separate a true clearance change from the PD clotting-factor changes; none was retrievable in this session.

2. Bartels PA, Hanff LM, Mathot RA, et al. BJOG 2007;114:230-233 [PMID 17166219](https://pubmed.ncbi.nlm.nih.gov/17166219/) · [doi:10.1111/j.1471-0528.2006.01200.x](https://doi.org/10.1111/j.1471-0528.2006.01200.x)  
   *Drugs:* nicardipine. *Why:* Maternal plasma concentrations at known infusion rates (1-6.5 mg/h) in 10 preeclamptic women, for an apparent clearance.

3. Brookfield KF, Su F, Elkomy MH, Drover DR, Lyell DJ, Carvalho B. Pharmacokinetics and placental transfer of magnesium sulfate in pregnant women. Am J Obstet Gynecol 2016;214:737.e1-9 [PMID 26767791](https://pubmed.ncbi.nlm.nih.gov/26767791/) · [doi:10.1016/j.ajog.2015.12.060](https://doi.org/10.1016/j.ajog.2015.12.060)  
   *Drugs:* magnesiumSulfate. *Why:* Gives the non-preeclamptic (tocolysis/neuroprotection) pregnant clearance and its covariates. It would let the help page say how a normotensive parturient differs from the model's preeclamptic population, and whether that difference is explained by serum creatinine.

4. Chen et al. J Clin Pharmacol 2026;66:e70205 [PMID 42130310](https://pubmed.ncbi.nlm.nih.gov/42130310/) · [doi:10.1002/jcph.70205](https://doi.org/10.1002/jcph.70205)  
   *Drugs:* ceftriaxone. *Why:* Gives which observed pregnancy data the PBPK model was calibrated to and the predicted CL and V separately; would decide whether CL x 1.2 is worth applying.

5. Leutritz AL et al. Br J Clin Pharmacol 2023;89(2):737-750 [PMID 36103361](https://pubmed.ncbi.nlm.nih.gov/36103361/) · [doi:10.1111/bcp.15533](https://doi.org/10.1111/bcp.15533)  
   *Drugs:* duloxetine. *Why:* Full text may contain a quantitative trimester-by-trimester dose-adjusted concentration ratio for duloxetine that would let a cl1 factor be derived; the abstract gives direction only.

6. O'Hare MF, et al. (propranolol pharmacokinetics in pregnancy, oral; named in the brief). Citation not retrieved in this session. (no PMID found; cited from another paper)  
   *Drugs:* propranolol. *Why:* An oral study would show whether first-pass (CL/F) changes as for metoprolol. That does not affect this IV model, but would matter for an oral propranolol. The paper could not be located by PubMed search here.

7. Shanmugalingam R et al. Am J Obstet Gynecol 2019;221:255.e1-255.e9 [PMID 31051121](https://pubmed.ncbi.nlm.nih.gov/31051121/) · [doi:10.1016/j.ajog.2019.04.027](https://doi.org/10.1016/j.ajog.2019.04.027)  
   *Drugs:* aspirin. *Why:* Gestational age at sampling, V/F of salicylate, whether the 44% is CL/F of SA, and the enteric-coated arm (the library's model is enteric-coated low-dose aspirin).

### Received from the Stanford library

- **Dam M, Christiansen J, Munck O, Mygind KI.** Antiepileptic drugs: metabolism in pregnancy. *Clin
  Pharmacokinet* 1979;4:53-62 (PMID 421411), received and read 2026-10-19. Table III gave the
  per-woman phenobarbital clearances: phenobarbital moved from `unquantified` to `applied`, clearance
  × 1.25 (the median of seven late-pregnancy over late-postpartum ratios). Table II's dose-adjusted
  phenytoin clearances give a median ratio of 1.37, which the phenytoin Km and Vmax factors (fitted to
  Tomson 1994) predict exactly at 15 mg/L; `test-drugs-phenytoin.R` now checks it. The carbamazepine
  data are figures only, in 3 women.
- **Moore RG, McBride WG.** The disposition kinetics of diazepam in pregnant women at parturition.
  *Eur J Clin Pharmacol* 1978;13:275-284 (PMID 668785), received and read 2026-10-19. Total plasma
  clearance at term 28.2 mL/min (n = 14) against 26-35 in nonpregnant series: diazepam stays
  `unchanged`, now from the full text. The longer half-life at term is postnatal redistribution, not
  elimination.
- **Gin T, Gregory MA, Chan K, Buckley T, Oh TE.** *Br J Anaesth* 1990;64:148-153 (PMID 2138487) and
  **Gin T, Yau G, Jong W, Tan P, Leung RKW, Chan K.** *Br J Anaesth* 1991;67:49-53 (PMID 1859759):
  propofol moved to `applied`, clearance × 1.28 on Eleveld and on the Schnider companion. The Eleveld
  model reproduces the 1990 nonpregnant women; the caesarean groups cleared 1.39 and 1.15 times the
  model at their own age and weight, and women 2-3 days postpartum 1.19 times.
- **Pihlajamäki K et al.** *Br J Anaesth* 1990;64:556-562 (PMID 2354094): epidural bupivacaine at term;
  no significant difference in normalised AUC or Cmax, a truncated AUC: bupivacaine stays `unchanged`.
- **Yerby MS et al.** *Epilepsy Res* 1990;5:223-228 (PMID 2384078): phenobarbital total clearance
  in the third trimester 1.25 times baseline (Table VII), the same as Dam 1979; phenytoin unbound
  clearance 1.40 times (the model gives 1.27) and free fraction 1.31 times (smaller than the 2.1
  derived from Tomson); carbamazepine not significantly changed.
- **Ali-Melkkilä T et al.** *Anaesthesia* 1990;45:634-637 (PMID 2400072): glycopyrrolate moved to
  `applied`, clearance × 1.9 (1.12 against the same laboratory's 0.58 L/h/kg, both intramuscular).
- **Ridd MJ et al.** *Clin Pharmacol Ther* 1989;45:506-512 (PMID 2721106): diazepam free clearance
  unchanged at caesarean; total clearance swings with the surgical rise in fatty acids. Stays
  `unchanged`.
- **Wilson CM et al.** *Anaesthesia* 1987;42:1057-1062 (PMID 3688386): midazolam revised from 1.45
  (derived from oral data) to 1.28, from the women awaiting caesarean section against the
  gynaecological women, each against the model at its own age and weight; labour is lower (0.91).
- **Miners JO et al.** *Br J Clin Pharmacol* 1986;22:359-362 (PMID 3768250): oral paracetamol CL/F
  1.58 times and V/F 1.14 times in the third trimester, supporting acetaminophen's × 1.4 and × 1.14.
- **Hogstedt S et al.** *Clin Pharmacol Ther* 1985;37:688-692 (PMID 4006368): metoprolol IV and oral in
  the same 5 women: bioavailability 0.21 vs 0.42 (the model's × 0.5, now measured), systemic
  clearance 1.38 vs 0.65 L/min, identical half-lives; volumes and Q now × 1.5 with the clearance.
- **Philipson A et al.** *Clin Pharmacokinet* 1987;12:136-144 (PMID 3829560): cephradine in the
  women at 24 weeks or later 1.50 times (cefalexin's × 1.5 confirmed), oral bioavailability
  unchanged; cefazolin total clearance 1.57 times, half-life 0.65 times.
- **Lazebnik N et al.** *Postgrad Med J* 1985;61:979-981 (PMID 4070116): creatinine clearance 1.50
  times at 28-34 weeks, gentamicin half-life 93.5 vs 158.9 min; gentamicin's × 1.5 confirmed.
- **Palahniuk RJ, Shnider SM, Eger EI.** *Anesthesiology* 1974;41:82-83 (PMID 4857789): MAC 25-40%
  lower in pregnant ewes near term (isoflurane 1.01 vs 1.58%); recorded on the gas pages, not applied.
- **Dailey PA et al.** *Anesthesiology* 1984;60:569-574 (PMID 6145374): vecuronium clearance 6.4
  mL/kg/min at term caesarean, against 5.7-6.2 from this library's model for a nonpregnant woman:
  vecuronium moved from `unquantified` to `unchanged`.
- **Luxford AM, Kellaway GS.** *Eur J Clin Pharmacol* 1983;25:117-121 (PMID 6617713): digoxin renal
  clearance 1.21 times and creatinine clearance 1.39 times in the third trimester, the two rising
  together (no change in tubular handling); confirms digoxin's GFR-term adjustment.
- **Tsuei SE et al.** *Clin Pharmacol Ther* 1980;28:88-98 (PMID 7389259): dexamethasone clearance
  559 vs 243 mL/min at term with an unchanged half-life, so the volume doubles with the clearance,
  confirming both of dexamethasone's factors.
- **Rubin PC et al.** *Br J Clin Pharmacol* 1983;15:465-470 (PMID 6849783): intravenous labetalol
  clearance 2.16 (pregnant) vs 2.35 (postpartum) vs 1.96 (controls) L/min, no difference: labetalol
  stays `unchanged` for its IV model.
- **Tomson T et al.** *Epilepsia* 1994;35:122-130 (PMID 8112234): phenytoin free fraction rose from
  6.1% to 13.2% in the third trimester (factor 2.15, the Km factor uses 2.10); carbamazepine free
  concentration essentially unchanged. Confirms both.
- **Gin T, Chan MTV.** *Anesthesiology* 1994;81:829-832 (PMID 7943833): isoflurane MAC 28% lower at
  8-12 weeks (0.775 vs 1.075%) by a tetanic stimulus; recorded on the gas pages, not applied (MAC40
  in the library is defined by incision).
- **Catalano PM, Drago NM, Amini SB.** *Diabetes Care* 1998;21:403-408 (PMID 9540023): the insulin
  clearance factor is corrected from 1.19 to 1.25 (the metabolic clearance rate was 422 mL/m2/min
  pregravid and 526 at term; the earlier value used a wrong pregravid figure). It is per m2, and
  Tham's CL/F is not size-scaled, so the absolute ratio is nearer 1.35; 1.25 is a measured lower bound.
- **Papini O et al.** *J Pharm Biomed Anal* 2005;40:397-403 (PMID 16143486): lorazepam CL/F 2.6
  mL/min/kg, V/F 178.8 L at term, no control group: stays `unquantified`, now from the full text.
- **Charles B et al.** *Ther Drug Monit* 2006;28:67-72 (PMID 16418696): metformin CL/F 28 L/h/70 kg in
  late pregnancy, judged unchanged against historical nonpregnant data; kept as the dissent, not
  followed over Liao's direct comparison. Metformin stays x1.27.
- **Bartels P et al.** *BJOG* 2007;114:230-233 (PMID 17166219): nicardipine maternal levels in
  pre-eclampsia too sparse and non-steady-state to yield a clearance; stays `unknown`.
 - **Popović J, Grujić Z, Sabo A.** *J Clin Pharm Ther* 2007;32:595-602 (PMID 18021337): the primary
   caesarean-vs-non-pregnant table. Cefazolin unchanged (ke 0.110 vs 0.111/h); gentamicin ke 1.29
   times but total clearance only 5% higher (volume lower); ceftriaxone clearance halved. Confirms
   cefazolin's GFR term, gentamicin's GFR term, and ceftriaxone's `unchanged` classification.
 - **Pacheco LD et al.** *Am J Perinatol* 2007;24:79-82 (PMID 17260328): a narrative corticosteroid
   review; confirms no quantified hydrocortisone pregnancy PK exists and restates the
   betamethasone/dexamethasone pattern (clearance and volume both rise, half-life unchanged).
 - **Hebert MF et al.** *Clin Pharmacol Ther* 2008;84:248-253 (PMID 18288078): the CYP3A/P-gp probe
   study. Midazolam total CL/F 2.1 times (unbound 1.72), half-life unchanged; digoxin total renal
   clearance 181 vs 115 mL/min, 1.57 times (unbound 1.49; the 163 vs 125 mL/min once recorded here as
   renal digoxin clearance are the creatinine clearances, 1.30 times; corrected 2026-10-24 after an
   adversarial review and confirmed against Table 2 of the paper 2026-10-27), half-life unchanged. Confirms midazolam's x1.28 (IV) and digoxin's GFR terms;
   digoxinIV's factor became 1.42.
 - **Allegaert K et al.** *Am J Obstet Gynecol* 2009;200:170.e1-7 (PMID 19006783): cefazolin popPK in
   82 pregnancies, CL 7.44 L/h (about twice healthy young adults), almost exclusively renal, no
   gestational-age trend; supports the renal (GFR) mechanism for cefazolin.
 - **Hebert MF et al.** *Clin Pharmacol Ther* 2009;85:607-614 (PMID 19295505): glyburide in GDM, CL/F
   1.9 times and V/F 1.57 times (binding and absorption unchanged); confirms glyburide's factors.
 - **Ngan Kee WD et al.** *Anesthesiology* 2009;111:506-512 (PMID 19672175): phenylephrine placental
   transfer; a titrated infusion with a single maternal concentration yields no clearance; phenylephrine
   stays `unknown`.
 - **Eyal S et al.** *Drug Metab Dispos* 2010;38:833-840 (PMID 20118196): metformin renal clearance
   1.31 times (late) but apparent oral CL/F only +11% (N.S.) and V/F unchanged; Liao's x1.27 is at the
   upper end of the evidence. Metformin stays x1.27/1.33.
 - **Muller AE et al.** *Antimicrob Agents Chemother* 2010;54:2175-2181 (PMID 20176904): clindamycin in
   7 peripartum women, CL 10.0 L/h with an implausible Vss (6320 L); no within-study non-pregnant
   comparison. Clindamycin stays `unquantified`.
 - **Wang X et al.** *J Matern Fetal Neonatal Med* 2011;24:444-448 (PMID 20608802): metronidazole CL/F,
   Vdss/F and half-life did not differ across trimesters and were in the nonpregnant range; metronidazole
   stays `unchanged`.
 - **Kulo A et al.** *Int J Obstet Anesth* 2012;21:334-338 (PMID 22901775): IV ketorolac at caesarean,
   absolute CL 3.92 vs 1.82 L/h against controls (BSA 2.11 vs 1.07 L/h/m2); confirms ketorolac's x1.5.
 - **Kulo A et al.** *Br J Clin Pharmacol* 2013;75:850-860 (PMID 22845052): paracetamol total CL 21.1 vs
   11.7 L/h delivery-vs-postpartum (disproportional glucuronidation); the volume increase could not be
   estimated reliably. Supports acetaminophen's direction; the applied factor stays below the paired ratio.
 - **Kulo A et al.** *Eur Rev Med Pharmacol Sci* 2014;18:599-604 (PMID 24668698): IV paracetamol CL +80%
   at delivery (11.7 vs 6.4 L/h/m2), distribution volume 56 vs 46 L but 0.69 vs 0.74 L/kg (unchanged per
   kg); supports acetaminophen's x1.4 clearance and modest x1.14 volume.
 - **Matsumura H et al.** *Hypertens Pregnancy* 2014;33:93-101 (PMID 24131296): nicardipine apparent
   clearance (dose/Css) 678 vs 756 mL/h/kg delivery-vs-postpartum, overlapping healthy adults; a crude
   estimate in preeclampsia, so nicardipine stays `unknown`.
 - **Elkomy MH et al.** *Antimicrob Agents Chemother* 2014;58:3504-3513 (PMID 24733461): cefazolin in 20
   parturients, one-compartment, V 9.44 L, pre- vs post-delivery clearance 7.18 vs 4.12 L/h (1.74);
   confirms cefazolin's higher clearance in late pregnancy.
