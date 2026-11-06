# -----------------------------------------------------------------------------
# Build inst/extdata/chemotherapySources.csv, chemotherapyRegistry.csv and
# chemotherapyRelations.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-05, at the request of Steven L. Shafer, who
# supplied "Chemotherapy pharmacokinetics for stanpumpR: final
# recommendations after audit" (a ChatGPT/Gemini handoff dated 2026-11-04,
# itself a correction of an earlier Gemini/"Argon" audit) and asked that it be
# audited and incorporated.  docs/chemotherapy.md records that audit: what was
# confirmed, what was wrong, and what was decided.
#
# NO CHEMOTHERAPY DRUG IS IN THE LIBRARY.  Every model here is a candidate.
# The handoff itself asks that none be implemented until its equations,
# units, covariates, observation and population have been read in a primary
# document, and the repository asks the same (AGENTS.md: a parameter is read
# from the PDF, never from a text rendering, a summary or another model's
# output).  In this environment the papers could not be read that way: none
# is in the stanpumpRPapers archive, the NCBI and publisher sites are
# blocked by the network policy, and the PubMed Central text the PubMed tool
# returns omits every table and equation, which is where a population model
# prints its parameters.  So every row is `needs_full_text` or
# `no_source_identified`, and each says what to obtain.
#
# THREE TABLES
# ============
# chemotherapySources.csv    one row per document.  `kind`: primary_study,
#                            guideline, regulatory, label.  The PubMed rows'
#                            first author, year, journal, pages, DOI and
#                            PMCID are PubMed's own (checked 2026-11-05 for
#                            all 89 PMIDs; the year is the print year, which
#                            for twelve records is a year after PubMed's
#                            e-pub date).  `review_depth`, the furthest this
#                            environment read the document:
#
#   record_checked      the bibliographic record only (labels, guidelines
#                       outside PubMed, regulatory guidance)
#   abstract_screened   the PubMed abstract
#   full_text_partial   the PubMed Central text, without its tables and
#                       equations: enough to know the structure, not the
#                       parameters
#
#                            (`full_text_read` and `parameters_verified` are
#                            reserved for a document read in full, as a PDF.)
#
# chemotherapyRegistry.csv   one row per drug, product and candidate source
#                            model.  Readiness:
#
#   implemented           in the library as a selectable drug (none yet)
#   complete_not_implemented
#                         read in full and checked, not offered (none yet)
#   needs_full_text       a candidate model exists; the numbers that fix it
#                         were not read here
#   needs_engine          read in full; a mechanism the engine lacks (none
#                         yet: no paper has been read in full)
#   no_source_identified  this review found no source model for the product.
#                         Not a claim that none exists.
#
#                            `engine` is what the abstract's structure would
#                            need, judged against the engines as they stand
#                            (AGENTS.md; docs/chemotherapy.md, section 4):
#
#   linear                a linear 1-3 compartment model, with or without
#                         first-order absorption: the closed-form engines
#   linear_metabolite     linear parent with one linear metabolite: the
#                         one-level metabolite interface, if its formation
#                         is identifiable as the library requires
#   michaelis_menten      saturable elimination with at most one linear
#                         peripheral compartment, and optionally saturable
#                         distribution to an amount compartment or rapid
#                         target binding: advanceMichaelisMenten()
#   new_engine            a mechanism no engine carries: a metabolite
#                         cascade, enzyme turnover (autoinduction), saturable
#                         tissue binding or distribution in three
#                         compartments, enterohepatic recirculation, release
#                         from a carrier, intracellular formation, or a
#                         time-varying clearance with its own state
#   unknown               the abstract does not say enough
#
# chemotherapyRelations.csv  which document supports which drug (or
#                            "global") and model, and in what role:
#                            structural_pk, covariate, external_validation,
#                            interaction_context, clinical_outcome_trial,
#                            binding_exposure_toxicity, dosing_formula,
#                            dosing_guideline, target_guideline,
#                            management_guideline (a rescue or monitoring
#                            threshold, not a therapeutic range),
#                            pharmacogenetic_guideline, product_label,
#                            regulatory_method, regulatory_safety,
#                            mechanistic_context.  A P-prefixed id is not by
#                            itself a structural model (P01, P11, P22 are
#                            not).
#
# Run with: Rscript data-raw/chemotherapyRegistry.R
# Checked by tests/testthat/test-chemotherapy-registry.R.
# -----------------------------------------------------------------------------

READINESS <- c("implemented", "complete_not_implemented", "needs_full_text",
               "needs_engine", "no_source_identified")
REVIEW    <- c("record_checked", "abstract_screened", "full_text_partial",
               "full_text_read", "parameters_verified")
ENGINE    <- c("linear", "linear_metabolite", "michaelis_menten", "new_engine",
               "unknown")
ROLES     <- c("structural_pk", "covariate", "external_validation",
               "interaction_context", "clinical_outcome_trial",
               "binding_exposure_toxicity", "dosing_formula", "dosing_guideline",
               "target_guideline", "management_guideline",
               "pharmacogenetic_guideline", "product_label", "regulatory_method",
               "regulatory_safety", "mechanistic_context")

# --- Sources -----------------------------------------------------------------
# The PubMed records, as PubMed gives them (title, first author, print year,
# journal, volume and pages, DOI, PMCID).  The ids are the handoff's, with its
# "also screen" pair for paediatric cisplatin named N01b and N01c.
pubmedSources <- utils::read.delim(encoding = "UTF-8", text = "
P01	2681557	Calvert	1989	J Clin Oncol	7(11):1748-56	10.1200/JCO.1989.7.11.1748		Carboplatin dosage: prospective evaluation of a simple formula based on renal function.
P02	7752255	Chatelut	1995	J Natl Cancer Inst	87(8):573-80	10.1093/jnci/87.8.573		Prediction of carboplatin clearance from standard morphological and biological patient characteristics.
P03	8541105	Peng	1995	Eur J Cancer	31A(11):1804-10	10.1016/0959-8049(95)00382-s		Comparison of methods for the estimation of carboplatin pharmacokinetics in paediatric cancer patients.
P04	15151521	Urien	2004	Br J Clin Pharmacol	57(6):756-63	10.1111/j.1365-2125.2004.02082.x	PMC1884526	Population pharmacokinetics of total and unbound plasma cisplatin in adult patients.
P05	39711135	Thompson	2025	J Clin Pharmacol	65(6):763-778	10.1002/jcph.6177	PMC12116245	Population Pharmacokinetic Model of Platinum Disposition in Cancer Patients Receiving Cisplatin and Randomized to 5-HTAntagonist Antiemetic Drugs.
P06	16306851	de Jonge	2005	Ther Drug Monit	27(6):756-65	10.1097/01.ftd.0000177224.19294.92		Population pharmacokinetics of cyclophosphamide and its metabolites 4-hydroxycyclophosphamide, 2-dechloroethylcyclophosphamide, and phosphoramide mustard in a high-dose combination with Thiotepa and Carboplatin.
P07	10594468	Hassan	1999	Br J Clin Pharmacol	48(5):669-77	10.1046/j.1365-2125.1999.00090.x	PMC2014348	A mechanism-based pharmacokinetic-enzyme model for cyclophosphamide autoinduction in breast cancer patients.
P08	11014408	Terret	2000	Clin Pharmacol Ther	68(3):270-9	10.1067/mcp.2000.109352		Dose and time dependencies of 5-fluorouracil pharmacokinetics.
P09	42168702	Tan	2026	Clin Pharmacokinet	65(7):1027-1042	10.1007/s40262-026-01645-1	PMC13356066	Advancing Precision Dosing of 5-FU: Population PK Model Development, Limited Sampling Strategies, and Fit-for-Use Application.
P10	42283826	Briki	2026	Cancer Chemother Pharmacol	96(1):63	10.1007/s00280-026-04885-x	PMC13661680	Model-based meta-analysis of individual patient data for the characterization of intravenous 5-fluorouracil population pharmacokinetics.
P11	18445839	Gamelin	2008	J Clin Oncol	26(13):2099-105	10.1200/JCO.2007.13.3934		Individual fluorouracil dose adjustment based on pharmacokinetic follow-up compared with conventional dosage: results of a multicenter randomized trial of patients with metastatic colorectal cancer.
P12	35998099	Ibarra	2023	Br J Clin Pharmacol	89(2):660-671	10.1111/bcp.15506		Insights from a pharmacometric analysis of HDMTX in adults with cancer: Clinically relevant covariates for application in precision dosing.
P13	42174327	Blackman	2026	Eur J Clin Pharmacol	82(6)	10.1007/s00228-026-04080-0	PMC13197298	Development and validation of high-dose methotrexate population pharmacokinetic models to inform clinical decisions on dosing.
P14	38081138	Chen	2024	J Clin Pharmacol	64(4):437-448	10.1002/jcph.2392		External Evaluation of Population Pharmacokinetic Models for High-Dose Methotrexate in Adult Patients with Hematological Tumors.
P15	23314734	Kontny	2013	Cancer Chemother Pharmacol	71(3):749-63	10.1007/s00280-013-2069-1		Population pharmacokinetics of doxorubicin: establishment of a NONMEM model for adults and children older than 3 years.
P16	27447545	Pérez-Blanco	2016	Br J Clin Pharmacol	82(6):1517-1527	10.1111/bcp.13070	PMC5099548	Population pharmacokinetics of doxorubicin and doxorubicinol in patients diagnosed with non-Hodgkin's lymphoma.
P17	16609028	Joerger	2006	Clin Cancer Res	12(7 Pt 1):2150-7	10.1158/1078-0432.CCR-05-2069		Quantitative effect of gender, age, liver function, and body size on the population pharmacokinetics of Paclitaxel in patients with solid tumors.
P18	35986122	Nijstad	2022	Pharm Res	39(10):2487-2495	10.1007/s11095-022-03364-1	PMC9556337	A Population Pharmacokinetic Modelling Approach to Unravel the Complex Pharmacokinetics of Vincristine in Children.
P19	12496745	Klein	2002	Clin Pharmacol Ther	72(6):638-47	10.1067/mcp.2002.129502		Population pharmacokinetic model for irinotecan and two of its metabolites, SN-38 and SN-38 glucuronide.
P20	30610273	Oyaga-Iriarte	2019	Eur J Clin Pharmacol	75(4):529-542	10.1007/s00228-018-02609-6		Population pharmacokinetic model of irinotecan and its metabolites in patients with metastatic colorectal cancer.
P21	9443625	Nguyen	1998	Cancer Chemother Pharmacol	41(2):125-32	10.1007/s002800050718		Population pharmacokinetics of total and unbound etoposide.
P22	8558207	Joel	1996	J Clin Oncol	14(1):257-67	10.1200/JCO.1996.14.1.257		Predicting etoposide toxicity: relationship to organ function and protein binding.
P23	24218510	McCune	2014	Clin Cancer Res	20(3):754-63	10.1158/1078-0432.CCR-13-1960	PMC3946385	Busulfan in infant to adult hematopoietic cell transplant recipients: a population pharmacokinetic model for initial and Bayesian dose personalization.
P24	8875345	Bruno	1996	J Pharmacokinet Biopharm	24(2):153-72	10.1007/BF02353487		A population pharmacokinetic model for docetaxel (Taxotere): model building and validation.
P25	9440742	Bruno	1998	J Clin Oncol	16(1):187-96	10.1200/JCO.1998.16.1.187		Population pharmacokinetics/pharmacodynamics of docetaxel in phase II studies in patients with cancer.
G01	29923599	Beumer	2019	Clin Pharmacol Ther	105(3):598-613	10.1002/cpt.1124	PMC6309286	Therapeutic Drug Monitoring in Oncology: International Association of Therapeutic Drug Monitoring and Clinical Toxicology Recommendations for 5-Fluorouracil Therapy.
G02	38513383	Hertz	2024	Eur J Cancer	202:114024	10.1016/j.ejca.2024.114024	PMC11053297	Paclitaxel therapeutic drug monitoring - International association of therapeutic drug monitoring and clinical toxicology recommendations.
G03	29079637	Ramsey	2018	Oncologist	23(1):52-61	10.1634/theoncologist.2017-0243	PMC5759822	Consensus Guideline for Use of Glucarpidase in Patients with High-Dose Methotrexate Induced Acute Kidney Injury and Delayed Methotrexate Clearance.
G04	39356310	Bielack	2024	J Cancer Res Clin Oncol	150(10):441	10.1007/s00432-024-05945-6	PMC11446969	A European consensus recommendation on the management of delayed methotrexate elimination: supportive measures, leucovorin rescue and glucarpidase treatment.
G05	39271948	Domingos	2024	Bone Marrow Transplant	59(12):1641-1653	10.1038/s41409-024-02413-0		A practical guide to therapeutic drug monitoring in busulfan: recommendations from the Pharmacist Committee of the European Society for Blood and Marrow Transplantation (EBMT).
G07	33939491	Griggs	2021	J Clin Oncol	39(18):2037-2048	10.1200/JCO.21.00471		Appropriate Systemic Therapy Dosing for Obese Adult Patients With Cancer: ASCO Guideline Update.
G08	29152729	Amstutz	2018	Clin Pharmacol Ther	103(2):210-216	10.1002/cpt.911	PMC5760397	Clinical Pharmacogenetics Implementation Consortium (CPIC) Guideline for Dihydropyrimidine Dehydrogenase Genotype and Fluoropyrimidine Dosing: 2017 Update.
G10	36443464	Hulshof	2023	Eur J Hum Genet	31(9):982-987	10.1038/s41431-022-01243-2	PMC10474017	Dutch pharmacogenetics working group (DPWG) guideline for the gene-drug interaction between UGT1A1 and irinotecan.
G11	41618934	Maillard	2026	Clin Pharmacol Ther	119(4):916-927	10.1002/cpt.70209	PMC12997511	Clinical Pharmacogenetics Implementation Consortium (CPIC) Guideline for Thiopurine Dosing Based on TPMT and NUDT15 Genotypes: 2025 Update.
G12	26928249	van der Sluis	2016	Haematologica	101(3):279-85	10.3324/haematol.2015.137380	PMC4815719	Consensus expert recommendations for identification and management of asparaginase hypersensitivity and silent inactivation.
G13	42578884	DeAngelo	2026	Cancer	132(16):e70559	10.1002/cncr.70559	PMC13459976	Optimizing safety and efficacy of asparaginase and blinatumomab therapy in frontline acute lymphoblastic leukemia regimens in diverse health care settings-international expert consensus.
C01	17085665	Ekhart	2006	Clin Cancer Res	12(21):6502-8	10.1158/1078-0432.CCR-05-1076		Flat dosing of carboplatin is justified in adult patients with normal renal function.
C02	9025772	Duffull	1997	Cancer Chemother Pharmacol	39(4):317-26	10.1007/s002800050578		A sequential Bayesian algorithm for dose individualisation of carboplatin.
C03	19401344	Schmitt	2009	Clin Cancer Res	15(10):3633-9	10.1158/1078-0432.CCR-09-0017		A universal formula based on cystatin C to perform individual dosing of carboplatin in normal weight, underweight, and obese patients.
C04	30261554	Duong	2019	Br J Clin Pharmacol	85(1):136-146	10.1111/bcp.13774	PMC6303207	Population pharmacokinetics of carboplatin, etoposide and melphalan in children: a re-evaluation of paediatric dosing formulas for carboplatin in patients with normal or mild impairment of renal function.
C05	12647013	Delord	2003	Cancer Chemother Pharmacol	51(2):127-31	10.1007/s00280-002-0550-3		Population pharmacokinetics of oxaliplatin.
C06	14597876	Bastian	2003	Anticancer Drugs	14(10):817-24	10.1097/00001813-200311000-00007		Population pharmacokinetics of oxaliplatin in patients with metastatic cancer.
C07	15379382	de Jonge	2004	J Pharmacokinet Pharmacodyn	31(2):135-56	10.1023/b:jopa.0000034405.03895.c2		Integrated Population Pharmacokinetic Model of both cyclophosphamide and thiotepa suggesting a mutual drug-drug interaction.
C08	18027989	Joerger	2007	Clin Pharmacokinet	46(12):1051-68	10.2165/00003088-200746120-00005		Population pharmacokinetics and pharmacodynamics of doxorubicin and cyclophosphamide in breast cancer patients: a study by the EORTC-PAMM-NDDG.
C09	18927240	McCune	2009	J Clin Pharmacol	49(1):88-102	10.1177/0091270008325928	PMC2652687	Population pharmacokinetics of cyclophosphamide and metabolites in children with neuroblastoma: a report from the Children's Oncology Group.
C10	10848719	Kerbusch	2000	Br J Clin Pharmacol	49(6):555-61	10.1046/j.1365-2125.2000.00217.x	PMC2015043	Evaluation of the autoinduction of ifosfamide metabolism by a population pharmacokinetic approach using NONMEM.
C11	11488525	Kerbusch	2001	Cancer Chemother Pharmacol	48(1):53-61	10.1007/s002800100277		Population pharmacokinetics of ifosfamide and its 2- and 3-dechloroethylated and 4-hydroxylated metabolites in resistant small-cell lung cancer patients.
C12	32558929	Taylor	2020	Clin Pharmacol Ther	108(3):635-643	10.1002/cpt.1957	PMC7484917	MTXPK.org: A Clinical Decision Support Tool Evaluating High-Dose Methotrexate Pharmacokinetics to Inform Post-Infusion Care and Use of Glucarpidase.
C13	37503924	Taylor	2023	Clin Transl Sci	16(11):2130-2143	10.1111/cts.13600	PMC10651646	Clinical covariates that improve the description of high dose methotrexate pharmacokinetics in a diverse population to inform MTXPK.org.
C14	17112298	Aumente	2006	Clin Pharmacokinet	45(12):1227-38	10.2165/00003088-200645120-00007		Population pharmacokinetics of high-dose methotrexate in children with acute lymphoblastic leukaemia.
C15	30810947	Beechinor	2019	Clin Pharmacokinet	58(7):899-910	10.1007/s40262-018-00734-0	PMC6658326	The Population Pharmacokinetics of High-Dose Methotrexate in Infants with Acute Lymphoblastic Leukemia Highlight the Need for Bedside Individualized Dose Adjustment: A Report from the Children's Oncology Group.
C16	12630975	Gieschke	2003	Br J Clin Pharmacol	55(3):252-63	10.1046/j.1365-2125.2003.01765.x	PMC1884209	Population pharmacokinetics and concentration-effect relationships of capecitabine metabolites in colorectal cancer patients.
C17	24801171	Daher Abdi	2014	Cancer Chemother Pharmacol	73(6):1285-93	10.1007/s00280-014-2466-0	PMC5299968	Pharmacokinetics and exposure-effect relationships of capecitabine in elderly patients with breast or colorectal cancer.
C18	18305939	Tham	2008	Cancer Chemother Pharmacol	63(1):55-64	10.1007/s00280-008-0707-9		Does saturable formation of gemcitabine triphosphate occur in patients?
C19	20608756	Sugiyama	2010	Clin Pharmacokinet	49(8):549-58	10.2165/11532970-000000000-00000		Population pharmacokinetics of gemcitabine and its metabolite in Japanese cancer patients: impact of genetic polymorphisms.
C20	16322991	Latz	2006	Cancer Chemother Pharmacol	57(4):401-11	10.1007/s00280-005-0036-1		Population pharmacokinetic analysis of ten phase II clinical trials of pemetrexed in cancer patients.
C21	8995498	Periclou	1996	Cancer Chemother Pharmacol	39(1-2):42-50	10.1007/s002800050536		NONMEM population pharmacokinetic studies of cytosine arabinoside after high-dose and after loading bolus followed by continuous infusion of the drug in pediatric patients with leukemias.
C22	22212298	Krogh-Madsen	2012	Cancer Chemother Pharmacol	69(5):1155-63	10.1007/s00280-011-1800-z		Population pharmacokinetics of cytarabine, etoposide, and daunorubicin in the treatment for acute myeloid leukemia.
C23	27738808	Varatharajan	2016	Cancer Chemother Pharmacol	78(5):1051-1058	10.1007/s00280-016-3166-8	PMC7115946	Population pharmacokinetics of Daunorubicin in adult patients with acute myeloid leukemia.
C24	15151522	Ralph	2004	Br J Clin Pharmacol	57(6):764-72	10.1111/j.1365-2125.2004.02084.x	PMC1884520	Maximum a posteriori Bayesian estimation of epirubicin clearance by limited sampling.
C25	2208112	Reid	1990	Cancer Res	50(20):6525-8			Plasma pharmacokinetics and cerebrospinal fluid concentrations of idarubicin and idarubicinol in pediatric leukemia patients: a Childrens Cancer Study Group report.
C26	29564719	Hsu	2018	Pharm Res	35(5):106	10.1007/s11095-018-2387-4		Investigation of the Discriminatory Ability of Pharmacokinetic Metrics for the Bioequivalence Assessment of PEGylated Liposomal Doxorubicin.
C27	24719309	Chen	2014	J Clin Pharmacol	54(10):1097-107	10.1002/jcph.304	PMC4302229	Pharmacokinetics and pharmacodynamics of nab-paclitaxel in patients with solid tumors: disposition kinetics and pharmacology distinct from solvent-based paclitaxel.
C28	28445610	Adiwijaya	2017	Clin Pharmacol Ther	102(6):997-1005	10.1002/cpt.720	PMC5697569	Population Pharmacokinetics of Liposomal Irinotecan in Patients With Cancer.
C29	12011819	Mould	2002	Clin Pharmacol Ther	71(5):334-48	10.1067/mcp.2002.123553		Population pharmacokinetic and adverse event analysis of topotecan in patients with solid tumors.
C30	20490798	Urien	2011	Cancer Chemother Pharmacol	67(3):597-603	10.1007/s00280-010-1357-2		Developmental pharmacokinetics of etoposide in 67 children: lack of dexamethasone effect.
C31	33416909	Igarashi	2021	Cancer Chemother Pharmacol	87(4):501-511	10.1007/s00280-020-04220-y		Population pharmacokinetic model development and exposure-response analysis of vincristine in patients with malignant lymphoma.
C32	12389069	Variol	2002	Eur J Clin Pharmacol	58(7):467-76	10.1007/s00228-002-0506-x		A simultaneous oral/intravenous population pharmacokinetic model for vinorelbine.
C33	20573084	Nath	2010	Br J Clin Pharmacol	69(5):484-97	10.1111/j.1365-2125.2010.03638.x	PMC2856049	Population pharmacokinetics of melphalan in patients with multiple myeloma undergoing high dose therapy.
C34	30327943	Langenhorst	2019	Clin Pharmacokinet	58(5):627-637	10.1007/s40262-018-0715-9	PMC6451721	Population Pharmacokinetics of Fludarabine in Children and Adults during Conditioning Prior to Allogeneic Hematopoietic Cell Transplantation.
C35	32067250	Langenhorst	2020	Br J Clin Pharmacol	86(8):1499-1509	10.1111/bcp.14256	PMC7373715	A semi-mechanistic model based on glutathione depletion to describe intra-individual reduction in busulfan clearance.
C36	15173079	Ostermann	2004	Clin Cancer Res	10(11):3728-36	10.1158/1078-0432.CCR-03-0807		Plasma and cerebrospinal fluid population pharmacokinetics of temozolomide in malignant glioma patients.
C37a	20140617	Owen	2010	Cancer Chemother Pharmacol	66(6):1039-49	10.1007/s00280-010-1254-8	PMC2956859	Bendamustine pharmacokinetic profile and exposure-response relationships in patients with indolent non-Hodgkin's lymphoma.
C37b	28255849	Savic	2017	Clin Pharmacokinet	56(10):1245-1253	10.1007/s40262-017-0516-6	PMC5591805	Population Pharmacokinetics of Cladribine in Patients with Multiple Sclerosis.
C38a	33595793	Würthwein	2021	Eur J Drug Metab Pharmacokinet	46(2):289-300	10.1007/s13318-021-00670-8	PMC7935823	Population Pharmacokinetics of PEGylated Asparaginase in Children with Acute Lymphoblastic Leukemia: Treatment Phase Dependency and Predictivity in Case of Missing Data.
C38b	18823306	Hawwa	2008	Br J Clin Pharmacol	66(6):826-37	10.1111/j.1365-2125.2008.03281.x	PMC2675766	Population pharmacokinetic and pharmacogenetic analysis of 6-mercaptopurine in paediatric patients with acute lymphoblastic leukaemia.
C39	25925711	Völler	2015	Clin Pharmacokinet	54(11):1139-49	10.1007/s40262-015-0272-4		Age-Dependent Pharmacokinetics of Doxorubicin in Children with Cancer.
C40	12235447	Xie	2002	Clin Pharmacol Ther	72(3):265-75	10.1067/mcp.2002.126741		Clinical pharmacokinetics of irinotecan and its metabolites in relation with diarrhea.
C41	889590	Owellen	1977	Cancer Res	37(8 Pt 1):2597-602			Pharmacokinetics and metabolism of vinblastine in humans.
C42	6183016	Hall	1982	Cancer Chemother Pharmacol	9(1):22-5	10.1007/BF00296756		Bleomycin clinical pharmacology by radioimmunoassay.
C43	7172404	Breithaupt	1982	Cancer Chemother Pharmacol	9(2):103-9	10.1007/BF00265388		Pharmacokinetics of dacarbazine (DTIC) and its metabolite 5-aminoimidazole-4-carboxamide (AIC) following different dose schedules.
N01	11599657	Erdlenbruch	2001	Eur J Clin Pharmacol	57(5):393-402	10.1007/s002280100319		Pharmacokinetics of cisplatin and relation to nephrotoxicity in paediatric patients.
N01b	9470840	Peng	1997	Eur J Cancer	33(11):1823-8	10.1016/s0959-8049(97)00341-9		Cisplatin pharmacokinetics in children with cancer.
N01c	6891625	Evans	1982	Cancer Chemother Pharmacol	10(1):22-6	10.1007/BF00257231		Pharmacokinetic modeling of cisplatin disposition in children and adolescents with cancer.
N02	9815740	Kitchen	1997	Clin Cancer Res	3(5):713-7			A pediatric phase I trial and pharmacokinetic study of thioguanine administered by continuous i.v. infusion.
N03	11320662	Lowe	2001	Cancer Chemother Pharmacol	47(3):199-205	10.1007/s002800000229		Plasma pharmacokinetics and cerebrospinal fluid penetration of thioguanine in children with acute lymphoblastic leukemia: a collaborative Pediatric Oncology Branch, NCI, and Children's Cancer Group study.
N04	15757511	Lindemalm	2005	BMC Pharmacol	5:4	10.1186/1471-2210-5-4	PMC1079880	Application of population pharmacokinetics to cladribine.
N05	7906999	Kearns	1994	Cancer Res	54(5):1235-9			Pharmacokinetics of cladribine (2-chlorodeoxyadenosine) in children with acute leukemia.
N06	10473105	Reid	1999	Clin Cancer Res	5(8):2192-7			Metabolic activation of dacarbazine by human cytochromes P450: the role of CYP1A1, CYP1A2, and CYP2E1.
N07	21204910	Bogason	2011	Br J Clin Pharmacol	71(4):514-21	10.1111/j.1365-2125.2010.03894.x		Inverse relationship between leukaemic cell burden and plasma concentrations of daunorubicin in patients with acute myeloid leukaemia.
", sep = "\t", header = FALSE, quote = "", colClasses = "character",
  col.names = c("source_id", "pmid", "first_author", "year", "journal",
                "volume_pages", "doi", "pmcid", "title"))
pubmedSources[pubmedSources == ""] <- NA

# How far each PubMed document was read here.  Every abstract was screened;
# the PubMed Central text (without tables or equations) was read for these.
FULL_TEXT_PARTIAL <- c("P05", "P13", "C12", "C15", "C34", "C35", "N04")
pubmedSources$kind <- ifelse(startsWith(pubmedSources$source_id, "G"),
                             "guideline", "primary_study")
# Read in full: every parameter the model uses, from the PubMed Central
# text, the PDF still to confirm (FULL_TEXT_READ); or confirmed on the PDF
# itself (PARAMETERS_VERIFIED: Briki 2026 and Lindemalm 2005, supplied by
# Shafer 2026-11-06)
FULL_TEXT_READ <- c("C01", "P02", "C35", "C03")
# McCune 2014 (P23): Table 2 transcribed from the PDF by Shafer, the
# supplement supplied, the Methods read in PubMed Central
PARAMETERS_VERIFIED <- c("P10", "N04", "C02", "P23", "C13", "P12", "P09", "C30", "C20", "C33", "C31", "P24", "C08", "P19", "C29", "C32", "P04", "C22", "C39", "N07", "C04", "C16", "C18", "P17", "P18", "C05", "C06")
pubmedSources$review_depth <- ifelse(
  pubmedSources$source_id %in% PARAMETERS_VERIFIED, "parameters_verified",
  ifelse(pubmedSources$source_id %in% FULL_TEXT_READ, "full_text_read",
  ifelse(pubmedSources$source_id %in% FULL_TEXT_PARTIAL,
         "full_text_partial", "abstract_screened")))
pubmedSources$url <- paste0("https://pubmed.ncbi.nlm.nih.gov/", pubmedSources$pmid, "/")
pubmedSources$citation <- sprintf(
  "%s et al. %s %s %s;%s.", pubmedSources$first_author,
  sub("\\.$", "", pubmedSources$title), pubmedSources$journal,
  pubmedSources$year, pubmedSources$volume_pages)

# Documents outside PubMed: guidance, regulatory documents and labels, each
# cited as the handoff gives it.  None was read here, and a label's version
# is still to be fixed (a DailyMed setid names a label family, not a version).
other <- function(source_id, kind, citation, year, url)
  data.frame(source_id, pmid = NA, first_author = NA, year, journal = NA,
             volume_pages = NA, doi = NA, pmcid = NA, title = citation, kind,
             review_depth = "record_checked", url, citation,
             stringsAsFactors = FALSE)
dailymed <- function(setid)
  paste0("https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", setid)
otherSources <- rbind(
  other("G06", "guideline", "International Consensus Guideline on Anticancer Drug Dosing in Kidney Dysfunction (ADDIKD). eviQ, 2022.", "2022",
        "https://www.eviq.org.au/getmedia/b33e4b06-cc18-4cd9-9d0f-3826530c6c53/ADDIKD-Guideline-2022.pdf"),
  other("G09", "guideline", "CPIC/ClinPGx living guideline for DPYD and fluoropyrimidines (version to fix).", NA,
        "https://www.clinpgx.org/guideline/PA166251462"),
  other("G14", "guideline", "eviQ ADDIKD carboplatin information (version to fix).", NA,
        "https://www.eviq.org.au/pages/addikd-frequently-asked-questions/carboplatin-information"),
  other("R01", "regulatory", "FDA. Population Pharmacokinetics: Guidance for Industry. February 2022.", "2022",
        "https://www.fda.gov/regulatory-information/search-fda-guidance-documents/population-pharmacokinetics"),
  other("R02", "regulatory", "FDA. Optimizing the Dosage of Human Prescription Drugs and Biological Products for the Treatment of Oncologic Diseases. 2024.", "2024",
        "https://www.fda.gov/regulatory-information/search-fda-guidance-documents/optimizing-dosage-human-prescription-drugs-and-biological-products-treatment-oncologic-diseases"),
  other("R03", "regulatory", "EMA. Guideline on reporting the results of population pharmacokinetic analyses. 2007 (effective 1 January 2008).", "2007",
        "https://www.ema.europa.eu/en/scientific-guidelines/reporting-results-population-pharmacokinetic-analyses"),
  other("R04", "regulatory", "EMA. Fluorouracil and fluorouracil-related substances referral: DPD testing. 2020.", "2020",
        "https://www.ema.europa.eu/en/medicines/human/referrals/fluorouracil-fluorouracil-related-substances-capecitabine-tegafur-flucytosine-containing-medicinal-products"),
  other("R05", "regulatory", "FDA. Safety labeling update for capecitabine and fluorouracil: risks associated with dihydropyrimidine dehydrogenase deficiency. 2026.", "2026",
        "https://www.fda.gov/drugs/resources-information-approved-drugs/safety-labeling-update-capecitabine-and-fluorouracil-5-fu-risks-associated-dihydropyrimidine"),
  other("L01", "label", "Carboplatin injection, US prescribing information (version to fix).", NA, dailymed("47677091-fd20-49af-933c-c9dd2be21de9")),
  other("L02", "label", "Cisplatin injection, US prescribing information (version to fix).", NA,
        "https://dailymed.nlm.nih.gov/dailymed/fda/fdaDrugXsl.cfm?setid=a440f077-46f6-4688-a209-65bce38d1c92&type=display"),
  other("L03", "label", "Cyclophosphamide injection, US prescribing information (version to fix).", NA, dailymed("20bda486-cb9a-4d67-8c41-50e9fd066434")),
  other("L04", "label", "Fluorouracil injection, US prescribing information, NDA 202668 (2026).", "2026",
        "https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/202668Orig1s011lbl.pdf"),
  other("L05", "label", "Methotrexate injection, US prescribing information (version to fix).", NA, dailymed("b080eb92-fc72-4ce6-a7ab-3f9ce2415526")),
  other("L06", "label", "Doxorubicin injection, US prescribing information (version to fix).", NA, dailymed("1fd148fb-0fbc-4b6f-b790-23546fb46a71")),
  other("L07", "label", "Paclitaxel injection, US prescribing information (version to fix).", NA, dailymed("e4be6cbf-72b9-4f7f-9b1b-459b2d35961a")),
  other("L08", "label", "Vincristine sulfate injection, US prescribing information (Pfizer; version to fix).", NA,
        "https://labeling.pfizer.com/ShowLabeling.aspx?id=4653"),
  other("L09", "label", "Irinotecan injection, US prescribing information (2023).", "2023", dailymed("d04f2471-3085-4fc8-a657-bb3918d48e6e")),
  other("L10", "label", "Etoposide injection, US prescribing information (version to fix).", NA, dailymed("fe870629-104d-4d67-a7c6-f53bc588121e")),
  other("L11", "label", "Busulfan Fresenius Kabi, EMA product information (version to fix).", NA,
        "https://www.ema.europa.eu/en/documents/product-information/busulfan-fresenius-kabi-epar-product-information_en.pdf"),
  other("L12", "label", "Abraxane, EMA product information (version to fix).", NA,
        "https://www.ema.europa.eu/en/documents/product-information/abraxane-epar-product-information_en.pdf"),
  other("L13", "label", "Caelyx pegylated liposomal, EMA product information (version to fix).", NA,
        "https://www.ema.europa.eu/en/documents/product-information/caelyx-pegylated-liposomal-epar-product-information_en.pdf"),
  other("L14", "label", "Onivyde pegylated liposomal, EMA EPAR (version to fix).", NA,
        "https://www.ema.europa.eu/en/medicines/human/EPAR/onivyde-pegylated-liposomal")
)

sources <- rbind(pubmedSources[, names(otherSources)], otherSources)
sources <- sources[, c("source_id", "kind", "citation", "first_author", "year",
                       "journal", "volume_pages", "pmid", "pmcid", "doi", "url",
                       "review_depth", "title")]
src <- function(id) sources[sources$source_id == id, ]

# --- The registry ------------------------------------------------------------
# One row per drug, product and candidate source model.  `structure` is what
# the abstract (or, for P23, the PubMed Central text) says, never a number
# from the handoff or its audit.
WAVE <- c(first = "first", second = "second", expansion = "expansion")

m <- function(model_id, drug, product, route, population, analyte, structure,
              wave, engine, source_id, note = "",
              readiness = "needs_full_text", stanpumpr_drug = NA,
              to_obtain = NULL) {
  s <- if (is.na(source_id)) NULL else src(source_id)
  base <- switch(readiness,
    needs_full_text = if (!is.null(s) && s$review_depth == "full_text_read")
      "Read in full, but incomplete." else
      "The parameters are in the paper's tables, which were not read here.",
    complete_not_implemented = "Read in full: the model is complete.",
    needs_engine = "Read in full.",
    implemented = if (!is.null(s) && s$review_depth == "parameters_verified")
      "Every parameter and equation confirmed on the PDF." else
      paste("Implemented from the PubMed Central text, every parameter read there;",
            "to be confirmed on the PDF."),
    "")
  data.frame(
    model_id, drug, product, route, population, analyte, structure,
    wave = WAVE[[wave]], readiness, engine, stanpumpr_drug,
    source_id = if (is.null(s)) NA else source_id,
    evidence_read = if (is.null(s)) "none" else s$review_depth,
    blocking_issue = trimws(paste(base, note)),
    to_obtain = if (!is.null(to_obtain)) to_obtain else
      if (is.null(s) || s$review_depth == "parameters_verified" ||
                    readiness %in% c("complete_not_implemented", "needs_engine")) NA else
      sprintf(if (readiness == "implemented")
                "%s (PMID %s%s): the PDF, to confirm the parameters read from the PubMed Central text."
              else "%s (PMID %s%s): the full text with its tables and supplement.",
              s$citation, s$pmid,
              ifelse(is.na(s$pmcid), "", paste0(", ", s$pmcid))),
    source_url = if (is.null(s)) NA else s$url,
    stringsAsFactors = FALSE)
}

none <- function(model_id, drug, product, route, wave, note)
  m(model_id, drug, product, route, population = "not identified",
    analyte = "not identified", structure = "not identified", wave = wave,
    engine = "unknown", source_id = NA, note = note,
    readiness = "no_source_identified")

registry <- rbind(
  # ---- First wave: carboplatin, high-dose methotrexate, 5-FU, busulfan -----
  m("carboplatin__adult__ekhart2006", "carboplatin", "carboplatin", "IV",
    "178 adults (280 courses), conventional and high dose, mostly normal renal function",
    "ultrafilterable platinum", "linear two compartments; no serum-creatinine estimate of renal function related to clearance",
    "first", "linear", "C01",
    "PDF read (supplied 2026-11-06), Table 2: two compartments, CL 8.33 L/h, V 16.3 L, k12 0.104 /h, k21 0.171 /h, no covariates (Model 1, the best fit). Concentrations are carboplatin equivalents (dose = AUC x 138.8 mL/min in the Discussion). Fitted in creatinine clearance above about 50 mL/min, so it says nothing of renal impairment; its finding that creatinine-based estimates do not predict clearance means a renal covariate must not be grafted on. [decision, asked 2026-11-06] Which carboplatin model the library offers: this one (fitted, blind to renal function), Chatelut 1995 (renal formula, disposition derived from its means), or both as two drugs.",
    readiness = "complete_not_implemented"),
  m("carboplatin__ovarian__duffull1997", "carboplatin", "carboplatin", "IV",
    "12 women with ovarian cancer", "intact carboplatin in serum (HPLC)",
    "two compartments, for sequential Bayesian dosing", "first", "linear", "C02",
    "PDF read (supplied 2026-11-06). A complete fitted model with a renal term: Cl (L/h) = 0.15 LBW^0.75 + 0.78 ClCr, Vss 0.38 LBW, V1 0.26 LBW, Clic 0.15 LBW^0.75 (LBW by Pesola; ClCr by Cockcroft-Gault with LBW and creatinine floored at 60 uM, in L/h), geometric means Cl 6.8 L/h, Vss 21.6 L, V1 14.8 L, Clic 4.4 L/h. Only 12 women, 49-68 years, ovarian cancer; serum carboplatin by HPLC. The erratum the handoff reported was not found: PubMed has none for it (searched 2026-11-06) and neither PDF mentions one. Shafer chose this model for the library's carboplatin (2026-11-06). The paper contradicts itself on Clic: equation 5.4 prints 0.15 LBW^0.75 (2.7-3.4 L/h over the study's LBW 48-64 kg), Table 2 a geometric mean of 4.4 L/h, range 3.9-5.0; the 0.15 repeats equation 5.1's non-renal coefficient. Decided (Shafer, 2026-11-06): Table 2's level in the equation's form, 4.4 (LBW/56)^0.75. Clic affects only the distribution phase, not the AUC.",
    readiness = "implemented", stanpumpr_drug = "carboplatin"),
  m("carboplatin__adult__schmitt2009", "carboplatin", "carboplatin", "IV",
    "357 adults, including the underweight and the obese", "carboplatin (analyte to confirm)",
    "a clearance equation in serum creatinine, cystatin C, weight, age and sex; the disposition model is not in the abstract",
    "first", "unknown", "C03",
    "Table 2: CL (mL/min) = 117.8 (Scr/75)^-0.450 (cysC/1.0)^-0.385 (ABW/65)^0.504 (age/56)^-0.366 x 0.847^sex (Scr in uM, sex 1 for women). The typical Vc, Vp and Q were taken from a rich-sampling database and are not printed (Vc and Vp proportional to BSA), so the paper does not fix a curve. The Patient Profile has no cystatin C, and serum creatinine is not a substitute for it (the chlorthalidone decision, docs/antihypertensives.md).",
    readiness = "needs_full_text",
    to_obtain = "Nothing published supplies the typical Vc, Vp and Q: Schmitt 2009's references 9 (Chatelut 2000, Eur J Cancer 36:264-269, PMID 10741287) and 11 (Leger 2002, PMID 11750839), both read in full 2026-11-06, describe the same two-compartment model on the same 103-patient database, volumes proportional to BSA, and print no volume or Q. Only the authors' database would fix the curve; and a cystatin C field, which the Patient Profile does not have."),
  m("carboplatin__adult__chatelut1995", "carboplatin", "carboplatin", "IV",
    "adults with cancer", "carboplatin (analyte to confirm)",
    "a clearance equation in creatinine, weight, age and sex (the Chatelut formula); whether the paper gives volumes was not established",
    "first", "unknown", "P02",
    "PDF read (supplied 2026-11-06). The clearance formula, CL (mL/min) = 0.134 weight + 218 weight (1 - 0.00457 age)(1 - 0.314 sex)/creatinine (uM), sex 1 for women, fitted in 34 patients (creatinine 56-353 uM) and validated prospectively in 36; Table 2 gives only the means of individual two-compartment fits (CL 107 mL/min, distribution half-life 8 min, elimination half-life 3.8 h, Vdss 28 L), which close a two-compartment model by derivation. The paper does not state the creatinine assay, but Leger 2002 (PMID 11750839, PDF read 2026-11-06) says the formula was developed with an ENZYMATIC assay (creatinine amidohydrolase), and that a Jaffe creatinine must be corrected before use, Scr = (Jaffe - 1.6)/1.08 uM (its NONMEM covariate analysis put the Jaffe value 11.6% too high); an earlier note here guessed Jaffe, which was wrong. Leger prints no typical Vc, Vp or Q either. Do not splice its clearance into another source's volumes. [decision, asked 2026-11-06] See the Ekhart 2006 row.",
    readiness = "complete_not_implemented"),
  m("carboplatin__children__peng1995", "carboplatin", "carboplatin", "IV",
    "children with cancer", "carboplatin (analyte to confirm)",
    "a comparison of methods of estimating carboplatin pharmacokinetics", "first", "unknown", "P03"),
  m("carboplatin__children_neuroblastoma__duong2019", "carboplatin", "carboplatin", "IV",
    "51 children with high-risk neuroblastoma, high-dose conditioning", "carboplatin (analyte to confirm)",
    "two compartments allometric in weight (CL, Q x (WT/WT_STD)^0.75, volumes x WT/WT_STD): CL 2.7 L/h, VC 8.3 L, Q 0.23 L/h, VP 0.74 L",
    "first", "linear", "C04",
    "PDF read (supplied 2026-11-06). High-dose conditioning only. WT_STD is not printed; 15.5 kg on Shafer's decision (2026-11-06), with which Table 4's carboplatin exposures reproduce (14.5, the printed median, puts them 2-7% low). Measured GFR, creatinine, age, sex and BSA not retained; Q and VP essentially unidentified. The handoff named the first author Nath; PubMed's is Duong.",
    readiness = "implemented", stanpumpr_drug = "carboplatinPediatric"),

  m("methotrexate__adult__ibarra2023", "methotrexate", "methotrexate (high dose)", "IV",
    "243 adults (693 courses), 18-88 years, 0.5-13.4 g/m2 over 3-24 h",
    "methotrexate",
    "three mammillary compartments allometric in weight: CL 5.92 L/h x (SCRpop/SCR)^0.694 x (albumin/4)^0.594, V1 14.4 L, Q2 0.571 L/h, V2 3.55 L, Q3 0.0365 L/h, V3 2.09 L",
    "first", "linear", "P12",
    "PDF read (supplied 2026-11-06). Table 2 reproduces the paper's half-lives (1.46, 4.93, 39.9 against 1.5, 4.9, 40 h) and Vss (20.0 L). SCRpop is not printed; the Methods' healthy-adult means 0.92 and 0.77 mg/dL are taken. 68% of courses were hypoalbuminaemic. [decided 2026-11-06, Shafer] An albumin field was added; Ibarra's (albumin/4)^0.594 and Taylor's severe-hypoalbuminaemia factors read it.",
    readiness = "implemented", stanpumpr_drug = "methotrexate"),
  m("methotrexate__mixed__blackman2026", "methotrexate", "methotrexate (high dose)", "IV",
    "patients receiving high-dose methotrexate", "methotrexate",
    "population models developed and validated for dosing decisions", "first", "unknown", "P13"),
  m("methotrexate__pediatric_all__taylor2020", "methotrexate", "methotrexate (high dose)", "IV",
    "772 children with ALL (Nordic protocol), 24-h infusions", "methotrexate",
    "three compartments; demographics and serum creatinine", "first", "linear", "C12",
    "The model behind MTXPK.org. A simulation must not replace measured levels or rescue guidance (G03, G04)."),
  m("methotrexate__pediatric_aya__taylor2023", "methotrexate", "methotrexate (high dose)", "IV",
    "1668 children, adolescents and young adults, 0.1-32.3 years, ALL, osteosarcoma and NHL",
    "methotrexate",
    "three mammillary compartments (ADVAN11 TRANS4) per 1.73 m2 (Mosteller): CL 8.24 L/h x (creatinine/40 uM)^-0.26, V1 22.1 L, Q2 0.225 L/h, V2 2.19 L, Q3 0.0342 L/h, V3 6.07 L; short infusion CL x 1.05, Down syndrome CL x 0.84, severe hypoalbuminaemia Q2 x 1.17 and Q3 x 1.34, pleural effusion V1 x 1.06 and V2 x 1.43",
    "first", "linear", "C13",
    "PDF read (Shafer placed it on Google Drive, 2026-11-06), with the supplement. Table 2's footnote calls Q3 the clearance between V2 and V3, but the Methods state ADVAN11 (mammillary) and the supplement agrees, so the footnote is a misprint; equation 3 prints Down syndrome 0.83 against Table 2's 0.84. Down syndrome, effusion and infusion length are not Patient Profile fields and are held at the reference (long infusion: a short one clears 5% faster); severe hypoalbuminaemia reads the Serum albumin field (2.5 g/dL or less), added 2026-11-06. Reproduces Table 5's 24-h mean concentration for 5 g/m2 over 24 h (85.9 against 84.8 uM).",
    readiness = "implemented", stanpumpr_drug = "methotrexatePediatric"),
  m("methotrexate__pediatric_all__aumente2006", "methotrexate", "methotrexate (high dose)", "IV",
    "49 children with ALL, 0.5-17 years", "methotrexate",
    "two compartments; age and weight on clearance and V1", "first", "linear", "C14"),
  m("methotrexate__infant_all__beechinor2019", "methotrexate", "methotrexate (high dose)", "IV",
    "71 infants with ALL", "methotrexate",
    "two compartments, allometric weight only; high interoccasion variability", "first", "linear", "C15",
    "Infants only; do not extend older children's models into infancy. Its supplement's two figures (supplied 2026-11-06) show weight-normalised clearance near 0.27 L/h/kg without trend in age (2-15 months) or treatment cycle, and a visual predictive check; Q and the peripheral volume are in the article's own table."),

  m("fluorouracil__adult__terret2000", "fluorouracil", "fluorouracil", "IV",
    "adults with cancer", "5-FU", "dose- and time-dependent pharmacokinetics (structure not in the abstract)",
    "first", "unknown", "P08"),
  m("fluorouracil__adult__tan2026", "fluorouracil", "fluorouracil", "IV",
    "140 Dutch adults from four prospective studies, 46-h infusions with or without a bolus", "5-FU",
    "two compartments with Michaelis-Menten elimination alone: Vmax 1900 mg/h x (BSA/1.94)^1.14, Km 8.04 mg/L, V1 19.7 L, Q 52.4 L/h, V2 15 L",
    "first", "michaelis_menten", "P09",
    "PDF read (supplied 2026-11-06). The library's second 5-FU drug, beside Briki 2026. Reproduces Table 3's median AUCs (20.8, 28.7 and 23.6 against 21, 29 and 24 mg h/L for 2400 mg/m2 over 46 h alone, after 400 and after 200 mg/m2 bolus). BSA formula not stated; Mosteller used. DPYD tested and not retained.",
    readiness = "implemented", stanpumpr_drug = "fluorouracilTan"),
  m("fluorouracil__adult__briki2026", "fluorouracil", "fluorouracil", "IV",
    "726 patients from three studies (individual-patient meta-analysis)", "5-FU",
    "two compartments with parallel Michaelis-Menten and linear elimination; BSA on CL, V lower in women and with age",
    "first", "michaelis_menten", "P10",
    "Run by advanceMichaelisMenten() with clLinear and one peripheral compartment. First read in the PubMed Central text, whose Table 3 had lost its subscripts; the PDF's Table 3 and printed equations confirm every identification. DPD deficiency is not represented.",
    readiness = "implemented", stanpumpr_drug = "fluorouracil"),

  m("busulfan__infant_adult__mccune2014", "busulfan", "busulfan", "IV",
    "1610 HCT recipients, 0.1-66 years (92% under 20)", "busulfan, plasma",
    "two compartments; theory-based allometry on normal fat mass (fat fractions 0.509 CL, 0.203 V); maturation of clearance by postmenstrual age (TM50 45.7 weeks, Hill 2.3); V1 and V2 7% higher in women; clearance x 0.932 from 6 h and x 0.919 from 36 h",
    "first", "linear", "P23",
    "Table 2 transcribed from the PDF by Shafer (2026-11-06): CL 11.4 L/h per 62 kg NFM, V1 13.9 L and V2 29.9 L per 59 kg NFM, Q 135.2 L/h. The clearance steps run through courseSteps on the event engine. Table S7's worked example does not follow the FFM equation the Methods state (49.7 against 56.1 kg for its 70-kg man); the stated equation is used.",
    readiness = "implemented", stanpumpr_drug = "busulfan"),
  m("busulfan__hct__langenhorst2020", "busulfan", "busulfan", "IV",
    "HCT recipients 0-73 years", "busulfan",
    "semi-mechanistic glutathione depletion driving a time-varying clearance; age over 40 an effect",
    "first", "new_engine", "C35",
    "PDF read (supplied 2026-11-06). A depletion state is a mechanism the engine lacks; not to be generalised to every regimen. Its empirical predecessor (a stepwise day-1 versus later clearance) is the source to look at for a PK-event version.",
    readiness = "needs_engine"),

  # ---- Second wave ---------------------------------------------------------
  m("paclitaxel__adult__joerger2006", "paclitaxel", "paclitaxel (solvent-based)", "IV",
    "168 adults with solid tumours (82 women), 100-250 mg/m2 over 3 h", "total plasma paclitaxel",
    "three compartments: saturable elimination (VMEL 37.4 umol/h x 1.2^male x (BSA/1.8)^0.842 x (bilirubin/7)^-0.167 x (age/56)^-0.352, KMEL 0.53 umol/L), saturable transport to a first peripheral compartment held as an amount (VMTR 169 umol/h x 1.2^male x (BSA/1.8)^0.911, KMTR 0.826 umol/L x 2.11^male) returning at first order (K21 1.15 /h x 0.893^male), linear distribution to a second (Q 20.1 L/h x (BSA/1.8)^0.724, V3 252 L x (BSA/1.8)^1.17), V1 12.8 L",
    "second", "michaelis_menten", "P17",
    "PDF read on the page images (2026-11-06). Run by advanceMichaelisMenten() with its saturableDistribution block, added for this drug (Shafer's decision, 2026-11-06). The ODEs are not printed; reconstructed from the text, they reproduce the paper's simulations (175 mg/m2 over 3 h at BSA 1.8: woman Cmax 3.83 umol/L, 17.7 h above 0.1 umol/L; man 3.29, 13.0 h; Figs. 3-4). The Results swap the KMEL and KMTR labels (Table 2 and Eq. D are right) and the Methods give Q in L/min (L/h is right). Bilirubin (no Patient Profile field) held at 7 umol/L; BSA formula not stated, Mosteller used; age floored at 18. Not transferable to nab-paclitaxel.",
    readiness = "implemented", stanpumpr_drug = "paclitaxel"),
  m("cisplatin__adult__urien2004", "cisplatin", "cisplatin", "IV",
    "43 adults with cancer", "unbound (ultrafilterable) and total plasma platinum",
    "unbound platinum in two compartments (V1 23.4 L x (BSA/1.7)^1.60, CL 35.5 L/h x (BSA/1.7)^0.83 x (CLCr/81)^0.36, Q 8.64 L/h, V2 12.0 L) binding irreversibly to proteins, a metabolite with fm/Vm 0.017 /L and CLm0/Vm 0.014 /h",
    "second", "linear_metabolite", "P04",
    "PDF read (supplied 2026-11-06). Table 4's V1 exponent 1.60 is used, on Shafer's decision (2026-11-06); the text's 2.3 belongs to the separate total-platinum model. Doses in mg of cisplatin, concentrations of platinum, as fitted (the unconverted reading reproduces the figures). Serum protein (70 g/L) and the dose per m2 (25 mg/m2, a regimen covariate) are held; they act only on binding.",
    readiness = "implemented", stanpumpr_drug = "cisplatin"),
  m("cisplatin__adult__thompson2025", "cisplatin", "cisplatin", "IV",
    "adults randomised between 5-HT3 antagonists", "platinum",
    "population model of platinum disposition", "second", "unknown", "P05"),
  m("cisplatin__children__erdlenbruch2001", "cisplatin", "cisplatin", "IV",
    "12 children (25 courses)", "free platinum",
    "two compartments fitted to each child, not a population model", "second", "linear", "N01",
    "Too small to be a paediatric population model."),
  m("cisplatin__children__peng1997", "cisplatin", "cisplatin", "IV",
    "21 children, 24-h infusions", "free platinum",
    "non-compartmental and compartmental analysis", "second", "unknown", "N01b"),
  m("cisplatin__children__evans1982", "cisplatin", "cisplatin", "IV",
    "14 children and adolescents", "platinum",
    "a physiological flow-limited model and a simplified multicompartment model", "second", "unknown", "N01c"),
  m("cyclophosphamide__highdose__dejonge2005", "cyclophosphamide", "cyclophosphamide", "IV",
    "adults, high-dose cyclophosphamide, thiotepa and carboplatin", "cyclophosphamide, 4-hydroxycyclophosphamide, 2-dechloroethylcyclophosphamide, phosphoramide mustard",
    "parent and three metabolites", "second", "new_engine", "P06",
    "A metabolite cascade; only one level of metabolite is carried."),
  m("cyclophosphamide__breast__hassan1999", "cyclophosphamide", "cyclophosphamide", "IV",
    "women with breast cancer", "cyclophosphamide",
    "mechanism-based enzyme model of autoinduction", "second", "new_engine", "P07",
    "Enzyme turnover is a state the engine lacks for this drug; the antiseizure induction machinery is a design precedent, not a validated substitute."),
  m("cyclophosphamide__breast__joerger2007", "cyclophosphamide", "cyclophosphamide", "IV",
    "65 women with breast cancer (with doxorubicin)", "cyclophosphamide",
    "one compartment: CL 4.23 L/h x 1.89^EIAD, V 34.6 L; joined to a neutropenia model",
    "second", "linear", "C08",
    "PDF read (supplied 2026-11-06). EIAD (an enzyme-inducing antiepileptic) held at 0. 4-hydroxycyclophosphamide was not measured and autoinduction cannot be identified from one first-cycle profile. The Methods' L/min and the Results' 4.25 L/h are misprints for Table IV's 4.23 L/h.",
    readiness = "implemented", stanpumpr_drug = "cyclophosphamide"),
  m("cyclophosphamide__neuroblastoma__mccune2009", "cyclophosphamide", "cyclophosphamide", "IV",
    "22 children with neuroblastoma", "cyclophosphamide, 4-hydroxycyclophosphamide, carboxyethylphosphoramide mustard",
    "inducible and non-inducible elimination with metabolites; GFR covariate", "second", "new_engine", "C09"),
  m("doxorubicin__adult_child__kontny2013", "doxorubicin", "doxorubicin (conventional)", "IV",
    "adults and children over 3 years", "doxorubicin",
    "population model (NONMEM); structure not in the abstract", "second", "unknown", "P15"),
  m("doxorubicin__nhl__perezblanco2016", "doxorubicin", "doxorubicin (conventional)", "IV",
    "adults with non-Hodgkin lymphoma", "doxorubicin and doxorubicinol",
    "parent and metabolite population model", "second", "linear_metabolite", "P16",
    "A metabolite drug (doxorubicinol) would join the library with it; formation must be identifiable as the library's metabolite rule requires."),
  m("doxorubicin__breast__joerger2007", "doxorubicin", "doxorubicin (conventional)", "IV",
    "65 women with breast cancer (with cyclophosphamide)", "doxorubicin and doxorubicinol",
    "two compartments forming one-compartment doxorubicinol: CL 47.6 L/h x (BSA/1.8)^1.4 x (AST/21)^-0.24 x (age/56)^-0.54, V1 12.3 L, Q 60.3 L/h, V2 421 L; CL_DOL/fm 108 x (CLcr/80)^0.54 L/h, V3/fm 1580 L",
    "second", "linear_metabolite", "C08",
    "PDF read (supplied 2026-11-06). AST has no Patient Profile field and is held at 21 U/L. Doses as the hydrochloride (Table VII's mean AUC is reproduced with it, missed by 8% with the base), so the volumes and clearances are divided by 0.937. Sampling stopped at 24 h, so the terminal half-life (11 h) is short of the usual 20-48 h.",
    readiness = "implemented", stanpumpr_drug = "doxorubicin"),
  m("doxorubicin__children__voller2015", "doxorubicin", "doxorubicin (conventional)", "IV",
    "94 children 0-18 years", "doxorubicin and doxorubicinol",
    "three compartments and one doxorubicinol compartment (fM 0.5 fixed); every parameter x (1 + 1.30 (BSA - 0.77)); CL 24.1 L/h/m2 at the median child x (1 + (age/5.32)^0.286)/2",
    "second", "linear_metabolite", "C39",
    "PDF read; Eq. 1 confirmed on the page image (2026-11-06). The only source for children under 3 years. Reproduces its three printed clearances (24.1, 12.0, 29.1 L/h/m2). The BSA formula is in the unread supplement (Mosteller used); the dose basis is not stated (doses as labelled).",
    readiness = "implemented", stanpumpr_drug = "doxorubicinPediatric"),
  m("vincristine__children__nijstad2022", "vincristine", "vincristine (conventional)", "IV",
    "206 patients 0.04-33.9 years (25 under 1 year)", "vincristine",
    "two compartments (CL 30.6 L/h x (WT/70)^0.75, Vc 5.39 L x WT/70, Q 63.2 L/h x (WT/70)^0.75, Vp 400 L x WT/70) with saturable binding to beta-tubulin from the central amount (Bmax 0.525 mg x WT/70 x (age/18)^-0.199, kon 1300 /h fixed, koff 11.5 /h); unbound central concentration observed",
    "second", "michaelis_menten", "P18",
    "PDF read on the page images and the supplement's Table S1 (2026-11-06). Run by advanceMichaelisMenten() as rapidBinding (the binding at quasi-equilibrium, kd = Bmax koff/kon/Vc) with the option plotted = \"unbound\", added for this drug (Shafer's decision, 2026-11-06): kon is beyond the Runge-Kutta step's stability, and the reduction is within 1.1% of the full binding ODE at 5 min and 0.8% from 30 min (test-drugs-vincristinePediatric.R). AUC = dose/CL (mass balance). Age floored at 0.04 years, the youngest studied. [decision] kon and koff taken as not allometrically scaled (the Methods say every parameter was; Table II labels only CL, Q, Vc, Vp as 70-kg values). [decision] Doses as labelled (vincristine sulfate), the paper never converting to the base. Not exclusively paediatric, but not an adult lymphoma model either. Intravenous only: never intrathecal.",
    readiness = "implemented", stanpumpr_drug = "vincristinePediatric"),
  m("vincristine__lymphoma__igarashi2021", "vincristine", "vincristine (conventional)", "IV",
    "30 adults with malignant lymphoma (R-CHOP)", "vincristine",
    "three compartments: CL 31.1 L/h x (BSA/1.60)^1.74 x (age/60)^-0.485, V1 13.2 L, Q2 60.2 L/h, V2 33.9 L (both fixed), Q3 81.1 L/h, V3 544 L",
    "second", "linear", "C31",
    "PDF read (supplied 2026-11-06). Reproduces its 1.7- and 0.8-fold covariate checks and Table 3's exposures. The BSA formula and the dose basis (sulfate or base) are not stated; Mosteller and the dose as labelled are used. V1 is poorly determined (first sample at 10 min).",
    readiness = "implemented", stanpumpr_drug = "vincristine"),
  m("irinotecan__adult__klein2002", "irinotecan", "irinotecan (conventional)", "IV",
    "adults with cancer", "irinotecan, SN-38, SN-38 glucuronide",
    "irinotecan in three compartments (Vc 5.52 L, k10 4.56, k12 57.8, k21 2.58, k13 1.96, k31 0.0812 /h) forming SN-38 (two compartments, Vc/Fmsn 200 or 466 L in a two-group mixture, k10 2.52, k12 2.6, k21 0.0584 /h) forming SN-38G (one compartment)",
    "second", "linear_metabolite", "P19",
    "PDF read (supplied 2026-11-06); implemented on Shafer's choice (2026-11-06) as irinotecan forming SN-38, SN-38G not carried (a third generation). Reproduces the printed CL, Vss and half-lives. The 466-L SN-38 group is used: Table I's mixture proportions appear swapped against Fig 2 and the validation set, and that group's Fmsn was fixed (12%). The covariate effects are printed only as percentages that contradict Table I's references and cannot be rebuilt; none is applied.",
    readiness = "implemented", stanpumpr_drug = "irinotecan"),
  m("irinotecan__crc__oyagairiarte2019", "irinotecan", "irinotecan (conventional)", "IV",
    "adults with metastatic colorectal cancer", "irinotecan and metabolites",
    "parent and metabolites", "second", "new_engine", "P20"),
  m("irinotecan__adult__xie2002", "irinotecan", "irinotecan (conventional)", "IV",
    "109 adults", "irinotecan, SN-38, SN-38 glucuronide",
    "irinotecan three compartments, SN-38 and SN-38G two each", "second", "new_engine", "C40"),
  m("etoposide__adult__nguyen1998", "etoposide", "etoposide", "IV and PO",
    "100 adults 25-85 years", "total and unbound etoposide",
    "linear two compartments, first-order oral absorption (F 66%); BSA on V; creatinine, weight, protein and albumin on clearance",
    "second", "linear", "P21",
    "Total protein is not a Patient Profile field (serum albumin is, since 2026-11-06); unbound concentration needs the paper's binding relation."),
  m("etoposide__children__urien2011", "etoposide", "etoposide", "IV",
    "67 children 14 weeks-16.7 years", "etoposide",
    "two compartments allometric in weight (3/4 on clearances, 1 on volumes, at 70 kg): CL 2.05 L/h, Vc 9.3 L, Q 0.68 L/h, Vp 5.7 L; no dexamethasone or CYP3A/MDR1 effect",
    "second", "linear", "C30",
    "PDF read (supplied 2026-11-06). The handoff named the first author You; PubMed's is Urien. Three misprints, none affecting the model: the Methods give the clearance exponent as 1/4 where the Abstract, Table 3 and the dosing formula give 3/4; Table 3's covariate column prints CL70 for every parameter; and its footnote's 15-kg check, 0.75 L/h, is 0.646 by its own equation. The Abstract's Vc is 9.21 L, Table 3's 9.3 (used).",
    readiness = "implemented", stanpumpr_drug = "etoposidePediatric"),
  m("etoposide__neuroblastoma__duong2019", "etoposide", "etoposide", "IV",
    "51 children with high-risk neuroblastoma", "etoposide",
    "two compartments allometric in weight: CL 0.59 L/h, VC 2.7 L, Q 0.07 L/h, VP 3.8 L at WT_STD",
    "second", "linear", "C04",
    "PDF read (supplied 2026-11-06). WT_STD 15.5 kg on Shafer's decision. VP unidentified (RSE 234%), giving a 42-h terminal half-life.",
    readiness = "implemented", stanpumpr_drug = "etoposideDuong"),
  m("etoposide__aml__kroghmadsen2012", "etoposide", "etoposide", "IV",
    "23 adults with AML", "etoposide",
    "two compartments: CL 0.99 + 0.00996 x CLCR L/h (Cockcroft-Gault) x (1 + 0.00185 (bWBC - median)), Vc 9.13 L x (1 - 0.2793 in women), Q 1.14 L/h, Vp 4.75 L",
    "second", "linear", "C22",
    "PDF read (supplied 2026-11-06); it reports the Q that Nguyen 1998 omits, so it is the adult model. Baseline white cell count is not a Patient Profile field and is held at its (unprinted) median. Table 2's renal slope does not reproduce the text's -30%/+24% or Fig 4's AUCs, which need about 0.0133; the table is used. The handoff named the first author Bogason; PubMed's is Krogh-Madsen.",
    readiness = "implemented", stanpumpr_drug = "etoposide"),

  # ---- Expansion -----------------------------------------------------------
  m("docetaxel__adult__bruno1996", "docetaxel", "docetaxel", "IV",
    "547 adults in phase I and II", "docetaxel",
    "three compartments: CL = BSA x (22.1 - 3.55 AAG - 0.095 age + 0.225 albumin) x (1 - 0.334 HEP12) L/h, V1 8.31 L, k12 1.07, k21 1.74, k13 1.28, k31 0.0787 /h",
    "expansion", "linear", "P24",
    "PDF read (supplied 2026-11-06; an older scan, every number read on the page images). Reproduces Table II's CL 36.7 L/h, Vss 149 L and three half-lives at the mean covariates. AAG (1.46 g/L) and HEP12 (0) have no Patient Profile field and are held; albumin reads the Patient Profile's field. The BSA formula is not named (Mosteller used).",
    readiness = "implemented", stanpumpr_drug = "docetaxel"),
  m("docetaxel__phase2__bruno1998", "docetaxel", "docetaxel", "IV",
    "640 adults in phase II", "docetaxel",
    "population PK and exposure-toxicity", "expansion", "unknown", "P25"),
  m("oxaliplatin__crc__delord2003", "oxaliplatin", "oxaliplatin", "IV",
    "40 adults with colorectal cancer (oxaliplatin, 5-FU, levofolinic acid)", "ultrafilterable platinum; whole-blood platinum",
    "ultrafiltrate: two compartments, CL = 5.49 BSA + 4.55 BSA (140 - age)(1 - 0.15 female)/SCr L/h, V1 40.8 L, V2 21.0 L, no Q; whole blood: three compartments, CL = 0.28 BSA (1 - 0.058 Hb) L/h, V1 17.9 L, k12 2.97, k21 1.77, k13 0.0747, k31 0.0119 /h",
    "expansion", "linear", "C05",
    "PDF read on the page images (2026-11-06). The ultrafilterable model, the analyte that matters, prints CL, V1 and V2 but never the intercompartmental clearance, and no Q makes Table 2 reproduce Fig. 1, so it cannot be run. The whole-blood model is complete but plots red-cell-bound platinum, largely inactive, and its haemoglobin term reaches zero clearance at 17.2 g/dL (inside the normal range); the authors advise against using it for dosing. Bastian 2003 is the library's model.",
    readiness = "complete_not_implemented"),
  m("oxaliplatin__adult__bastian2003", "oxaliplatin", "oxaliplatin", "IV",
    "56 adults with metastatic cancer", "ultrafilterable platinum, as oxaliplatin",
    "two compartments, CL 14.1 L/h x (BW/71)^1.10 x (SCr/87 umol/L)^-0.57 x 0.60 if female, V1 24.9 L x (BW/71)^1.29, Q 34.8 L/h x (BW/71)^1.01, V2 136 L; a gallbladder store filled at k1B 1.78 /h until TLAG 2 h after each course starts, emptied as a bolus into a gut depot returning at Ka 0.23 /h",
    "expansion", "new_engine", "C06",
    "PDF read on the page images (supplied 2026-11-06). Implemented AS PUBLISHED on Shafer's decision (2026-11-06): the store stops filling at TLAG, the only reading that reproduces the paper's AUC = Dose/CL and its terminal half-lives; Plusquellec & Houin 1992, whose equations it cites, keep the store filling after its last emptying (a sink). A new exact engine, R/advanceBiliaryStore.R. Concentrations taken as oxaliplatin equivalents, which the paper's AUC arithmetic requires.",
    readiness = "implemented", stanpumpr_drug = "oxaliplatin"),
  m("capecitabine__crc__gieschke2003", "capecitabine", "capecitabine", "PO",
    "481 adults with colorectal cancer", "5'-DFUR, 5-FU, FBAL",
    "a linear chain, molar: dose -> 5'-DFUR (ka 1.09 /h, V1 90.6 L, CL1 75.8 L/h) -> 5-FU (V2 17.8 L fixed, CL2 1190 L/h x (ALP/140)^-0.169) -> FBAL (V3 73.6 L x (BSA/1.8)^0.812 x (CLCR/80)^0.394, CL3 27.5 L/h x (CLCR/80)^0.615)",
    "expansion", "linear_metabolite", "C16",
    "PDFs read (Gieschke and Blesch, supplied 2026-11-06). Molar amounts, the only reading that reproduces Table 6. Implemented on Shafer's request as 5'-DFUR forming 5-FU (capecitabine5FU); FBAL, the third step, needs a cascade engine and is not carried; ALP held at 140 U/L.",
    readiness = "implemented", stanpumpr_drug = "capecitabine"),
  m("capecitabine__elderly__daherabdi2014", "capecitabine", "capecitabine", "PO",
    "20 patients 75-92 years and 40 younger adults", "capecitabine and three metabolites",
    "four sequential compartments; slower absorption at 75 years and over; time-varying 5-FU elimination",
    "expansion", "new_engine", "C17"),
  m("gemcitabine__lung__tham2008", "gemcitabine", "gemcitabine", "IV",
    "56 Asian adults with lung cancer", "gemcitabine, dFdU, intracellular dFdCTP",
    "gemcitabine in two compartments (V1 2.96 L, V2 47.6 L, Q 156 L/h, CLG 305.2 L/h per 70 kg/1.76 m) forming dFdU (73%; one compartment, 52.6 L, 21.5 L/h) and PBMC dFdCTP (23%); lean body mass by Janmahasatian, as the 2015 erratum corrects",
    "expansion", "linear_metabolite", "C18",
    "PDF and erratum read (supplied 2026-11-06). The LBM constants and exponents are not printed: ffmAlSallami() (Janmahasatian in adults; Shafer's choice) with the male standard (56.13 kg), exponents 0.75/1. Intracellular dFdCTP (a second metabolite, not a plasma concentration) and the unselected saturable variant are not carried.",
    readiness = "implemented", stanpumpr_drug = "gemcitabine"),
  m("gemcitabine__japanese__sugiyama2010", "gemcitabine", "gemcitabine", "IV",
    "250 Japanese adults", "gemcitabine and dFdU",
    "two compartments each; CDA genotype, S-1, age and creatinine", "expansion", "linear_metabolite", "C19",
    "A CDA genotype is not a Patient Profile field."),
  m("cytarabine__children__periclou1996", "cytarabine", "cytarabine", "IV",
    "52 children 2 months-19 years with leukaemia", "cytarabine and Ara-U",
    "cytarabine two compartments, Ara-U one; age and BSA on every parameter", "expansion", "linear_metabolite", "C21",
    "Not an intracellular Ara-CTP model."),
  m("cytarabine__aml__kroghmadsen2012", "cytarabine", "cytarabine", "IV",
    "23 adults with AML", "cytarabine",
    "two compartments: CL 272 L/h x (1 - 0.0020 (bWBC - median)), Vc 62.8 L, Q 13.7 L/h, Vp 75.4 L",
    "expansion", "linear", "C22",
    "PDF read (supplied 2026-11-06), complete; not yet implemented. Samples fell below the limit after about 2 h, so the terminal phase is weak (Vp RSE 32%).",
    readiness = "complete_not_implemented"),
  m("pemetrexed__phase2__latz2006", "pemetrexed", "pemetrexed", "IV",
    "287 adults from ten phase II trials", "pemetrexed",
    "two compartments: CL 43.0 + 47.2 x CrCL/92.6 mL/min (Cockcroft-Gault), V1 6.13 x BSA^1.32 L, Q 14.5 mL/min, V2 3.38 L",
    "expansion", "linear", "C20",
    "PDF read (supplied 2026-11-06). Reproduces its terminal half-life (3.50 h) and Vss (16.3 L at its own V1). Fitted without folate and B12 supplementation: PK may transfer, toxicity does not. Q is 14.5 mL/min in Table 3 and 14.4 in the text; neither the BSA formula nor the Cockcroft-Gault weight is named (Mosteller and total weight used). The linear clearance's 43 mL/min intercept is untested below a CrCL of about 40 mL/min.",
    readiness = "implemented", stanpumpr_drug = "pemetrexed"),
  m("ifosfamide__sarcoma__kerbusch2000", "ifosfamide", "ifosfamide", "IV",
    "15 adults with soft-tissue sarcoma, 72-h infusion", "ifosfamide",
    "one compartment with enzyme-turnover autoinduction", "expansion", "new_engine", "C10"),
  m("ifosfamide__sclc__kerbusch2001", "ifosfamide", "ifosfamide", "IV",
    "14 adults with small-cell lung cancer", "ifosfamide, 2- and 3-dechloroethylifosfamide, 4-hydroxyifosfamide",
    "autoinduction with metabolite compartments", "expansion", "new_engine", "C11",
    "The dechloroethylated metabolites are inactive; chloroacetaldehyde, released as they form, is the neurotoxic species and is not measured."),
  m("melphalan__neuroblastoma__duong2019", "melphalan", "melphalan", "IV",
    "51 children with high-risk neuroblastoma", "melphalan",
    "two compartments allometric in weight: CL 8.9 L/h, VC 6.8 L, Q 4.1 L/h, VP 4.6 L at WT_STD",
    "expansion", "linear", "C04",
    "PDF read (supplied 2026-11-06). WT_STD 15.5 kg on Shafer's decision.",
    readiness = "implemented", stanpumpr_drug = "melphalanPediatric"),
  m("melphalan__myeloma__nath2010", "melphalan", "melphalan", "IV",
    "100 adults with myeloma, 36-73 years", "total and unbound melphalan",
    "two compartments, total melphalan: CL 17 x (HCT/34)^0.462 x (FFM/50)^0.75 + 11.1 x CLcr/88 L/h (Cockcroft-Gault per 70 kg), V1 13.2 x FFM/50 L, Q 30.6 L/h, V2 15 L",
    "expansion", "linear", "C33",
    "PDF read (supplied 2026-11-06). Its own fat-free-mass (Janmahasatian) and Cockcroft-Gault equations, which the library's ffmAlSallami() (Janmahasatian in adults) and creatinineClearanceCG() at 70 kg reproduce (to 0.05%). Reproduces its CL (28.1 L/h, 40% renal), Table 7's half-lives and median AUC. The FFM reference is 50 kg as printed, though the Methods centre on the median 53.3. Haematocrit has no Patient Profile field and is held at 34%. The unbound model is not used.",
    readiness = "implemented", stanpumpr_drug = "melphalan"),
  m("fludarabine__hct__langenhorst2019", "fludarabine", "fludarabine", "IV",
    "258 HCT recipients 0.3-74 years", "F-ara-A",
    "three compartments; allometric weight; eGFR on renal clearance", "expansion", "linear", "C34",
    "Dosed as fludarabine phosphate, measured as F-ara-A: the dose basis must be fixed from the paper."),
  m("thiotepa__highdose__dejonge2004", "thiotepa", "thiotepa", "IV",
    "49 adults (86 courses), high-dose cyclophosphamide, thiotepa and carboplatin", "thiotepa and TEPA",
    "thiotepa and TEPA two compartments each; inducible and non-inducible thiotepa elimination, the enzyme induced by cyclophosphamide; joined to the cyclophosphamide model",
    "expansion", "new_engine", "C07",
    "A joint model of two drugs with mutual interaction. The handoff's audit gave a different first author (Huitema); PubMed's is de Jonge."),
  m("topotecan__adult__mould2002", "topotecan", "topotecan", "IV",
    "245 adults with solid tumours, nine trials", "total topotecan (lactone and carboxylate)",
    "two compartments: CL (5.77 + 12.8 x CLCR/70 x 70/WT) x (WT/70)^0.75 x exp(-0.12 (ECOG - 1)) L/h, V1 19.3, V2 45.7 L x WT/70, Q 90.4 L/h x (WT/70)^0.75",
    "expansion", "linear", "C29",
    "PDF read (supplied 2026-11-06). The printed renal-function equation (Cockcroft-Gault standardised to 70 kg) is used, on Shafer's decision of 2026-11-06, although Table VI's worked clearances and the Discussion's median AUC reproduce only without the 70/WT term. ECOG held at 1; the research formulation's factor not applied; the fitted infusion duration replaced by the entered one.",
    readiness = "implemented", stanpumpr_drug = "topotecan"),
  m("temozolomide__glioma__ostermann2004", "temozolomide", "temozolomide", "PO",
    "35 adults with malignant glioma", "temozolomide, plasma and CSF",
    "oral three compartments with first-order absorption and plasma-CSF transfer; BSA and sex",
    "expansion", "linear", "C36",
    "Oral only; the CSF compartment is a measured site, not a fitted effect site."),
  m("mercaptopurine__all__hawwa2008", "mercaptopurine", "mercaptopurine", "PO",
    "19 children with ALL", "red-cell 6-TGN and 6-mMPN",
    "red-cell metabolite model; BSA and TPMT genotype", "expansion", "new_engine", "C38b",
    "Red-cell nucleotides are not plasma concentrations; TPMT is not a Patient Profile field."),
  m("thioguanine__civ__kitchen1997", "thioguanine", "thioguanine", "IV",
    "18 patients 4-25 years (median 18), mostly solid tumours, continuous infusion", "thioguanine",
    "two compartments with capacity-limited (Michaelis-Menten) elimination", "expansion", "michaelis_menten", "N02",
    "Continuous intravenous infusion only; does not apply to oral maintenance."),
  m("thioguanine__all__lowe2001", "thioguanine", "thioguanine", "PO and IV",
    "children with standard-risk ALL", "thioguanine, plasma and CSF",
    "no structural model in the abstract", "expansion", "unknown", "N03"),
  none("thioguanine__oral_maintenance", "thioguanine", "thioguanine (oral maintenance)", "PO", "expansion",
       "No contemporary oral oncology population model was identified. Do not substitute an IBD model or a mercaptopurine model."),
  m("pegaspargase__all__wurthwein2021", "asparaginase", "pegaspargase", "IV",
    "1374 children with ALL (1253 for validation)", "asparaginase activity",
    "population model of activity; clearance and volume depend on treatment phase; age over 8 and sex",
    "expansion", "unknown", "C38a",
    "Activity units (IU) are carried. Phase-dependent parameters might be PK events; silent inactivation is not modelled."),
  none("calaspargase__none", "asparaginase", "calaspargase pegol", "IV", "expansion",
       "No source model identified in this review; pegaspargase's does not transfer."),
  none("asparaginase_ecoli_native__none", "asparaginase", "native E. coli asparaginase", "IM or IV", "expansion",
       "No source model identified in this review."),
  none("asparaginase_erwinia__none", "asparaginase", "Erwinia-derived and recombinant Erwinia asparaginase", "IM or IV", "expansion",
       "No source model identified in this review."),
  m("daunorubicin__aml__kroghmadsen2012", "daunorubicin", "daunorubicin (conventional)", "IV",
    "23 adults with AML", "daunorubicin",
    "two compartments: CL 129 L/h, Vc 453 L x (1 + 0.0185 (bWBC - 39)), Q 152 L/h, Vp 911 L, estimated with Bogason 2011 as a NONMEM prior",
    "expansion", "linear", "C22",
    "PDF read (supplied 2026-11-06), complete but mostly the prior's: every sample after 9 h was below the limit, and the data alone gave Vc 198 and Vp 223 L against the prior's 373 and 1120. The dose basis (hydrochloride or base) is not stated. Not implemented.",
    readiness = "complete_not_implemented"),
  m("daunorubicin__aml__bogason2011", "daunorubicin", "daunorubicin (conventional)", "IV",
    "40 adults with AML, 33-83 years", "daunorubicin",
    "two compartments (basic model): CL 115 L/h, Vc 373 L, Q 252 L/h, Vp 1120 L; WBC model Vc 412 x (1 + 0.0138 (WBC - 39))",
    "expansion", "linear", "N07",
    "PDF read (supplied 2026-11-06). The basic model is used (the white count has no field, and its term's RSE is 47%). Krogh-Madsen 2012 used this model as a NONMEM prior. The dose basis is not named; doses as labelled. Daunorubicinol measured but not in the final model.",
    readiness = "implemented", stanpumpr_drug = "daunorubicin"),
  m("daunorubicin__aml__varatharajan2016", "daunorubicin", "daunorubicin (conventional)", "IV",
    "70 adults with de novo AML", "daunorubicin and daunorubicinol",
    "parent and metabolite (Monolix); pharmacogenetics", "expansion", "linear_metabolite", "C23",
    "Not the fixed-ratio liposomal daunorubicin-cytarabine product."),
  m("epirubicin__adult__ralph2004", "epirubicin", "epirubicin", "IV",
    "105 + 18 adults with breast cancer or hepatocellular carcinoma", "epirubicin",
    "limited-sampling MAP Bayesian clearance on an underlying population model", "expansion", "unknown", "C24",
    "The underlying population model is what is needed; MAP sampling validation is not clinical validation."),
  m("idarubicin__children__reid1990", "idarubicin", "idarubicin", "IV and PO",
    "21 children with relapsed leukaemia", "idarubicin, idarubicinol, CSF",
    "classical three compartments; idarubicinol half-life 56.8 h", "expansion", "linear", "C25",
    "Classical, not a covariate population model; idarubicinol's own parameters to extract."),
  m("bleomycin__adult__hall1982", "bleomycin", "bleomycin", "IV and SC",
    "19 patients (11 IV, 8 SC)", "bleomycin by radioimmunoassay (units)",
    "biexponential after IV; clearance per m2 correlated with serum creatinine", "expansion", "linear", "C42",
    "Dosed in units (activity), not mass. The handoff named the first author Crooke; PubMed's is Hall."),
  m("dacarbazine__adult__breithaupt1982", "dacarbazine", "dacarbazine", "IV",
    "8 adults with melanoma or sarcoma", "dacarbazine and AIC",
    "dacarbazine biexponential, AIC monoexponential; renal and extrarenal clearance", "expansion", "linear", "C43",
    "AIC is a downstream product, not the active methylating species (MTIC; N06): the parent curve only."),
  m("bendamustine__nhl__owen2010", "bendamustine", "bendamustine", "IV",
    "adults with indolent non-Hodgkin lymphoma", "bendamustine and metabolites",
    "triphasic decline; no covariate effects", "expansion", "linear", "C37a"),
  m("vinblastine__adult__owellen1977", "vinblastine", "vinblastine", "IV",
    "adults (number not in the abstract)", "vinblastine (radioimmunoassay)",
    "classical three compartments; deacetylvinblastine an active metabolite", "expansion", "linear", "C41",
    "A classical study, not a population analysis."),
  m("vinorelbine__adult__variol2002", "vinorelbine", "vinorelbine", "IV and PO",
    "175 adults in phase I and II", "vinorelbine",
    "three compartments fitted to oral and IV together (Table 3: CL 41.9 L/h, Vc 22.2 L, K24 2.46, K42 0.402, K23 1.39, K32 0.0337 /h; F 0.36, ka 0.481 /h, lag 0.15 h); BSA, platelets, creatinine clearance and transaminases on clearance in Eq. 1",
    "expansion", "linear", "C32",
    "PDF read (supplied 2026-11-06). Table 3's values are used, without covariates, on Shafer's decision (2026-11-06): Eq. 1 gives 53.1 L/h at the mean BSA, 27% above Table 3, whose 41.9 L/h fits Fig 1's tail. Whole blood. The IV infusion duration and the dose basis are not stated.",
    readiness = "implemented", stanpumpr_drug = "vinorelbine"),
  m("cladribine__lymphoid__lindemalm2005", "cladribine", "cladribine", "IV, SC and PO",
    "161 adults with lymphoid malignancies", "cladribine",
    "three compartments; F 100% SC and 35% oral; no covariates", "expansion", "linear", "N04",
    "Table 3 confirmed on the PDF. No renal term (no patient had severe impairment); the oral route is the solution, not the multiple-sclerosis tablet.",
    readiness = "implemented", stanpumpr_drug = "cladribine"),
  m("cladribine__children__kearns1994", "cladribine", "cladribine", "IV",
    "25 children with acute leukaemia, 5-day infusion", "cladribine, plasma and CSF",
    "two compartments (radioimmunoassay); renal clearance about half", "expansion", "linear", "N05"),
  m("cladribine__ms__savic2017", "cladribine", "cladribine (tablets, multiple sclerosis)", "PO",
    "173 adults with multiple sclerosis", "cladribine and 2-chloroadenine",
    "cladribine three compartments, 2-chloroadenine one; creatinine clearance on renal clearance",
    "expansion", "linear_metabolite", "C37b",
    "Multiple sclerosis tablets: context for an oncology model, not one."),
  m("paclitaxel_nab__adult__chen2014", "paclitaxel", "paclitaxel protein-bound (nab-paclitaxel)", "IV",
    "150 adults with solid tumours", "paclitaxel",
    "three compartments with saturable distribution and saturable elimination; albumin on elimination",
    "expansion", "new_engine", "C27",
    "Nonlinear distribution in three compartments; the Michaelis-Menten engine carries one peripheral compartment."),
  m("doxorubicin_liposomal__hsu2018", "doxorubicin", "pegylated liposomal doxorubicin", "IV",
    "not stated in the abstract", "encapsulated and free doxorubicin",
    "encapsulated one compartment releasing into free doxorubicin in two compartments; bioequivalence simulation",
    "expansion", "new_engine", "C26",
    "Release from a carrier. Conventional doxorubicin's parameters must not be borrowed."),
  m("irinotecan_liposomal__adiwijaya2017", "irinotecan", "liposomal irinotecan", "IV",
    "353 adults from six studies", "total irinotecan and total SN-38",
    "joint model of the liposomal product; race, BSA and bilirubin", "expansion", "new_engine", "C28",
    "Total species from a carrier; retained covariates to confirm.")
)

# --- Relations ----------------------------------------------------------------
# Every registry row's source as its structural model, then the documents that
# support a drug in another role, then the guidance that applies to all.
r <- function(source_id, drug, role, model_id = NA, note = NA)
  data.frame(source_id, drug, model_id, role, note, stringsAsFactors = FALSE)

withSource <- registry[!is.na(registry$source_id), ]
relations <- rbind(
  r(withSource$source_id, withSource$drug, "structural_pk", withSource$model_id),
  r("P01", "carboplatin", "dosing_formula", note = "The Calvert formula: a dose from GFR and a target AUC, not a concentration model."),
  r("G06", "carboplatin", "dosing_guideline"),
  r("G14", "carboplatin", "dosing_guideline", note = "Linked to carboplatin, as the handoff asks."),
  r("L01", "carboplatin", "product_label"),
  r("G06", "cisplatin", "dosing_guideline"),
  r("L02", "cisplatin", "product_label"),
  r("C07", "cyclophosphamide", "interaction_context", "thiotepa__highdose__dejonge2004",
    "The joint model includes cyclophosphamide's induction of thiotepa's elimination."),
  r("L03", "cyclophosphamide", "product_label"),
  r("P06", "thiotepa", "interaction_context", note = "A cyclophosphamide model in a regimen containing thiotepa; not a thiotepa model. P07 is not thiotepa evidence at all."),
  r("P11", "fluorouracil", "clinical_outcome_trial", note = "Randomised PK-guided dosing against BSA dosing; not a structural model."),
  r("G01", "fluorouracil", "target_guideline"),
  r("G08", "fluorouracil", "pharmacogenetic_guideline", note = "A genotype dose recommendation, not a fitted clearance multiplier."),
  r("G09", "fluorouracil", "pharmacogenetic_guideline"),
  r("R04", "fluorouracil", "regulatory_safety"),
  r("R05", "fluorouracil", "regulatory_safety"),
  r("L04", "fluorouracil", "product_label"),
  r("R04", "capecitabine", "regulatory_safety"),
  r("R05", "capecitabine", "regulatory_safety"),
  r("G08", "capecitabine", "pharmacogenetic_guideline"),
  r("P14", "methotrexate", "external_validation", note = "An external evaluation of published HDMTX models in adults with haematological tumours."),
  r("G03", "methotrexate", "management_guideline", note = "Glucarpidase: a rescue threshold, not a therapeutic range."),
  r("G04", "methotrexate", "management_guideline"),
  r("G06", "methotrexate", "dosing_guideline"),
  r("L05", "methotrexate", "product_label"),
  r("L06", "doxorubicin", "product_label"),
  r("G02", "paclitaxel", "target_guideline", note = "A regimen-specific time above a concentration, not a band."),
  r("L07", "paclitaxel", "product_label"),
  r("L08", "vincristine", "product_label"),
  r("G10", "irinotecan", "pharmacogenetic_guideline"),
  r("L09", "irinotecan", "product_label"),
  r("P22", "etoposide", "binding_exposure_toxicity", note = "Organ function and binding against toxicity; not a structural model."),
  r("L10", "etoposide", "product_label"),
  r("G05", "busulfan", "target_guideline"),
  r("L11", "busulfan", "product_label"),
  r("L12", "paclitaxel", "product_label", "paclitaxel_nab__adult__chen2014"),
  r("L13", "doxorubicin", "product_label", "doxorubicin_liposomal__hsu2018"),
  r("L14", "irinotecan", "product_label", "irinotecan_liposomal__adiwijaya2017"),
  r("G11", "mercaptopurine", "pharmacogenetic_guideline"),
  r("G11", "thioguanine", "pharmacogenetic_guideline"),
  r("G12", "asparaginase", "management_guideline"),
  r("G13", "asparaginase", "management_guideline", note = "Also discusses blinatumomab, which is not part of this project."),
  r("N06", "dacarbazine", "mechanistic_context", note = "In vitro activation by CYP; not a clinical model."),
  r("G07", "global", "dosing_guideline", note = "Obesity: applies to each drug as its own guidance says."),
  r("R01", "global", "regulatory_method"),
  r("R02", "global", "regulatory_method"),
  r("R03", "global", "regulatory_method")
)

stopifnot(!anyDuplicated(sources$source_id), !anyDuplicated(registry$model_id),
          all(registry$readiness %in% READINESS), all(registry$engine %in% ENGINE),
          all(sources$review_depth %in% REVIEW), all(relations$role %in% ROLES),
          all(relations$source_id %in% sources$source_id))

dir <- file.path("inst", "extdata")
utils::write.csv(sources, file.path(dir, "chemotherapySources.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(registry, file.path(dir, "chemotherapyRegistry.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(relations, file.path(dir, "chemotherapyRelations.csv"), row.names = FALSE, fileEncoding = "UTF-8")
