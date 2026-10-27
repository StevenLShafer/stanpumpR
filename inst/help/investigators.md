stanpumpR is a thin layer of software over a thick layer of other people's work. Every curve it draws rests on a clinical study in which patients or volunteers were given a drug, blood was sampled, and a model was fitted. This page names the investigators whose work the program uses, and what of theirs is in it. Citations are on the [Bibliography](help:references) page and on each drug's page. Wherever an investigator is named in the help, or in the References panel under the plot, the name is a link that opens a summary of their contributions.

## The program

**Steven L. Shafer**, Stanford University. Author of STANPUMP (1987 to 1997) and of stanpumpR. The effect-site targeting algorithm (with Keith Gregg), the plasma targeting algorithm (with James Bailey), the time-to-peak-effect method for carrying ke0 between models (with John Varvel), the pooled fentanyl kinetics, and the design decisions recorded throughout the code are his.

**Dean Attali**. Co-author of the stanpumpR package. The Shiny architecture of the current app, its testing and deployment pipeline, the dose-table draft and undo mechanism and the `{undomanager}` package it uses are his work.

**Alexander Clarke** wrote the vignettes that document the scripting interface.

**Richard H. Epstein**, University of Miami, validated the inhaled-gas engine against GasMan, running the comparison scenarios through the GasMan API and tracing each disagreement to its cause.

Portions of the code written since 2026, including the inhaled-gas engine, the time-until-threshold calculation and this help system, were drafted with Claude Code at Dr Shafer's direction and verified by the test suite; each such file records this in a provenance header.

## CATIA, STANPUMP and the first generation of TCI

The first target-controlled infusion was given in Bonn in 1979, and STANPUMP was one of several programs that, in the 1980s and 1990s, followed it in using pharmacokinetic models to control infusion pumps. Their authors exchanged concepts and algorithms freely:

- **Donald R. Stanski** and Steven Shafer, Stanford (STANPUMP). Stanski's laboratory also produced the fentanyl and alfentanil kinetics (with **Jeffrey C. Scott**) and, with Lewis Sheiner, the effect-compartment model itself.
- **Daniel B. Raemer**, Brigham and Women's Hospital, whose 1990 alfentanil study with the Stanford group introduced the median performance error and median absolute performance error as the measures of a pump's bias and inaccuracy.
- **Jürgen Schüttler** and **Helmut Schwilden**, University of Bonn (CATIA).
- **Max E. Ausems** and **Carl C. Hug**, University of Leiden and Emory (TIAC): the accuracy of alfentanil kinetics under computer-assisted infusion (1985) and, with **Jaap Vuyk** and Stanski, its comparison with intermittent boluses (1988).
- **Joseph G. Reves** and **J. Michael Alvis**, University of Alabama (CACI), and **James R. Jacobs** and Reves at Duke (CACI II).
- **Johan F. Coetzee** and Pina, Stellenbosch University (STELPUMP).
- **Tom De Smet** and **Michel M. R. F. Struys**, University of Ghent (RUGLOOP).

Struys and colleagues reviewed this history in *The History of Target-Controlled Infusion* (*Anesth Analg* 2016;122:56-69), with two companion reviews: *Target-Controlled Infusion: A Mature Technology* by Absalom and colleagues (*Anesth Analg* 2016;122:70-78) and *The Safety of Target-Controlled Infusions* by Schnider and colleagues (*Anesth Analg* 2016;122:79-85). See [From CATIA to stanpumpR](help:history).

## The pharmacokinetic models

| Drug | Investigators | What is used |
|---|---|---|
| propofol | **Douglas J. Eleveld** and colleagues, Groningen | The 2018 general-purpose propofol model, with its fat-free-mass, maturation and ageing covariates. **Thomas W. Schnider**'s 1999 time to peak effect gives ke0; his 1998 kinetic model, the one in most pumps, is run beside it when *Minto / Schnider* is ticked ([The Minto and Schnider models](help:models/minto-schnider)). |
| remifentanil | Eleveld and colleagues (2017); **Tae Kyun Kim**, **Talmage D. Egan** and colleagues (2017); **Charles F. Minto**, Schnider, Shafer and colleagues (1997) | Eleveld's allometric model below BMI 30 and Kim's obesity model at and above it. Minto's 1997 model, the basis of STANPUMP's and most pumps' remifentanil kinetics, is run beside them when *Minto / Schnider* is ticked. |
| fentanyl, alfentanil | Scott and Stanski (1987); Shafer, **John R. Varvel** and colleagues (1990) | Alfentanil's parameters are Scott and Stanski's. Fentanyl's are the pooled analysis of Shafer and Varvel, scaled allometrically, with Scott and Stanski cited. |
| sufentanil | **Elisabeth Gepts** and colleagues (1995) | Three-compartment kinetics in surgical patients |
| morphine | **Jörn Lötsch** and colleagues (2002); **N. Atrux-Tallau** and colleagues (2022) | Three-compartment kinetics, weight-scaled V1; oral tablet and liquid absorption |
| pethidine | **Sven Björkman** (2003) | A physiologically based model, recast as compartments |
| hydromorphone | **David R. Drover** and colleagues (2002); **B. A. Coda** and colleagues (2003); **Terhi J. Lohela** and colleagues (2021) | Intravenous kinetics; intranasal absorption; oral bioavailability and absorption (the intramuscular route is still provisional) |
| methadone | **Thomas K. Henthorn** and **Evan D. Kharasch** (2025); **Charles E. Inturrisi** and colleagues (1987); Kharasch and colleagues (2004); **C. B. Eap** and colleagues (2002) | Separate R(-) and S(+) enantiomer kinetics in volunteers, summed to racemic methadone; the time to peak effect (10 min, Kharasch's peak miosis, within Inturrisi's range for analgesia); oral bioavailability (Kharasch) and absorption (Eap) |
| ketamine | **Edward F. Domino** and colleagues (1984) | Kinetics in volunteers |
| dexmedetomidine | **Jeffrey B. Dyck** and colleagues (1993); **Athena F. Zuppa** and colleagues (2019) | Adult kinetics; the infant model with cardiopulmonary-bypass parameters |
| midazolam | **Michael Bührer**, Stanski and colleagues (1990); **Katayoun Zomorodi**, Shafer and colleagues (1998) | Three-compartment kinetics fitted to Bührer's volunteer data, the set STANPUMP first used for midazolam TCI, printed in Zomorodi's Table 3; Bührer's EEG time to peak effect |
| diazepam | **Orlando R. Hung**, Shafer, Stanski and colleagues (1996); Bührer, Stanski and colleagues (1990) | Three-compartment kinetics with arterial sampling; the EEG time to peak effect |
| lorazepam | **F. Nielsen-Kudsk** and colleagues (1983); **David J. Greenblatt** and colleagues (1982, 2000); **Juliana Barr** and colleagues (2001) | Two-compartment kinetics from mean half-lives; oral and intramuscular absorption; the EEG time to peak effect; the sedation band |
| alprazolam | **C. Lindsay DeVane** and colleagues (1993); **Karthik Venkatakrishnan** and colleagues (2005) | Apparent oral kinetics with weight and age; the EEG effect-site rate constant |
| clonazepam | **Fábio M. dos Santos**, **François Noël** and colleagues (2009) | Apparent oral two-compartment kinetics of tablets; the finding that the effect follows plasma directly |
| zolpidem | **Hyun Chul Kim** and colleagues (2026) | Apparent oral kinetics with transit absorption, as a lag |
| temazepam | **Alfred L. van Steveninck**, **Adam F. Cohen** and colleagues (1994); **N. J. Halliday**, **John W. Dundee** and colleagues (1987); **F. O. Müller** and colleagues (1987) | Intravenous mean data from two studies, to which two compartments were fitted; oral absorption |
| etomidate | **John R. Arden** and colleagues (1986) | Kinetics in patients, including the elderly |
| lidocaine | Schnider and colleagues (1996); **M. J. Simon** and colleagues (2002) | Two-compartment kinetics during an infusion; a regional anesthesia (RA) absorption rate fitted to axillary block with epinephrine, F = 1 assumed |
| bupivacaine | **A. G. Burm** and colleagues (1987); **C. Robison** and colleagues (1991) | Two-compartment intravenous kinetics from a stable-isotope tracer; a provisional RA absorption rate matched to the mean peak after femoral and sciatic block, F = 1 assumed |
| ropivacaine | Simon and colleagues (2006); **Y. Hartog**; **V. A. Vainionpää** and colleagues (1995) | Three-compartment intravenous tracer kinetics in patients 61 years and over, as completed by Hartog; a provisional RA absorption rate matched to the mean peak after axillary block, F = 1 assumed |
| mepivacaine | Burm and colleagues (1997); Simon and colleagues (2002) | Intravenous clearances and volumes of the two enantiomers, reduced exactly to one two-compartment model of the racemate; RA absorption through fast and slow depots in parallel, fitted to axillary block with epinephrine, F = 1 assumed |
| rocuronium | **Bertrand Plaud** and colleagues (1995); **Luis I. Cortínez** and colleagues (2007) | Kinetics; time to peak effect |
| vecuronium | **Kevin R. Vega-Villa**, **Tae Hyun Han** and colleagues (2014); **Stephen M. Rupp**, **Dennis M. Fisher**, **Ronald D. Miller** and colleagues (1987) | Three-compartment kinetics in non-burn surgical adults; the effect-site rate constant and potency |
| cisatracurium | **Shahpoor S. Sorooshian**, **Peter M. C. Wright** and colleagues (1996) | Population kinetics and pharmacodynamics in young and elderly adults |
| acetaminophen | **James D. Morse** and colleagues (2022); **Brian J. Anderson** and colleagues (2001) | Intravenous and oral kinetics with clearance on normal fat mass; the analgesic effect-site rate constant |
| ibuprofen | **James D. Morse** and colleagues (2022); **Jacqueline A. Hannam**, **Brian J. Anderson** and colleagues (2018) | Intravenous and oral kinetics with clearance and volumes on normal fat mass; the analgesic effect-site rate constant |
| naproxen | **Pyry Välitalo**, **Elina Kumpulainen**, **Hannu Kokki** and colleagues (2012); **Marcus A. Björnsson** and **Ulrika S. H. Simonsson** (2011) | Two-compartment oral kinetics, fitted in children; the unbound analgesic EC50 used as the threshold |
| diclofenac | **J. F. Standing** and colleagues (2011) | Three-compartment intravenous and oral kinetics; the dispersible tablet's two lagged absorption paths |
| diclofenacEC | **C. Bartels**, M. Armogida and B. Hamren (2010) | Apparent oral two-compartment kinetics pooled over four release types; the enteric-coated tablet, with bioavailability 0.784 relative to immediate release |
| meloxicam | **T. Aoyama** and colleagues (2017); US Food and Drug Administration, ANJESO clinical pharmacology review (2020) | Apparent oral two-compartment kinetics with CYP2C9 genotype; a separate three-compartment intravenous model with weight and eGFR, the two routes' concentrations added |
| ketorolac | **M. E. Cloesmeijer** and colleagues (2021); **E. J. Mroszczak** and colleagues (1990) | S and R enantiomers fitted separately and summed, doses as ketorolac tromethamine; a provisional oral absorption rate |
| celecoxib | US Food and Drug Administration, clinical pharmacology review of NDA 211759 (2025); **S. Krishnaswami** and colleagues (2012); Hannam and colleagues (2023) | Two-compartment apparent oral kinetics fitted for stanpumpR to the review's mean capsule curve, not a population model; weight exponents (Krishnaswami); the effect-site rate constant (Hannam) |
| aspirin, salicylate | **J. Koh** and colleagues (2025) | Enteric-coated low-dose aspirin, one compartment, with salicylate as its metabolite; absorption and pre-systemic steps approximated by two lagged depots; linear, so low doses only |
| naloxone | **J. Dowling** and colleagues (2008); **C. M. Laffont** and colleagues (2024); **A. Yassen** and colleagues (2007) | Intravenous kinetics with clearance on lean body weight; the nasal route; the effect-site rate constant |
| oxytocin | **James C. Eisenach** (unpublished); Tanaka and colleagues | Human kinetics from unpublished data; a rat model |
| oxycodone | **Reino Pöyhiä** and colleagues (1991); **Maija Kirvelä** and colleagues (1996); **Antti Liukas** and colleagues (2011); **Bojan Lalovic** and colleagues (2006); **Rajiv Balyan** and colleagues (2017); **Merja Kokki** and colleagues (2012) | Pooled intravenous kinetics with age (Liukas) and renal (Kirvelä) effects on clearance; oral absorption and the effect-site rate constant (Lalovic); CYP2D6 and oxymorphone (Balyan, Lalovic); measured analgesic concentrations behind the MEAC (Kokki) |
| oliceridine | **Albert Dahan** and colleagues (2020) | Two-compartment kinetics, the analgesic (cold-pressor) potency used for MEAC and the respiratory potency used as the typical concentration |
| remimazolam | Eleveld and colleagues (2025) | Kinetics with size, age and sex covariates |
| codeine | **Kristin Persson**, **David R. Guay** and colleagues; **Muhammad W. Ashraf** and colleagues (2024) | One-compartment disposition; CYP2D6-dependent formation of morphine |
| tramadol, desmetramadol | **Sophie Holford** and colleagues (2014); **N. Brvar** and colleagues (2014); **Ulrike M. Stamer** and colleagues (2007) | Joint parent-and-metabolite kinetics; oral input timing (Brvar); CYP2D6 phenotype weights |
| hydrocodone | **Murad R. Melhem** and colleagues (2013); **Ram P. Kapil** and colleagues (2015); **S. Victoria Otton** and colleagues (1993) | Apparent oral disposition; CYP2D6-dependent formation of hydromorphone |
| gabapentin | **P. Tran** and colleagues (2017) | One-compartment kinetics with saturable absorption; clearance on creatinine clearance |
| pregabalin | **Phylinda L. S. Chan** and colleagues (2021); **Michiel J. van Esdonk** and colleagues (2018) | One-compartment kinetics with clearance on creatinine clearance; the delay to analgesia, as a time to peak effect |
| oxymorphone | Endo (manufacturer summary); **Michael P. Adams** and **Harry Ahdieh** (2005) | One-compartment disposition; also oxycodone's metabolite |
| buprenorphine | **M. Björnsson** and colleagues (2023); **J. Eriksen** and colleagues (1989); Yassen and colleagues (2006) | Three-compartment intravenous kinetics; sublingual absorption as one first-order input with dose-dependent bioavailability (Björnsson); the intranasal route, withdrawn in October 2026 because the model could not reproduce its measured peak (Eriksen); the effect-site rate constant (Yassen) |
| diamorphine | **L. Cai** and colleagues (2025) | The parent drug's plasma kinetics after intramuscular and intranasal doses, in the opt-in *Illicit drugs* group; its metabolites, 6-monoacetylmorphine and morphine, are not modelled |
| cocaine | **Melvin J. Chow** and colleagues (1985); **A. Robert Jeffcoat** and colleagues (1989); **Karin Fattinger** and colleagues (2000) | Intravenous disposition, reduced here to one compartment (Chow); the intranasal absorption rate and bioavailability (Jeffcoat); the two-path nasal input that the single path here simplifies (Fattinger). In the opt-in *Illicit drugs* group, plasma only |
| methamphetamine | **Debra S. Harris** and colleagues (2003); **C. E. Cook** and colleagues (1992, 1993); **John E. Mendelson** and colleagues (2006) | Intranasal bioavailability and the disposition read as one compartment (Harris); the one-compartment oral structure and the oral and smoked bioavailabilities (Cook); the difference between the enantiomers (Mendelson). In the opt-in *Illicit drugs* group, plasma only |
| LSD | **Patrick C. Dolder** and colleagues (2017); **Friederike Holze** and colleagues (2024) | The one-compartment oral model with apparent clearance and volume (Dolder); the later, longer half-lives that this model does not carry (Holze). In the opt-in *Illicit drugs* group, plasma only |
| psilocybin | **Friederike Holze** and colleagues (2023); **Martin K. Madsen** and colleagues (2019) | Apparent oral kinetics of free plasma psilocin (Holze); receptor occupancy as a direct function of plasma psilocin, which is why no effect site is plotted (Madsen). In the opt-in *Illicit drugs* group |
| MDMA | **Marilyn A. Huestis** and colleagues (2025) | Apparent oral kinetics on a hydrochloride dose basis, with the published zero-order depot filling discretised; its MDA metabolite, fitted on a formation fraction fixed rather than estimated, is not plotted. In the opt-in *Illicit drugs* group, plasma only |
| cefazolin | **T. Komatsu** and colleagues (2024) | Unbound two-compartment kinetics with creatinine clearance |
| cefalexin | **A. S. Haynes** and colleagues (2024) | Apparent oral kinetics, fitted in children |
| ceftriaxone | **M. Sanz-Codina** and colleagues (2023) | Two-compartment kinetics of total ceftriaxone |
| clindamycin | **N. Bouazza** and colleagues (2012) | One-compartment kinetics, intravenous and oral |
| gentamicin | **C. Smit** and colleagues (2020) | Two-compartment kinetics with de-indexed eGFR |
| metronidazole | **M. J. J. da Silva Neto** and colleagues (2021) | Intravenous one-compartment kinetics |
| escitalopram | **Liu** and colleagues (2022) | Apparent oral one-compartment kinetics in Chinese psychiatric patients, with CYP2C19 phenotype on clearance |
| citalopram | **Ayman Akil**, **Robert R. Bies**, **Bruce G. Pollock** and colleagues (2016) | Apparent oral kinetics of R- and S-citalopram in older adults with Alzheimer's agitation (CitAD), with age, sex, weight and CYP2C19, combined exactly into one two-compartment model of the racemate |
| sertraline | **Ali A. Alhadab** and **Richard C. Brundage** (2020) | Two-compartment kinetics from a model-based meta-analysis of intravenous and oral studies in healthy adults, with absorption that accelerates after the dose and bioavailability that rises with the dose |
| paroxetine | **Jung-Ryul Kim**, **Doh Kwan Kim**, **Soo-Youn Lee** and colleagues (2015) | Apparent oral one-compartment kinetics from therapeutic drug monitoring, with age and an empirical power of the daily dose on clearance |
| duloxetine | **Zhong** and colleagues (2026) | Apparent oral one-compartment kinetics in depressed inpatients, with clearance 25% lower in women |
| mirtazapine | **Yan** and colleagues (2026) | Apparent oral one-compartment kinetics in depressed inpatients, with clearance 29.4% lower at a BMI of 28 or more |
| bupropion, hydroxybupropion | **Ghimire** and colleagues (2026) | Apparent oral two-compartment kinetics of the sustained-release tablet and its metabolite after a single 150 mg dose in 19 adults, most with chronic kidney disease, with a fixed forming fraction of 0.1 |
| fluoxetine, norfluoxetine | **Han** and colleagues (2025) | Joint apparent oral one-compartment kinetics of fluoxetine and norfluoxetine from steady-state troughs in Chinese psychiatric patients, with sex on clearance |
| venlafaxine, desvenlafaxine | **Wang** and colleagues (2022) | Joint apparent oral kinetics of venlafaxine and O-desmethylvenlafaxine with first-pass formation, in healthy volunteers and psychiatric patients |
| vancomycin | **A. H. Thomson** and colleagues (2009) | Two-compartment kinetics with creatinine clearance |
| hydrocortisone | **D. Bindellini** and colleagues (2024) | Cortisol kinetics, linearised for stress doses |
| methylprednisolone, dexamethasone | **Y. Hong** and colleagues (2007) | Intravenous kinetics |
| prednisolone, prednisone | **J. Xu**, **J. Winkler** and **H. Derendorf** (2007) | The reversible prednisone-prednisolone pair |
| sugammadex | **H. J. Kleijn** and colleagues (2011) | Two-compartment kinetics of total sugammadex with creatinine clearance |
| neostigmine | **T. N. Calvey** and colleagues (1979); **T. Heier** and colleagues (2002) | One patient's two-compartment fit; the time to peak effect |
| glycopyrrolate | **C. Bartels** and colleagues (2013) | Three-compartment kinetics of the active cation |
| mannitol | **K. Kaneda** and colleagues (2010); **A. Rudehill** and colleagues (1993) | Three-compartment kinetics; the conversion to serum osmolality (Rudehill); clearance scaled by creatinine clearance, a stanpumpR assumption |
| calcium chloride, calcium gluconate | **J. R. Ansari** and colleagues (2025) | Two-compartment kinetics of the rise in ionized calcium above baseline, fitted in parturients at cesarean delivery; one model for both salts, with gluconate's dose scaled by the fitted bioequivalence ratio |
| amiodarone, desethylamiodarone | **P. Timothy Pollak**, **Thomas Bouillon** and Shafer (2000) | Apparent oral two-compartment kinetics of the parent and its metabolite in long-term therapy, with each day's oral dose as a constant rate |
| amiodaroneIV | **J. M. Korth-Bradley** and colleagues (1996) | Per-kilogram two-compartment kinetics of acute intravenous amiodarone, for the first one to three days |
| ondansetron (18 and over) | **M. D. Chiang** and colleagues (2021) | Two-compartment intravenous kinetics in adults, with age on central volume |
| ondansetron (below 18 years) | **J. T. Mondick** and colleagues (2010) | Two-compartment intravenous kinetics in children 1-48 months, with allometric weight scaling and maturation of clearance |
| aprepitant, fosaprepitant | **A. L. Nijstad** and colleagues (2023) | One-compartment kinetics with allometric weight scaling, fitted in children |
| methylphenidate | **Y. K. Lyauk** and colleagues (2016); **C. Stage** and colleagues (2017); **A. C. Childress** and colleagues (2025) | d-Methylphenidate after immediate-release racemic tablets in healthy adults, with transit absorption as a lag; the extended-release (Concerta) input, fitted to Childress's curve |
| methylphenidatePediatric | **R. I. Shader** and colleagues (1999) | One-compartment kinetics of total methylphenidate after immediate-release tablets in children 5 to 18 years, clearance proportional to weight |
| lisdexamfetamine | **Y. Tsuda** and colleagues (2020); **S. W. Boellner** and colleagues (2010) | d-Amphetamine after lisdexamfetamine in children with ADHD, with non-Japanese clearance and the dose converted to d-amphetamine base |
| mixedAmphetamineSalts | **J. J. McGough** and colleagues (2003); the Adderall and Adderall XR labels | d-Amphetamine in children 6 to 12, fitted for stanpumpR to the typical patient; extended release as two half-doses 4 h apart; weight exponents from Tsuda |
| quetiapine | **Z. Q. Zheng** and colleagues (2024); **M. Nord** and colleagues (2011) | Apparent oral kinetics of the immediate-release tablet in bipolar inpatients; D2 receptor occupancy (Nord) |
| risperidone, hydroxyrisperidone | **E. Storset** and colleagues (2024) | Apparent oral kinetics of risperidone and 9-hydroxyrisperidone in serum, with CYP2D6 phenotype |
| aripiprazole, dehydroaripiprazole | **J. R. Kim** and colleagues (2008); **E. Kim** and colleagues (2012) | Apparent oral kinetics of aripiprazole and dehydroaripiprazole; the effect site (E. Kim) |
| olanzapine | **L. Sun** and colleagues (2021) | Apparent oral kinetics for a non-smoker, fasted, taking no interacting drugs |
| haloperidol | **V. Pilla Reddy** and colleagues (2013); **J. Y. Park** and colleagues (2006); **H. S. Lim** and colleagues (2013) | Apparent oral kinetics, not yet checked against the full text; CYP2D6 (Park); the effect site (Lim) |
| haloperidolIV | **L. Li** and colleagues (2022); Lim and colleagues (2013) | Intravenous kinetics in critically ill adults; the effect site |
| droperidol | **K. A. Lehmann** and colleagues (1988) | Mean intravenous curves of 30 surgical adults, pooled per mg |
| droperidolIM | **L. K. Foo** and colleagues (2016) | Apparent intramuscular kinetics in acutely agitated patients |
| phenytoin | **A. Odani** and colleagues (1996, 1997); **J. H. Karnes** and colleagues (CPIC, 2021); **B. A. Boucher** and colleagues (1989) | One compartment with Michaelis-Menten elimination of total phenytoin, in Japanese patients; CYP2C9 genotype (Odani 1997, CPIC); the fosphenytoin conversion (the Cerebyx label, Boucher) |
| carbamazepine | **N. M. Graves** and colleagues (1998) | One compartment, apparent oral, in the chronic autoinduced state; single-dose kinetics and the epoxide are not modelled |
| valproate | **P. Teixeira-da-Silva** and colleagues (2022) | One compartment, apparent oral, with weight; delayed-release, syrup and extended-release inputs; intravenous doses on the label's 1:1 conversion |
| phenobarbital | The Munich group, *Epilepsia* (2025) | One compartment in critically ill adults with refractory status epilepticus, allometric on ideal body weight; oral and intravenous |
| pentobarbital | *J Clin Pharmacol* (2026) | Two-compartment intravenous kinetics in critically ill children, allometric on weight; adults extrapolated |
| ethosuximide | **L. Diezi** and colleagues (2023) | Two compartments, apparent oral (syrup), in healthy adults |
| topiramate | **Bamgboye** and colleagues (2026); **A. M. Clark** and colleagues (2013) | Three compartments from a stable-isotope intravenous dose, allometric on weight; oral bioavailability of 1 (Clark) |
| lacosamide | *BMC Pharmacol Toxicol* (2026) | One compartment in Chinese adults, with sex and Cockcroft-Gault creatinine clearance on clearance; intravenous doses on the label's 1:1 conversion |
| lamotrigine | **D. Milosheska** and colleagues (2016) | One compartment, apparent oral; the base model, with no co-medication applied |
| zonisamide | **R. Silva** and colleagues (2025) | One compartment, apparent oral, in adults with refractory epilepsy |
| tiagabine | **S. H. Ingwersen** and colleagues (2000) | One compartment, apparent oral, on monotherapy |
| levetiracetam | **S. J. Rhee** and colleagues (2017) | One compartment, with clearance split into a renal part (on Cockcroft-Gault creatinine clearance) and a non-renal part; intravenous doses on the label's 1:1 conversion |
| eslicarbazepine | **A. Falcão** and colleagues (2012) | One compartment, apparent oral, for the active eslicarbazepine formed from eslicarbazepine acetate, with absorption from a 2013 Simulations Plus poster |
| cenobamate | **V. Vashi** and colleagues (2023) | Two compartments, apparent oral, with an absorption lag; the sponsor's model of healthy volunteers and adults with focal seizures, without its dose, size, ethnicity or co-medication terms; checked against the ascending-dose studies in healthy volunteers of **L. Vernillet** and colleagues (2020) |

## Pharmacodynamics and methods

- **Lewis B. Sheiner**, Stanski and colleagues (1979): the effect compartment.
- Minto, Schnider, Gregg, Henthorn and Shafer (2003): time to peak effect as the model-independent way to carry ke0. Shafer and Varvel had reported the opioids' times to peak effect in 1991; the use in place of ke0 came a decade later. Vecuronium and cisatracurium are the worked cases: each drug's ke0 is converted to the time to peak effect it gives with the kinetics it was fitted with, then solved again against the kinetics the library uses.
- **Keith M. Gregg** and Shafer (1992): the effect-site targeting algorithm, implemented in STANPUMP and in stanpumpR's [target-controlled infusion](help:tci).
- **James M. Bailey** and Shafer (1991): the plasma targeting algorithm.
- **Michael A. Hughes**, **Peter S. A. Glass** and James Jacobs (1992): the context-sensitive half-time. Minto, Schnider and Shafer (1997): the effect-site decrement times showing that remifentanil's stays at a few minutes however long the infusion.
- **Elizabeth J. Youngs** and Shafer (1994): decrement times, the context-sensitive half-time generalised to any percentage decrease.
- **Thomas W. Bouillon** and colleagues (2004): the propofol-remifentanil response surface.
- **Kay L. Austin**, **John V. Stapleton**, **Laurence E. Mather** (1980) and **Geoffrey K. Gourlay** and colleagues (1988): the minimum effective analgesic concentration.
- **W. P. T. James** (1976): the lean body mass of the Schnider and Minto models, a parabola in weight. **Thomas W. Bouillon** and Shafer (1998) showed where it turns over. **Hesham S. Al-Sallami**, **Nick Holford**, **Stephen Duffull** and colleagues (2015) and **Sarayut Janmahasatian** and colleagues (2005): fat-free mass, used to [scale most of the models](help:models/fat-free-mass). **Brian J. Anderson** and Holford (2008): allometry and maturation.
- **Brunner**, **Katoh**, **Lang**, **Westmoreland**, **Sebel** and their colleagues (1992 to 1999): the opioid reduction of MAC that the approximate interaction model is fitted to.

## The inhaled anesthetics

- **James H. Philip**, Brigham and Women's Hospital and Harvard Medical School: GasMan®, whose structure and parameters the inhaled-gas engine follows and against which it is validated.
- **William W. Mapleson** (1996): the decline of MAC with age.
- **Edmond I. Eger II**, with Saidman and Brandstater (1965): MAC as the standard of anesthetic potency; and, with **Donald D. Koblin** and colleagues (1998), the nitrogen MAC of 110 atmospheres in rats that flags GasMan's value.
- **Jan F. A. Hendrickx**, **Hendrikus J. M. Lemmens** and Shafer (2006): the four-compartment structure of the gas model, and the demonstration that fitted volumes and clearances need not equal tissue volumes and blood flows.
- **Jeffrey M. Feldman**, **Samsun Lampotang** and Hendrickx (2022): the rule of thumb that rebreathing stops when fresh gas flow reaches minute ventilation, which the gas model follows with the threshold raised by the gas being taken up.

## Becoming a maintainer

It is hoped that each drug in the library will eventually be maintained by an investigator who keeps its pharmacokinetics up to date. If that could be you, see [Contributing a drug or a model](help:contributing).
