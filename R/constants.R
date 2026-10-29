UNIT_YEAR <- 1
UNIT_MONTH <- 0.08333333 # 1/12
UNIT_KG <- 1
UNIT_LB <- 0.453592
UNIT_CM <- 1
UNIT_INCH <- 2.54

SEX_MALE <- "male"
SEX_FEMALE <- "female"
SEX_VALUES <- c(SEX_MALE, SEX_FEMALE)

# Which plasma curve a drug's tPeak was observed against.  getDrugPK() solves
# ke0 so the effect site peaks at tPeak, and the answer depends on whether the
# observation followed an intravenous bolus or an oral dose.
ROUTE_IV <- "IV"
ROUTE_PO <- "PO"
ROUTE_IM <- "IM"
ROUTE_IN <- "IN"
ROUTE_SL <- "SL"
# Regional anesthesia: a local anesthetic injected into tissue (a peripheral
# nerve block or a wound infiltration), absorbed first-order from the tissue
# into the systemic circulation.
ROUTE_RA <- "RA"
# Subcutaneous injection (the incretin peptides, the insulins, the
# low-molecular-weight heparins and fondaparinux): absorbed
# first-order from a tissue depot, as an RA dose is.  The engines carry both
# on the same internal tissue-depot states ("RA" in getDrugPK() and the
# engines), which is why a drug may offer SC or RA but not both; see
# TISSUE_ROUTES.  An SC drug writes its absorption as ka_SC,
# bioavailability_SC and tlag_SC (optionally ka_SC_slow, fraction_SC_slow and
# tlag_SC_slow for a second, slower depot), and getDrugPK() moves them onto
# the tissue-depot fields.  (Claude Code, 2026-10-17, at the request of
# Steven L. Shafer.)
ROUTE_SC <- "SC"
TISSUE_ROUTES <- c(ROUTE_RA, ROUTE_SC)
TPEAK_ROUTES <- c(ROUTE_IV, ROUTE_PO)

# Every route a dose can take, in the order the units dropdowns list them.  A
# dose's route is carried by its Units string; see doseRoute() in R/routes.R.
DOSE_ROUTES <- c(ROUTE_IV, ROUTE_PO, ROUTE_SL, ROUTE_IM, ROUTE_IN, ROUTE_RA,
                 ROUTE_SC)

# CYP2D6 metaboliser phenotype.  Four categories, using the CPIC terms the
# genotyping laboratories report, rather than the three the UI carried before
# any drug used them.  "normal" is the reference: a drug's formation parameters
# are published relative to it.
CYP2D6_POOR         <- "poor"
CYP2D6_INTERMEDIATE <- "intermediate"
CYP2D6_NORMAL       <- "normal"
CYP2D6_ULTRARAPID   <- "ultrarapid"
CYP2D6_VALUES  <- c(CYP2D6_POOR, CYP2D6_INTERMEDIATE, CYP2D6_NORMAL,
                    CYP2D6_ULTRARAPID)
CYP2D6_DEFAULT <- CYP2D6_NORMAL

# CYP2C19 metaboliser phenotype.  The five CPIC categories the genotyping
# laboratories report (Lima et al., Clin Pharmacol Ther 2021;109:1417-1423),
# read by the antidepressants whose published clearance depends on it
# (escitalopram, citalopram).  "normal" is the reference.  A source that
# pooled several of these (Akil 2016: "EM/RM" and "IM/PM") maps each of the
# five onto its own groups in the drug file, and says so there.
CYP2C19_POOR         <- "poor"
CYP2C19_INTERMEDIATE <- "intermediate"
CYP2C19_NORMAL       <- "normal"
CYP2C19_RAPID        <- "rapid"
CYP2C19_ULTRARAPID   <- "ultrarapid"
CYP2C19_VALUES  <- c(CYP2C19_POOR, CYP2C19_INTERMEDIATE, CYP2C19_NORMAL,
                     CYP2C19_RAPID, CYP2C19_ULTRARAPID)
CYP2C19_DEFAULT <- CYP2C19_NORMAL

# CYP2B6 metaboliser phenotype, the five CPIC categories (Robinson et al.,
# Clin Pharmacol Ther 2024;116:932-938, Table 1), read by the models that name
# it (methadone, whose hepatic clearance of each enantiomer depends on it;
# R/drugs_methadone.R).  "normal" (*1/*1) is the reference: Henthorn and
# Kharasch fitted their typical values in normal metabolisers.  CPIC's rapid
# and ultrarapid categories carry no transportable estimate for methadone, so
# a model may legitimately give them the normal metaboliser's parameters; the
# drug file says so where it does.
CYP2B6_POOR         <- "poor"
CYP2B6_INTERMEDIATE <- "intermediate"
CYP2B6_NORMAL       <- "normal"
CYP2B6_RAPID        <- "rapid"
CYP2B6_ULTRARAPID   <- "ultrarapid"
CYP2B6_VALUES  <- c(CYP2B6_POOR, CYP2B6_INTERMEDIATE, CYP2B6_NORMAL,
                    CYP2B6_RAPID, CYP2B6_ULTRARAPID)
CYP2B6_DEFAULT <- CYP2B6_NORMAL

# OPIOID TOLERANCE, the Patient Profile's Opioid tolerance field: each
# opioid's MEAC is multiplied by the factor (Steven L. Shafer, 2026-10-28), so
# the % MEAC panel and overlay, the MEAC sum, the propofol-opioid interaction
# and the opioid-MAC interaction all see a tolerant patient's opioid as less
# effective, and so are its shaded band and its time-until-threshold value
# (recalculatePK(); an opioid without a MEAC is left alone).
OPIOID_TOLERANCE_FACTORS <- c(none = 1, mild = 1.5, somewhat = 2, high = 3)
OPIOID_TOLERANCE_VALUES  <- names(OPIOID_TOLERANCE_FACTORS)
OPIOID_TOLERANCE_DEFAULT <- "none"

# CYP2C9 DIPLOTYPE, read by the models that name it: meloxicam, whose genotype
# effect on clearance is estimated per allele (Aoyama 2017), and phenytoin,
# which maps the diplotype to a maximal-elimination-rate multiplier through the
# CPIC activity score (Karnes et al., Clin Pharmacol Ther 2021;109:302-309;
# Odani 1997 measured the *1/*3 effect).  The six common *1/*2/*3 diplotypes;
# *1/*1 (two normal-function alleles) is the reference.  Kept apart from CYP2D6
# and CYP2C19, and from any drug interaction: each is its own field.
CYP2C9_VALUES  <- c("*1/*1", "*1/*2", "*1/*3", "*2/*2", "*2/*3", "*3/*3")
CYP2C9_DEFAULT <- "*1/*1"

# VKORC1 -1639G>A GENOTYPE (rs9923231), read by the models that name it: the
# library warfarin model, where AA lowers the S-warfarin C50 x0.725 against GA
# or GG (Xue 2017); the Hamberg 2007 companion, where the genotype sets the
# EC50 outright (GG 4.61, GA 3.02, AA 2.20 mg/L; R/alternativeModels.R); and
# fluindione and acenocoumarol, where AA is the source's sensitive reference
# and a G allele RAISES the C50, x1.30 and x2.29 (R/drugs_fluindione.R).  Three
# genotypes.  Its own Patient Profile field, kept apart from the CYP phenotypes.
#
# The default is AA, which is the most sensitive genotype in all four models,
# by Steven L. Shafer's decision (2026-10-28): the rule CYP4F2_DEFAULT already
# follows, that an unentered genotype should predict the LARGER INR rise, so a
# dose chosen from the curve errs toward underdosing.  An underdose at the
# start of therapy is corrected by raising the dose; an overdose risks
# bleeding, the vitamin-K antagonists' primary toxicity, and may need reversal
# with vitamin K.  Warfarin is uniquely dangerous, which makes the conservative
# choice doubly justified here.  (This differs from CYP2C9_DEFAULT, where one
# field is shared with phenytoin and meloxicam and the default is the most
# likely genotype, *1/*1.  VKORC1 is read by vitamin-K antagonists only.)
#
# ANCESTRY.  The -1639A allele frequency differs greatly between populations,
# from a tenth of alleles to nine tenths in the cohort below, so the genotype
# is worth setting from the patient's ancestry when it has not been measured
# (the measured genotype is always better than either).  In the International
# Warfarin Pharmacogenetics Consortium cohort of 4886 chronic warfarin users
# (Limdi NA, Wadelius M, Cavallari L et al., Blood 2010;115:3827-3834, Table 3,
# doi 10.1182/blood-2009-12-255992) the A allele frequency was 91.0% in 641
# Asian, 37.8% in 681 White and 10.1% in 368 Black patients (the paper's own
# group labels).  The Asian group EXCLUDES South Indians: its 91 Indian
# subjects had substantially different VKORC1 allele frequencies and were left
# out of every analysis, so the Asian figure does not describe them and the
# cohort gives none for a South Indian or other South Asian patient.  Within
# each group the paper found the haplotype distribution consistent across
# geographically separated populations.  Under Hardy-Weinberg those give an
# AA/GA/GG distribution of about 83/16/1% in Asian, 14/47/39% in White and
# 1/18/81% in Black patients.  So the AA default is also the commonest
# genotype in that Asian group -- Xue 2017's own 264 Chinese patients were
# 82.6% AA, 17.0% GA and 0.4% GG, within a point of that expectation -- while
# the most likely single genotype is GG in a Black patient and GA in a White
# one.  These are group frequencies -- population context for an unmeasured
# genotype, never a finding about a patient.
VKORC1_VALUES  <- c("GG", "GA", "AA")
VKORC1_DEFAULT <- "AA"

# CYP4F2 rs2108622 (1297C>T, V433M) GENOTYPE, read only by the library warfarin
# (Xue 2017), where a C allele (CC or CT) lowers the S-warfarin C50 x0.770
# against TT: the T allele's enzyme oxidises less vitamin K1, so a T/T patient
# needs more warfarin.  Its own Patient Profile field.
# The default is CC, the most sensitive genotype, by Steven L. Shafer's
# decision (2026-10-27): when the genotype is not entered, the model should
# predict the LARGER INR rise, so that a dose chosen from the curve errs toward
# underdosing.  An underdose at the start of therapy is corrected by raising
# the dose; an overdose risks bleeding, warfarin's primary toxicity, and may
# need reversal with vitamin K.  (Xue's Table S6 offers x0.783 for an unknown
# CYP4F2, nearly the same C50; CC gives x0.770.)
CYP4F2_VALUES  <- c("CC", "CT", "TT")
CYP4F2_DEFAULT <- "CC"

# Baseline serum osmolality, mOsm/kg, before any osmotic agent is given.  Read
# only by models that declare an `osmolality` argument (mannitol), which add
# their own contribution on top of it.  The default, 280, is within the normal
# adult range (about 275 to 295); the user enters the patient's measured value.
OSMOLALITY_DEFAULT <- 280

# Remifentanil switches from Eleveld's model to Kim's obesity model at BMI 30,
# but only from this age: Kim's subjects were adults (20 to 85 years), while
# Eleveld's model was fitted down to infancy (R/drugs_remifentanil.R).
REMIFENTANIL_KIM_MIN_AGE <- 18
MIN_OSMOLALITY <- 200
MAX_OSMOLALITY <- 400

# Serum creatinine, mg/dL, for the renally cleared models.  Optional: when the
# patient's is not entered (NULL, or NA from an empty field) each model uses
# the assumed normal value for the patient's age and sex (R/renalFunction.R).
MIN_CREATININE <- 0.2
MAX_CREATININE <- 15

# Gestational age at birth, weeks, for the neonatal models whose clearance
# matures with postmenstrual age (dobutamineNeonatal).  Optional: when it is
# not entered (NULL, or NA from an empty field) the birth is taken as at term,
# GESTATIONAL_AGE_TERM.  Postmenstrual age is this plus the postnatal age
# (postmenstrualAge()).  Read only by models that name `gestationalAge`.
GESTATIONAL_AGE_TERM <- 40
MIN_GESTATIONAL_AGE <- 22
MAX_GESTATIONAL_AGE <- 44

MIN_AGE <- 0
MAX_AGE <- 90
# The youngest age the adult-fitted covariate terms see.  Several models carry
# age as a negative power ((age / 65)^-0.429, say), which is infinite at an age
# of 0 (a legal entry) and steep in infancy, where none of them was fitted; a
# child below this age receives the term's value at this age, as buprenorphine
# already does (BUPRENORPHINE_MIN_AGE).  Read by apixaban, rivaroxaban,
# fondaparinux and paroxetine, and by egfrMDRD() in R/renalFunction.R.
# The rule (AGENTS.md): every age term floors here unless the PK study
# explicitly permits a lower floor, which is then the youngest age the source
# studied; valproate's, fitted from 0.1 years, is the example
# (VALPROATE_MIN_AGE).
ADULT_AGE_TERM_FLOOR <- 18
MIN_WEIGHT <- 0.1
MAX_WEIGHT <- 500
MIN_HEIGHT <- 10
MAX_HEIGHT <- 200

# default variables
defaultAge <- 50
defaultAgeUnit <- UNIT_YEAR
defaultWeight <- 60
defaultWeightUnit <- UNIT_KG
defaultHeight <- 66
defaultHeightUnit <- UNIT_INCH
defaultSex <- SEX_FEMALE

# Resolution for linear interpolation
RESOLUTION <- 100

# Max number of emails a single session can send out, to limit abuse
EMAIL_SESSION_LIMIT <- 25

MAX_DOSE_ROWS <- 500L
MAX_EVENT_ROWS <- 100L
MAX_TARGET_ROWS <- 20L
MAX_DOSE_VALUE <- 1e9
MAX_INPUT_TEXT <- 2000L
MAX_PLOT_WIDTH <- 4096L
MIN_PLOT_WIDTH <- 100L
MAX_YAXIS_HEIGHT <- 350L
MIN_YAXIS_HEIGHT <- 150L
MAX_DRUGNAME_LENGTH <- 128L
MAX_TIME_STRING_LENGTH <- 32L
MAX_UNIT_STRING_LENGTH <- 32L

# Be sure there are more items below then potential facets on the simulation plot
#                     1     2     3     4     5     6     7     8     9    10    11    12    13    14   15
bolusUnits <- c("g","mg","mcg", "ng","g/kg","mg/kg","mcg/kg","ng/kg")
infusionUnits <- c("g/min","g/hr","g/kg/hr","mg/min","mg/hr","mg/kg/min","mg/kg/hr","mcg/min","mcg/hr","mcg/kg/min","mcg/kg/hr")
poUnits <- c("g PO", "g/kg PO", "mg PO", "mg/kg PO", "mcg PO", "mcg/kg PO")
inUnits <- c("g IN", "g/kg IN", "mg IN", "mg/kg IN", "mcg IN", "mcg/kg IN")
imUnits <- c("g IM", "g/kg IM", "mg IM", "mg/kg IM", "mcg IM", "mcg/kg IM")
# Sublingual: first-order absorption with its own ka, bioavailability and lag,
# exactly as the oral, intramuscular and intranasal routes.
slUnits <- c("mg SL", "mg/kg SL", "mcg SL", "mcg/kg SL")
# Regional anesthesia (ROUTE_RA): a single injection into tissue, the whole
# dose entering a depot that is absorbed first-order.  No scheduled
# frequencies.
raUnits <- c("g RA", "g/kg RA", "mg RA", "mg/kg RA", "mcg RA", "mcg/kg RA")
# A continuous perineural (catheter) infusion: a constant rate INTO the
# tissue depot, absorbed from it first-order as an RA dose is.  A rate by
# kind (isRateUnit()) and RA by route: simCpCe() flags its rows RArate, and
# advanceClosedFormPO_IM_IN() advances them exactly, as an infusion through
# the depot.  (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
raRateUnits <- c("mg/hr RA")

# Constant-rate oral input: a daily oral dose spread evenly over the day, as
# Pollak, Bouillon and Shafer modelled long-term oral amiodarone (400 mg/d as
# 16.7 mg/h for 24 h; R/drugs_amiodarone.R).  Oral by route, so doseRoute()
# reads it as PO, but a rate by kind: simCpCe() runs each row as the drug's
# running input rate until the next, like an infusion row, on the drug's
# apparent oral parameters (no ka, no bioavailability).  Kept out of
# infusionUnits, which lists the intravenous rates.  (Claude Code,
# 2026-10-07, at the request of Steven L. Shafer.)
poRateUnits <- c("mg/day PO")

# Subcutaneous injection (ROUTE_SC): the whole dose into the SC depot.  The
# peptides are dosed by mass; the insulins in units (U), which are a potency
# unit of each preparation, not a mass, so a drug offering them carries
# concentrations in microunits per mL ("mcU" in drugDefaults_global.csv) and
# no mass unit at all.  "U/hr SC" is continuous subcutaneous insulin
# infusion (an insulin pump): a rate into the SC depot, which the engines
# carry as they carry "mg/hr RA".  Only a product whose label permits pump
# use offers it; a product that must not be used in a pump (Humalog U200,
# the fixed-ratio pens) simply has no such unit, and the dose table refuses
# a unit the drug does not list.  (Claude Code, 2026-10-17, at the request
# of Steven L. Shafer.)
scUnits <- c("mg SC", "mg/kg SC", "mcg SC", "mcg/kg SC")
insulinUnits <- c("U SC", "U/kg SC")
insulinRateUnits <- c("U/hr SC")

# Oral formulations.  A drug whose tablets and liquid are absorbed at different
# rates offers a unit per formulation, "mg PO tablet" and "mg PO liquid".  The
# formulation word follows the route, so doseRoute() still reads these as PO
# and a frequency may still follow ("mg PO liquid bid").  The drug model's own
# ka_PO, bioavailability_PO and tlag_PO describe its default oral form; each
# other form it lists in `oralFormulations` (getDrugPK()) has absorption of its
# own, which simCpCe() superposes.  First user: morphine.  A drug may also
# offer a formulation unit without listing it, when the forms are
# bioequivalent: its doses then take the default oral absorption, as plain
# "mg PO" does (hydromorphone's liquid).  (Claude Code, 2026-10-10, at the
# request of Steven L. Shafer.)
#
# The antiseizure drugs added "ER" (extended release) and "DR" (delayed
# release, enteric coated): one drug, one systemic disposition, and an input
# of each product's own, so a patient switched from immediate- to extended-
# release is one row each on the same drug rather than two drugs.  (Claude
# Code, 2026-10-10, at the request of Steven L. Shafer.)
#
# "XR" is an extended-release product released in pulses: a drug that lists
# it in `oralPulses` gives each XR dose as fixed fractions at fixed delays,
# each absorbed as its default oral form (R/oral-pulses.R; first user:
# mixedAmphetamineSalts, Adderall XR).
ORAL_FORMULATIONS <- c("tablet", "liquid", "ER", "DR", "XR")
poFormulationUnits <- as.vector(t(outer(c("mg PO", "mg/kg PO"), ORAL_FORMULATIONS, paste)))

# A pulsed release does not have to be a named formulation.  An
# immediate-release tablet has no formulation word in its unit -- it is dosed
# as plain "mg PO" -- and a drug whose published absorption is a shape the
# engine lacks may still be reproduced by pulses of the default oral form.
# `oralPulses` therefore accepts this reserved name alongside
# ORAL_FORMULATIONS, keyed to every plain oral row (hydrochlorothiazide, whose
# source fits a Weibull input; R/oral-pulses.R).  It is not a formulation and
# never appears in a dose unit, so it can never collide with one.
ORAL_PULSE_DEFAULT_FORM <- "default"

# Prodrug equivalents: fosphenytoin is prescribed in phenytoin sodium
# equivalents (PE), and its units say so, so a dose is never read as mg of
# fosphenytoin.  Offered only by a drug with a Michaelis-Menten block that
# carries the conversion (R/drugs_phenytoin.R, R/advanceMichaelisMenten.R).
peUnits <- c("mg PE", "mg/kg PE", "mg PE IM", "mg/kg PE IM", "mg PE/min")

# Intravenous calcium (R/drugs_calcium.R): one drug, two salts, and every
# dose must say which, because 1 g of calcium gluconate carries a third of
# the calcium in 1 g of calcium chloride and the substitution has killed
# patients (Ansari 2025).  The salt is a word after the mass unit, as "PE"
# names the prodrug; it carries no route word, so doseRoute() reads these
# as intravenous, and isRateUnit() still finds "/min" and "/hr".  The
# `molar` block of the drug names the grams of each salt per mole of what is
# plotted, keyed by these words (validateMolar() in R/getDrugPK.R).  All
# of one salt's units, then all of the other's.
CALCIUM_SALTS <- c("chloride", "gluconate")
calciumBaseUnits <- c("g", "mg", "mg/kg", "mg/min", "mg/hr", "mg/kg/hr", "g/hr")
calciumUnits <- as.vector(outer(calciumBaseUnits, CALCIUM_SALTS, paste))

# International units of activity: a drug whose CSV Concentration.Units is
# "IU" (unfractionated heparin, R/drugs_heparin.R) is dosed in IU, and its
# curve is activity in IU/mL.  Heparin's potency is defined by its activity,
# not its mass, so these units have no conversion to mg, and simCpCe() refuses
# a mass unit for such a drug and an IU unit for any other.  No route word:
# they are intravenous.  (Claude Code, 2026-10-17, at the request of Steven L.
# Shafer.)
iuUnits <- c("IU", "IU/kg", "IU/hr", "IU/kg/hr")

# Intravenous doses in units (U) of a potency-defined peptide: vasopressin
# (R/drugs_vasopressin.R), labelled and given in units, a bolus or a rate per
# minute or hour.  The same unit as the insulins (insulinUnits), so the same
# rules hold: the drug's Concentration.Units is "mcU", simCpCe() turns U into
# mU, and refuses a mass unit for it and a U unit for any other drug.  No
# route word: they are intravenous.  (Claude Code, 2026-10-22, at the request
# of Steven L. Shafer.)
ivPotencyUnits <- c("U", "U/min", "U/hr", "U/kg/min", "U/kg/hr")

# Subcutaneous activity units (ROUTE_SC) for a drug plotted as anti-Xa
# activity (enoxaparin, "IU SC"); the mass units are scUnits, above.  They
# start with "IU" so simCpCe()'s international-unit check reads them, and
# carry no mass.  No scheduled frequencies and no rate.  (Claude Code,
# 2026-10-17, at the request of Steven L. Shafer.)
iuScUnits <- c("IU SC", "IU/kg SC")

# GHB (R/drugs_GHB.R).  The dose is SODIUM OXYBATE and the curve is GHB, the
# free acid, which are 21% apart by mass, and both bases are in common use, so
# the unit names the salt that is swallowed rather than leaving it to prose.
# "oxybate" is not in ORAL_FORMULATIONS, so doseFormulation() reads no
# formulation from it, and doseRoute() still finds the PO; the conversion is
# the michaelisMenten saltFactor of the drug.
GHB_SALT <- "oxybate"
ghbUnits <- paste(c("g PO", "mg PO"), GHB_SALT)

allUnits <- c(bolusUnits, infusionUnits, poUnits, poRateUnits, poFormulationUnits,
              slUnits, inUnits, imUnits, raUnits, raRateUnits, peUnits, calciumUnits,
              iuUnits, scUnits, iuScUnits, ghbUnits, insulinUnits, insulinRateUnits,
              ivPotencyUnits)

# Target-controlled infusion (tci.R).  The "dose" of a target row is the target
# concentration, in the drug's concentration units per ml.
TCI_UNIT_PLASMA <- "Plasma target"
TCI_UNIT_EFFECT <- "Effect site target"
tciUnits <- c(TCI_UNIT_PLASMA, TCI_UNIT_EFFECT)
# The dose table's Dose column is headed "Dose/Target": a target row's number
# is a concentration.  The data column is still "Dose" (rColHeaders); the
# grid's hooks match the header's first word (findColByHeader() in
# inst/www/hot_funs.js).
DOSE_COLUMN_HEADER <- "Dose/Target"
TCI_INTERVAL <- 10 / 60        # minutes between rate changes (10 s, as STANPUMP)
TCI_PLASMA_SWITCH <- 0.05      # effect site this close to target: hold the plasma
TCI_MAX_RATE <- Inf            # pump ceiling in base mass units per minute

# Scheduled (repeating) doses (scheduled.R).  A bolus, PO, SL, IM or IN unit with
# one of these suffixes, e.g. "mg PO bid", gives the dose at the entered time
# and then again every interval (minutes) until the end of the plot.  Every
# drug that offers qid also offers q4h (drugDefaults_global.csv).
SCHEDULE_INTERVALS <- c(qd = 24 * 60, bid = 12 * 60, tid = 8 * 60, qid = 6 * 60,
                        q4h = 4 * 60)
# Separate morning ("qam") and evening ("qpm") once-daily slots, oral and
# sublingual only, offered only on the antiseizure drugs.  Unlike the whole-day
# frequencies above, a qam and a qpm schedule DO NOT replace each other, so a
# split regimen with a different morning and evening dose can be entered (the
# antiseizure drugs are sedating and affect memory, so the morning dose is
# often smaller than the evening one).  See scheduled.R for the rules.
SLOT_SCHEDULE_INTERVALS <- c(qam = 24 * 60, qpm = 24 * 60)
# Once weekly ("qwk"), offered only on the subcutaneous units: semaglutide,
# dulaglutide and tirzepatide are injected once a week.  Kept out of
# SCHEDULE_INTERVALS, the daily frequencies every other scheduled drug offers;
# scheduled.R reads ALL_SCHEDULE_INTERVALS.
WEEKLY_SCHEDULE_INTERVAL <- c(qwk = 7 * 24 * 60)
ALL_SCHEDULE_INTERVALS <- c(SCHEDULE_INTERVALS, SLOT_SCHEDULE_INTERVALS,
                            WEEKLY_SCHEDULE_INTERVAL)
# A scheduled repeat this close (minutes) to the row that ends its schedule
# is replaced by that row (scheduled.R): the 0.001-minute snap of a time
# written in hours or days (TIME_SNAP_DIGITS).
SCHEDULE_TIME_TOLERANCE <- 1e-3
scheduledUnits <- c(
  as.vector(t(outer(
    c(bolusUnits, poUnits, poFormulationUnits, slUnits, imUnits, inUnits),
    names(SCHEDULE_INTERVALS), paste
  ))),
  # qam/qpm: the oral and sublingual forms only (the antiseizure drugs).
  as.vector(t(outer(
    c(poUnits, poFormulationUnits, slUnits),
    names(SLOT_SCHEDULE_INTERVALS), paste
  ))),
  # The subcutaneous units take the daily frequencies and qwk, not qam/qpm.
  as.vector(t(outer(c(scUnits, insulinUnits),
                    names(c(SCHEDULE_INTERVALS, WEEKLY_SCHEDULE_INTERVAL)), paste)))
)

# Units for the inhaled gases (Class "gas" in drugDefaults_global.csv): carrier
# gases are flowmeter settings in L/min, potent agents are vaporizer settings in %.
# Kept out of allUnits, which lists the mass-based units offered for
# IV/PO/SL/IM/IN/RA
# drugs, but they are legitimate entries in the dose table.
gasUnits <- c("L/min", "%")

# The startup drug menu (R/startup-drugs.R).  The categories, in the order the
# menu lists them; each drug's is the Category column of
# drugDefaults_global.csv, and a drug with no category is not offered.
DRUG_CATEGORIES <- c(
  "Hypnotics and sedatives",
  "Opioids",
  "Oral analgesics",
  "Neuromuscular blockade",
  "Inhaled anesthetics",
  "Antibiotics",
  "Corticosteroids",
  "Antidepressants",
  "Stimulants",
  "Local anesthetics",
  "Antiemetics",
  "Antiseizure",
  "Antipsychotics",
  "Antidiabetic",
  "Anticoagulants",
  "Antihypertensives",
  "Vasoactive and resuscitation",
  "Other",
  "Illicit drugs"
)
# The category of the intravenous vasoactive, antiarrhythmic and resuscitation
# drugs of intensive care (inst/help/models/vasoactive.md): concentration
# only, every one catalogued in inst/extdata/vasoactiveRegistry.csv.
VASOACTIVE_DRUG_CATEGORY <- "Vasoactive and resuscitation"
# The categories whose every drug, and every metabolite of one, must have a
# row in inst/extdata/therapeuticRanges.csv (R/therapeuticRanges.R): the
# range the band draws, or why there is none.  Every category since
# 2026-10-27.  Checked by test-therapeutic-ranges.R.
THERAPEUTIC_RANGE_CATEGORIES <- DRUG_CATEGORIES
# What a therapeutic-range row may say about the band (R/therapeuticRanges.R).
THERAPEUTIC_RANGE_STATUSES <- c(
  "drawn",                # the band is this range, on this drug's row
  "drawn_on_metabolite",  # the range is drawn on band_drug's row instead
  "sum_not_drawn",        # the range is for a parent plus a metabolite whose
                          # shares vary, so it cannot be drawn on either row
  "pending_verification", # a candidate range not yet read in a primary source
  "not_applicable",       # a range exists, but not for the use this model plots
  "band_differs",         # the range is for another use or analyte, and the
                          # band is the library's own for what the row plots
  "library_band",         # no range is established for what the row plots;
                          # the band is the library's own (an effect-site
                          # target, an MBC band, what ordinary doses reach)
  "none_established"      # no range has been established
)
# The category of the opt-in research models of drugs of abuse
# (R/illicit-drugs.R).  Hidden unless the user turns on input$showIllicitDrugs,
# and never offered in the startup menu.
ILLICIT_DRUG_CATEGORY <- "Illicit drugs"
# The four drugs the app opened with before it had a menu.  The menu opens
# with nothing ticked; ticking these gives doseTableInit, the dose table the
# tests start from (test-startup-drugs.R holds them equal).
STARTUP_DRUGS_DEFAULT <- c("propofol", "fentanyl", "remifentanil", "rocuronium")
# A chosen drug starts with one zero-dose row in its Default.Units, except
# these, which start with a bolus row and an infusion row, as they always have.
STARTUP_UNITS <- list(
  propofol     = c("mg", "mcg/kg/min"),
  remifentanil = c("mcg", "mcg/kg/min")
)
# Blank rows below the chosen drugs, to type into
STARTUP_BLANK_ROWS <- 6L

MINS_PER_HOUR <- 60
MINS_PER_DAY  <- 60 * 24
MINS_PER_WEEK <- 60 * 24 * 7
MINS_PER_YEAR <- 525600  # more than 52 weeks because of leap years

# Time units (R/utils-time.R).  The unit is a display and entry setting only:
# the engine, the scenarios, the events, the simulation cache and the exported
# Time columns are in minutes whatever it is.  A bare number typed into the
# dose table is in the unit; an entry with a colon is a clock time or an
# elapsed H:MM and is never scaled.  Values are minutes per unit.
TIME_UNITS <- c(minutes = 1, hours = MINS_PER_HOUR, days = MINS_PER_DAY, weeks = MINS_PER_WEEK)
TIME_UNIT_DEFAULT <- "minutes"
# Clock ("Actual time") entry addresses only the 24 hours after the procedure
# start, so it is offered for these units only; days and weeks are elapsed.
CLOCK_TIME_UNITS <- c("minutes", "hours")
TIME_MODES <- c("clock", "relative")
# The Time Display the app opens with: elapsed time, counted from zero.  Actual
# (clock) time is a choice in the Time panel for minutes and hours.
TIME_MODE_DEFAULT <- "relative"
# A time converted to another unit is rounded to TIME_SNAP_DIGITS decimal
# places of a minute (0.001 min) and written with TIME_STRING_DIGITS
# significant digits.  Ten digits make every conversion, and any chain of them,
# return the identical minutes on a 0.001 minute grid up to a year (six digits
# did not: day 5 became 0.714286 weeks, 7200.00288 minutes, which moved a
# scheduled stop past a repeat).
TIME_STRING_DIGITS <- 10
TIME_SNAP_DIGITS <- 3

# TCI target rows and the inhaled agents are simulated only on plots of this
# length or less.  Beyond a week the TCI controller writes ever more rows, and
# the gas engine's uptake coupling is frozen over steps of about maximum/601.
ACUTE_MAX_PLOT_MINUTES <- MINS_PER_WEEK

# Drugs that are only meaningful over weeks to months.  Adding one to a plot
# shorter than a week offers to switch the Time units to days, 365 days.
LONG_TERM_DRUGS <- c("amiodarone", "semaglutide", "semaglutideOral", "dulaglutide",
                     "tirzepatide")
LONG_TERM_PLOT_MINUTES <- 365 * MINS_PER_DAY

# The Max time choices for each time unit: the durations (minutes) and the
# tick spacing (minutes) of each.  "minutes" and "hours" offer the same
# durations, so switching between them never changes Max time.  365 days is
# not a multiple of its 30-day ticks: the axis runs past the last tick.
MAX_TIMES <- list(
  minutes = data.frame(
    times = MINS_PER_HOUR * c(1, 2, 4, 6, 8, 12, 18, 24),
    steps = c(10, 15, 30, 60, 60, 120, 180, 240)
  ),
  hours = data.frame(
    times = MINS_PER_HOUR * c(1, 2, 4, 6, 8, 12, 18, 24),
    steps = MINS_PER_HOUR * c(0.25, 0.25, 0.5, 1, 1, 2, 3, 4)
  ),
  days = data.frame(
    times = MINS_PER_DAY * c(2, 3, 4, 7, 14, 28, 56, 91, 182, 365),
    steps = MINS_PER_DAY * c(0.5, 0.5, 1, 1, 2, 7, 7, 7, 14, 30)
  ),
  weeks = data.frame(
    times = MINS_PER_WEEK * c(4, 8, 13, 26, 39, 52),
    steps = MINS_PER_WEEK * c(1, 1, 1, 2, 3, 4)
  )
)
# The word each unit's Max time choices are labelled in ("1 hour", "365 days")
MAX_TIME_LABEL_UNITS <- c(minutes = "hour", hours = "hour", days = "day", weeks = "week")
# Every Max time any unit offers.  input$maximum outside this is refused; one
# inside it but not in the current unit's list is a moment when the browser has
# not yet caught up with a change of unit.
MAX_TIME_VALUES <- sort(unique(unlist(lapply(MAX_TIMES, `[[`, "times"))))
# The Max time and Min time sliders (timeSliderRange() in R/utils-time.R):
# minutes from 0 to the unit's longest Max time, in these steps.  The values
# stay in minutes whatever the unit, as input$maximum always has; app.js shows
# them in the unit.
TIME_SLIDER_STEPS <- c(minutes = 5, hours = 15, days = MINS_PER_DAY, weeks = MINS_PER_WEEK)
# Tick spacings (minutes) for a plot that is not one of the MAX_TIMES lengths
# from 0, such as a window from 15 to 16 weeks: the smallest that leaves at
# most AXIS_MAX_TICK_INTERVALS intervals across the window (axisTimeStep()).
AXIS_TICK_STEPS <- list(
  minutes = c(1, 2, 5, 10, 15, 20, 30, 60, 120, 180, 240, 360),
  hours   = MINS_PER_HOUR * c(0.25, 0.5, 1, 2, 3, 4, 6),
  days    = MINS_PER_DAY * c(0.25, 0.5, 1, 2, 7, 14, 30, 60),
  weeks   = MINS_PER_WEEK * c(0.25, 0.5, 1, 2, 4, 8)
)
AXIS_MAX_TICK_INTERVALS <- 8
# How close (minutes) the last dose or event may come to the end of the plot
# before the plot is lengthened to show what follows it.  30 minutes is the
# original rule, kept so that the minute and hour plots are unchanged.
TIME_EXTEND_MARGIN <- c(minutes = 30, hours = 30, days = MINS_PER_DAY / 2, weeks = MINS_PER_WEEK)

REFERENCE_TIME_NONE <- "none"
NORMALIZE_NONE <- "none"
PK_EVENT_DEFAULT <- "default"

PLOT_ID_EVENTS      <- "Events"
PLOT_ID_MEAC        <- "MEAC"
PLOT_ID_INTERACTION <- "Interaction"
PLOT_NAME_EVENTS      <- "Events"
PLOT_NAME_MEAC        <- "% MEAC"
PLOT_NAME_INTERACTION <- "p response"

# The time line the closed-form engines simulate on (R/simulationTimeGrid.R).
# PRE_DOSE_OFFSET puts a point just before each dose, where the time until
# threshold jumps.  Up to GRID_LEGACY_MAXIMUM (a day) each gap between knots
# gets the GRID_LOG_POINTS geometric offsets it always had, so those plots are
# unchanged point for point.  Beyond it the fill starts at
# maximum / GRID_FINE_POINTS (or at the drug's own start, if that is later)
# and no step is longer than maximum / GRID_UNIFORM_POINTS.
#
# The two counts were chosen by measurement, the closed form evaluated at 8 to
# 32 points inside every step against the straight line the plot draws across
# it (Claude Code, 2026-10-07):
#
#   GRID_UNIFORM_POINTS = 500.  On a 52-week plot of a two-compartment model
#   with Pollak 2000's amiodarone parameters (half-lives 17.3 h and 55.4 d),
#   given as Pollak's seven-step oral regimen at constant rates or as
#   400 mg/day stopped at day 180, no chord strays from the curve by more than
#   0.4% of the peak.  1000 does no better -- the largest error sits where the
#   geometric steps hand over to uniform ones, and that point moves with the
#   step -- and doubles the points on a plot with few doses.  The uniform
#   steps there are 17.5 h, two to four pixels on a full-width plot.
#
#   GRID_FINE_POINTS = 20000.  What decides it is the peak after an oral dose,
#   which nothing else puts a point near.  The earliest in the library is
#   oxycodone's plasma peak, 30 min after the dose.  Starting the fill at
#   maximum / 20000 (26 min on a 52-week plot) draws every oral drug's plasma
#   and effect-site peak, under daily dosing for 52 weeks, to within 0.4% of
#   its height; starting it at maximum / 2000 (4.4 h) drew oxycodone's at 42%,
#   and maximum / 10000 at 94%.  On a plot shorter than 20000 x start (about
#   two weeks) the fill starts exactly where it always did.
#
#   Cost, 10 mg of oxycodone four times a day for 52 weeks with the time until
#   threshold: 24,752 points and 1.7 s, against 52,416 points and 10.2 s on the
#   line before this change (5,824 points and 0.4 s at 2000).  Once a day:
#   9,100 points and 0.5 s, against 15,652 and 2.7 s.
PRE_DOSE_OFFSET     <- 0.01
GRID_LOG_POINTS     <- 41
GRID_LEGACY_MAXIMUM <- MINS_PER_DAY
GRID_UNIFORM_POINTS <- 500
GRID_FINE_POINTS    <- 20000

DEBUG_LEVEL_OFF <- 0
DEBUG_LEVEL_NORMAL <- 1
DEBUG_LEVEL_VERBOSE <- 2

# Every https URL on one of these websites in a drug reference, wherever it
# stands in the citation, is shown in the UI as a link labelled with the
# site's name (citationLinks() in R/utils-shiny-ui.R).
CITATION_WEBSITES <- c(
  "PubMed" = "pubmed.ncbi.nlm.nih.gov",
  "DOI"    = "doi.org",
  "PAGE"   = "www.page-meeting.org"
)

DEFAULT_CONFIG <- list(
  title = "stanpumpR",
  source_link = "https://github.com/StevenLShafer/stanpumpR",
  debug = DEBUG_LEVEL_OFF,
  long_title = FALSE
)

# TCI API (tci-api.R).  The stateless HTTP API that hands a pump controller its
# next infusion rate; docs/tci-api.md is the specification (version 1.0), and
# GET /tci/v1/drugs publishes these values under `limits`.  Within version 1
# nothing here changes meaning: a token, a drug or a bound may only be added.
TCI_API_VERSION <- "1.0"
TCI_API_MIN_INTERVAL <- 1              # s: the controller's update interval, the time a rate is held
TCI_API_MAX_INTERVAL <- 60             # s; above tPeak / 3 for the drug the response warns INTERVAL_LONG
TCI_API_MAX_HISTORY <- 20000L          # events in one request; longer is 413 HISTORY_TOO_LONG
TCI_API_LOOKAHEAD_MAX <- 30000         # s: how far the predictions look, STANPUMP's look_ahead() horizon
TCI_API_LOOKAHEAD_EXACT_STEPS <- 300L  # plans targetReachedAt runs at the controller's own interval before ...
TCI_API_LOOKAHEAD_MAX_STEP <- 60       # ... its step grows by a tenth per plan, up to this many seconds
TCI_API_CLOCK_SKEW <- 2                # s a history event may lie after `now` and be clamped to it
TCI_API_SERVER_SKEW <- 30              # s between the controller's clock and the server's before CLOCK_SKEW
TCI_API_MAX_RATE <- 1e5                # ceiling on pump.maxRate, in the pump's own rate units
TCI_API_TARGET_CEILING <- 5            # x the library's Upper: a higher target is refused
TCI_API_ON_TARGET <- 0.01              # within this fraction of the target counts as on target
TCI_API_DELIVERY_TOLERANCE <- 0.02     # the pump's totaliser may differ from the history by this fraction ...
TCI_API_DELIVERY_TOLERANCE_ML <- 0.5   # ... or by this many mL of the syringe, whichever is larger
TCI_API_MIN_INFUSATE <- 1e-3           # least syringe concentration, in its own units (0.001 mg/mL or 0.001 mcg/mL)
TCI_API_PK_CACHE_SIZE <- 256L          # distinct covariate sets whose PK the server keeps (tciApiPK())
# The unit tokens: one closed, case-sensitive list in the library's spellings.
TCI_API_RATE_UNITS <- c("mL/hr", "mg/hr", "mg/min", "mcg/hr", "mcg/min",
                        "mg/kg/hr", "mg/kg/min", "mcg/kg/hr", "mcg/kg/min")
TCI_API_AMOUNT_UNITS <- c("mL", "mg", "mcg")
TCI_API_BOLUS_UNITS <- c(TCI_API_AMOUNT_UNITS, "mg/kg", "mcg/kg")
TCI_API_INFUSATE_UNITS <- c("mg/mL", "mcg/mL")
TCI_API_CONCENTRATION_UNITS <- c("mcg/mL", "ng/mL")   # the drug decides which
TCI_API_VOLUME_UNITS <- "mL"
TCI_API_PUMP_STATES <- c("running", "paused", "alarm", "occlusion",
                         "syringeEmpty", "purging", "standby")
# An RFC 3339 time as the API accepts it: date and clock fields within their
# ranges (the 60th second for a leap second), optional fractional seconds, and
# a mandatory Z or numeric offset.  Mirrored in inst/api/openapi.json.
TCI_API_TIME_PATTERN <- paste0(
  "^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])",
  "[Tt ]([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?",
  "([Zz]|[+-]([01][0-9]|2[0-3]):[0-5][0-9])$")

# PREGNANCY.  The Patient Profile's Pregnant field, read only by the models
# that name `pregnant` in their signature (R/pregnancy.R lists them, and the
# help's Cautions page says which).  The field is offered for a woman from
# PREGNANCY_MIN_AGE up to but not including PREGNANCY_MAX_AGE years; outside
# that, or for a man, the flag is FALSE whatever the control last said.
# The factors each model applies are from that drug's own pregnancy sources
# (inst/extdata/pregnancyRegistry.csv; the first eight from the review of
# Ansari, Carvalho, Shafer and Flood, Anesth Analg 2016;122:786-804,
# PREGNANCY_REFERENCE in R/pregnancy.R), and describe late pregnancy, the
# third trimester and term, where most of the data were collected.
PREGNANCY_MIN_AGE <- 12
PREGNANCY_MAX_AGE <- 60
# Renal blood flow and the glomerular filtration rate "are increased 50% by
# the second trimester and remain increased until 3 months postpartum"
# (Ansari 2016, Clearance).  The models of drugs cleared mostly by filtration
# apply it to the renal function their published clearance terms read, and
# only when the serum creatinine is blank (pregnancyGfrFactor()): a measured
# pregnant creatinine already carries the rise.
PREGNANCY_GFR_FACTOR <- 1.5
