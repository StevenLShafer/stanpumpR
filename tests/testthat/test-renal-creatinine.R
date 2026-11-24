# The serum creatinine covariate, shared by every model with a renal term.
# See R/renalFunction.R.

renalDrugs <- c("mannitol", "vancomycin", "vancomycinKidneyTransplant", "gentamicin", "cefazolin", "sugammadex",
                "gabapentin", "pregabalin", "oxycodone", "hydrocodone", "meloxicam",
                "lacosamide", "levetiracetam", "metformin", "ertugliflozin", "empagliflozin",
                "exenatide", "dapagliflozin", "canagliflozin", "lixisenatide",
                # Anticoagulants with a renal clearance term (R/drugs_apixaban.R,
                # R/drugs_rivaroxaban.R, R/drugs_edoxaban.R, R/drugs_dabigatran.R,
                # R/drugs_enoxaparin.R and R/drugs_fondaparinux.R, which use MDRD)
                "apixaban", "apixabanVTE", "rivaroxaban", "edoxaban", "dabigatran",
                # dabigatranHip (Trocóniz 2007): Cockcroft-Gault CrCl and serum
                # creatinine both act on the day-2-to-10 phase (R/drugs_dabigatranHip.R)
                "dabigatranHip",
                "enoxaparin", "fondaparinux",
                # Renal function is hydrochlorothiazide's ONLY covariate
                # (R/drugs_hydrochlorothiazide.R, Van Wart 2013): the source
                # fitted no body-size term either, so weight reaches it only
                # through Cockcroft-Gault.
                "hydrochlorothiazide",
                # Carboplatin (Duffull 1997): an adjusted Cockcroft-Gault term
                # (R/drugs_carboplatin.R)
                "carboplatin",
                # methotrexatePediatric (Taylor 2023): the patient's own
                # creatinine, a power referenced to 40 uM
                "methotrexatePediatric",
                # methotrexate (Ibarra 2023): (SCRpop/SCR)^0.694
                "methotrexate",
                # pemetrexed (Latz 2006): a linear Cockcroft-Gault term
                "pemetrexed",
                # fludarabine (Langenhorst 2019): eGFR (Cockcroft-Gault or Schwartz, the patient's own creatinine) on clearance
                "fludarabine",
                # oxaliplatinNikanjam (Nikanjam 2015): (0.6/SCr)^0.67 on clearance, the patient's own creatinine
                "oxaliplatinNikanjam",
                # cisplatinBound (Urien 2004): Cockcroft-Gault in fm/Vm
                "cisplatinBound",
                # etoposide (Krogh-Madsen 2012): a linear Cockcroft-Gault term
                "etoposide",
                # cisplatin (Urien 2004): (CLCr/81)^0.36 on unbound clearance
                "cisplatin",
                # topotecan (Mould 2002): Cockcroft-Gault standardised to 70 kg
                "topotecan",
                # doxorubicinol (Joerger 2007): Cockcroft-Gault on its clearance
                "doxorubicinol",
                # melphalan (Nath 2010): Cockcroft-Gault per 70 kg, a linear renal term
                "melphalan",
                # oxaliplatin (Bastian 2003): (SCr/87 umol/L)^-0.57 on clearance
                "oxaliplatin",
                # the candidate models under their own names, since the drugs
                # above became the program's choice (R/modelSelection.R)
                "carboplatinDuffull", "methotrexateIbarra", "melphalanNath",
                "doxorubicinolJoerger", "etoposideKroghMadsen", "oxaliplatinBastian",
                # Serum creatinine itself, not an estimate of its clearance,
                # is magnesium's covariate (R/drugs_magnesiumSulfate.R, Du 2019)
                "magnesiumSulfate",
                # The renin-angiotensin drugs whose sources put a power of the
                # serum creatinine CONCENTRATION, in umol/L, on clearance:
                # lisinopril (Thomson 1989, ^-0.887 over 70) and olmesartan
                # (Yoshihara 2005, ^-0.344 over 80, its strongest covariate).
                # Valsartan is deliberately NOT here: Heo 2016 screened serum
                # creatinine and rejected it, so its signature does not name
                # the covariate and this test asserts that it ignores it.
                "lisinopril", "olmesartan",
                # Fosinopril (Hui 1991) is the odd one of the three: its
                # covariate is Cockcroft-Gault indexed to 1.73 m^2, the unit
                # Hui reports, and it is a two-plateau RAMP rather than a
                # power -- clearance halves from normal renal function to
                # impaired and is then flat, because the hepatobiliary route
                # takes 63 to 85% of a shrinking total.  So unlike every other
                # renal model here, a large change in creatinine can leave its
                # clearance untouched (docs/antihypertensives.md section 18).
                "fosinopril",
                # Grasela and Sheiner's Equation 7 (R/drugs_procainamide.R)
                "procainamide",
                # Cockcroft-Gault on clearance (R/drugs_digoxin.R, Yukawa 2001)
                "digoxin",
                # Bedside Schwartz at every age, on clearance
                # (R/drugs_milrinonePediatric.R, Hornik 2019)
                "milrinonePediatric",
                # Cheng 1997's regressions on Cockcroft-Gault (R/drugs_digoxinIV.R)
                "digoxinIV",
                # el-Desoky 2005's serum creatinine term
                # (R/drugs_digoxinIVPediatric.R)
                "digoxinIVPediatric",
                # Cockcroft-Gault in a hockey stick on clearance, both tablets
                # (R/drugs_pramipexole.R, FDA review of NDA 22-421)
                "pramipexole", "pramipexoleER",
                # Cockcroft-Gault, as a power, on oral bioavailability (F1)
                # (R/drugs_mirabegron.R, FDA review of NDA 202611)
                "mirabegron",
                # MDRD eGFR, proportional, on the elimination rate constant
                # (R/drugs_amantadineER.R, FDA review of NDA 209410)
                "amantadineER",
                # Cockcroft-Gault, as a power, on clearance
                # (R/drugs_tranexamicAcid.R, Lanoiselee 2018)
                "tranexamicAcid",
                # Cockcroft-Gault, as a power, on clearance
                # (R/drugs_sotalol.R, Yellepeddi 2025)
                "sotalol",
                # Cockcroft-Gault, additive and linear, on clearance
                # (R/drugs_tobramycin.R, Conil 2011; R/drugs_amikacin.R, Tod 1998)
                "tobramycin", "amikacin",
                # Cockcroft-Gault on clearance (R/drugs_ganciclovir.R, Caldés 2009)
                "ganciclovir",
                # creatinine clearance on clearance: dofetilide (FDA review of
                # NDA 20-931), quinidine (Fattinger 1991)
                "dofetilide", "quinidine",
                # Cockcroft-Gault at 65 kg on renal clearance (Boak 2014)
                "linezolid",
                # the Japanese eGFR on renal clearance (Doki 2012)
                "flecainide",
                # MDRD eGFR, a sigmoid, on renal clearance (FDA review of NDA 209776)
                "meropenem",
                # creatinine against its normal for age, exponential (Kong 2025)
                "piperacillin",
                # Cockcroft-Gault, linear per mL/min, on CL/F (Saleh 2016)
                "riociguat",
                # MDRD eGFR below 60, a step on aprocitentan's clearance
                # (Bartolucci 2021, R/drugs_aprocitentan.R)
                "macitentan", "aprocitentan",
                # Garonzik's creatinine clearance (Cockcroft-Gault per 1.73
                # m2) on CMS's and colistin's clearances (R/drugs_colistimethate.R)
                "colistimethate", "colistin",
                # the 2012 CKD-EPI creatinine eGFR, de-indexed to mL/min, in
                # proportion on clearance (Barreto 2023, R/drugs_cefepime.R)
                "cefepime",
                # Cockcroft-Gault on the Janmahasatian lean body weight, in
                # proportion on CL/F (Duong 2013, R/drugs_metforminER.R)
                "metforminER",
                # Cockcroft-Gault, a power on CL/F with its own coefficient
                # below 30 mL/min (Naik 2016, R/drugs_alogliptin.R)
                "alogliptin",
                # rocuronium (Kleijn 2011): Cockcroft-Gault in an exponential
                # on V1, exp[-0.00143 (CR - 119)], not on its clearance
                # (R/drugs_rocuronium.R)
                "rocuronium")

# The elimination clearance of each of a drug's systems: its own and those of
# its parallel systems (meloxicam's renal term is on its intravenous system),
# and the later-phase set of a dose-phase drug (dabigatranHip's renal term is on
# its day-2-to-10 phase, not the day-0-1 default set).
systemClearances <- function(pk)
  c(pk$PK$default$cl1,
    vapply(pk$parallelSystems, function(s) s$PK$default$cl1, numeric(1)),
    if (!is.null(pk$dosePhase)) pk$PK[[pk$dosePhase$late]]$cl1,
    # a metabolite the saturable engine advances with its parent, whose renal
    # term may be the metabolite's (macitentan's aprocitentan)
    pk$michaelisMenten$metabolite$cl)

test_that("a blank creatinine is the assumed normal value for age and sex", {
  expect_equal(patientCreatinine(NULL, 40, "male"), SCR_ASSUMED_MALE)
  expect_equal(patientCreatinine(NA, 40, "female"), SCR_ASSUMED_FEMALE)
  expect_equal(patientCreatinine(2.5, 40, "female"), 2.5)
  expect_equal(patientCreatinine(NULL, 5, "male"), assumedCreatinine(5, "male"))
  expect_equal(patientCreatinine(0.9, 5, "male"), 0.9)
})

test_that("every renal model takes the creatinine, and the rest ignore it", {
  for (drug in getDrugDefaultsGlobal()$Drug[!isGasDrug(getDrugDefaultsGlobal()$Drug)]) {
    takes <- "creatinine" %in% names(formals(get(drug, mode = "function")))
    expect_identical(takes, drug %in% renalDrugs, info = drug)
  }
})

test_that("each renal model is unchanged at the assumed creatinine", {
  # Entering the assumed value must reproduce leaving the field blank, so
  # existing simulations do not move.
  for (drug in renalDrugs) {
    for (sex in SEX_VALUES) {
      blank   <- getDrugPK(drug, 70, 170, 50, sex)
      entered <- getDrugPK(drug, 70, 170, 50, sex,
                           creatinine = assumedCreatinine(50, sex))
      expect_equal(entered$PK, blank$PK, info = paste(drug, sex))
      expect_equal(entered$parallelSystems, blank$parallelSystems, info = paste(drug, sex))
    }
  }
})

test_that("a child's normal creatinine for age, entered, reproduces a blank field", {
  # Before the normal values for age, a blank field was an adult's
  # creatinine, so entering a child's real, normal value roughly doubled the
  # renal clearance of the Cockcroft-Gault models.
  children <- list(c(age = 0.5, weight = 7, height = 65),
                   c(age = 5, weight = 20, height = 110),
                   c(age = 15, weight = 55, height = 165))
  for (drug in renalDrugs) {
    for (sex in SEX_VALUES) {
      for (p in children) {
        blank   <- getDrugPK(drug, p[["weight"]], p[["height"]], p[["age"]], sex)
        entered <- getDrugPK(drug, p[["weight"]], p[["height"]], p[["age"]], sex,
                             creatinine = assumedCreatinine(p[["age"]], sex))
        expect_equal(entered$PK, blank$PK, info = paste(drug, sex, p[["age"]]))
      }
    }
  }
})

test_that("a child's creatinine is read against the normal for age", {
  # Mannitol's clearance is proportional to Cockcroft-Gault, so a 5 y boy at
  # twice the normal creatinine for his age clears it at half the rate he
  # does at the normal, and at the normal he clears it as before the
  # normal values for age: at Cockcroft-Gault on an adult's creatinine.
  q <- assumedCreatinine(5, "male")
  normal <- mannitol(20, 110, 5, "male", adjustToFFM = FALSE)$PK$default$cl1
  double <- mannitol(20, 110, 5, "male", adjustToFFM = FALSE, creatinine = 2 * q)$PK$default$cl1
  expect_equal(double, normal / 2)
  reference <- creatinineClearanceCG(FFM_REFERENCE_WEIGHT, FFM_REFERENCE_AGE,
                                     FFM_REFERENCE_SEX, SCR_ASSUMED_MALE)
  expect_equal(normal / mannitol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default$cl1,
               creatinineClearanceCG(20, 5, "male", SCR_ASSUMED_MALE) / reference)
})

# Models whose renal term acts on oral bioavailability, not on a clearance:
# mirabegron's F1 x (CrCL / 92)^-0.258 (FDA review of NDA 202611), an
# empirical covariate fitted in renal impairment.  A raised creatinine
# raises their bioavailability instead.
renalOnBioavailability <- c("mirabegron")

test_that("a raised creatinine raises the bioavailability where the renal term is on F", {
  for (drug in renalOnBioavailability) {
    normal <- getDrugPK(drug, 70, 170, 50, "male")
    raised <- getDrugPK(drug, 70, 170, 50, "male", creatinine = 3)
    expect_gt(raised$PK[[1]]$bioavailability_PO, normal$PK[[1]]$bioavailability_PO, label = drug)
    expect_identical(systemClearances(raised), systemClearances(normal), label = drug)
  }
})

# Models whose renal term acts on a volume, not on a clearance: rocuronium's
# V1 x exp[-0.00143 (CR - 119)] (Kleijn 2011).  A raised creatinine lowers
# the creatinine clearance and so raises V1.
renalOnVolume <- c("rocuronium")

test_that("a raised creatinine raises V1 where the renal term is on the volume", {
  for (drug in renalOnVolume) {
    normal <- getDrugPK(drug, 70, 170, 50, "male")
    raised <- getDrugPK(drug, 70, 170, 50, "male", creatinine = 3)
    expect_gt(raised$PK$default$v1, normal$PK$default$v1, label = drug)
    expect_identical(systemClearances(raised), systemClearances(normal), label = drug)
  }
})

test_that("a raised creatinine lowers every renal clearance", {
  for (drug in setdiff(renalDrugs, c(renalOnBioavailability, renalOnVolume))) {
    normal <- systemClearances(getDrugPK(drug, 70, 170, 50, "male"))
    raised <- systemClearances(getDrugPK(drug, 70, 170, 50, "male", creatinine = 3))
    expect_lt(min(raised - normal), 0, label = drug)
    expect_true(all(raised <= normal), label = drug)
  }
})

test_that("a non-renal model ignores the creatinine", {
  expect_identical(getDrugPK("propofol", 70, 170, 50, "male", creatinine = 3),
                   getDrugPK("propofol", 70, 170, 50, "male"))
})

test_that("vancomycin floors the creatinine at 60 umol/L as Thomson did", {
  # 0.3 mg/dL is below the 0.68 mg/dL floor, so it gives the floor's clearance.
  low   <- vancomycin(70, 170, 50, "male", creatinine = 0.3)$PK$default$cl1
  floor <- vancomycin(70, 170, 50, "male", creatinine = 60 / 88.42)$PK$default$cl1
  expect_equal(low, floor)
})
