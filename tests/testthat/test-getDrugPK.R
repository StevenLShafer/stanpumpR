test_that("it returns the same value", {
  drug <- "propofol"
  weight <- 70
  height <- 170
  age <- 50
  sex <- "male"
  drugDefaults <- getDrugDefaultsGlobal()

  actual <- getDrugPK(drug, weight, height, age, sex, drugDefaults)

  expected <- list(
    drug = "propofol",
    PK = list(
      default = list(
        v1 = 6.283078,
        v2 = 20.17078,
        v3 = 136.7249,
        cl1 = 1.551355,
        cl2 = 1.516118,
        cl3 = 0.6672707,
        k10 = 0.24691,
        k12 = 0.2413017,
        k13 = 0.1062012,
        k21 = 0.07516406,
        k31 = 0.004880391,
        ka_PO = 0,
        bioavailability_PO = 0,
        tlag_PO = 0,
        ka_IM = 0,
        bioavailability_IM = 0,
        tlag_IM = 0,
        ka_IN = 0,
        bioavailability_IN = 0,
        tlag_IN = 0,
        ka_SL = 0,
        bioavailability_SL = 0,
        tlag_SL = 0,
        ka_RA = 0,
        bioavailability_RA = 0,
        tlag_RA = 0,
        ka_RAslow = 0,
        bioavailability_RAslow = 0,
        tlag_RAslow = 0,
        ka_PO2 = 0,
        bioavailability_PO2 = 0,
        tlag_PO2 = 0,
        customFunction = "",
        lambda_1 = 0.6280493,
        lambda_2 = 0.04305881,
        lambda_3 = 0.003349251,
        ke0 = 0.6871175,
        p_coef_bolus_l1 = 0.1500541,
        p_coef_bolus_l2 = 0.008398034,
        p_coef_bolus_l3 = 0.0007054883,
        e_coef_bolus_l1 = 1.745523,
        e_coef_bolus_l2 = 0.008959488,
        e_coef_bolus_l3 = 0.0007089439,
        e_coef_bolus_ke0 = -1.755191,
        p_coef_infusion_l1 = 0.2389209,
        p_coef_infusion_l2 = 0.1950364,
        p_coef_infusion_l3 = 0.2106406,
        e_coef_infusion_l1 = 2.779276,
        e_coef_infusion_l2 = 0.2080756,
        e_coef_infusion_l3 = 0.2116724,
        e_coef_infusion_ke0 = -2.554426,
        p_coef_PO_l1 = 0,
        p_coef_PO_l2 = 0,
        p_coef_PO_l3 = 0,
        p_coef_PO_ka = 0,
        e_coef_PO_l1 = 0,
        e_coef_PO_l2 = 0,
        e_coef_PO_l3 = 0,
        e_coef_PO_ke0 = 0,
        e_coef_PO_ka = 0,
        p_coef_IM_l1 = 0,
        p_coef_IM_l2 = 0,
        p_coef_IM_l3 = 0,
        p_coef_IM_ka = 0,
        e_coef_IM_l1 = 0,
        e_coef_IM_l2 = 0,
        e_coef_IM_l3 = 0,
        e_coef_IM_ke0 = 0,
        e_coef_IM_ka = 0,
        p_coef_IN_l1 = 0,
        p_coef_IN_l2 = 0,
        p_coef_IN_l3 = 0,
        p_coef_IN_ka = 0,
        e_coef_IN_l1 = 0,
        e_coef_IN_l2 = 0,
        e_coef_IN_l3 = 0,
        e_coef_IN_ke0 = 0,
        e_coef_IN_ka = 0,
        p_coef_SL_l1 = 0,
        p_coef_SL_l2 = 0,
        p_coef_SL_l3 = 0,
        p_coef_SL_ka = 0,
        e_coef_SL_l1 = 0,
        e_coef_SL_l2 = 0,
        e_coef_SL_l3 = 0,
        e_coef_SL_ke0 = 0,
        e_coef_SL_ka = 0,
        p_coef_RA_l1 = 0,
        p_coef_RA_l2 = 0,
        p_coef_RA_l3 = 0,
        p_coef_RA_ka = 0,
        e_coef_RA_l1 = 0,
        e_coef_RA_l2 = 0,
        e_coef_RA_l3 = 0,
        e_coef_RA_ke0 = 0,
        e_coef_RA_ka = 0,
        p_coef_RAslow_l1 = 0,
        p_coef_RAslow_l2 = 0,
        p_coef_RAslow_l3 = 0,
        p_coef_RAslow_ka = 0,
        e_coef_RAslow_l1 = 0,
        e_coef_RAslow_l2 = 0,
        e_coef_RAslow_l3 = 0,
        e_coef_RAslow_ke0 = 0,
        e_coef_RAslow_ka = 0,
        p_coef_PO2_l1 = 0,
        p_coef_PO2_l2 = 0,
        p_coef_PO2_l3 = 0,
        p_coef_PO2_ka = 0,
        e_coef_PO2_l1 = 0,
        e_coef_PO2_l2 = 0,
        e_coef_PO2_l3 = 0,
        e_coef_PO2_ke0 = 0,
        e_coef_PO2_ka = 0
      )
    ),
    # Which curve tPeak was measured against; "IV" for every drug whose model
    # predates oral-only drugs, which is to say all of them but hydrocodone.
    tPeakRoute = ROUTE_IV,
    tPeak = 1.6,
    pkEvents = "default",
    reference = "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959. https://pubmed.ncbi.nlm.nih.gov/29661412/",
    weight = 70,
    height = 170,
    age = 50,
    sex = "male",
    upperTypical        = drugDefaults$Upper,
    lowerTypical        = drugDefaults$Lower,
    typical             = drugDefaults$Typical,
    MEAC                = drugDefaults$MEAC,
    Concentration.Units = drugDefaults$Concentration.Units,
    Bolus.Units         = drugDefaults$Bolus.Units,
    Infusion.Units      = drugDefaults$Infusion.Units,
    Units               = drugDefaults$Units,
    Default.Units       = drugDefaults$Default.Units,
    endCe               = drugDefaults$endCe
  )

  expect_equal_rounded(actual, expected)
})

test_that("it falls back to 'Not Available' when a drug function omits a reference", {
  expect_equal(getDrugPK("propofol", 70, 170, 50, "male")$reference, "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959. https://pubmed.ncbi.nlm.nih.gov/29661412/")

  realPropofol <- propofol
  local_mocked_bindings(
    propofol = function(...) {
      X <- realPropofol(...)
      X$reference <- NULL
      X
    }
  )

  expect_equal(getDrugPK("propofol", 70, 170, 50, "male")$reference, "Not Available")
})

test_that("every drug in the library supplies a reference", {
  # The library also carries the inhaled gases, which are not intravenous drugs:
  # they have no drugs_<name>.R covariate function, never reach getDrugPK(), and
  # are routed to the gas engine instead (see test-gas-routing.R).  Their
  # parameter provenance is recorded in R/gasProperties.R rather than in a
  # reference field, so they are excluded here rather than made to fake one.
  ivDrugs <- getDrugDefaultsGlobal()$Drug
  ivDrugs <- ivDrugs[!isGasDrug(ivDrugs)]

  references <- vapply(
    ivDrugs,
    function(drug) getDrugPK(drug, 70, 170, 50, "male")$reference,
    character(1)
  )

  missing <- names(references)[!nzchar(references) | references == "Not Available"]

  expect_equal(missing, character(0))
})

test_that("getDrugPK accepts both documented sex values", {
  dd <- getDrugDefaults("propofol")
  expect_no_error(getDrugPK("propofol", 70, 170, 50, "male", dd))
  expect_no_error(getDrugPK("propofol", 70, 170, 50, "female", dd))
})

test_that("getDrugPK rejects an unrecognized sex instead of silently guessing", {
  dd <- getDrugDefaults("propofol")
  expect_error(getDrugPK("propofol", 70, 170, 50, "F", dd), "Invalid sex")
  expect_error(getDrugPK("propofol", 70, 170, 50, "Female", dd), "Invalid sex")
  expect_error(getDrugPK("propofol", 70, 170, 50, "", dd), "Invalid sex")
  expect_error(getDrugPK("propofol", 70, 170, 50, NA, dd), "Invalid sex")
  expect_error(getDrugPK("propofol", 70, 170, 50, c("male", "female"), dd), "Invalid sex")
})

test_that("male and female produce different parameters", {
  dd <- getDrugDefaults("propofol")
  m <- getDrugPK("propofol", 70, 170, 50, SEX_MALE, dd)$PK[[PK_EVENT_DEFAULT]]
  f <- getDrugPK("propofol", 70, 170, 50, SEX_FEMALE, dd)$PK[[PK_EVENT_DEFAULT]]
  expect_false(isTRUE(all.equal(m$cl1, f$cl1)))
})


test_that("getDrugPK resolves drug functions from a caller that cannot see them", {
  # Regression for a bug that passed 1504 tests and broke R CMD check on four
  # platforms.  getDrugPK looked a drug function up twice, one line apart, in
  # two different environments: exists() searched its own frame, whose
  # enclosure is this namespace, while match.fun() searched parent.frame(2),
  # the environment of getDrugPK's CALLER.  Under devtools::load_all() the
  # drug functions are visible to any caller and it resolved; in an installed
  # package they are internal, the caller is the user's workspace, and it
  # failed with "object 'remifentanil' of mode 'function' was not found".
  #
  # The bug is about the CALLER's environment, not about installation, which
  # is what makes it testable here.  Enclosing the caller in an environment
  # whose parent is baseenv() means the lookup walks frame, that environment,
  # baseenv, emptyenv, and never reaches the attached package -- exactly the
  # condition an installed package creates.
  #
  # Harness contributed by the recovery session, which was right that CI is
  # not a good enough guard for this: six minutes after a push, against two
  # seconds here.
  caller <- function()
    fn("remifentanil", 70, 170, 50, "male", dd("remifentanil"))
  blind <- new.env(parent = baseenv())
  assign("fn", getDrugPK, blind)
  assign("dd", getDrugDefaults, blind)
  environment(caller) <- blind

  expect_false(exists("remifentanil", envir = blind, mode = "function"))
  expect_no_error(PK <- caller())
  expect_equal(PK$drug, "remifentanil")

  # and the same for a drug that DOES take the optional covariate, since that
  # is the branch the faulty lookup sat in
  caller2 <- function()
    fn("codeine", 70, 170, 50, "male", dd("codeine"), cyp2d6 = "ultrarapid")
  environment(caller2) <- blind
  expect_no_error(cod <- caller2())
  expect_equal(cod$drug, "codeine")
  # the phenotype really did reach the model
  expect_equal(cod$PK$default$metabolite$coefs$K,
               getDrugPK("codeine", 70, 170, 50, "male",
                         getDrugDefaults("codeine"),
                         cyp2d6 = "ultrarapid")$PK$default$metabolite$coefs$K)
})

# Audit finding F03 (October 2026): getDrugPK() returned no endCe, its
# `emerge` field reading an Emerge column the library does not have, so a
# direct getDrugPK() + simCpCe() call reported every time until threshold as
# zero.  Only the app and simulateDrugsWithCovariates(), which set endCe by
# hand, got a time.
test_that("getDrugPK() carries the library's endCe, so direct recovery works", {
  dd <- getDrugDefaults("propofol")
  PK <- getDrugPK("propofol", 70, 170, 35, "male")
  expect_equal(PK$endCe, dd$endCe)
  expect_gt(PK$endCe, 0)
  expect_null(PK$emerge)

  # an edited threshold, as the app passes its session's row, is the one used
  edited <- dd
  edited$endCe <- 2.5
  expect_equal(getDrugPK("propofol", 70, 170, 35, "male", edited)$endCe, 2.5)

  dose <- data.frame(Drug = "propofol", Time = 0, Dose = 140, Units = "mg")
  events <- data.frame(Time = double(), Event = character())
  sim <- simCpCe(dose, events, PK, maximum = 60, plotRecovery = TRUE)
  expect_gt(sim$max$Recovery, 0)
  # Ce peaks above the threshold soon after the bolus, so there is a wait
  peak <- which.max(sim$equiSpace$Ce)
  expect_gt(sim$equiSpace$Ce[peak], PK$endCe)
  expect_gt(sim$equiSpace$Recovery[peak], 0)

  # and it is what simulateDrugsWithCovariates() gives, which no longer sets
  # endCe itself
  via <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male",
                                     maximum = 60, plotRecovery = TRUE)
  expect_equal(via$propofol$max$Recovery, sim$max$Recovery)
  expect_equal(via$propofol$equiSpace$Recovery, sim$equiSpace$Recovery)
})

test_that("the CYP2C19 phenotype is validated and reaches only the models that name it", {
  expect_error(getDrugPK("remifentanil", 70, 170, 50, "male", cyp2c19 = "extensive"),
               "Invalid cyp2c19")
  expect_error(getDrugPK("remifentanil", 70, 170, 50, "male", cyp2c19 = CYP2C19_VALUES),
               "Invalid cyp2c19")
  # A model that does not name it is unchanged by it
  expect_equal(getDrugPK("remifentanil", 70, 170, 50, "male", cyp2c19 = "poor")$PK,
               getDrugPK("remifentanil", 70, 170, 50, "male")$PK)
  # One that does is changed by it
  expect_lt(getDrugPK("escitalopram", 70, 170, 50, "male", cyp2c19 = "poor")$PK$default$cl1,
            getDrugPK("escitalopram", 70, 170, 50, "male")$PK$default$cl1)
})

test_that("parallel-system routes must be dose routes", {
  expect_null(parallelRoutes(NULL, "x"))
  expect_equal(parallelRoutes(c("PO", "PO"), "x"), "PO")
  expect_error(parallelRoutes("oral", "x"), "Invalid routes for x")
  expect_error(parallelRoutes(character(0), "x"), "Invalid routes for x")
})
