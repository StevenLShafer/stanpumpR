context("getDrugPKtest")

#library(shiny)
library(here)

source(here('data', 'drugs', 'propofol.R'))

test_that("it returns the same value", {
  drug <- "propofol"
  weight <- 70
  height <- 170
  age <- 50
  sex <- "male"
  drugDefaults <- drugDefaults_global
#  drugDefaults <- shiny::reactiveVal(drugDefaults_global)/


  actual <- getDrugPK(drug, weight, height, age, sex, drugDefaults)
  expected <- list(
    drug = "propofol",
    PK = list(
      default = list(
        v1 = 6.283078,
        v2 = 20.17078,
        v3 = 1004.196,
        cl1 = 1.551355,
        cl2 = 1.516118,
        cl3 = 2.97701,
        k10 = 0.24691,
        k12 = 0.2413017,
        k13 = 0.473814,
        k21 = 0.07516406,
        k31 = 0.002964571,
        ka_PO = 0,
        bioavailability_PO = 0,
        tlag_PO = 0,
        ka_IM = 0,
        bioavailability_IM = 0,
        tlag_IM = 0,
        ka_IN = 0,
        bioavailability_IN = 0,
        tlag_IN = 0,
        customFunction = "",
        lambda_1 = 0.9834275,
        lambda_2 = 0.05572286,
        lambda_3 = 0.001004003,
        ke0 = 0.3973057,
        p_coef_bolus_l1 = 0.1555113,
        p_coef_bolus_l2 = 0.00321584,
        p_coef_bolus_l3 = 0.0004304708,
        e_coef_bolus_l1 = -0.1054142,
        e_coef_bolus_l2 = 0.003740444,
        e_coef_bolus_l3 = 0.0004315614,
        e_coef_bolus_ke0 = 0.1012422,
        p_coef_infusion_l1 = 0.158132,
        p_coef_infusion_l2 = 0.05771132,
        p_coef_infusion_l3 = 0.4287546,
        e_coef_infusion_l1 = -0.1071906,
        e_coef_infusion_l2 = 0.06712584,
        e_coef_infusion_l3 = 0.4298409,
        e_coef_infusion_ke0 = 0.2548219,
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
        e_coef_IN_ka = 0
      )
    ),
    tPeak = 1.6,
    pkEvents = "default",
    reference = "Not Available",
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
    emerge              = drugDefaults$Emerge
  )

  expect_equal_rounded(actual, expected)
})
