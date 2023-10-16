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
