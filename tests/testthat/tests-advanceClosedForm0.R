

context("processDoseTable")

test_that("it returns the correct array", {
  dose <- list(
    Drug = "remifentanil",
    Time = 0,
    Dose = 0,
    Units = "mcg/kg/min",
    Bolus = FALSE,
    PO = FALSE,
    IM = FALSE,
    IN = FALSE
  )


  pkSet <- list(
    v1 = 3.968095,
    v2 = 7.254726,
    v3 = 3.017993,
    cl1 = 2.280839,
    cl2 = 1.58675,
    cl3 = 0.07812496,
    k10 = 0.5747944,
    k12 = 0.3998769,
    k13 = 0.01968828,
    k21 = 0.2187194,
    k31 = 0.0258864,
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
    lambda_1 = 1.094682,
    lambda_2 = 0.1193807,
    lambda_3 = 0.02490288,
    ke0 = 0.4410741,
    p_coef_bolus_l1 = 0.2261336,
    p_coef_bolus_l2 = 0.02540114,
    p_coef_bolus_l3 = 0.0004752982,
    e_coef_bolus_l1 = -0.1526017,
    e_coef_bolus_l2 = 0.03482752,
    e_coef_bolus_l3 = 0.0005037391,
    e_coef_bolus_ke0 = 0.1172705,
    p_coef_infusion_l1 = 0.2065748,
    p_coef_infusion_l2 = 0.2127743,
    p_coef_infusion_l3 = 0.01908607,
    e_coef_infusion_l1 = -0.1394028,
    e_coef_infusion_l2 = 0.291735,
    e_coef_infusion_l3 = 0.02022815,
    e_coef_infusion_ke0 = 0.2658748,
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

  maximum <- 60
  plotRecovery <- FALSE
  emerge <- 1

  actual <- advanceClosedForm0(dose, pkSet, maximum, plotRecovery, emerge)

  expected <- data.frame(
    Time = c(0.0000000, 0.3927911, 0.4798366, 0.5861721, 0.7160723, 0.8747594, 1.0686127, 1.3054254, 1.5947176, 1.9481191, 2.3798371, 2.9072271, 3.5514907, 4.3385281, 5.2999789, 6.4744945, 7.9092917, 9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000),
    Cp = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Ce = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Recovery = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)
})
