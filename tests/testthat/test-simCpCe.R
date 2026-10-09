test_that("it returns the correct array", {
  dose <- data.frame(
    Drug = "remifentanil",
    Time = 0,
    Dose = 0,
    Units = "mcg/kg/min"
  )

  events <- data.frame( Time = double(), Event = character())

  PK <- list(
    Color = "#0000C0",
    endCe = 1,
    drug = "remifentanil",
    PK = list(
      default = list(
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
    ),
    tPeak = 1.6,
    pkEvents = "default",
    reference = "Not Available",
    weight = 60,
    height = 168.96,
    age = 50,
    sex = "female",
    upperTypical = 2,
    lowerTypical = 0.8,
    typical = 1.2,
    MEAC = 1,
    "Concentration.Units" = "ng",
    "Bolus.Units" = "mcg",
    "Infusion.Units" = "mcg/kg/min",
    Units = c("mcg", "mcg/kg", "mcg/kg/min"),
    "Default.Units" = "mcg/kg/min",
    maxCp = 1,
    maxCe = 1,
    recovery = 1
  )

  maximum <- 60
  plotRecovery <- FALSE

  actual <- simCpCe(dose, events, PK, maximum, plotRecovery)

  expected <- list(
    results = data.frame(
      Drug = c("remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil"),
      Time = c(0.0000000,  0.3927911,  0.4798366,  0.5861721,  0.7160723,  0.8747594,  1.0686127,  1.3054254,  1.5947176, 1.9481191,  2.3798371,  2.9072271,  3.5514907,  4.3385281,  5.2999789,  6.4744945,  7.9092917,  9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000,  0.0000000,  0.3927911,  0.4798366,  0.5861721,  0.7160723,  0.8747594,  1.0686127,  1.3054254, 1.5947176,  1.9481191,  2.3798371,  2.9072271,  3.5514907,  4.3385281,  5.2999789,  6.4744945,  7.9092917, 9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000,  0.0000000,  0.3927911,  0.4798366,  0.5861721,  0.7160723,  0.8747594,  1.0686127, 1.3054254,  1.5947176,  1.9481191,  2.3798371,  2.9072271,  3.5514907,  4.3385281,  5.2999789,  6.4744945, 7.9092917,  9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000,  0.0000000,  0.3927911,  0.4798366,  0.5861721,  0.7160723,  0.8747594, 1.0686127,  1.3054254,  1.5947176,  1.9481191,  2.3798371,  2.9072271,  3.5514907,  4.3385281,  5.2999789, 6.4744945,  7.9092917,  9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000,  0.0000000,  0.3927911,  0.4798366,  0.5861721,  0.7160723, 0.8747594,  1.0686127,  1.3054254,  1.5947176,  1.9481191,  2.3798371,  2.9072271,  3.5514907,  4.3385281, 5.2999789,  6.4744945,  7.9092917,  9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000,  0.0000000,  0.3927911,  0.4798366,  0.5861721, 0.7160723,  0.8747594,  1.0686127,  1.3054254,  1.5947176,  1.9481191,  2.3798371,  2.9072271,  3.5514907, 4.3385281,  5.2999789,  6.4744945,  7.9092917,  9.6620508, 11.8032348, 14.4189213, 17.6142639, 21.5177187, 26.2862088, 32.1114325, 39.2275701, 47.9206978, 58.5402888, 60.0000000),
      Site = c( "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Plasma", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "Effect Site", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CpNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CeNormCp", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CpNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe", "CeNormCe"),
      Y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      stringsAsFactors = FALSE
    ),
    equiSpace = data.frame(
      Drug = c("remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil", "remifentanil"),
      Time = c(0.0000000, 0.6060606, 1.2121212, 1.8181818, 2.4242424, 3.0303030, 3.6363636, 4.2424242, 4.8484848, 5.4545455, 6.0606061, 6.6666667, 7.2727273, 7.8787879, 8.4848485, 9.0909091, 9.6969697, 10.3030303, 10.9090909, 11.5151515, 12.1212121, 12.7272727, 13.3333333, 13.9393939, 14.5454545, 15.1515152, 15.7575758, 16.3636364, 16.9696970, 17.5757576, 18.1818182, 18.7878788, 19.3939394, 20.0000000, 20.6060606, 21.2121212, 21.8181818, 22.4242424, 23.0303030, 23.6363636, 24.2424242, 24.8484848, 25.4545455, 26.0606061, 26.6666667, 27.2727273, 27.8787879, 28.4848485, 29.0909091, 29.6969697, 30.3030303, 30.9090909, 31.5151515, 32.1212121, 32.7272727, 33.3333333, 33.9393939, 34.5454545, 35.1515152, 35.7575758, 36.3636364, 36.9696970, 37.5757576, 38.1818182, 38.7878788, 39.3939394, 40.0000000, 40.6060606, 41.2121212, 41.8181818, 42.4242424, 43.0303030, 43.6363636, 44.2424242, 44.8484848, 45.4545455, 46.0606061, 46.6666667, 47.2727273, 47.8787879, 48.4848485, 49.0909091, 49.6969697, 50.3030303, 50.9090909, 51.5151515, 52.1212121, 52.7272727, 53.3333333, 53.9393939, 54.5454545, 55.1515152, 55.7575758, 56.3636364, 56.9696970, 57.5757576, 58.1818182, 58.7878788, 59.3939394, 60.0000000),
      Ce = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Time.1 = c(0.0000000, 0.6060606, 1.2121212, 1.8181818, 2.4242424, 3.0303030, 3.6363636, 4.2424242, 4.8484848, 5.4545455, 6.0606061, 6.6666667, 7.2727273, 7.8787879, 8.4848485, 9.0909091, 9.6969697, 10.3030303, 10.9090909, 11.5151515, 12.1212121, 12.7272727, 13.3333333, 13.9393939, 14.5454545, 15.1515152, 15.7575758, 16.3636364, 16.9696970, 17.5757576, 18.1818182, 18.7878788, 19.3939394, 20.0000000, 20.6060606, 21.2121212, 21.8181818, 22.4242424, 23.0303030, 23.6363636, 24.2424242, 24.8484848, 25.4545455, 26.0606061, 26.6666667, 27.2727273, 27.8787879, 28.4848485, 29.0909091, 29.6969697, 30.3030303, 30.9090909, 31.5151515, 32.1212121, 32.7272727, 33.3333333, 33.9393939, 34.5454545, 35.1515152, 35.7575758, 36.3636364, 36.9696970, 37.5757576, 38.1818182, 38.7878788, 39.3939394, 40.0000000, 40.6060606, 41.2121212, 41.8181818, 42.4242424, 43.0303030, 43.6363636, 44.2424242, 44.8484848, 45.4545455, 46.0606061, 46.6666667, 47.2727273, 47.8787879, 48.4848485, 49.0909091, 49.6969697, 50.3030303, 50.9090909, 51.5151515, 52.1212121, 52.7272727, 53.3333333, 53.9393939, 54.5454545, 55.1515152, 55.7575758, 56.3636364, 56.9696970, 57.5757576, 58.1818182, 58.7878788, 59.3939394, 60.0000000),
      Recovery = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      MEAC = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      stringsAsFactors = FALSE
    ),
    max = data.frame(
      Drug = "remifentanil",
      Recovery = 0,
      Cp = 0,
      Ce = 0,
      stringsAsFactors = FALSE
    )
  )
  # simCpCe additionally returns `wide`, the Time/Plasma/Effect Site/Recovery
  # series the drug was built from, which foldMetabolites() needs when another
  # drug's active metabolite has to be added to this one's row.  It is checked
  # for shape rather than spelled out row by row.
  expect_equal(actual[c("results", "equiSpace", "max")], expected)
  expect_named(actual$wide, c("Time", "Plasma", "Effect Site", "Recovery"))
  expect_equal(nrow(actual$wide), length(unique(actual$wide$Time)))
  # Nothing was given, so nothing is present
  expect_true(all(actual$wide$Plasma == 0))
  # A drug with no active metabolite carries no metabolite series
  expect_null(actual$metaboliteSeries)
  expect_null(actual$metaboliteName)
})

# Audit finding F20 (October 2026): a dose after `maximum` was simulated, so a
# direct call returned curves running past the window, and the maxima and the
# normalised series came from a peak outside it.  The result now covers 0 to
# maximum and nothing else.
test_that("simCpCe() covers 0 to maximum: a later dose changes nothing inside it", {
  events <- data.frame(Time = double(), Event = character())
  PK <- getDrugPK("propofol", 70, 170, 40, "male")
  early <- data.frame(Drug = "propofol", Time = 0, Dose = 1, Units = "mg")
  both  <- data.frame(Drug = "propofol", Time = c(0, 120, 60), Dose = c(1, 100, 50),
                      Units = "mg")

  inside <- simCpCe(early, events, PK, maximum = 60, plotRecovery = TRUE)
  out <- simCpCe(both, events, PK, maximum = 60, plotRecovery = TRUE)
  expect_equal(max(out$results$Time), 60)
  expect_equal(out$results, inside$results)
  expect_equal(out$max, inside$max)
  expect_equal(out$equiSpace, inside$equiSpace)
  expect_equal(out$wide, inside$wide)
  expect_equal(out$recoveryStates, inside$recoveryStates)
  # normalised to the peak inside the window: 100%, not 1%
  norm <- out$results$Y[out$results$Site == "CpNormCp"]
  expect_equal(max(norm), 100)
  expect_equal(out$max$Cp, max(out$results$Y[out$results$Site == "Plasma"]))

  # the dose at 120 is simulated once the window reaches it
  longer <- simCpCe(both, events, PK, maximum = 180, plotRecovery = FALSE)
  expect_gt(longer$max$Cp, 10 * out$max$Cp)
})

test_that("simCpCe() cuts a lagged oral dose's absorption knot at maximum", {
  # gabapentin's oral lag is about 19 minutes: a dose at 50 begins to be
  # absorbed after a 60-minute window ends, which put a point at about 69
  events <- data.frame(Time = double(), Event = character())
  PK <- getDrugPK("gabapentin", 70, 170, 40, "male")
  expect_gt(PK$PK$default$tlag_PO, 10)
  dose <- data.frame(Drug = "gabapentin", Time = c(0, 50), Dose = c(300, 300),
                     Units = "mg PO")
  out <- simCpCe(dose, events, PK, maximum = 60, plotRecovery = TRUE)
  expect_equal(max(out$results$Time), 60)
  expect_equal(max(out$wide$Time), 60)
  expect_equal(max(out$recoveryStates$time), 60)
  expect_equal(nrow(out$recoveryStates$state), nrow(out$wide))
  expect_equal(nrow(out$equiSpace), RESOLUTION)

  # the same curve inside the window as on a longer run
  long <- simCpCe(dose, events, PK, maximum = 120, plotRecovery = FALSE)
  w <- long$wide[long$wide$Time <= 60, ]
  expect_equal(out$wide$Plasma[match(w$Time, out$wide$Time)], w$Plasma)
})

test_that("scheduled, TCI and metabolite doses respect the window too", {
  events <- data.frame(Time = double(), Event = character())

  # a scheduled dose starting after maximum gives nothing; one before it
  # repeats only inside the window
  PK <- getDrugPK("morphine", 70, 170, 40, "male")
  late <- data.frame(Drug = "morphine", Time = c(0, 600), Dose = c(4, 10),
                     Units = c("mg", "mg qid"))
  out <- simCpCe(late, events, PK, maximum = 480, plotRecovery = FALSE)
  expect_null(out$scheduled)
  expect_equal(max(out$results$Time), 480)
  sched <- data.frame(Drug = "morphine", Time = 0, Dose = 4, Units = "mg qid")
  out <- simCpCe(sched, events, PK, maximum = 480, plotRecovery = FALSE)
  expect_equal(out$scheduled$Time, 360)

  # a target set after maximum is not run
  PK <- getDrugPK("propofol", 70, 170, 40, "male")
  tci <- data.frame(Drug = "propofol", Time = c(0, 90), Dose = c(3, 4),
                    Units = "Plasma target")
  out <- simCpCe(tci, events, PK, maximum = 60, plotRecovery = FALSE)
  first <- simCpCe(tci[1, ], events, PK, maximum = 60, plotRecovery = FALSE)
  expect_equal(out$results, first$results)
  expect_equal(out$tci, first$tci)

  # a parent dose after maximum forms no metabolite inside the window
  multi <- data.frame(Drug = "codeine", Time = c(0, 300), Dose = c(30, 60), Units = "mg PO")
  a <- simulateDrugsWithCovariates(multi, events, 70, 170, 40, "male",
                                   maximum = 240, plotRecovery = TRUE)
  b <- simulateDrugsWithCovariates(multi[1, ], events, 70, 170, 40, "male",
                                   maximum = 240, plotRecovery = TRUE)
  expect_setequal(names(a), names(b))
  for (drug in names(b)) {
    expect_equal(a[[drug]]$max, b[[drug]]$max, info = drug)
    expect_equal(max(a[[drug]]$results$Time), 240, info = drug)
  }
})
