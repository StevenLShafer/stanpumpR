test_that("it returns the correct array", {
  dose <- data.frame(
    Drug = "remifentanil",
    Time = 0,
    Dose = 0,
    Units = "mcg/kg/min"
  )

  events <- data.frame( Time = double(), Event = character(), Fill = character())

  PK <- list(
    Color = "#0000C0",
    endCe = 1,
    endCeText = "ventilation",
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
  expect_equal(actual, expected)
})
