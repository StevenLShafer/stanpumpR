# tolcapone: see R/drugs_tolcapone.R (Jorga 2000, Table 3, fluctuator model).

test_that("Table 3 at the median lean body weight, switch off", {
  # a 70 kg, 170 cm man has a James lean body weight of 55.3 kg
  lbw <- lbmJames(70, 170, "male")
  p <- tolcapone(70, 170, 65, "male", FALSE)$PK$default
  expect_equal(p$cl1 * 60, 4.8 * (lbw / 55)^0.73)
  expect_equal(p$v1, 16 * (lbw / 55)^0.65)
  expect_equal(p$cl2 * 60, 5.2)
  expect_equal(p$v2, 12)
  expect_equal(p$ka_PO * 60, 1.7)
  expect_equal(p$bioavailability_PO, 0.6)
  expect_equal(p$tlag_PO, 0)
})

test_that("lean body weight 15 kg either side of 55 gives 80% and 119% of CL (the paper)", {
  expect_equal_rounded(0.792, (40 / 55)^TOLCAPONE_CL_LBW, tolerance = 0.001)
  expect_equal_rounded(1.193, (70 / 55)^TOLCAPONE_CL_LBW, tolerance = 0.001)
  # switch on: James at the pharmacokinetic weight, the library factors on Q and Vp
  size <- pkSizeFactors(120, 170, 65, "female", TRUE)
  on <- tolcapone(120, 170, 65, "female", TRUE)$PK$default
  expect_equal(on$cl1 * 60, 4.8 * (lbmJames(size$pkWeight, 170, "female") / 55)^0.73)
  expect_equal(on$v2, 12 * size$volume)
})

test_that("serum albumin on Vp: (albumin/44 g/L)^2.82, the median when blank", {
  blank <- tolcapone(70, 170, 65, "male", FALSE)$PK$default
  expect_equal(blank$v2, 12)
  expect_equal(tolcapone(70, 170, 65, "male", FALSE, albumin = 4.4)$PK$default$v2, 12)
  low <- tolcapone(70, 170, 65, "male", FALSE, albumin = 3.5)$PK$default
  expect_equal_rounded(12 * 0.524, low$v2, tolerance = 0.001)
  # only Vp moves: clearance, Q, Vc and so the steady-state exposure do not
  expect_equal(low[c("cl1", "cl2", "v1")], blank[c("cl1", "cl2", "v1")])
  # held within the fluctuators' 26-55 g/L (Jorga 2000, Table 1)
  expect_equal(tolcapone(70, 170, 65, "male", FALSE, albumin = 0.5)$PK$default$v2,
               12 * (26 / 44)^2.82)
  expect_equal(tolcapone(70, 170, 65, "male", FALSE, albumin = 6)$PK$default$v2,
               12 * (55 / 44)^2.82)
  expect_equal_rounded(12 * 0.227,
                       tolcapone(70, 170, 65, "male", FALSE, albumin = 2.6)$PK$default$v2,
                       tolerance = 0.001)
  # getDrugPK() passes the Patient Profile's field to it
  expect_true("albumin" %in% names(formals(tolcapone)))
  expect_true("tolcapone" %in% patientFieldReaders("albumin"))
  expect_match(tolcapone(70, 170, 65, "male", FALSE)$reference, "held at the median 44 g/L")
  expect_match(tolcapone(70, 170, 65, "male", FALSE, albumin = 3)$reference,
               "as entered, held within the studied 26-55 g/L")
})

test_that("steady state on 200 mg three times a day: AUC, half-life, the band", {
  res <- pdSimulate(pdRegimen("tolcapone", "mg PO", 200, 360, 40), maximum = 40 * 360,
                    age = 65)
  lbw <- lbmJames(70, 170, "male")
  cl <- 4.8 * (lbw / 55)^0.73
  expect_equal(pdAUC(res, 37 * 360, 38 * 360), 200 * 0.6 / cl, tolerance = 0.01)
  expect_equal_rounded(4.88, pdTerminalHalfLife("tolcapone", 70, 170, 65, "male",
                                                adjustToFFM = FALSE), tolerance = 0.01)
  # two hours after a dose, inside the AGNP 2017 update's 3-6 mcg/mL (quoted by
  # Muller 2024), which the update 2026 no longer lists, so no band is drawn
  s <- pdSite(res)
  c2 <- s$Y[which.min(abs(s$Time - (37 * 360 + 120)))]
  expect_gt(c2, 3); expect_lt(c2, 6)
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tolcapone", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
})
