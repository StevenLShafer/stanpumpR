# Metoprolol (R/drugs_metoprolol.R): Lammers 2017's two compartments with
# first-order oral absorption in healthy men, by CYP2D6 phenotype.
# Drafted by Claude Code, 2026-10-19.

# Lammers LA et al., Clin Pharmacokinet 2017;56:1231-1244, Table 3 (control)
CL <- 65.8; V1 <- 187; Q <- 74.5; V2 <- 97.6; KA <- 1.19; F <- 0.45

events <- data.frame(Time = double(), Event = character())

test_that("the parameters are Table 3's, in minutes", {
  PK <- getDrugPK("metoprolol", 80, 186, 23, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(V1, set$v1)
  expect_equal_rounded(V2, set$v2)
  expect_equal_rounded(CL / 60, set$cl1)
  expect_equal_rounded(Q / 60, set$cl2)
  expect_equal(set$cl3, 0)
  expect_equal_rounded(KA / 60, set$ka_PO)
  expect_equal_rounded(F, set$bioavailability_PO)
  expect_equal(PK$tPeak, 0)
  # Vss 285 L; terminal half-life 3.4 h
  expect_equal(round(log(2) / set$lambda_2 / 60, 1), 3.4)
})

test_that("intermediate and normal metabolisers are the typical subject", {
  normal <- metoprolol(80, 186, 23, "male", FALSE, cyp2d6 = "normal")$PK$default
  inter  <- metoprolol(80, 186, 23, "male", FALSE, cyp2d6 = "intermediate")$PK$default
  expect_identical(normal, inter)
})

test_that("the poor and ultrarapid metaboliser factors are the fixed ones", {
  poor <- metoprolol(80, 186, 23, "male", FALSE, cyp2d6 = "poor")$PK$default
  ultra <- metoprolol(80, 186, 23, "male", FALSE, cyp2d6 = "ultrarapid")$PK$default
  expect_equal_rounded(CL * 0.44 / 60, poor$cl1)
  expect_equal(poor$bioavailability_PO, 1)        # 0.45 x 2.28 = 1.03, held at 1
  expect_equal_rounded(CL * 2.01 / 60, ultra$cl1)
  expect_equal_rounded(F * 0.43, ultra$bioavailability_PO)
  # getDrugPK passes the Patient Profile's phenotype through
  PK <- getDrugPK("metoprolol", 80, 186, 23, "male", adjustToFFM = FALSE,
                  cyp2d6 = "poor")$PK$default
  expect_equal(PK$cl1, poor$cl1)
  expect_error(metoprolol(80, 186, 23, "male", cyp2d6 = "fast"), "Invalid cyp2d6")
})

test_that("an oral dose is 45% bioavailable: AUC ratio to IV", {
  run <- function(units) simulateDrugsWithCovariates(
    data.frame(Drug = "metoprolol", Time = 0, Dose = 100, Units = units),
    events, 80, 186, 23, "male", 3000, FALSE, adjustToFFM = FALSE)$metoprolol$wide
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  ratio <- auc(run("mg PO")) / auc(run("mg"))
  expect_lt(abs(ratio - F), 0.02)
})

test_that("size scaling: fat-free mass with the switch on, published values off", {
  size <- pkSizeFactors(110, 180, 50, "male", TRUE, legacyVolume = 1)
  on <- metoprolol(110, 180, 50, "male", adjustToFFM = TRUE)$PK$default
  off <- metoprolol(110, 180, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(V1 * size$volume, on$v1)
  expect_equal_rounded(CL / 60 * size$clearance, on$cl1)
  expect_equal_rounded(V1, off$v1)
})

test_that("metoprolol is a plasma-only vasoactive drug", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "metoprolol", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$MEAC, row$Lower, row$Upper), c(0, 0, 0))
  expect_true(all(c("mg", "mg PO", "mg PO bid") %in% unlist(row$Units)))
})

test_that("pregnancy: clearance x 1.5 and oral bioavailability x 0.5, not in a poor metaboliser", {
  # Ryu 2015's CL/F x 2.96 split by a well-stirred liver at extraction 0.5.
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.
  on  <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default; b <- off$PK$default
  expect_equal(a$cl1 / b$cl1, METOPROLOL_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(a$bioavailability_PO / b$bioavailability_PO,
               METOPROLOL_PREGNANCY_F_FACTOR, tolerance = 1e-6)
  expect_equal(c(METOPROLOL_PREGNANCY_CL_FACTOR, METOPROLOL_PREGNANCY_F_FACTOR),
               c(1.5, 0.5))
  expect_equal_rounded(1.399444225, a$cl1)
  expect_equal_rounded(0.225, a$bioavailability_PO)
  expect_identical(a[c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "tlag_PO")],
                   b[c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "tlag_PO")])
  expect_match(on$reference, "Ryu", fixed = TRUE)
  expect_no_match(off$reference, "Ryu", fixed = TRUE)
  # The other phenotypes take the factors on top of their own
  for (phenotype in c("intermediate", "ultrarapid")) {
    p1 <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE, cyp2d6 = phenotype,
                    pregnant = TRUE)$PK$default
    p0 <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE,
                    cyp2d6 = phenotype)$PK$default
    expect_equal(p1$cl1 / p0$cl1, METOPROLOL_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
    expect_equal(p1$bioavailability_PO / p0$bioavailability_PO,
                 METOPROLOL_PREGNANCY_F_FACTOR, tolerance = 1e-6)
  }
  # Pregnancy cannot induce an absent enzyme: a poor metaboliser is unchanged
  poor1 <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE, cyp2d6 = "poor",
                     pregnant = TRUE)
  poor0 <- getDrugPK("metoprolol", 70, 165, 30, SEX_FEMALE, cyp2d6 = "poor")
  expect_identical(poor1, poor0)
  expect_no_match(poor1$reference, "Ryu", fixed = TRUE)
})
