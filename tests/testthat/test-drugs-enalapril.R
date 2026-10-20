# Enalapril and enalaprilat (R/drugs_enalapril.R, R/drugs_enalaprilat.R):
# Faisal 2019's simultaneous serum-and-urine model, the library's first
# transit-chain drug.  Drafted by Claude Code, 2026-10-20.
#
# The expectations are written from Table 1 of Faisal M, Cawello W, Burckhardt
# BB, de Hoon J, Laer S, Front Pediatr 2019;7:281 independently of the drug
# files, in the paper's own units (per hour, litres), and converted here.
# Table 2 supplies the reference individual.

KA_H    <- 6.010      # /h    Table 1, out of the LAST transit compartment
VC_L    <- 51.10      # L
F1      <- 0.606
MTT1_H  <- 0.558      # h
TRANSITS <- 8L        # "Eight transits were added for reference and pooled"
KREN_H  <- 0.305      # /h    renal elimination of enalapril
KM_H    <- 0.688      # /h    elimination of enalapril by conversion
VM_L    <- 46.10      # L     enalaprilat
KQ1_H   <- 0.060      # /h
KQ2_H   <- 0.054      # /h
KME_H   <- 0.184      # /h
MTT2_H  <- 0.910      # h
WT_REF  <- 69.76      # kg    Table 2's MEAN weight, which the source normalised to
HT_REF  <- 174.5      # cm    Table 2
AGE_REF <- 28.00      # y     Table 2

noEvents <- data.frame(Time = numeric(0), Event = character(0))

at <- function(wt = WT_REF, ht = HT_REF, age = AGE_REF, sex = "male",
               ffm = FALSE, ...)
  getDrugPK("enalapril", wt, ht, age, sex, adjustToFFM = ffm, ...)

atMet <- function(wt = WT_REF, ffm = FALSE)
  getDrugPK("enalaprilat", wt, HT_REF, AGE_REF, "male", adjustToFFM = ffm)

runOne <- function(dose = 10, wt = WT_REF, maximum = 2880, units = "mg PO",
                   ffm = FALSE)
  simulateDrugsWithCovariates(
    data.frame(Drug = "enalapril", Time = 0, Dose = dose, Units = units,
               stringsAsFactors = FALSE),
    noEvents, wt, HT_REF, AGE_REF, "male", maximum, FALSE, adjustToFFM = ffm)


test_that("Table 1's enalapril parameters are reproduced at the reference man", {
  set <- at()$PK$default
  expect_equal_rounded(VC_L, set$v1)
  # The total elimination, renal plus conversion, is what the clearance covers
  expect_equal_rounded((KREN_H + KM_H) / 60 * VC_L, set$cl1)
  expect_equal_rounded(KREN_H + KM_H, set$k10 * 60)
  expect_equal_rounded(KA_H, set$ka_PO * 60)
  expect_equal_rounded(F1, set$bioavailability_PO)
  expect_equal(set$tlag_PO, 0)     # a lag model failed; the chain replaced it
  expect_equal(set$v2, 1)          # one compartment for the parent
  expect_equal(set$v3, 1)
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
})

test_that("the chain is eight deep, with KTR through it and KA out of the last", {
  # The source is explicit: KTR is "the rate transfer through these transits",
  # obtained from MTT1 = (N+1)/KTR, and KA is "the transfer rate of the drug
  # from the last transit into enalapril central compartment".  Getting this
  # round the wrong way moves the peak, so it is pinned.
  PK <- at()
  tc <- PK$transitChain
  expect_equal(tc$n, TRANSITS)
  expect_equal_rounded((TRANSITS + 1) / MTT1_H, tc$ktr * 60)
  expect_equal_rounded(16.129032, tc$ktr * 60)         # 9 / 0.558
  # The last compartment empties at ka_PO, not at ktr
  expect_equal_rounded(KA_H, PK$PK$default$ka_PO * 60)
  expect_gt(tc$ktr, PK$PK$default$ka_PO)
  # The derived summary the source's own two statements cannot both satisfy:
  # seven steps at KTR and one at KA give a true mean input time of 0.60 h
  # against the printed MTT1 of 0.558 h.  Recorded rather than smoothed over.
  trueMtt <- (TRANSITS - 1) / (tc$ktr * 60) + 1 / KA_H
  expect_equal(round(trueMtt, 3), 0.600)
  expect_gt(trueMtt, MTT1_H)
})

test_that("the parent's elimination splits into the source's two paths", {
  # A swapped kOut and kMetabolic leaves the parent curve identical and gets
  # the metabolite wrong by the ratio, so the split is pinned on both sides.
  PK <- at()
  set <- PK$PK$default
  expect_equal_rounded(KM_H, PK$transitChain$kMetabolic * 60)
  expect_equal_rounded(KREN_H, (set$k10 - PK$transitChain$kMetabolic) * 60)
  # About 69% of what is absorbed becomes enalaprilat
  expect_equal(round(100 * PK$transitChain$kMetabolic / set$k10, 1), 69.3)
})

test_that("the metabolite block is enalaprilat's own set, from one place", {
  PK <- at()
  m <- PK$transitChain$metabolite
  own <- atMet()$PK$default
  expect_identical(m$name, "enalaprilat")
  expect_identical(PK$metaboliteName, "enalaprilat")
  expect_equal_rounded(VM_L, m$v)
  expect_equal_rounded(m$v, own$v1)                   # the same numbers
  expect_equal_rounded(KME_H, m$kel * 60)
  expect_equal_rounded(KQ1_H, m$k12 * 60)
  expect_equal_rounded(KQ2_H, m$k21 * 60)
  # Two formation transits, every one of them at ktr: the source names no
  # separate rate out of the last, as it does for the parent's chain.
  expect_equal(m$n, 2L)
  expect_equal_rounded(3 / MTT2_H, m$ktr * 60)
  # Both moieties report in ng/mL, so no unit rescaling is needed -- but the
  # factor must be present and right, not absent.
  expect_equal(m$unitScale, 1)
})

test_that("enalaprilat's own set is two compartments with the implied volume", {
  own <- atMet()$PK$default
  expect_equal_rounded(VM_L, own$v1)
  # Not fitted: implied by the rate constants
  expect_equal_rounded(VM_L * KQ1_H / KQ2_H, own$v2)
  expect_equal_rounded(51.2222, own$v2, tolerance = 1e-4)
  expect_equal_rounded(KME_H * VM_L, own$cl1 * 60)
  expect_equal_rounded(KQ1_H * VM_L, own$cl2 * 60)
  expect_equal(own$v3, 1)
  expect_equal(own$cl3, 0)
  # The bi-phasic decay the source reports, which the one-compartment model it
  # rejected could not produce
  halves <- sort(log(2) / unlist(own[c("lambda_1", "lambda_2")]) / 60)
  expect_equal(round(halves[[1]], 1), 2.7)
  expect_equal(round(halves[[2]], 1), 18.1)
})

test_that("the curve reproduces the source's own reported observations", {
  # Not numbers it was fitted to: the Results section's prose.  "the Cmax of
  # enalapril was achieved in almost 1 h"; "The drug is eliminated in almost
  # 10 h from the serum"; "Biotransformed enalaprilat achieved Cmax in around
  # 3 to 4 h".
  r <- runOne()
  p <- r$enalapril$wide
  m <- r$enalaprilat$wide
  tmaxP <- p$Time[which.max(p$Plasma)] / 60
  tmaxM <- m$Time[which.max(m$Plasma)] / 60
  expect_equal(round(tmaxP, 2), 0.87)
  expect_gt(tmaxM, 3); expect_lt(tmaxM, 4)
  expect_equal(round(tmaxM, 2), 3.27)
  # Gone from the serum by 10 h: below the assay's own lower limit of
  # quantification for enalapril, 0.195 ng/mL
  expect_lt(approx(p$Time, p$Plasma, 600)$y, 0.195)
  # Both peaks sit inside the calibration ranges the assay was validated over
  # (enalapril 0.195 to 200 ng/mL, enalaprilat 0.180 to 188 ng/mL), which a
  # model off by a factor would not
  expect_gt(max(p$Plasma), 0.195); expect_lt(max(p$Plasma), 200)
  expect_gt(max(m$Plasma), 0.180); expect_lt(max(m$Plasma), 188)
  # Pinned, so that a change to the engine or the reading shows up here
  expect_equal(round(max(p$Plasma), 1), 75.8)
  expect_equal(round(max(m$Plasma), 1), 55.9)
})

test_that("the metabolite peak reproduces a study the model was not fitted to", {
  # EXTERNAL VALIDATION.  Arafat T, Awad R, Hamad M, Azzam R, Al-Nasan A,
  # Jehanli A, Matalka K, J Clin Pharm Ther 2005;30:319-328, two brands of
  # 10 mg enalapril maleate in healthy volunteers, measured with SPECIFIC
  # enzyme immunoassays for each moiety rather than the ACE-inhibition assay
  # most earlier studies used.  Nothing of this was an input to Faisal's fit.
  r <- runOne(10)
  m <- r$enalaprilat$wide
  cmax <- max(m$Plasma)
  # Arafat's two brands: 54.8 +/- 29.5 and 57.2 +/- 29.0 ng/mL
  expect_gt(cmax, 54.8 * 0.95)
  expect_lt(cmax, 57.2 * 1.05)
  # AND THIS IS WHAT PINS THE DOSE BASIS.  Faisal's text does not say whether
  # F1 = 0.606 also absorbs the maleate-to-base mass fraction (376.45/492.52
  # = 0.764), and the model is used on the basis it was fitted on.  If that
  # choice were wrong in either direction the curve would be off by 1.308,
  # and BOTH alternatives fall outside Arafat's measured range -- so an
  # ambiguity the source could not settle is settled by a study that never
  # entered it.
  salt <- 492.52 / 376.45
  expect_false(cmax * salt > 54.8 * 0.95 && cmax * salt < 57.2 * 1.05)
  expect_false(cmax / salt > 54.8 * 0.95 && cmax / salt < 57.2 * 1.05)
})

test_that("the external study disagrees about the tail, and that is recorded", {
  # The same study's metabolite AREA is about half the model's, and Arafat
  # says why in their own abstract: "enalaprilat concentrations at 12-24 h
  # following a single oral dose of enalapril in healthy volunteers were
  # LOWER than those reported in the literature", which they report as a
  # finding of theirs rather than as agreement. So the two studies agree on
  # the peak and disagree on the terminal phase. Recorded, not reconciled:
  # rescaling a primary fit to another study's number is fitting to a result,
  # the same reason benazepril keeps Kaiser's scale against Sun 1994.
  r <- runOne(10, maximum = 20160)
  m <- r$enalaprilat$wide
  auc <- sum(diff(m$Time) * (head(m$Plasma, -1) + tail(m$Plasma, -1)) / 2) / 60
  expect_equal(round(auc / 266.9, 1), 1.9)      # Arafat brand 1, 266.9 ng.h/mL
  # The parent cannot be compared at all, and the authors are the ones who
  # say so: "Cmax values for enalapril are about 10 times those published in
  # the literature". Their 313.5 ng/mL is an assay disagreement, not a target.
  p <- r$enalapril$wide
  expect_lt(max(p$Plasma), 313.5)
})

test_that("the three rejected readings of the chain are recorded, not hidden", {
  # All four readings reproduce the source's reported peak times, so the
  # choice is a documented rounding decision rather than a fitted result.
  # These are what the others would give, computed here from the same block,
  # so that a future reader can see the size of what was decided.
  PK <- at()
  s <- PK$PK$default
  tc <- PK$transitChain
  mt <- tc$metabolite
  peak <- function(nChain, kThrough, kLast) {
    nS <- nChain + 1L + mt$n + 2L
    iC <- nChain + 1L; iMC <- iC + mt$n + 1L; iMP <- iMC + 1L
    A <- matrix(0, nS, nS)
    ch <- c(rep(kThrough, nChain - 1L), kLast)
    for (j in seq_len(nChain)) { A[j, j] <- -ch[j]; A[j + 1L, j] <- ch[j] }
    A[iC, iC] <- -s$k10
    A[iC + 1L, iC] <- tc$kMetabolic
    for (j in seq_len(mt$n)) {
      A[iC + j, iC + j] <- -mt$ktr; A[iC + j + 1L, iC + j] <- mt$ktr
    }
    A[iMC, iMC] <- -(mt$kel + mt$k12); A[iMC, iMP] <- mt$k21
    A[iMP, iMC] <- mt$k12;             A[iMP, iMP] <- -mt$k21
    x <- numeric(nS); x[1] <- 1
    grid <- seq(0, 600, by = 0.25)
    cp <- cm <- numeric(length(grid))
    step <- expmScaleSquare(A * 0.25)
    for (i in 2:length(grid)) {
      x <- as.vector(step %*% x)
      cp[i] <- x[iC]; cm[i] <- x[iMC]
    }
    c(grid[which.max(cp)] / 60, grid[which.max(cm)] / 60)
  }
  ktr9 <- 9 / MTT1_H / 60
  # 1: nine chain compartments, eight at 9/MTT1 and a depot at KA
  r1 <- peak(9L, ktr9, s$ka_PO)
  # 2: the implemented reading -- eight, the last at KA
  r2 <- peak(8L, ktr9, s$ka_PO)
  # 3: eight all at 8/MTT1, KA unused
  r3 <- peak(8L, 8 / MTT1_H / 60, 8 / MTT1_H / 60)
  # 4: the through-rate solved so the true mean input time equals MTT1
  r4 <- peak(9L, 8 / (MTT1_H - 1 / KA_H) / 60, s$ka_PO)
  parent <- c(r1[1], r2[1], r3[1], r4[1])
  metab  <- c(r1[2], r2[2], r3[2], r4[2])
  # Every reading lands on the source's "almost 1 h" and "around 3 to 4 h",
  # which is why the ambiguity was not a blocker (docs/antihypertensives.md
  # section 14.3)
  expect_true(all(parent > 0.8 & parent < 1.0))
  expect_true(all(metab > 3.1 & metab < 3.4))
  # Which reading lands nearest the reported "almost 1 h" is READING 1, not the
  # implemented one: every peak is below 1 h, so the latest is the nearest.
  # Pinned explicitly because an earlier draft of the header, the registry, the
  # documentation and the help page all claimed the implemented reading was the
  # closest, and this assertion is what disproves it.  The implemented reading
  # is chosen because the text states it, not because it fits the reported peak
  # best -- it is the second nearest, ahead of readings 3 and 4.
  expect_equal(which.max(parent), 1L)
  expect_gt(r1[1], r2[1])
  expect_gt(r2[1], r3[1]); expect_gt(r2[1], r4[1])
})

test_that("weight scales the parent's central volume and nothing else", {
  # The only covariate retained: weight to the first power on VC, normalised
  # to the study's mean weight.  The rate constants must stay weight-invariant,
  # which is what makes the clearance scale with weight too.
  for (wt in c(51.8, 69.76, 95.6))
  {
    PK <- at(wt = wt)
    set <- PK$PK$default
    expect_equal_rounded(VC_L * wt / WT_REF, set$v1)
    expect_equal_rounded((KREN_H + KM_H) / 60 * VC_L * wt / WT_REF, set$cl1)
    # Weight-invariant, every one of them
    expect_equal_rounded(KREN_H + KM_H, set$k10 * 60)
    expect_equal_rounded(KA_H, set$ka_PO * 60)
    expect_equal_rounded((TRANSITS + 1) / MTT1_H, PK$transitChain$ktr * 60)
    expect_equal_rounded(KM_H, PK$transitChain$kMetabolic * 60)
  }
  # At the reference weight the published value stands exactly
  expect_equal_rounded(VC_L, at(wt = WT_REF)$PK$default$v1)
})

test_that("the metabolite carries no weight term, because none was fitted", {
  # The source tested weight stepwise and retained it on the PARENT's central
  # volume only.  With the fat-free-mass switch off, enalaprilat's published
  # values must therefore stand at every weight.
  for (wt in c(51.8, 69.76, 95.6))
  {
    expect_equal_rounded(VM_L, atMet(wt = wt)$PK$default$v1)
    expect_equal_rounded(VM_L, at(wt = wt)$transitChain$metabolite$v)
  }
})

test_that("the fat-free-mass switch moves both moieties through the library factors", {
  heavy <- 110
  size <- pkSizeFactors(heavy, HT_REF, AGE_REF, "male", TRUE)
  on <- at(wt = heavy, ffm = TRUE)
  off <- at(wt = heavy)
  expect_equal_rounded(VC_L * size$volume, on$PK$default$v1)
  expect_lt(on$PK$default$v1, off$PK$default$v1)   # fat-free mass is below 110
  # The metabolite follows the same factor, from its legacy base of 1
  expect_equal_rounded(VM_L * size$volume, on$transitChain$metabolite$v)
  expect_equal_rounded(VM_L * size$volume, atMet(wt = heavy, ffm = TRUE)$PK$default$v1)
  # The rate constants stay put under the switch as well
  expect_equal_rounded(KA_H, on$PK$default$ka_PO * 60)
  expect_equal_rounded(KREN_H + KM_H, on$PK$default$k10 * 60)
})

test_that("no renal, age or sex covariate reaches either moiety", {
  # Both moieties are cleared renally and by nothing else, and the source
  # still carries no renal term: its population was 24 healthy young adults.
  # The signature must therefore not name creatinine, which is how getDrugPK
  # decides whether to pass it.
  expect_false("creatinine" %in% names(formals(enalapril)))
  expect_false("creatinine" %in% names(formals(enalaprilat)))
  base <- at()$PK$default$cl1
  expect_equal(at(creatinine = 4)$PK$default$cl1, base)
  expect_equal(at(age = 80)$PK$default$cl1, base)
  expect_equal(at(sex = "female")$PK$default$cl1, base)
  expect_equal(atMet()$PK$default$cl1,
               getDrugPK("enalaprilat", WT_REF, HT_REF, 80, "female",
                         adjustToFFM = FALSE)$PK$default$cl1)
})

test_that("the dose is entered as prescribed, with no molar or salt basis", {
  # The source dosed "two 5 mg strength of enalapril maleate" tablets, so its
  # doses are the labelled strength and F1 is a fitted scalar on it.  A molar
  # block would rescale every dose by the maleate-to-base ratio and break the
  # reproduction above.
  PK <- at()
  expect_null(PK$molar)
  expect_null(PK$osmotic)
  expect_identical(PK$Concentration.Units, "ng")
  # Linear in the dose, so a 20 mg tablet is exactly twice a 10 mg one
  ten <- runOne(10); twenty <- runOne(20)
  expect_equal(max(twenty$enalapril$wide$Plasma),
               2 * max(ten$enalapril$wide$Plasma), tolerance = 1e-9)
  expect_equal(max(twenty$enalaprilat$wide$Plasma),
               2 * max(ten$enalaprilat$wide$Plasma), tolerance = 1e-9)
})

test_that("the metabolite is folded into its own row and panel", {
  r <- runOne()
  expect_true("enalaprilat" %in% names(r))
  expect_identical(r$enalapril$metaboliteName, "enalaprilat")
  # enalaprilat was never dosed, so its whole curve comes from the fold
  expect_null(r$enalaprilat$wideOwn)
  expect_gt(max(r$enalaprilat$wide$Plasma), 0)
  # The formed metabolite brings its own peripheral compartment with it
  expect_true("V2" %in% names(r$enalaprilat$metaboliteCompartments) ||
              "V2" %in% names(r$enalapril$metaboliteCompartments))
})

test_that("both are plasma only, with no effect site, band or threshold", {
  for (PK in list(at(), atMet()))
  {
    expect_equal(PK$tPeak, 0)
    expect_equal(PK$PK$default$ke0, 0)
    expect_equal(PK$MEAC, 0)
  }
  r <- runOne()
  expect_true(all(is.na(r$enalapril$wide$`Effect Site`)))
  dd <- getDrugDefaultsGlobal()
  for (drug in c("enalapril", "enalaprilat"))
  {
    expect_equal(dd$MEAC[dd$Drug == drug], 0)
    expect_equal(dd$Lower[dd$Drug == drug], 0)
    expect_equal(dd$Upper[dd$Drug == drug], 0)
    expect_equal(dd$endCe[dd$Drug == drug], 0)
  }
  expect_identical(dd$Category[dd$Drug == "enalapril"], "Antihypertensives")
  # The metabolite is not independently dosable: no Units, no Category, so it
  # never reaches the autocomplete, the Add a dose dialog or the startup menu
  # Both fields read as NA from an empty CSV cell, which is exactly what
  # salicylate and norfluoxetine carry.
  expect_true(is.na(dd$Category[dd$Drug == "enalaprilat"]))
  expect_true(is.na(unlist(dd$Units[dd$Drug == "enalaprilat"])))
  # So it is in no startup-menu category, while the parent is
  expect_false("enalaprilat" %in% unlist(startupDrugChoices()))
  expect_true("enalapril" %in% startupDrugChoices()$Antihypertensives)
  # The parent is oral only: these parameters came from an oral study, and
  # enalaprilat injection is a separate record this library does not plot
  expect_identical(unlist(dd$Units[dd$Drug == "enalapril"]),
                   c("mg PO", "mg PO qd", "mg PO bid"))
})

test_that("a twice-daily schedule accumulates, and the references say what is plotted", {
  # The unit the CSV offers has to actually run through the scheduled
  # expansion and the new engine together.
  r <- simulateDrugsWithCovariates(
    data.frame(Drug = "enalapril", Time = 0, Dose = 10, Units = "mg PO bid",
               stringsAsFactors = FALSE),
    noEvents, WT_REF, HT_REF, AGE_REF, "male", 4320, FALSE, adjustToFFM = FALSE)
  m <- r$enalaprilat$wide
  # The metabolite's 18 h terminal half-life means a twice-daily dose
  # accumulates: the trough before the last dose is above the first trough.
  early <- approx(m$Time, m$Plasma, 720)$y
  late  <- approx(m$Time, m$Plasma, 4320 - 1)$y
  expect_gt(late, early)
  expect_match(at()$reference, "PRODRUG, PLOTTED WITH ITS ACTIVE METABOLITE",
               fixed = TRUE)
  expect_match(atMet()$reference, "THE ACTIVE MOIETY of enalapril",
               fixed = TRUE)
  expect_match(at()$reference, "Front Pediatr 2019;7:281", fixed = TRUE)
})
