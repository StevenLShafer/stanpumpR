# Process the dose table: simulate each drug, reusing the previous simulation
# of any drug whose inputs have not changed, then fold the active metabolites.
#
# `drugs` comes fresh from recalculatePK() on every call, so it holds PK but no
# simulation.  `cache` is the list the previous call returned (the app keeps it
# between reactive invalidations; scripts and tests pass NULL).  A drug is
# re-simulated only when something simCpCe() reads has changed: its doses, its
# PK events (for a drug with more than one PK set), its resolved PK (which
# carries the covariates), the plot length, the start of the window drawn, or
# the recovery switch.
#
# `plotMinimum` is where the plot's axis starts (Min time).  Every drug is
# still simulated from 0; inside the window the time lines are as fine as a
# plot of the window's length (withGridWindow(), R/simulationTimeGrid.R).
#
# The reused outputs are the drug's OWN simulation, saved before folding.  The
# folded series of a metabolite drug depends on its parents' doses too, so it
# is never cached: foldMetabolites() rebuilds it from the own series each time.
#
# A companion entry (an alternative model of a library drug, with
# $companionOf naming the drug; R/alternativeModels.R) takes its parent's dose
# rows and is cached under its own name, like any other drug.
processdoseTable <- function (DT, ET, drugs, plotMaximum, plotRecovery, cache = NULL,
                              plotMinimum = 0, antiseizureInteractions = FALSE)
{
  # Every time line inside is refined across the window drawn (Min time).
  withGridWindow(plotMinimum, {
    for (drug in names(drugs))
    {
      # A companion entry (an alternative model of a library drug; see
      # R/alternativeModels.R) is simulated on its parent's dose rows, and never
      # with the time until threshold, which stays on the library's own model.
      parent <- drugs[[drug]]$companionOf
      source <- if (is.null(parent)) drug else parent
      recovery <- if (is.null(parent)) plotRecovery else FALSE

      tempDT <- DT[DT$Drug == source,]
      tempET <- ET[gsub(" ","", ET$Event) %in% drugs[[drug]]$pkEvents,]

      if (nrow(tempDT) == 0) next  # e.g. a metabolite that was not given directly

      key <- simulationKey(drugs[[drug]], tempDT, tempET, plotMaximum, recovery, plotMinimum)
      sim <- cache[[drug]]$sim
      if (is.null(sim) || !identical(cache[[drug]]$simKey, key))
      {
        X <- simCpCe(
          tempDT,
          tempET,
          drugs[[drug]],
          plotMaximum,
          recovery
          )
        sim <- simulationFields(X, tempDT, tempET)
      }
      for (field in names(sim)) drugs[[drug]][[field]] <- sim[[field]]
      drugs[[drug]]$sim    <- sim
      drugs[[drug]]$simKey <- key
    }

    # The pharmacokinetic interactions among the antiseizure drugs (Graph
    # Options), which re-simulate the drugs they change on top of their own
    # simulations; see R/antiseizureInteractions.R.
    if (isTRUE(antiseizureInteractions))
      drugs <- applyAntiseizureInteractions(drugs, DT, ET, plotMaximum,
                                            plotRecovery, cache = cache)

    foldMetabolites(drugs, plotMaximum, plotRecovery)
  })
}

# Everything a drug's simulation depends on, for processdoseTable() to compare
# with the cached copy.  `PK` is the drug's entry as recalculatePK() built it,
# before any simulation is attached.  Row names are dropped from the tables
# because they shift when another drug's rows are added or removed above.
simulationKey <- function(PK, DT, ET, plotMaximum, plotRecovery, plotMinimum = 0)
{
  rownames(DT) <- NULL
  rownames(ET) <- NULL
  list(
    PK           = PK,
    DT           = DT,
    # The event table only reaches the engine when the drug switches PK sets.
    ET           = if (length(PK$pkEvents) > 1) ET,
    plotMaximum  = plotMaximum,
    plotMinimum  = plotMinimum,
    plotRecovery = isTRUE(plotRecovery)
  )
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex,
                          cyp2d6 = CYP2D6_DEFAULT,
                          adjustToFFM = TRUE,
                          osmolality = OSMOLALITY_DEFAULT,
                          creatinine = NULL,
                          cyp2c19 = CYP2C19_DEFAULT,
                          cyp2c9 = CYP2C9_DEFAULT,
                          vkorc1 = VKORC1_DEFAULT,
                          cyp4f2 = CYP4F2_DEFAULT,
                          cyp2b6 = CYP2B6_DEFAULT,
                          pregnant = FALSE,
                          alternativeModels = FALSE,
                          antiseizureInteractions = FALSE,
                          gestationalAge = NULL,
                          opioidTolerance = OPIOID_TOLERANCE_DEFAULT) {
  # The Patient Profile's Opioid tolerance raises every opioid's MEAC by its
  # factor (OPIOID_TOLERANCE_FACTORS), here, where each drug's MEAC enters
  # its entry, so everything that reads it downstream sees the tolerant MEAC.
  # For an opioid with a MEAC, the shaded band (multiples of the MEAC) and
  # the time-until-threshold value (the MEAC itself, for most) rise with it
  # (Steven L. Shafer, 2026-10-28); an opioid without one (codeine, whose
  # band is only what ordinary doses reach) is left alone.  The session's
  # drugDefaults keeps the naive values, which the propofol-opioid
  # interaction uses to convert a % MEAC to remifentanil-equivalents, so
  # tolerance lowers that interaction too.
  toleranceFactor <- OPIOID_TOLERANCE_FACTORS[[match.arg(opioidTolerance, OPIOID_TOLERANCE_VALUES)]]
  #  for (idx in seq(nrow(drugDefaults))) {
  #    drug <- drugDefaults$Drug[idx]
  resolve <- function(drugs, drug) {
    idx <- which(drugDefaults$Drug==drug)
    row <- drugDefaults[idx, ]
    if (isTRUE(row$MEAC > 0))
      for (column in c("MEAC", "Lower", "Typical", "Upper", "endCe"))
        row[[column]] <- row[[column]] * toleranceFactor
    drugs[[drug]]$Color <- drugDefaults$Color[idx]
    # endCe, the time-until-threshold concentration, comes with the PK:
    # getDrugPK() reads it from this session's drugDefaults row, with any
    # Drug Thresholds edit.
    outputComments("Getting PK for", drug)
    drugs[[drug]] <- utils::modifyList(
      drugs[[drug]],
      getDrugPK(
        drug = drug,
        weight = weight,
        height = height,
        age = age,
        sex = sex,
        drugDefaults = row,
        cyp2d6 = cyp2d6,
        cyp2c19 = cyp2c19,
        cyp2c9 = cyp2c9,
        vkorc1 = vkorc1,
        cyp4f2 = cyp4f2,
        cyp2b6 = cyp2b6,
        osmolality = osmolality,
        creatinine = creatinine,
        pregnant = pregnant,
        adjustToFFM = adjustToFFM,
        gestationalAge = gestationalAge
      )
    )
    drugs
  }

  dosed <- unique(doseTable$Drug)
  for (drug in dosed) drugs <- resolve(drugs, drug)

  # A drug that forms an active metabolite needs the metabolite's own row to
  # exist even when the metabolite was never given directly: giving codeine has
  # to show the morphine it produces.  Resolve those too, so foldMetabolites()
  # has somewhere to put the contribution.
  for (drug in dosed)
  {
    target <- drugs[[drug]]$metaboliteName
    if (is.null(target) || !nzchar(target)) next
    if (target %in% dosed) next
    if (!target %in% drugDefaults$Drug) next
    drugs <- resolve(drugs, target)
    # It carries no doses of its own, so its own simulation is empty and its
    # whole curve, and its whole time until threshold, will come from the fold.
    drugs[[target]]$DT      <- NULL
    drugs[[target]]$wideOwn <- NULL
    drugs[[target]]$wide    <- NULL
    drugs[[target]]$recoveryStatesOwn        <- NULL
    drugs[[target]]$metaboliteRecoveryStates <- NULL
    drugs[[target]]$compartmentsOwn <- NULL
    drugs[[target]]$compartments    <- NULL
  }

  # The alternative models (R/alternativeModels.R): for each Graph Options
  # companion switch that is on (the Minto / Schnider box, the Hamberg box),
  # each dosed drug in that group gets a companion entry, named "<drug>
  # (<model>)", which processdoseTable() simulates on the parent's doses and
  # simulationPlot() draws in the parent's panel.  Appended after every library
  # drug, so that a panel's first series is always the library's own.  A model
  # that is not defined for this patient (a negative clearance, say) gives no
  # entry; alternativeModelProblems() says why.  `alternativeModels` is the set
  # of enabled group keys; TRUE means every group (for scripts and tests).
  enabledGroups <- if (isTRUE(alternativeModels)) names(COMPANION_MODEL_GROUPS)
                   else if (is.character(alternativeModels)) alternativeModels
                   else character(0)
  for (drug in intersect(dosed, ALTERNATIVE_MODEL_DRUGS))
  {
    if (!companionModelGroup(drug) %in% enabledGroups) next
    idx <- which(drugDefaults$Drug == drug)[1]
    entry <- alternativeModelEntry(
      drug, weight, height, age, sex, drugDefaults[idx, ], adjustToFFM,
      pregnant = pregnant, cyp2c9 = cyp2c9, vkorc1 = vkorc1
    )
    if (is.null(entry)) next
    outputComments("Getting PK for", entry$drug)
    drugs[[entry$drug]] <- entry
  }

  # With the antiseizure interactions on, each drug they change is drawn
  # beside its own curve without them, as a companion entry
  # (R/antiseizureInteractions.R).
  if (isTRUE(antiseizureInteractions))
    drugs <- addAntiseizureReferenceEntries(drugs, dosed)

  drugs
}
