# Process the dose table: simulate each drug, reusing the previous simulation
# of any drug whose inputs have not changed, then fold the active metabolites.
#
# `drugs` comes fresh from recalculatePK() on every call, so it holds PK but no
# simulation.  `cache` is the list the previous call returned (the app keeps it
# between reactive invalidations; scripts and tests pass NULL).  A drug is
# re-simulated only when something simCpCe() reads has changed: its doses, its
# PK events (for a drug with more than one PK set), its resolved PK (which
# carries the covariates), the plot length, or the recovery switch.
#
# The reused outputs are the drug's OWN simulation, saved before folding.  The
# folded series of a metabolite drug depends on its parents' doses too, so it
# is never cached: foldMetabolites() rebuilds it from the own series each time.
processdoseTable <- function (DT, ET, drugs, plotMaximum, plotRecovery, cache = NULL)
{
  for (drug in names(drugs))
  {
    tempDT <- DT[DT$Drug == drug,]
    tempET <- ET[gsub(" ","", ET$Event) %in% drugs[[drug]]$pkEvents,]

    if (nrow(tempDT) == 0) next  # e.g. a metabolite that was not given directly

    key <- simulationKey(drugs[[drug]], tempDT, tempET, plotMaximum, plotRecovery)
    sim <- cache[[drug]]$sim
    if (is.null(sim) || !identical(cache[[drug]]$simKey, key))
    {
      X <- simCpCe(
        tempDT,
        tempET,
        drugs[[drug]],
        plotMaximum,
        plotRecovery
        )
      sim <- list(
        DT                = tempDT,
        ET                = tempET,
        results           = X$results,
        equiSpace         = X$equiSpace,
        max               = X$max,
        # wideOwn is this drug's own simulation and nothing else.  wide is what
        # gets plotted, and may additionally carry metabolite formed from
        # another drug.  Keeping them apart is what makes folding idempotent.
        wideOwn           = X$wide,
        wide              = X$wide,
        metaboliteSeries  = X$metaboliteSeries,
        # The effect-site states behind Recovery, which foldMetabolites() needs
        # to solve the combined time until threshold.  Own and formed are kept
        # apart for the same reason wideOwn and wide are.
        recoveryStatesOwn        = X$recoveryStates,
        metaboliteRecoveryStates = X$metaboliteRecoveryStates,
        tci               = X$tci
      )
    }
    for (field in names(sim)) drugs[[drug]][[field]] <- sim[[field]]
    drugs[[drug]]$sim    <- sim
    drugs[[drug]]$simKey <- key
  }

  foldMetabolites(drugs, plotMaximum, plotRecovery)
}

# Everything a drug's simulation depends on, for processdoseTable() to compare
# with the cached copy.  `PK` is the drug's entry as recalculatePK() built it,
# before any simulation is attached.  Row names are dropped from the tables
# because they shift when another drug's rows are added or removed above.
simulationKey <- function(PK, DT, ET, plotMaximum, plotRecovery)
{
  rownames(DT) <- NULL
  rownames(ET) <- NULL
  list(
    PK           = PK,
    DT           = DT,
    # The event table only reaches the engine when the drug switches PK sets.
    ET           = if (length(PK$pkEvents) > 1) ET,
    plotMaximum  = plotMaximum,
    plotRecovery = isTRUE(plotRecovery)
  )
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex,
                          cyp2d6 = CYP2D6_DEFAULT,
                          adjustToFFM = TRUE,
                          osmolality = OSMOLALITY_DEFAULT,
                          creatinine = NULL) {
  #  for (idx in seq(nrow(drugDefaults))) {
  #    drug <- drugDefaults$Drug[idx]
  resolve <- function(drugs, drug) {
    idx <- which(drugDefaults$Drug==drug)
    drugs[[drug]]$Color <- drugDefaults$Color[idx]
    drugs[[drug]]$endCe <- drugDefaults$endCe[idx]
    outputComments("Getting PK for", drug)
    drugs[[drug]] <- utils::modifyList(
      drugs[[drug]],
      getDrugPK(
        drug = drug,
        weight = weight,
        height = height,
        age = age,
        sex = sex,
        drugDefaults = drugDefaults[idx, ],
        cyp2d6 = cyp2d6,
        osmolality = osmolality,
        creatinine = creatinine,
        adjustToFFM = adjustToFFM
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
  }

  drugs
}
