# process the Dose Table
# including removing simulations of drugs no longer mentioned
# and simulating any drugs for which there has been a change in the
# table.
# If there has been no changed in the dose table for a specific drug
# then it is skipped.
processdoseTable <- function (DT, ET, drugs, plotMaximum, plotRecovery)
{
  # Now, process dose table for each drug
  drugList <- names(drugs)
  for (i in 1:length(drugList))
  {
    drug <- drugList[i]
    tempDT <- DT[DT$Drug == drug,]
    tempET <- ET[gsub(" ","", ET$Event) %in% drugs[[drug]]$pkEvents,]

    if (!identical(tempDT, drugs[[drug]]$DT) |
         (length(drugs[[drug]]$pkEvents) > 1 &
          !identical(drugs[[drug]]$ET, tempET))
      )
    {
      if (nrow(tempDT) == 0 ) # Delete anything that should be deleted
      {
        drugs[[drug]]$DT        <- NULL
        drugs[[drug]]$ET        <- NULL
        drugs[[drug]]$results   <- NULL
        drugs[[drug]]$equiSpace <- NULL
        drugs[[drug]]$max       <- NULL
        # A drug no longer in the dose table forms no metabolite, and keeping
        # a stale contribution would go on feeding the metabolite's row.
        drugs[[drug]]$wideOwn         <- NULL
        drugs[[drug]]$wide            <- NULL
        drugs[[drug]]$metaboliteSeries <- NULL
        drugs[[drug]]$formedFrom      <- NULL
      } else {
        X <- simCpCe(
          tempDT,
          tempET,
          drugs[[drug]],
          plotMaximum,
          plotRecovery
          )
        drugs[[drug]]$DT                <- tempDT
        drugs[[drug]]$ET                <- tempET
        drugs[[drug]]$results           <- X$results
        drugs[[drug]]$equiSpace         <- X$equiSpace
        drugs[[drug]]$max               <- X$max
        # wideOwn is this drug's own simulation and nothing else.  wide is what
        # gets plotted, and may additionally carry metabolite formed from
        # another drug.  Keeping them apart is what makes folding idempotent.
        drugs[[drug]]$wideOwn           <- X$wide
        drugs[[drug]]$wide              <- X$wide
        drugs[[drug]]$metaboliteSeries  <- X$metaboliteSeries
        drugs[[drug]]$formedFrom        <- NULL
      }
    }
  }

  foldMetabolites(drugs, plotMaximum, plotRecovery)
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex,
                          cyp2d6 = CYP2D6_DEFAULT) {
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
        cyp2d6 = cyp2d6
      )
    )
    drugs[[drug]]$DT <- NULL # Remove old dose table, if any
    drugs[[drug]]$equiSpace <- NULL # Ditto
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
    # whole curve will come from the fold.
    drugs[[target]]$DT      <- NULL
    drugs[[target]]$wideOwn <- NULL
    drugs[[target]]$wide    <- NULL
  }

  drugs
}
