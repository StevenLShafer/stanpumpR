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
      }
    }
  }
  drugs
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex) {
  #  for (idx in seq(nrow(drugDefaults))) {
  #    drug <- drugDefaults$Drug[idx]
  for (drug in unique(doseTable$Drug)) {
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
        drugDefaults = drugDefaults[idx, ]
      )
    )
    drugs[[drug]]$DT <- NULL # Remove old dose table, if any
    drugs[[drug]]$equiSpace <- NULL # Ditto
  }

  drugs
}
