# process the Dose Table
# including removing simulations of drugs no longer mentioned
# and simulating any drugs for which there has been a change in the
# table.
# If there has been no changed in the dose table for a specific drug
# then it is skipped.
processdoseTable <- function (DT, ET, drugs, plotMaximum, prior, plotRecovery)
{
  # Now, process dose table for each drug
  # cat("Entering processdoseTable()\n")
  drugList <- names(drugs)
  for (i in 1:length(drugList))
  {
    drug <- drugList[i]
    # cat("drug = ",drug,"\n")
    tempDT <- DT[DT$Drug == drug,]
    # cat("Structure of tempDT\n")
    print(str(tempDT))
    # cat("ET$Event",ET$Event,"\n")
    # cat("drugs[[drug]]$pkEvents",drugs[[drug]]$pkEvents,"\n")
    tempET <- ET[gsub(" ","", ET$Event) %in% drugs[[drug]]$pkEvents,]
    # cat("Structure of tempET\n")
    # print(str(tempET))

    if (!isTRUE(all_equal(tempDT, drugs[[drug]]$DT)) |
        plotMaximum != prior$plotMaximum
        | (length(drugs[[drug]]$pkEvents) > 1 &
           !isTRUE(all_equal(drugs[[drug]]$ET, tempET)) |
        (plotRecovery & !prior$plotRecovery)
        )
      )
    {
      # cat("Seems not\n")
      if (nrow(tempDT) == 0 ) # Delete anything that should be deleted
      {
        # cat("starting to set everything to NULL\n")
        drugs[[drug]]$DT        <- NULL
        drugs[[drug]]$ET        <- NULL
        drugs[[drug]]$results   <- NULL
        drugs[[drug]]$equiSpace <- NULL
        drugs[[drug]]$max       <- NULL
        # cat("everything to NULL\n")
      } else {
        # cat("calling simCpCe\n")
        X <- simCpCe(
          tempDT,
          tempET,
          drugs[[drug]],
          plotMaximum,
          plotRecovery
          )
        # cat("back from simCpCe\n")
        drugs[[drug]]$DT                <- tempDT
        drugs[[drug]]$ET                <- tempET
        drugs[[drug]]$results           <- X$results
        drugs[[drug]]$equiSpace         <- X$equiSpace
        drugs[[drug]]$max               <- X$max
        # cat("drugs[] updated\n")
      }
    }
  }
  # cat("Exiting processdoseTable()\n")
  return()
}



