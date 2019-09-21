# process the Dose Table
# including removing simulations of drugs no longer mentioned
# and simulating any drugs for which there has been a change in the 
# table.
# If there has been no changed in the dose table for a specific drug
# then it is skipped.
processdoseTable <- function (DT, ET, drugs, plotMaximum, prior, plotRecovery)
{
  # Now, process dose table for each drug
  for (i in 1:length(drugs))
  {
    cat("in processdoseTable, processing ",names(drugs)[i],"\n")
    tempDT <- DT[DT$Drug == names(drugs)[i],]
    cat("ET$Event",ET$Event,"\n")
    cat("drugs[[i]]$pkEvents",drugs[[i]]$pkEvents,"\n")
    tempET <- ET[gsub(" ","", ET$Event) %in% drugs[[i]]$pkEvents,]
    
    if (!sameTable(tempDT, drugs[[i]]$DT) |
        plotMaximum != prior$plotMaximum
        | (length(drugs[[i]]$pkEvents) > 1 & 
           !sameTable(drugs[[i]]$ET, tempET)
        )
      )
    {
      if (nrow(tempDT) == 0 ) # Delete anything that should be deleted
      {
        drugs[[i]]$DT        <- NULL
        drugs[[i]]$ET        <- NULL
        drugs[[i]]$CpCe      <- NULL
        drugs[[i]]$equiSpace <- NULL
        drugs[[i]]$maxCp     <- 1
        drugs[[i]]$maxCe     <- 1
      } else {
        X <- simCpCe(
          tempDT,
          tempET,
          drugs[[i]],
          plotMaximum,
          plotRecovery
          )
        drugs[[i]]$DT                <- tempDT
        drugs[[i]]$ET                <- tempET
        drugs[[i]]$results           <- X$results
        drugs[[i]]$equiSpace         <- X$equiSpace
        drugs[[i]]$max               <- X$max
        # cat("from process Dose Table: ",drugs[[i]]$maxCp, drugs[[i]]$maxCe, "\n")
      }
    }
  }
  return(list(drugs = drugs))
}



