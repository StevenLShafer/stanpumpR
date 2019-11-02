# simulate plasma and effect site concentration from time 0 to maximum
simCpCe <- function(dose, events, PK, maximum, plotRecovery)
  {
    # dose <- doseTable
    # pK <- PK
    # maximum <- max
    # Convert all doses to base units
#  cat("in simCpCe\n")
    switch(
      PK$Concentration.Units,  # Units (per ml)
      mcg = {                  # 1 mcg/ml = 1000 mg/L
        mg_Conv  <- 1          # 1 mcg/ml = 1 mg / L
        mcg_Conv <- 1000       # 1 mcg/ml = 1,000 mcg / L
        ng_Conv  <- 1000000    # 1 mcg/ml = 1,000 mcg / L = 1,000,000 ng / L
      },
      ng = {                   # 1 ng/ml = 1000 mcg/L
        mg_Conv  <- .001       # 1 ng/ml = 0.001 mcg/ml = 0.001 mg/L80
        mcg_Conv <- 1        # Native unit
        ng_Conv  <- 1000
      }
    )

    use <- grep("mg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / mg_Conv
    use <- grep("mcg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / mcg_Conv
    use <- grep("ng",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / ng_Conv

    # Convert dose per kg  to absolute dose
    use <- grep("kg",dose$Units)
    dose$Dose[use] <- dose$Dose[use] * PK$weight

    # Convert dose per hour to dose per minute
    use <- grep("hr",dose$Units)
    dose$Dose[use] <- dose$Dose[use] / 60

#    cat("Completed conversion of dose units\n")

    # Identify bolus doses
    dose$Bolus <- !(grepl("min", dose$Units) |
                      grepl("hr", dose$Units) |
                      grepl("PO", dose$Units) |
                      grepl("IM", dose$Units) |
                      grepl("IN", dose$Units))

#    cat("Bolus Doses identified\n")

    # Identify PO doses
    dose$PO <- grepl("PO", dose$Units)
    dose$IM <- grepl("IM", dose$Units)
    dose$IN <- grepl("IN", dose$Units)

    # cat("Other Doses identified\n")
    # cat("starting on events\n")
#    print(str(events))
    events <- events[,c(1,2)]
#    cat("Did that process OK?\n")

    pkSets <- PK$PK
    pkEvents <- PK$pkEvents

#    cat("All Events:\n")
#    print(events)

    events$Event <- gsub(" ","", events$Event)
    events <- events[events$Event %in% pkEvents,]
    # cat("Retained Events:\n")
    # print(events)
    if (length(pkEvents) == 1 | nrow(events) == 0)
    {
      if (sum(dose$PO) + sum(dose$IM) + sum(dose$IN) == 0)
      {
 #       cat("calling advancedClosedForm0\n")

        results <- advanceClosedForm0(dose,pkSets[[1]], maximum, plotRecovery, PK$emerge)
      } else {
        results <- advanceClosedFormPO_IM_IN(dose,pkSets[[1]], maximum, plotRecovery, PK$emerge)
      }
    } else {
      # Process Events
      defaultEvent <- data.frame(
        Time = 0,
        Event = "default",
        stringsAsFactors = FALSE
      )
      if (events$Time[1] > 0)
        events <- rbind(defaultEvent,events)
      events <- events[events$Time < maximum,]
      events <- rbind(events, events[nrow(events),])
      events$Time[nrow(events)] <- maximum
      results <- advanceClosedForm1(dose, events, pkSets, maximum, plotRecovery, PK$emerge)
    }

  names(results) <- c("Time", "Plasma","Effect Site", "Recovery")
  # print(str(results))
  maxCp <- max(results$Plasma)
  maxCe <- max(results$"Effect Site")
  if (maxCp == 0)
  {
    results$CpNormCp <- 0
    results$CeNormCp <- 0
    results$CpNormCe <- 0
    results$CeNormCe <- 0

  } else {
    results$CpNormCp <- results$Plasma        / maxCp * 100
    results$CeNormCp <- results$"Effect Site" / maxCp * 100
    results$CpNormCe <- results$Plasma        / maxCe * 100
    results$CeNormCe <- results$"Effect Site" / maxCe * 100
  }

  # Calculate equispaced output
  xout <- seq(from = 0, to = maximum, length.out = resolution)
  equiSpace <- data.frame(
    Drug = PK$drug,
    Time = xout,
    Ce = approx(
      x = results$Time,
      y = results$"Effect Site",
      xout = xout
      )$y,
    Time = xout,
    Recovery = approx(
      x = results$Time,
      y = results$Recovery,
      xout = xout
    )$y,
    stringsAsFactors = FALSE
    )

  equiSpace$Ce[1] <- 0  # Approx tends to make it a very small negative number
  if (PK$MEAC == 0)
  {
    equiSpace$MEAC <- 0
  } else {
    equiSpace$MEAC <- equiSpace$Ce / PK$MEAC * 100
  }
  max <- data.frame(
    Drug = PK$drug,
    Recovery = max(results$Recovery),
    Cp = max(results$Plasma),
    Ce = max(results$"Effect Site"),
    stringsAsFactors = FALSE
    )
#  print(str(max))
  results$Recovery <- NULL
  results <- results %>% gather("Site","Y",-Time)
  results$Drug <- PK$drug
  results <- results[,c(4,1,2,3)]
#  print(results)
  # Structure of results
  # Four columns: Drug, Time, Site, Y
  # 7 Sites: Plasma, Effect Site, CpNormCp, CeNormCp, CpNormCE, CeNormCe, and MEAC
  # These will be subset in simulation plot as needed.

  return(
    list(
      results = results,
      equiSpace = equiSpace,
      max =   max
    )
  )
}
