suggest <- function(
    targetDrug,
    targetTable,
    endTime,
    drugs,
    drugList,
    eventTable,
    referenceTime)
{
  targetTable$Time <- as.character(targetTable$Time)
  targetTable$Target <- as.character(targetTable$Target)
  # print(str(targetTable))
  # Process and clean up targetTable
  for (i in seq_len(nrow(targetTable)))
  {
    targetTable$Time[i] <- validateTime(targetTable$Time[i])
    targetTable$Target[i] <- validateDose(targetTable$Target[i]) # should work for target too
  }
  # cat("After validating time and dose\n")
  # print(str(targetTable))
  targetTable$Target    <- as.numeric(targetTable$Target)
  targetTable$Time    <- as.character(targetTable$Time)  # Stored as factors... Arrgh.....
  targetTable <- targetTable[targetTable$Time!=0  & targetTable$Target!=0, ]
  outputComments("structure of targetTable")
  outputComments(targetTable)
  # # Remove blank values of targetTable
  if (referenceTime == "none")
  {
    targetTable$Time <- as.numeric(targetTable$Time)
    endTime <- as.numeric(endTime)
  } else {
    targetTable$Time    <- clockTimeToDelta(referenceTime, targetTable$Time)
    endTime <- clockTimeToDelta(referenceTime, endTime)
  }
  outputComments("End Time =", endTime)
  outputComments("Structure of targetTable after processing time")
  outputComments(targetTable)
  targetTable <- targetTable[
    !is.na(targetTable$Target) &
      !is.na(targetTable$Time), ]
  targetTable <- targetTable[targetTable$Time < endTime,]
  if (nrow(targetTable) == 0)
  {
    outputComments("Target table is empty")
    return(NULL)
  }
  targetTable <- targetTable[order(targetTable$Time), ]
  # Remove decreasing
  if (nrow(targetTable) > 1)
  {
    for (i in 2:nrow(targetTable))
    {
      targetTable$Target[i] <- max(targetTable$Target[i], targetTable$Target[i-1])
    }
  }
  # Calculate time offset (table must start at time 0)
  offsetTime <- min(targetTable$Time)
  targetTable$Time <- targetTable$Time - offsetTime
  endTime <- endTime - offsetTime

  # print(str(targetTable))
  #
  outputComments("Ready to search for the target dose")
  drug <- drugList[which(targetDrug == drugList)]

  outputComments("Drug is", drug)

  infusionT1 <- round(c(targetTable$Time + drugs[[drug]]$tPeak, endTime), 0)
  infusionT2 <- round(infusionT1[1:(length(infusionT1)-1)] +
                        (infusionT1[2:(length(infusionT1))] - infusionT1[1:(length(infusionT1)-1)]) / 5 , 0)

  testTable <- data.frame(
    Time = c(targetTable$Time[],infusionT1, infusionT2),
    Dose = 1,
    Units = c(rep(drugs[[drug]]$Bolus.Units, nrow(targetTable)),
              rep(drugs[[drug]]$Infusion.Units,nrow(targetTable)*2+1))
  )
  testTable <- testTable[order(testTable$Time),]
  testTable$Dose[nrow(testTable)] <- 0

  outputComments("Structure of testTable")
  outputComments(testTable)
  outputComments("In Target, ET =")
  outputComments(eventTable)
  results <- simCpCe(
    testTable,
    eventTable,
    drugs[[drug]],
    endTime,
    plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
  # plot <- ggplot(results,aes(x=Time, y=Ce)) +
  #   geom_line() +
  #   labs(title="First pass")
  # print(plot)


  # Now calculate the infusion rate that would based on the end infusion concentrations
  USE <- 1:(nrow(testTable)-1)
  for (x in 1:10)
  {
    results <- simCpCe(testTable, eventTable, drugs[[drug]] ,endTime, plotRecovery = FALSE)$equiSpace[,c("Time","Ce")]
    for (i in USE)
    {
      testTable$resultTime[i] <- max(results$Time[results$Time < testTable$Time[i+1]])
      t <- max(results$Time[results$Time < testTable$Time[i+1]])
      Ce <- results$Ce[results$Time == t]
      t <- max(targetTable$Time[targetTable$Time <= testTable$Time[i]])
      Target <- targetTable$Target[targetTable$Time == t]

      testTable$Dose[i] <- testTable$Dose[i] / Ce * Target
    }
  }
  # plot <- ggplot(results,aes(x=Time, y=Ce)) +
  #  geom_line() +
  #  labs(title="After 10 iterations")
  # print(plot)

  # Now set up for nlm
  obj <- function(Dose, Time, Units, PK, maximum)
  {
    Dose[Dose < 0] <- 0  # Prevent negative doses
    DT <- data.frame(
      Time = Time,
      Dose = Dose,
      Units = Units
    )
    ce <- simCpCe(
      DT,
      eventTable, # TODO why is the eventTable in simCpCe?
      PK,
      endTime,
      plotRecovery = FALSE
    )$equiSpace[,c("Time","Ce")]

    target <- sapply(
      results$Time,
      function(x)
      {
        targetTable$Target[
          which(
            targetTable$Time == max(
              targetTable$Time[targetTable$Time <= x]
            )
          )
        ]
      }
    )
    return(sum((ce-target)^2))
  }

  outputComments("About to run nlm")
  testTable$Dose <- stats::nlm(
    obj,
    testTable$Dose,
    testTable$Time,
    testTable$Units,
    drugs[[drug]],
    endTime
  )$estimate

  testTable$Dose[testTable$Dose < 0] <- 0
  testTable$Dose <- signif(testTable$Dose,3)
  results <- simCpCe(
    testTable,
    eventTable,
    drugs[[drug]],
    endTime,
    plotRecovery = FALSE
  )$equiSpace[,c("Time","Ce")]

  # Interim plot
  # plot <- ggplot(results,aes(x=Time, y=Ce)) +
  #   geom_line() +
  #   labs(title="After nlm")
  # print(plot)

  testTable$Drug <- targetDrug
  testTable$Time <- testTable$Time + offsetTime
  # print(str(testTable))

  testTable

  # plot <- ggplot(results,aes(x=Time, y=Ce)) +
  #         geom_line() +
  #         labs(title="After nlm")
}
