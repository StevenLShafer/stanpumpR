# Simulation Plot
simulationPlot <- function(
  drugs,
  events,
  drugDefaults,
  eventDefaults,
  xBreaks = c(0:6*10),
  xLabels = c(0:6*10),
  xAxisLabel = "Time (Minutes)",
  plasmaLinetype = "solid",
  effectsiteLinetype = "dashed",
  normalization = c(NORMALIZE_NONE),
  plotMEAC = FALSE,
  plotInteraction = FALSE,
  plotCost = FALSE,
  plotEvents = FALSE,
  plotRecovery = FALSE,
  typical = c("Mid"),
  logY = FALSE,
  yAxisHeight = 150,
  width = 800
  )
{
  # Notes on what happens below
  # The time courses for ggplot are held in plotResults
  # "Drug","Time","Y","Site","Wrap","Label"
  # Drug determines the color
  # Y is the value plotted
  # Site determines the linetype
  # Wrap determines the facet that the data will be plotted
  # Label is used for events. Otherwise, it is blank.

  # The following objects are created, and the first three are returned at the end of this function
  # A: plotObject
  # B: plotResults
  # C: allResults
  # D: plotTable
  # E: allEquispace

  outputComments("Entering simulationPlot", level = DEBUG_LEVEL_VERBOSE)

  # Step D1: create plotTable from `drugs`

  plotTable <- as.data.frame(
    cbind(
      purrr::map_chr(drugs, "drug"),
      purrr::map_chr(drugs, "Color"),
      purrr::map_chr(drugs, "Concentration.Units"),
      purrr::map_chr(drugs, \(x) as.character(purrr::pluck(x, "typical"))),
      purrr::map_chr(drugs, \(x) as.character(purrr::pluck(x, "lowerTypical"))),
      purrr::map_chr(drugs, \(x) as.character(purrr::pluck(x, "upperTypical"))),
      purrr::map_chr(drugs, \(x) as.character(purrr::pluck(x, "MEAC"))),
      purrr::map_chr(drugs, \(x) as.character(purrr::pluck(x, "endCe")))
      )
  )

  names(plotTable) <- c("Drug", "drugColor", "Concentration.Units",
                        "typical", "lowerTypical", "upperTypical",
                        "MEAC", "endCe")

  outputComments("plotTable created", level = DEBUG_LEVEL_VERBOSE)

  # Step C1: create allResults from `drugs`

  allResults <- purrr::map_dfr(drugs, "results")

  # Four columns: Drug, Time, Site, Y
  # 8 Sites: Plasma, Effect Site, CpNormCp, CeNormCp, CpNormCe, CeNormCe

  # Step D2: extend plotTable

  plotTable <- plotTable[plotTable$Drug %in% allResults$Drug,]
  plotTable$typical <- as.numeric(plotTable$typical)
  plotTable$lowerTypical <- as.numeric(plotTable$lowerTypical)
  plotTable$upperTypical <- as.numeric(plotTable$upperTypical)
  plotTable$MEAC  <- as.numeric(plotTable$MEAC)
  plotTable$endCe <- as.numeric(plotTable$endCe)
  plotTable$alpha <- 0.2
  allMax <- purrr::map_dfr(drugs, "max")
  allMax <- allMax[allMax$Drug %in% plotTable$Drug,]

  CROWS <- match(plotTable$Drug, allMax$Drug)
  plotTable$MaxCp <- allMax$Cp[CROWS]
  plotTable$MaxCe <- allMax$Ce[CROWS]
  plotTable$MaxRecovery <- allMax$Recovery[CROWS]

  # Step E1: create allEquispace

  allEquispace  <- purrr::map_dfr(drugs, "equiSpace")
  allEquispace <- allEquispace[allEquispace$Drug %in% plotTable$Drug,]

  # Step C2: check and clean and extend allResults

  if (nrow(allResults) == 0)
  {
    message("Returning Null, nrow(allResults) == 0")
    return(NULL)
  }

  # Remove unnecessary rows from allResults and process normalization
  switch(
    normalization,
    "none" = {
      allResults <- allResults[allResults$Site != "CpNormCp" &
                               allResults$Site != "CeNormCp" &
                               allResults$Site != "CpNormCe" &
                               allResults$Site != "CeNormCe",]
    },
    "Peak plasma" = {
      allResults <- allResults[allResults$Site == "CpNormCp" | allResults$Site == "CeNormCp",]
      allResults$Site[allResults$Site == "CpNormCp"] <- "Plasma"
      allResults$Site[allResults$Site == "CeNormCp"] <- "Effect Site"
      plotRecovery <- FALSE
      plotMEAC <- FALSE
      plotInteraction <- FALSE
    },
    "Peak effect site" = {
      allResults <- allResults[allResults$Site == "CpNormCe" | allResults$Site == "CeNormCe",]
      allResults$Site[allResults$Site == "CpNormCe"] <- "Plasma"
      allResults$Site[allResults$Site == "CeNormCe"] <- "Effect Site"
      plotRecovery <- FALSE
      plotMEAC <- FALSE
      plotInteraction <- FALSE
    }
  )

  if (!plotRecovery)
    allResults <- allResults[allResults$Site != "Recovery",]

  # Step D3: extend plotTable

  if (plasmaLinetype == "blank")
  {
    #   cat ("removing plasma concentrations\n")
    # Hiding the plasma line must not hide a drug entirely.
    #
    # A drug with no effect site -- a prodrug such as codeine or tramadol, or
    # one whose potency has not been supplied yet -- carries NA in its
    # effect-site column, and those rows are dropped further down.  Removing
    # its plasma rows here as well would leave it with nothing at all to
    # draw: an empty panel with axes and a typical-range band and no curve.
    # Since the default setting is a blank plasma line, that is what such a
    # drug looked like out of the box.
    #
    # So plasma is removed only from the drugs that have an effect site to
    # show instead.  Note the NA test: the effect-site rows still exist at
    # this point and are dropped later, so their mere presence does not mean
    # the drug has anything to plot.
    # The exception applies only when the effect site is actually being
    # shown.  Blanking BOTH lines is a deliberate request for an empty plot,
    # and forcing plasma back on there would override an explicit choice
    # rather than rescue an accidental one.
    withEffectSite <- if (effectsiteLinetype == "blank") {
      unique(allResults$Drug)
    } else {
      unique(
        allResults$Drug[allResults$Site == "Effect Site" & !is.na(allResults$Y)]
      )
    }
    allResults <- allResults[
      allResults$Site != "Plasma" | !(allResults$Drug %in% withEffectSite),
    ]
    if (any(allResults$Site == "Plasma"))
    {
      # Whatever plasma survived is the only curve those drugs have, so it is
      # drawn rather than blanked, and keeps its own legend entry.
      plasmaLinetype <- "solid"
    } else {
      plasmaLinetype <- NULL
      plotTable$MaxCp <- 0
    }
  }
  if (effectsiteLinetype == "blank")
  {
    allResults <- allResults[allResults$Site != "Effect Site",]
    effectsiteLinetype <- NULL
    plotTable$MaxCe <- 0
  }

  # Both series can be blanked, which is a legitimate request for an empty
  # plot.  Nothing then survives the two filters above, and the assignments
  # below cannot write a length-one value into a zero-row data frame.
  #
  # The zero-row guard at Step C2 cannot catch this: it runs before the
  # linetype filters, when the rows still exist.  Pre-existing rather than new
  # -- master errors here identically -- but nothing exercised the path until
  # a drug appeared that has only one of the two series.
  #
  # Same answer as that earlier guard: nothing to plot is NULL, which
  # output$PlotSimulation already handles.
  if (nrow(allResults) == 0)
  {
    message("Returning Null, both linetypes blank")
    return(NULL)
  }

  allResults$Wrap <- ""
  allResults$Label <- ""

  minimum <- min(xBreaks)
  maximum <- max(xBreaks)
  plotTable$xmin <- minimum
  plotTable$xmax <- maximum

  nplotTable <- nrow(plotTable)
  addPlots <- plotMEAC + plotInteraction + plotCost + plotEvents

  # Step D4: finish plotTable apart from extensions below

  # The panel title.  "MAC" is the series' internal name; what is plotted is
  # the alveolar concentration as a multiple of the (age-adjusted) MAC, which
  # changes through the case while MAC itself does not, so the panel says so.
  panelName <- plotTable$Drug
  panelName[panelName == "MAC"] <- "MAC equivalents"

  switch(
    normalization,
    "none" = {
      # Intravenous concentrations are per millilitre; the inhaled gases are a
      # percentage of one atmosphere and MAC equivalents are dimensionless, so
      # neither takes the "/ml" suffix.
      unitText <- paste0(plotTable$Concentration.Units, "/ml")
      gasRow <- isGasSeries(plotTable$Drug)
      unitText[gasRow] <- plotTable$Concentration.Units[gasRow]
      # An osmotic agent is plotted as serum osmolality (drugs_mannitol.R).
      unitText[plotTable$Concentration.Units == "mOsm"] <- "mOsm/kg"
      plotTable$Wrap <- paste0(panelName, "\n(", unitText, ")")
      plotTable$ymin <- plotTable$lowerTypical
      plotTable$ymax <- plotTable$upperTypical
      plotTable$y    <- plotTable$typical
    },
    "Peak plasma" = {
      plotTable$Wrap <- paste0(
                          panelName,
                          "\n(% Peak Cp)")
      plotTable$ymin <- 0
      plotTable$ymax <- 0
      plotTable$y    <- 0
    },
    "Peak effect site" = {
      plotTable$Wrap <- paste0(
                          panelName,
                          "\n(% Peak Ce)")
      plotTable$ymin <- 0
      plotTable$ymax <- 0
      plotTable$y    <- 0
    }
  )

  # Step C3: finish allResults

  allResults$Wrap <- plotTable$Wrap[match(allResults$Drug, plotTable$Drug)]

  # Step B1: create plotResults

  plotResults <- allResults[,c("Drug","Time","Y","Site","Wrap","Label")]

  # Step B2 and D5: add MEAC and Interaction

  if (plotMEAC | plotInteraction)
  {
  # Need this table both for plotMEAC and for Interaction
    X <- allEquispace %>%
      dplyr::group_by(Time) %>%
      dplyr::summarize(SUM = mean(MEAC)*dplyr::n())
    totalMEAC <- data.frame(
      Drug = "total opioid",
      Time = X$Time,
      Y = X$SUM,
      Site = "Effect Site",
      Wrap = PLOT_NAME_MEAC,
      Label = ""
      )
    opioids <- plotTable$Drug[plotTable$MEAC > 0]
    # MEAC plot
    if (length(opioids) > 0 & plotMEAC)
    {
      resultsMEAC <- allEquispace[!is.na(allEquispace$MEAC),c("Drug","Time","MEAC")]
      names(resultsMEAC)[3] <- "Y"
      resultsMEAC$Site = "Effect Site"
      resultsMEAC$Wrap <- PLOT_NAME_MEAC
      resultsMEAC$Label <- ""

      # Add data for plot
      plotResults <- rbind(plotResults, resultsMEAC[,names(plotResults)])

      # Add plot to plotTable
      newplotTable <- plotTable[1,]
      newplotTable$Drug <- "total opioid"
      newplotTable$drugColor <- "black"
      newplotTable$Concentration.Units <- "%"
      newplotTable$y <- 120
      newplotTable$ymin <- 80
      newplotTable$ymax <- 200
      newplotTable$Wrap <- PLOT_NAME_MEAC
      newplotTable$endCe <- 0
      # don't care about MEAC, maxCp, or maxCe
      plotTable <- rbind(plotTable, newplotTable)

      if (length(opioids) > 1)
      {
        # Add in the total MEAC
        plotResults <- rbind(plotResults, totalMEAC)
      }
    }

    # Step B3 and D6: add Interaction

    PropCe <- allEquispace$Ce[allEquispace$Drug == "propofol"]
    if (length(opioids) > 0 & length(PropCe) > 0 & plotInteraction)
    {
      Time <- allEquispace$Time[allEquispace$Drug == plotTable$Drug[1]]
      x <- modelInteraction(PropCe, totalMEAC$Y)
      resultsInteraction <- data.frame(
        Drug = PLOT_NAME_INTERACTION,
        Time = Time,
        Y = x$pNR,
        Site = "Effect Site",
        Wrap = PLOT_NAME_INTERACTION,
        Label = ""
        )

      # Add data for plot
      plotResults <- rbind(plotResults, resultsInteraction)

      #Add plot to plotTable
      newplotTable <- plotTable[1,]
      newplotTable$Drug <- PLOT_NAME_INTERACTION
      newplotTable$drugColor <- "blue"
      newplotTable$Concentration.Units <- ""
      newplotTable$y <- 0
      newplotTable$ymin <- 0
      newplotTable$ymax <- 0
      newplotTable$Wrap <- PLOT_NAME_INTERACTION
      plotTable <- rbind(plotTable, newplotTable)

      # Add in propofol data
      if (min(x$pNRprop) < 1)
      {
        resultsInteractionPropofol <- data.frame(
          Drug = "propofol",
          Time = Time,
          Y = x$pNRprop,
          Site = "Effect Site",
          Wrap = PLOT_NAME_INTERACTION,
          Label = ""
          )
        plotResults <- rbind(plotResults, resultsInteractionPropofol)
      }

      # Add in opioid data
      if (min(x$pNRopioid) < 1)
      {
        resultsInteractionOpioid <- data.frame(
          Drug = "All Opioid",
          Time = Time,
          Y = x$pNRopioid,
          Site = "Effect Site",
          Wrap = PLOT_NAME_INTERACTION,
          Label = ""
        )
        plotResults <- rbind(plotResults, resultsInteractionOpioid)

        # Add plot to plotTable
        newplotTable <- plotTable[1,]
        newplotTable$Drug <- "Total Opioid"
        newplotTable$drugColor <- "red"
        newplotTable$Concentration.Units <- ""
        newplotTable$y <- 0
        newplotTable$ymin <- 0
        newplotTable$ymax <- 0
        newplotTable$Wrap <- PLOT_NAME_INTERACTION
        plotTable <- rbind(plotTable, newplotTable)
      }
    }
  }

  # Step B4 and D7: add Events

  if (plotEvents)
  {
    if (nrow(events) == 0)
    {
      resultsEvents <- data.frame(
        Drug = PLOT_NAME_EVENTS,
        Time = 0,
        Y = 0.875,
        Site = PLOT_ID_EVENTS,
        Wrap = PLOT_NAME_EVENTS,
        Label = ""
      )
    } else {
      resultsEvents <- data.frame(
        Drug = PLOT_NAME_EVENTS,
        Time = events$Time,
        Y =   0.875 - ((1:nrow(events) - 1) %% 4)/4,
        Site = PLOT_ID_EVENTS,
        Wrap = PLOT_NAME_EVENTS,
        Label = events$Event
      )
    }

    # Add data for plot
    plotResults <- rbind(plotResults, resultsEvents)

    # Add Plot to PlotTable

    newplotTable <- plotTable[1,]
    newplotTable$Drug <- PLOT_ID_EVENTS
    newplotTable$drugColor <- "white"
    newplotTable$Concentration.Units <- ""
    newplotTable$y <- 0
    newplotTable$ymin <- 0
    newplotTable$ymax <- 1
    newplotTable$Wrap <- PLOT_NAME_EVENTS
    newplotTable$alpha <- 1
    newplotTable$endCe <- 0
    plotTable <- rbind(plotTable, newplotTable)

  }

  # Step B4b and D7b: add the TCI infusion-rate panels

  # One panel per drug under target-controlled infusion, directly below the
  # concentration panels, showing the pump rate the controller ran (tci.R).
  # The series is named "<drug> TCI" so that it can take the drug's colour
  # without colliding with the concentration series; the loading dose is
  # written on the panel as a number rather than drawn, since its rate would
  # flatten the rest of the panel to zero.
  tciRates   <- purrr::map_dfr(drugs, \(x) purrr::pluck(x, "tci", "rates"))
  tciBoluses <- purrr::map_dfr(drugs, \(x) purrr::pluck(x, "tci", "boluses"))
  tciLabels  <- NULL
  if (nrow(tciRates) > 0)
  {
    tciRates <- tciRates[tciRates$Drug %in% plotTable$Drug, ]
    for (drug in unique(tciRates$Drug))
    {
      r <- tciRates[tciRates$Drug == drug, ]
      r <- r[order(r$Time), ]
      # A loading interval is drawn at the rate that follows it; the number
      # on the panel says what was given.
      for (k in rev(which(r$Bolus))) r$Rate[k] <- if (k < nrow(r)) r$Rate[k + 1] else 0
      # Hold the last rate out to the end of the plot.
      r <- rbind(r, r[nrow(r), ])
      r$Time[nrow(r)] <- maximum

      name <- paste(drug, "TCI")
      wrap <- paste0(drug, " TCI
(", r$Units[1], ")")
      plotResults <- rbind(plotResults, data.frame(
        Drug = name, Time = r$Time, Y = r$Rate, Site = "Rate", Wrap = wrap, Label = ""
      ))

      newplotTable <- plotTable[plotTable$Drug == drug, ][1, ]
      newplotTable$Drug <- name
      newplotTable$Concentration.Units <- r$Units[1]
      newplotTable$typical <- 0
      newplotTable$lowerTypical <- 0
      newplotTable$upperTypical <- 0
      newplotTable$y <- 0
      newplotTable$ymin <- 0
      newplotTable$ymax <- 0
      newplotTable$Wrap <- wrap
      newplotTable$endCe <- 0
      newplotTable$MaxRecovery <- 0
      plotTable <- rbind(plotTable, newplotTable)

      b <- tciBoluses[tciBoluses$Drug == drug, ]
      if (nrow(b) > 0)
      {
        top <- max(r$Rate, 0)
        if (top == 0) top <- 1
        tciLabels <- rbind(tciLabels, data.frame(
          Drug = name,
          Time = b$Time,
          y = top,
          Label = paste(signif(b$Amount, 3), b$Units),
          Wrap = wrap
        ))
      }
    }
  }

  # Step B5 and D8: finalize plotResults and plotTable

  ##################################################

  plotResults$Site <- factor(plotResults$Site,levels=c("Plasma", "Effect Site", PLOT_ID_EVENTS, "Recovery", "Rate"), ordered=TRUE)
  plotResults <- plotResults[!is.na(plotResults$Y),]

  # Convert $Drug and $Wrap to factors to preserve order from plotTable

  drugFactors <- c(drugDefaults$Drug, paste(drugDefaults$Drug, "TCI"), "total opioid", PLOT_NAME_INTERACTION, "Recovery", PLOT_NAME_EVENTS)
  plotTable$Factor <- factor(plotTable$Drug, levels = drugFactors, ordered = TRUE)
  plotTable <- plotTable[order(plotTable$Factor),]

  drugFactors <- c(plotTable$Drug, "Recovery")
  wrapFactors <- plotTable$Wrap
  drugColors <-  c(plotTable$drugColor, "black")
  # Named, so that a colour scale with explicit breaks (the TCI rate series
  # are kept out of the legend) still colours every series.
  names(drugColors) <- drugFactors

  plotResults$Drug  <- factor(plotResults$Drug,  levels = drugFactors, ordered = TRUE)
  plotTable$Drug    <- factor(plotTable$Drug,    levels = drugFactors, ordered = TRUE)

  plotResults$Wrap  <- factor(plotResults$Wrap, levels=wrapFactors, ordered = TRUE)
  plotTable$Wrap    <- factor(plotTable$Wrap  , levels=wrapFactors, ordered = TRUE)

  ##################################################################################
  # Begin plotting                                                                 #
  ##################################################################################

  linetypes <- c(plasmaLinetype, effectsiteLinetype, "blank", "dotted", "solid")
  names(linetypes) <- c(
    if (!is.null(plasmaLinetype)) "Plasma",
    if (!is.null(effectsiteLinetype)) "Effect Site",
    PLOT_ID_EVENTS, "Recovery", "Rate"
  )

  # Step A1: create plotObject with lines from `plotResults`

  data <- subset(plotResults, Wrap != PLOT_NAME_EVENTS & Site != "Rate")
  rateData <- subset(plotResults, Site == "Rate")

  if (logY) {
    data <- data[data$Y>0,]
    rateData <- rateData[rateData$Y>0,]
  }

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(
        x = Time,
        y = Y,
        color = Drug,
        linetype = Site
      ),
      linewidth=1
    )

  # The pump rate is piecewise constant, so it is drawn as steps; the loading
  # dose is marked and written as a number.
  if (nrow(rateData) > 0)
  {
    plotObject <- plotObject +
      ggplot2::geom_step(
        data = rateData,
        ggplot2::aes(x = Time, y = Y, color = Drug),
        linewidth = 1,
        show.legend = FALSE
      )
  }
  if (!is.null(tciLabels))
  {
    tciLabels$Wrap <- factor(tciLabels$Wrap, levels = wrapFactors, ordered = TRUE)
    tciLabels$Drug <- factor(tciLabels$Drug, levels = drugFactors, ordered = TRUE)
    plotObject <- plotObject +
      ggplot2::geom_segment(
        data = tciLabels,
        ggplot2::aes(x = Time, xend = Time, y = 0, yend = y, color = Drug),
        linetype = "dashed",
        linewidth = 0.5,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      ggplot2::geom_label(
        data = tciLabels,
        ggplot2::aes(x = Time, y = y, label = Label),
        color = "black",
        hjust = -0.05,
        vjust = 1,
        size = 3.5,
        inherit.aes = FALSE,
        show.legend = FALSE,
        label.padding = grid::unit(0.5, "mm")
      )
  }

  # Step A2: add scales to plotObject

  plotObject <- plotObject +
    ggplot2::coord_cartesian(xlim = c(min(xBreaks), max(xBreaks)), clip="off") +
    ggplot2::scale_x_continuous(expand = c(0,0), breaks = xBreaks, labels = xLabels) +
    ggplot2::scale_color_manual(values=drugColors, breaks = drugFactors[!grepl(" TCI$", drugFactors)]) +
    ggplot2::scale_fill_manual(values=drugColors)  +
    ggplot2::scale_alpha_manual(values = c(plotTable$alpha, 0.5)) +
    ggplot2::scale_linetype_manual(values=linetypes, breaks = c("Plasma", "Effect Site"))

  # Step A3: handle logarithmic Y axis

  if (logY)
  {
    plotObject <- plotObject + ggplot2::scale_y_log10()
  } else {
    # Every panel starts at zero, except a serum osmolality panel: anchored
    # at zero, a rise from 290 to 320 mOsm/kg would be a ripple along the top.
    # It starts a little below the lower of the baseline and the typical band
    # instead.  This replaces scale_y_continuous(limits = c(0, NA)), which
    # cannot vary by facet; a blank point at the floor of each panel gives the
    # same zero-anchored axis everywhere else.
    yFloor <- data.frame(Wrap = plotTable$Wrap, Y = 0)
    osmoticRows <- normalization == NORMALIZE_NONE &
      plotTable$Concentration.Units == "mOsm"
    for (i in which(osmoticRows))
    {
      panelY <- plotResults$Y[plotResults$Wrap == plotTable$Wrap[i] &
                              plotResults$Site == "Plasma"]
      lowest <- min(c(panelY, plotTable$ymin[i], plotTable$ymax[i]), na.rm = TRUE)
      yFloor$Y[i] <- 10 * floor((lowest - 5) / 10)
    }
    plotObject <- plotObject +
      ggplot2::geom_blank(data = yFloor, ggplot2::aes(y = Y), inherit.aes = FALSE) +
      ggplot2::scale_y_continuous()
  }

  # Step A3: labs and themes

  nFacets <- length(unique(plotResults$Wrap))
  width <- width - 200 # roughly account for legend and Y axis labels
  aspect <- yAxisHeight / width
  height <- yAxisHeight * nFacets + 50
  plotObject <- plotObject + ggplot2::labs(x = xAxisLabel) +
    ggplot2::theme(aspect.ratio = aspect) +
    ggplot2::theme(legend.text = ggplot2::element_text(size=12)) +
    ggplot2::theme(legend.title = ggplot2::element_text(color="darkblue", size=13, face="bold"))

  # Step A4: add typical values

  switch(
    typical,
    "Range" = {
      plotObject <-
        plotObject +
        ggplot2::geom_rect(
        data=plotTable,
        ggplot2::aes(
          xmin=xmin,
          xmax=xmax,
          ymin=ymin,
          ymax=ymax,
          fill=Drug,
          alpha = Drug
        ),
        inherit.aes=FALSE,
        show.legend=FALSE
      )
    },
    "Mid" = {
      plotObject <-
        plotObject +
        ggplot2::geom_rect(
          data=plotTable,
          mapping=ggplot2::aes(
            xmin=xmin,
            xmax=xmax,
            ymin=typical*0.95,
            ymax=typical*1.05,
            fill=Drug
          ),
          alpha = 0.35,
          linewidth=1,
          inherit.aes=FALSE,
          show.legend=FALSE
        )
    }
  )

  # Step A5: add events (moved to end because the color scheme will change)

  if (plotEvents)
  {
    plotObject <- plotObject +
      ggplot2::geom_rect(
        data = subset(plotResults, Wrap == PLOT_NAME_EVENTS),
        ggplot2::aes(
          xmin = 0, # xmin,
          xmax = maximum, # xmax,
          ymin = 0, # ymin,
          ymax = 1 # ymax
        ),
        color="white",
        fill = "white",
        alpha = 1,
        inherit.aes = FALSE,
        show.legend = FALSE
      )

    plotLabels <- subset(plotResults, Label != "")
    crows <- match(plotLabels$Label, eventDefaults$Event)
    plotLabels$Color <- eventDefaults$Color[crows]

    if (nrow(plotLabels) > 0)
      for (i in 1:nrow(plotLabels))
      {
        plotObject <- plotObject +
          ggplot2::geom_label(
          data = plotLabels[i,],
            mapping = ggplot2::aes(
              x = Time,
              y = Y,
              label = Label
            ),
          color = "black",
          fill = plotLabels$Color[i],
          hjust = 0,
          alpha = 0.25,
          show.legend = FALSE,
          inherit.aes=FALSE,
          label.padding = grid::unit(0.25,"mm"),
          fontface = "bold"
          )
      }
  }

  # Step A6: facet wrap

  # This code should work if facetscales gets fixed
  # scales_y <- sapply(as.character(unique(plotTable$Wrap)), function(x) x = scale_y_continuous())
  # if (plotEvents) scales_y$Events <- scale_y_continuous(labels = NULL)
  plotObject <- plotObject +
    ggplot2::facet_grid(
      Wrap ~ .,
#      ncol = 1,
      scales="free_y",
      switch = "y",
#      strip.position = "left",
      shrink=FALSE
#      scales = list(y = scales_y)
      ) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
          strip.placement = "outside",
          strip.text.y = ggplot2::element_text(
            size = 18,
            angle = 270
          ),
          axis.text.y = ggplot2::element_text(size = 15),
          panel.spacing = grid::unit(2, "lines"),
          legend.background = ggplot2::element_blank(),
          legend.box.background = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank()
          )

  # Step A7: add in process plotRecovery

  if (plotRecovery)
  {

    x <- ggplot2::ggplot_build(plotObject)
    recovery <- allEquispace[,c("Drug","Time","Recovery")]
    recovery$Wrap <- ""
    recoveryLabels <- data.frame(
      Drug   = rep("",100),
      y  = 0,
      new = 0,
      x = maximum,
      Wrap = ""
    )
    start <- 1
    for (i in 1:nplotTable)
    {
      USE <- recovery$Drug == as.character(plotTable$Drug[i])
      if (isTRUE(plotTable$MaxRecovery[i] > 0))
      {
#        labels <- as.numeric(x$layout$panel_params[[i]]$y.labels)
        labels <- as.numeric(stats::na.omit(x$layout$panel_params[[i]]$y$get_labels()))
        nLabels <- length(labels) - 1 # Subtract 1 because 0 is always included
        end <- start + nLabels
        recoveryLabels$Drug[start:end] <- as.character(plotTable$Drug[i])
        recoveryLabels$y[start:end] <- labels
        recoveryLabels$Wrap[start:end] <- as.character(plotTable$Wrap[i])
        plotTable$MaxRecovery[i] <- ceiling(plotTable$MaxRecovery[i] / nLabels) * nLabels
#        plotTable$MaxY[i] <- recoveryLabels$y[end]
#        recoveryLabels$new[start:end] <- paste(labels /  plotTable$MaxY[i] * plotTable$MaxRecovery[i], "min")
#        recovery$Recovery[USE] <- recovery$Recovery[USE] / plotTable$MaxRecovery[i] * plotTable$MaxY[i]
#        plotTable does not have MaxY, and it is not needed in the table, so
        MaxYi <- recoveryLabels$y[end]
        recoveryLabels$new[start:end] <- paste(labels /  MaxYi * plotTable$MaxRecovery[i], "min")
        recovery$Recovery[USE] <- recovery$Recovery[USE] / plotTable$MaxRecovery[i] * MaxYi
        recovery$Wrap[USE] <- as.character(plotTable$Wrap[i])
        start <- end + 1
      } else {
        recovery <- recovery[!USE,]
      }
    }
    recoveryLabels <- recoveryLabels[recoveryLabels$Drug != "",]

    # `Wrap =`, not `Wrap <-`: the assignment form named the column after the
    # whole expression, and the code below only found it because `$` on a data
    # frame matches partial names.  (Fixed 2026-10-05.)
    arrows <- data.frame(
      Drug = plotTable$Drug,
      y = plotTable$endCe,
      new = "\u2190 Threshold",
      x = maximum,
      Wrap = as.character(plotTable$Wrap)
    )
    # No threshold, no arrow.  Oxygen, the carrier gases and any panel without
    # an endCe would otherwise get one pointing at zero.
    arrows <- arrows[!is.na(arrows$y) & arrows$y > 0, , drop = FALSE]

    recoveryLabels$Wrap <- factor(recoveryLabels$Wrap, levels=wrapFactors, ordered = TRUE)
    recovery$Wrap <- factor(recovery$Wrap, levels=wrapFactors, ordered = TRUE)
    arrows$Wrap   <- factor(arrows$Wrap, levels=wrapFactors, ordered = TRUE)

    # Recovery is missing wherever it could not be computed -- a dose given
    # that has not begun to be absorbed; see pendingDoseTimes().  Break the
    # line there rather than drawing across the gap, which would assert a time
    # for the one stretch that has none.  An explicit group, rather than
    # relying on how the geom happens to treat a missing value.  With nothing
    # missing the group is constant and the line is exactly the one drawn
    # before.
    recovery$Segment <- cumsum(is.na(recovery$Recovery))
    recovery <- recovery[!is.na(recovery$Recovery), , drop = FALSE]

    # Step A7: finish plotObject

    plotObject <- plotObject +
      ggplot2::geom_text(
        data=recoveryLabels,
        mapping=ggplot2::aes(
          x=x,
          y=y,
          label = new
        ),
        color = "black",
        inherit.aes=FALSE,
        show.legend=FALSE,
        hjust = 1.1,
        vjust = -.05,
        size = 3 # font size to mm
      ) +
      ggplot2::geom_text(
        data=arrows,
        mapping=ggplot2::aes(
          x=x,
          y=y,
          label = new
        ),
        color = "black",
        inherit.aes=FALSE,
        show.legend=FALSE,
        hjust = -.05,
        vjust = 0.5,
        size = 3 # font size to mm
      ) +
      ggplot2::geom_rect(
        data=plotTable,
        mapping=ggplot2::aes(
          xmin=xmin,
          xmax=xmax,
          ymin=0,
          ymax=endCe
        ),
        fill = "grey",
        alpha = 0.2,
        linewidth=0,
        inherit.aes=FALSE,
        show.legend=FALSE
      ) +
      ggplot2::geom_line(
       data = recovery,
       ggplot2::aes(
         x = Time,
         y = Recovery,
         group = Segment
        ),
       show.legend = FALSE,
       color = "black",
       linetype = "solid",
       linewidth = 0.5
     )
  }

#  plotObject
  outputComments("Exiting simulationPlot", level = DEBUG_LEVEL_VERBOSE)
  return(list(plotObject = plotObject, allResults = allResults, plotResults = plotResults, plotHeight = height))
}
