#require(rsconnect)

#defaultxLabels <- 0:8*12

drugDefaults <- read.xlsx("Drug Defaults.xlsx")
drugList <- drugDefaults$Drug

colorList <-  c("black","red","green","blue", "orange","brown","grey", "white")

doseTable <- data.frame(
  Drug = "",
  Color = "",
  Minute = rep(0,6),
  Bolus = FALSE,
  Dose = 0,
  Units = ""
)
doseTableNewRow <- doseTable[1,]

bolusUnits <- c("mg","mcg")
infusionUnits <- c("mg/min","mg/hr","mg/kg/min","mg/kg/hr","mcg/min","mcg/hr","mcg/kg/min","mcg/kg/hr")

units <- c(bolusUnits, infusionUnits)

compareTable <- function(A,B)
{
  if (nrow(A) != nrow(B)) return(-1)
  {
    compare <- (A != B)
    cell <- c(which(rowSums(compare)>0),which(colSums(compare) > 0))
  }
  if (length(cell) == 0) return(0)
  return(cell)
}

PK <- vector("list",length=length(drugList))
names(PK) <- drugList

# Setup Theme
cat("Setting up theme")
theme_update(panel.background = element_rect(fill = "white", color = "white"))
theme_update(legend.box.background = element_rect(fill = "white", color = "white"))
theme_update(panel.grid.major.y = element_line(color = "lightgrey"))
theme_update(panel.grid.major.x = element_line(color = "lightgrey"))
theme_update(axis.ticks = element_line(color = "lightgrey"))
theme_update(axis.ticks.length = unit(.25, "cm"))
theme_update(axis.title = element_text(size = rel(1.5)))
theme_update(axis.text = element_text(size = rel(1.2)))
theme_update(axis.line = element_line(size = 1, color = "black"))
theme_update(axis.title = element_text(size = rel(1.5)))
theme_update(legend.key = element_rect(fill = "white"))
theme_update(aspect.ratio = 0.6)
theme_update(plot.title = element_text(size = rel(1.5)))
theme_update(legend.text = element_text(size = rel(0.9)))
theme_update(legend.position="right")
theme_update(legend.key = element_blank())


plot <- NULL
slide <- 0

workingPlot <- ggplot() +
theme_void() +
  annotate(
    "text",
    label="Working",
    x=0.5,
    y=0.5,
    size=7
  )

stats2groupsPlot <- ggplot() +
  theme_void() +
  annotate(
    "text",
    label="Stats requires 2 groups",
    x=0.5,
    y=0.5,
    size=7
  )

nothingtoPlot <- ggplot() +
  theme_void() +
  annotate(
    "text",
    label="Nothing to plot",
    x=0.5,
    y=0.5,
    size=7
  )

nolinesPlot <- ggplot() +
  theme_void() +
  annotate(
    "text",
    label="Select a linetype other than 'blank'",
    x=0.5,
    y=0.5,
    size=7
  )

checkaxisPlot <- ggplot() +
  theme_void() +
  annotate(
    "text",
    label="Check axis settings",
    x=0.5,
    y=0.5,
    size=7
  )


sendSlide <- function(recipient, title)
{
  cat("In function sendSlide()\n")
  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  DATE <- format(Sys.Date(), "%m/%d/%y")
  PPTX <- read_pptx("Template.pptx")
  MASTER <- "Office Theme"

  PPTX <- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <- ph_with_text(PPTX, type = "title", str = title)
  suppressWarnings(
    PPTX <<- ph_with_vg(PPTX, code = print(plot), type = "body")
  )

  PPTX <- ph_with_text(PPTX, type = "dt", str = DATE)
  slide <- slide + 1
  fileName <- paste0("Slides/Slide", slide, ".", TIMESTAMP, ".pptx")
  print(PPTX, target = fileName)
  if (is.null(recipient))
  {
    cat("recipient is null \n")
    return(paste0("Figure saved in ", getwd(),"/slides"))
  } else {
    addresses <- recipient
    send.mail(from = "steven.shafer@stanford.edu",
              to = addresses,
              subject = "Figure from stanpumpR",
              body = paste(
                "Here is the simulation you ran\n on",Sys.Date(),
                "Please let me know if you have any questions or suggestions\n",
                "Sincerely,\n",
                "Steve Shafer"),
              smtp = list(
                host.name = "smtp.office365.com",
                port = 587,
                user.name = "steve@concentricanalgesics.com",
                passwd = "Dl33*560JKllad",
                tls = TRUE),
              attach.files = fileName,
              authenticate = TRUE,
              send = TRUE)
    return(paste("Figure sent to", paste(recipient,collapse="")))
  }
}

  # Set up PowerPoint
  TIMESTAMP <<- format(Sys.time(), format = "%y%m%d-%H%M%S")
  DATE <<- format(Sys.Date(), "%m/%d/%y")
  PPTX <<- read_pptx("Template.pptx")
  MASTER <<- "Office Theme"

  #Footer Function
  WriteFooter <<- function(X)
  {
    PPTX <<- ph_with_text(PPTX, type = "ftr", str = X)
    PPTX <<- ph_with_text(PPTX, type = "dt", str = DATE)
    PPTX <<- ph_with_text(PPTX, type = "sldNum", str = SLIDE)
    SLIDE <<- SLIDE + 1
  }
  SLIDE <<- 1
  # Title Slide


nextSlide <<- function (title, PLOT,footer=FOOTER)
{
  suppressWarnings(
    print(PLOT)
  )
  PPTX <<- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <<- ph_with_text(PPTX, type = "title", str = title)
  suppressWarnings(
    PPTX <<- ph_with_vg(PPTX, code = print(PLOT), type = "body")
  )
  WriteFooter(footer)
}


deploy <- function()
{
  setwd("c:/google drive/projects/stanpumpR")
  library(rsconnect)
  deployApp(forceUpdate = TRUE, account="steveshafer")
}

setxLabels <- function (min,max,step)
{
  min <- as.numeric(min)
  max <- max(as.numeric(max))
  step <- as.numeric(step)
  if (is.na(max)) return(0)
  if (is.na(min)) return(0)
  if (is.na(step)) return(0)
  if (max <= min) return(0)
  result <- min + 0:ceiling((max-min)/step) * step
  result <- result[result <= max]
  if (length(result) < 4 | length(result) > 13) return(pretty(c(min,max)))
  return(result)
}

processdoseTable <- function (doseTable)
{
 doseTable$Drug <- as.character(doseTable$Drug)
doseTable$Color <- as.character(doseTable$Color)
doseTable$Units <- as.character(doseTable$Units)
lastRow <- nrow(doseTable)
if (doseTable$Drug[lastRow]!="")
  doseTable <- rbind(doseTable,doseTableNewRow)


# Just reset bad units to blanks
doseTable$Units[doseTable$Drug == ""] <- ""
doseTable$Units[doseTable$Drug != "" &
                  !doseTable$Bolus &
                  !grepl("/",doseTable$Units)] <- ""
doseTable$Units[doseTable$Drug != "" &
                  doseTable$Bolus &
                  grepl("/",doseTable$Units)] <- ""
# Now fix all the blanks, using the drug defaults
FIX <- doseTable$Drug != "" & doseTable$Units == ""
CROWS <- match(doseTable$Drug,drugDefaults$Drug)
doseTable$Units[FIX & doseTable$Bolus] <-
  drugDefaults$Bolus.Units[CROWS[FIX & doseTable$Bolus]]
doseTable$Units[FIX & !doseTable$Bolus] <-
  drugDefaults$Infusion.Units[CROWS[FIX & !doseTable$Bolus]]

for (i in 1:nrow(drugDefaults))
{
  USE <- doseTable$Drug == drugDefaults$Drug[i]
  if (sum(USE) > 0)
  {
    # Process color column
    colors <- doseTable$Color[USE]
    if (length(colors) == 1 & colors[1] == "")
      colors <- colorList[which(!colorList %in% doseTable$Color)[1]]
    colors <- colors[colors != ""]
    colorTable <- as.data.frame(ftable(colors),stringsAsFactors = FALSE)
    colorTable$Freq <- as.numeric(colorTable$Freq)
    colorTable$colors <- as.character(colorTable$colors)
    replacementColor <-
      colorTable$colors[which(colorTable$Freq == min(colorTable$Freq))]
    if (length(replacementColor) > 1)
      replacementColor <-
      doseTable$Color[tail(which(doseTable$Color %in% replacementColor),1)]
    doseTable$Color[USE] <- replacementColor
  }
}

HOT <- rhandsontable(
  doseTable,
  rowHeaders = NULL,
  #      width = 550,
  height = 220,
  selectCallback = TRUE
) %>%
  hot_col(
    col = "Drug",
    type = "dropdown",
    source = drugList,
    strict = TRUE,
    halign = "htLeft",
    valign = "vtMiddle",
    allowInvalid=FALSE
  ) %>%
  hot_col(
    col = "Color",
    type = "dropdown",
    source = colorList,
    strict = TRUE,
    halign = "htLeft",
    valign = "htMiddle",
    allowInvalid = FALSE
  ) %>%
  hot_col(
    col = "Minute",
    type="numeric",
    format="0",
    halign = "htRight",
    #          valign = "htTop",
    allowInvalid = FALSE
  ) %>%
  hot_col(
    col = "Bolus",
    type = "checkbox",
    halign = "htRight",
    valign = "htMiddle",
    allowInvalid = FALSE
  ) %>%
  hot_col(
    col = "Dose",
    type = "numeric",
    format = "0.000",
    halign = "htRight",
    # valign = "htTop",
    allowInvalid = FALSE
  ) %>%
  hot_col(
    col = "Units",
    type = "dropdown",
    source = units,
    strict = TRUE,
    halign = "htLeft",
    valign = "vtMiddle",
    allowInvalid=FALSE
  ) %>%
  #        hot_validate_numeric(cols = c("Minute"), min = 0, max = 1440) %>%
  hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
  hot_rows(rowHeights = 10) %>%
  hot_cols(colWidths = c(100,60,60,50,70,100))
HOT$sizingPolicy$viewer$padding <- 20 # no effect
HOT$sizingPolicy$browser$padding <- 20 # no effect
HOT$sizingPolicy$viewer$defaultHeight <- 80 # no effect

return(list(doseTable = doseTable, HOT = HOT))
}
