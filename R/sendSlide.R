# Send a copy of the current plot to the designated recipient
sendSlide <- function(
  values,
  recipient,
  plotObject,
  allResults,
  plotResults,
  height,
  width,
  slide,
  drugs,
  drugDefaults,
  email_username,
  email_password
)
{
  tryCatch({
    prevEcho <- options("ECHO_OUTPUT_COMMENTS" = TRUE)
    on.exit(options("ECHO_OUTPUT_COMMENTS" = prevEcho[[1]]))

    outputComments("Sending email to", recipient)

    if (is.null(email_username)) {
      stop("Email username missing")
    }
    if (is.null(email_password)) {
      stop("Email password missing")
    }

    emailData <- generateEmail(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, drugDefaults)

    outputComments("Sending email")

    msg <- emayili::envelope() |>
      emayili::from(paste0("stanpumpR <", email_username, ">")) |>
      emayili::to(recipient) |>
      emayili::subject(emailData$title, interpolate = FALSE) |>
      emayili::html(emailData$bodyText, interpolate = FALSE) |>
      emayili::attachment(emailData$pptxfileName) |>
      emayili::attachment(emailData$pngfileName) |>
      emayili::attachment(emailData$xlsxfileName)

    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 587,
      username = email_username,
      password = email_password
    )
    smtp(msg, verbose = FALSE)

    unlink(emailData$pptxfileName)
    unlink(emailData$pngfileName)
    unlink(emailData$xlsxfileName)
    outputComments("Leaving sendMail()")
    return(TRUE)
  }, error = function(e) {
    return(e$message)
  })
}

generateEmail <- function(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, drugDefaults) {
  title <- paste("stanpumpR simulation on", format(Sys.time()))
  DT <- values$DT
  url <- values$url

  outputComments("In function sendSlide()")

  if (!file.exists("Slides")) dir.create("Slides")
  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  DATE <- format(Sys.Date(), "%m/%d/%y")
  outputComments("reading Template.pptx")
  PPTX <- officer::read_pptx(system.file("extdata", "Template.pptx", package = "stanpumpR"))
  outputComments("Template.pptx loaded")
  MASTER <- "Office Theme"

  PPTX <- officer::add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <- officer::ph_with(PPTX, title, location = officer::ph_location_type("title"))
  PPTX <- officer::ph_with(PPTX, rvg::dml(code = print(plotObject)), location = officer::ph_location_type("body"))

  PPTX <- officer::ph_with(PPTX, DATE, location = officer::ph_location_type ("dt"))
  PPTX <- officer::ph_with(PPTX, slide, location = officer::ph_location_type ("sldNum"))
  PPTX <- officer::ph_with(PPTX, "From StanpumpR", location = officer::ph_location_type ("ftr"))
  pptxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".pptx")

  outputComments("Saving PPTX")
  print(PPTX, target = pptxfileName)

  xlsxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".xlsx")

  outputComments("Creating PNG file")

  pngfileName <- paste0("Slides/Preview.", slide, ".", TIMESTAMP, ".png")
  ggplot2::ggsave(
    plotObject +
      ggplot2::theme(
        strip.text.y = ggplot2::element_text(size = 6, angle = 180),
        axis.text.y = ggplot2::element_text(size = 6),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(size = 12),
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size=8),
        legend.title = ggplot2::element_text(color="darkblue", size=10, face="bold")
      ),
    filename = pngfileName,
    dpi = 150,
    height = height,
    width = width,
    units = "px"
  )

  outputComments("Fixing Units for export")
  if (values$ageUnit == "1")
  {
    ageUnit <- "years"
  } else {
    ageUnit <- "months"
  }

  if (values$weightUnit == "1")
  {
    weightUnit <- "kilograms"
  } else {
    weightUnit <- "pounds"
  }

  if (values$heightUnit == "1")
  {
    heightUnit <- "cms"
  } else {
    heightUnit <- "inches"
  }

  # Every Time column in the workbook is in minutes, which is what the engine
  # works in and what a script reading the sheets expects.  When the plot was
  # shown in hours, days or weeks, each of those columns is followed by the
  # same times in that unit, and the covariates sheet and the email say so.
  # NULL (a caller that predates time units) means minutes.
  timeUnit <- if (is.null(values$timeUnit)) "minutes" else values$timeUnit

  outputComments("Creating workbook")
  wb <- openxlsx::createWorkbook("SLS")
  covariates <- data.frame(
    Covariate = c(
      "Age",
      "Age Unit",
      "Weight",
      "Weight Unit",
      "Height",
      "Height Unit",
      "Sex",
      "Adjust weight to fat-free mass",
      "Baseline serum osmolality (mOsm/kg)",
      "Serum creatinine (mg/dL)"
    ),
    Value = c(
      values$age / values$ageUnit,
      ageUnit,
      values$weight / values$weightUnit,
      weightUnit,
      values$height / values$heightUnit,
      heightUnit,
      values$sex,
      if (isTRUE(values$adjustToFFM)) "yes" else "no",
      if (is.null(values$osmolality)) OSMOLALITY_DEFAULT else values$osmolality,
      if (is.null(values$creatinine)) "not entered (assumed normal)" else values$creatinine
    ))
  covariates <- rbind(covariates, plotTimeSettings(values$maximum, timeUnit))
  outputComments("Writing covariates")
  openxlsx::addWorksheet(wb, "Covariates")
  openxlsx::writeData(wb, sheet = 1, covariates)

  # The TCI infusion rows and the repeats of scheduled (qd/bid/tid/qid) doses
  # live outside the dose table in the app so that the table stays usable;
  # they are merged in for the export (tci.R, scheduled.R).
  outputComments("Writing dose table")
  openxlsx::addWorksheet(wb, "Dose Table")
  openxlsx::writeData(wb, sheet = 2, exportDoseTable(DT, drugs, timeUnit = timeUnit))

  outputComments("Writing simulation results")
  openxlsx::addWorksheet(wb, "Simulation Results")
  openxlsx::writeData(wb, sheet = 3, addDisplayTimeColumn(allResults, timeUnit))

  outputComments("Writing results for plotting")
  openxlsx::addWorksheet(wb, "Results for Plotting")
  openxlsx::writeData(wb, sheet = 4, addDisplayTimeColumn(plotResults, timeUnit))

  outputComments("Writing PK parameters")
  sheet = 5
  for (drug in sort(unique(as.character(DT$Drug))))
  {
    thisDrug <- which(drugDefaults$Drug == drug)

    pkSets <- drugs[[drug]]$PK
    parameters <-   as.data.frame(
      cbind(
        v1 = purrr::map_dbl(pkSets, "v1"),
        v2 = purrr::map_dbl(pkSets, "v2"),
        v3 = purrr::map_dbl(pkSets, "v3"),
        cl1 = purrr::map_dbl(pkSets, "cl1"),
        cl2 = purrr::map_dbl(pkSets, "cl2"),
        cl3 = purrr::map_dbl(pkSets, "cl3"),
        k10 = purrr::map_dbl(pkSets, "k10"),
        k12 = purrr::map_dbl(pkSets, "k12"),
        k13 = purrr::map_dbl(pkSets, "k13"),
        k21 = purrr::map_dbl(pkSets, "k21"),
        k31 = purrr::map_dbl(pkSets, "k31"),
        lambda_1 = purrr::map_dbl(pkSets, "lambda_1"),
        lambda_2 = purrr::map_dbl(pkSets, "lambda_2"),
        lambda_3 = purrr::map_dbl(pkSets, "lambda_3"),
        ke0 = purrr::map_dbl(pkSets, "ke0"),
        p_coef_bolus_l1 = purrr::map_dbl(pkSets, "p_coef_bolus_l1"),
        p_coef_bolus_l2 = purrr::map_dbl(pkSets, "p_coef_bolus_l2"),
        p_coef_bolus_l3 = purrr::map_dbl(pkSets, "p_coef_bolus_l3"),
        e_coef_bolus_l1 = purrr::map_dbl(pkSets, "e_coef_bolus_l1"),
        e_coef_bolus_l2 = purrr::map_dbl(pkSets, "e_coef_bolus_l2"),
        e_coef_bolus_l3 = purrr::map_dbl(pkSets, "e_coef_bolus_l3"),
        e_coef_bolus_ke0 = purrr::map_dbl(pkSets, "e_coef_bolus_ke0"),
        p_coef_infusion_l1 = purrr::map_dbl(pkSets, "p_coef_infusion_l1"),
        p_coef_infusion_l2 = purrr::map_dbl(pkSets, "p_coef_infusion_l2"),
        p_coef_infusion_l3 = purrr::map_dbl(pkSets, "p_coef_infusion_l3"),
        e_coef_infusion_l1 = purrr::map_dbl(pkSets, "e_coef_infusion_l1"),
        e_coef_infusion_l2 = purrr::map_dbl(pkSets, "e_coef_infusion_l2"),
        e_coef_infusion_l3 = purrr::map_dbl(pkSets, "e_coef_infusion_l3"),
        e_coef_infusion_ke0 = purrr::map_dbl(pkSets, "e_coef_infusion_ke0"),
        ka_PO = purrr::map_dbl(pkSets, "ka_PO"),
        bioavailability_PO = purrr::map_dbl(pkSets, "bioavailability_PO"),
        tlag_PO = purrr::map_dbl(pkSets, "tlag_PO"),
        ka_IM = purrr::map_dbl(pkSets, "ka_IM"),
        bioavailability_IM = purrr::map_dbl(pkSets, "bioavailability_IM"),
        tlag_IM = purrr::map_dbl(pkSets, "tlag_IM"),
        ka_IN = purrr::map_dbl(pkSets, "ka_IN"),
        bioavailability_IN = purrr::map_dbl(pkSets, "bioavailability_IN"),
        tlag_IN = purrr::map_dbl(pkSets, "tlag_IN"),
        ka_RA = purrr::map_dbl(pkSets, "ka_RA"),
        bioavailability_RA = purrr::map_dbl(pkSets, "bioavailability_RA"),
        tlag_RA = purrr::map_dbl(pkSets, "tlag_RA"),
        ka_RAslow = purrr::map_dbl(pkSets, "ka_RAslow"),
        bioavailability_RAslow = purrr::map_dbl(pkSets, "bioavailability_RAslow"),
        tlag_RAslow = purrr::map_dbl(pkSets, "tlag_RAslow"),
        ka_PO2 = purrr::map_dbl(pkSets, "ka_PO2"),
        bioavailability_PO2 = purrr::map_dbl(pkSets, "bioavailability_PO2"),
        tlag_PO2 = purrr::map_dbl(pkSets, "tlag_PO2"),
        p_coef_PO_l1 = purrr::map_dbl(pkSets, "p_coef_PO_l1"),
        p_coef_PO_l2 = purrr::map_dbl(pkSets, "p_coef_PO_l2"),
        p_coef_PO_l3 = purrr::map_dbl(pkSets, "p_coef_PO_l3"),
        p_coef_PO_ka = purrr::map_dbl(pkSets, "p_coef_PO_ka"),
        e_coef_PO_l1 = purrr::map_dbl(pkSets, "e_coef_PO_l1"),
        e_coef_PO_l2 = purrr::map_dbl(pkSets, "e_coef_PO_l2"),
        e_coef_PO_l3 = purrr::map_dbl(pkSets, "e_coef_PO_l3"),
        e_coef_PO_ke0 = purrr::map_dbl(pkSets, "e_coef_PO_ke0"),
        e_coef_PO_ka = purrr::map_dbl(pkSets, "e_coef_PO_ka"),
        p_coef_IM_l1 = purrr::map_dbl(pkSets, "p_coef_IM_l1"),
        p_coef_IM_l2 = purrr::map_dbl(pkSets, "p_coef_IM_l2"),
        p_coef_IM_l3 = purrr::map_dbl(pkSets, "p_coef_IM_l3"),
        p_coef_IM_ka = purrr::map_dbl(pkSets, "p_coef_IM_ka"),
        e_coef_IM_l1 = purrr::map_dbl(pkSets, "e_coef_IM_l1"),
        e_coef_IM_l2 = purrr::map_dbl(pkSets, "e_coef_IM_l2"),
        e_coef_IM_l3 = purrr::map_dbl(pkSets, "e_coef_IM_l3"),
        e_coef_IM_ke0 = purrr::map_dbl(pkSets, "e_coef_IM_ke0"),
        e_coef_IM_ka = purrr::map_dbl(pkSets, "e_coef_IM_ka"),
        p_coef_IN_l1 = purrr::map_dbl(pkSets, "p_coef_IN_l1"),
        p_coef_IN_l2 = purrr::map_dbl(pkSets, "p_coef_IN_l2"),
        p_coef_IN_l3 = purrr::map_dbl(pkSets, "p_coef_IN_l3"),
        p_coef_IN_ka = purrr::map_dbl(pkSets, "p_coef_IN_ka"),
        e_coef_IN_l1 = purrr::map_dbl(pkSets, "e_coef_IN_l1"),
        e_coef_IN_l2 = purrr::map_dbl(pkSets, "e_coef_IN_l2"),
        e_coef_IN_l3 = purrr::map_dbl(pkSets, "e_coef_IN_l3"),
        e_coef_IN_ke0 = purrr::map_dbl(pkSets, "e_coef_IN_ke0"),
        e_coef_IN_ka = purrr::map_dbl(pkSets, "e_coef_IN_ka"),
        p_coef_RA_l1 = purrr::map_dbl(pkSets, "p_coef_RA_l1"),
        p_coef_RA_l2 = purrr::map_dbl(pkSets, "p_coef_RA_l2"),
        p_coef_RA_l3 = purrr::map_dbl(pkSets, "p_coef_RA_l3"),
        p_coef_RA_ka = purrr::map_dbl(pkSets, "p_coef_RA_ka"),
        e_coef_RA_l1 = purrr::map_dbl(pkSets, "e_coef_RA_l1"),
        e_coef_RA_l2 = purrr::map_dbl(pkSets, "e_coef_RA_l2"),
        e_coef_RA_l3 = purrr::map_dbl(pkSets, "e_coef_RA_l3"),
        e_coef_RA_ke0 = purrr::map_dbl(pkSets, "e_coef_RA_ke0"),
        e_coef_RA_ka = purrr::map_dbl(pkSets, "e_coef_RA_ka"),
        p_coef_RAslow_l1 = purrr::map_dbl(pkSets, "p_coef_RAslow_l1"),
        p_coef_RAslow_l2 = purrr::map_dbl(pkSets, "p_coef_RAslow_l2"),
        p_coef_RAslow_l3 = purrr::map_dbl(pkSets, "p_coef_RAslow_l3"),
        p_coef_RAslow_ka = purrr::map_dbl(pkSets, "p_coef_RAslow_ka"),
        e_coef_RAslow_l1 = purrr::map_dbl(pkSets, "e_coef_RAslow_l1"),
        e_coef_RAslow_l2 = purrr::map_dbl(pkSets, "e_coef_RAslow_l2"),
        e_coef_RAslow_l3 = purrr::map_dbl(pkSets, "e_coef_RAslow_l3"),
        e_coef_RAslow_ke0 = purrr::map_dbl(pkSets, "e_coef_RAslow_ke0"),
        e_coef_RAslow_ka = purrr::map_dbl(pkSets, "e_coef_RAslow_ka")
      ))
    parameters <- t(parameters)
    openxlsx::addWorksheet(wb, paste(drug,"PK"))
    openxlsx::writeData(wb, sheet = sheet, parameters, rowNames=TRUE)
    sheet <- sheet + 1
  }
  outputComments("Saving Workbook")
  openxlsx::saveWorkbook(wb, xlsxfileName, overwrite = TRUE)

  outputComments("Creating e-mail")
  bodyText <- generateBodyText(recipient, values, ageUnit, weightUnit, heightUnit, url, values$comments)

  return(list(
    title = title,
    bodyText = bodyText,
    pptxfileName = pptxfileName,
    xlsxfileName = xlsxfileName,
    pngfileName = pngfileName
    )
  )
}

 generateBodyText <- function(recipient, values, ageUnit, weightUnit, heightUnit, url, comments = ""){
  return(paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear ",htmltools::htmlEscape(gsub("@", " at ",as.character(recipient))),":<p>&nbsp;</p>",
    "<p>Here is the simulation you requested from stanpumpR on ", Sys.Date(),".</p><p>&nbsp;</p>",
    "<p>The simulation is for a ",values$age / values$ageUnit, " ", ageUnit, "-old ",htmltools::htmlEscape(values$sex),
    " weighing ", values$weight / values$weightUnit, " ",weightUnit,
    " and ", values$height / values$heightUnit, " ", heightUnit, " tall.</p><p>&nbsp;</p>",
    plotTimeText(values$maximum, values$timeUnit),
    if (nchar(trimws(comments)) > 0) paste0("<p>Additional comments: ", htmltools::htmlEscape(comments), "</p><p>&nbsp;</p>") else "",
    "<p>You should be able to reload the file from ",
    "<a href=\"",htmltools::htmlEscape(url, attribute = TRUE),"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>If you have any questions or suggestions, please just reply to this e-mail. This is an early release of stanpumpR. ",
    "If you encounter any errors or crashes, please also contact me at steven.shafer@stanford.edu.</p><p>&nbsp;</p>",
    "<p>Thank you for using stanpumpR.</p><p>&nbsp;</p>",
    "<p>Sincerely,</p><p>&nbsp;</p>",
    "<p>Steve Shafer</p><p>&nbsp;</p>",
    "<p>PS: stanpumpR is an open-source program. The code is freely available at  ",
    "<a href=\"https://www.github.com/StevenLShafer/stanpumpR\">GitHub</a>.</p>",
    "<p>Collaborators are particularly needed to \"own\" individual drug libraries and keep the library up-to-date with the ",
    "pharmacokinetic literature. ",
    "If you are interested in collaborating on stanpumpR, please contact me at steven.shafer@stanford.edu",
     "</p><p>&nbsp;</p>",
    "</div></body></html>"
  ))
 }

# The plot's time settings as rows for the Covariates sheet: the unit the plot
# was shown in and how long it ran, in that unit and in minutes.  The length
# is the plot's own, which may run past the Max time chosen when a dose falls
# near the end.  No maximum (a caller that predates time units) adds nothing.
plotTimeSettings <- function(maximum, timeUnit = "minutes")
{
  if (is.null(maximum)) return(NULL)
  if (is.null(timeUnit)) timeUnit <- "minutes"
  data.frame(
    Covariate = c("Time units", "Max time", "Max time (minutes)"),
    Value = c(timeUnit, formatElapsed(maximum, timeUnit), plainNumber(maximum))
  )
}

# The same for the email body: how long the plot runs and, when it was not in
# minutes, that the workbook's times are minutes with the unit beside them.
plotTimeText <- function(maximum, timeUnit = "minutes")
{
  if (is.null(maximum)) return("")
  if (is.null(timeUnit)) timeUnit <- "minutes"
  paste0(
    "<p>The plot runs for ", formatElapsed(maximum, timeUnit), ".",
    if (timeUnit != "minutes")
      paste0(" Times in the workbook are in minutes, each followed by the same time in ",
             timeUnit, ".")
    else "",
    "</p><p>&nbsp;</p>"
  )
}
