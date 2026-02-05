# Send a copy of the current plot to the designated recipient
sendSlide <- function(
  values,
  recipient,
  plotObject,
  allResults,
  plotResults,
  isShinyLocal,
  slide,
  drugs,
  drugDefaults,
  email_username,
  email_password
)
{
  tryCatchLog::tryCatchLog({
  prevEcho <- options("ECHO_OUTPUT_COMMENTS" = TRUE)
  on.exit(options("ECHO_OUTPUT_COMMENTS" = prevEcho[[1]]))

  outputComments(paste("Sending email to", recipient))

  if (missing(email_username) || is.null(email_username)) {
    stop("email username missing")
  }
  if (missing(email_password) || is.null(email_password)) {
    stop("email password missing")
  }

  emailData <- generateEmail(values, recipient, plotObject, allResults, plotResults, slide, drugs, drugDefaults)

  outputComments("Sending email")
  email <- mailR::send.mail(
    from = paste0("stanpumpR <", email_username, ">"),
    to = recipient,
    subject = emailData$title,
    body = emailData$bodyText,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 587,
      user.name = email_username,
      passwd = email_password,
      ssl = TRUE),
    attach.files = c(
      emailData$pptxfileName,
      emailData$xlsxfileName
    ),
    authenticate = TRUE
  )
  unlink(emailData$pptxfileName)
  unlink(emailData$xlsxfileName)
  outputComments("Leaving sendMail()")
  })
  return(emailData$pngfileName)
}

generateEmail <- function(values, recipient, plotObject, allResults, plotResults, slide, drugs, drugDefaults) {
  title = values$title
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
  pngfileName <- paste0("Slides/Preview.", slide, ".", TIMESTAMP, ".png")

  outputComments("Starting ggexport()")
  ggpubr::ggexport(
    plotObject +
      ggplot2::labs(title = "", caption = NULL, x = NULL, y = NULL) +
      ggplot2::theme(
        strip.text.y = ggplot2::element_text(size = 6, angle = 180),
        axis.text.y = ggplot2::element_text(size = 6),
        axis.text.x = ggplot2::element_text(size = 4),
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size=4),
        legend.title = ggplot2::element_text(color="darkblue", size=6, face="bold")
      ),
    filename = pngfileName,
    resolution = 72,
    height = 144,
    width = 240,
    pointsize = 4,
    verbose = FALSE
  )
  pngfileName <- gsub(".png","001.png",pngfileName) #Weird!

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
      "Sex"
    ),
    Value = c(
      values$age / values$ageUnit,
      ageUnit,
      values$weight / values$weightUnit,
      weightUnit,
      values$height / values$heightUnit,
      heightUnit,
      values$sex
    ))
  outputComments("Writing covariates")
  openxlsx::addWorksheet(wb, "Covariates")
  openxlsx::writeData(wb, sheet = 1, covariates)

  outputComments("Writing dose table")
  openxlsx::addWorksheet(wb, "Dose Table")
  openxlsx::writeData(wb, sheet = 2, DT)

  outputComments("Writing simulation results")
  openxlsx::addWorksheet(wb, "Simulation Results")
  openxlsx::writeData(wb, sheet = 3, allResults)

  outputComments("Writing results for plotting")
  openxlsx::addWorksheet(wb, "Results for Plotting")
  openxlsx::writeData(wb, sheet = 4, plotResults)

  outputComments("Writing PK parameters")
  sheet = 5
  for (drug in sort(unique(as.character(DT$Drug))))
  {
    cat("Drug = ", drug, "\n")
    thisDrug <- which(drugDefaults$Drug == drug)
    cat("thisDrug = ", thisDrug, "\n")

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
        e_coef_IN_ka = purrr::map_dbl(pkSets, "e_coef_IN_ka")
      ))
    parameters <- t(parameters)
    openxlsx::addWorksheet(wb, paste(drug,"PK"))
    openxlsx::writeData(wb, sheet = sheet, parameters, rowNames=TRUE)
    sheet <- sheet + 1
  }
  outputComments("Saving Workbook")
  openxlsx::saveWorkbook(wb, xlsxfileName, overwrite = TRUE)

  outputComments("Creating e-mail")
  bodyText <- generateBodyText(recipient, values, ageUnit, weightUnit, heightUnit, url)

  return(list(
    title = title,
    bodyText = bodyText,
    pptxfileName = pptxfileName,
    xlsxfileName = xlsxfileName,
    pngfileName = pngfileName
    )
  )
}

 generateBodyText <- function(recipient, values, ageUnit, weightUnit, heightUnit, url){
  return(paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear ",gsub("@", " at ",as.character(recipient)),":<p>&nbsp;</p>",
    "<p>Here is the simulation you requested from stanpumpR on ",Sys.Date(),".</p><p>&nbsp;</p>",
    "<p>The simulation is for a ",values$age / values$ageUnit, " ", ageUnit, "-old ",values$sex,
    " weighing ", values$weight / values$weightUnit, " ",weightUnit,
    " and ", values$height / values$heightUnit, " ", heightUnit, " tall.</p><p>&nbsp;</p>",
    "<p>You should be able to reload the file from ",
    "<a href=\"",url,"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>If you have any questions or suggestions, please just reply to this e-mail. This is an early release of stanpumpR. ",
    "If you encounter any errors or crashes, please also contact me at steven.shafer@stanford.edu.</p><p>&nbsp;</p>",
    "<p>Thank you for using stanpumpR.</p><p>&nbsp;</p>",
    "<p>Sincerely,</p><p>&nbsp;</p>",
    "<p>Steve Shafer</p><p>&nbsp;</p>",
    "<p>PS: stanpumpR is an open-source program. The code is freely available at  ",
    "<a href=\"https://www.github.net/StevenLShafer/stanpumpR\">GitHub</a>.</p>",
    "<p>Collaborators are particularly needed to \"own\" individual drug libraries and keep the library up-to-date with the ",
    "pharmacokinetic literature. ",
    "If you are interested in collaborating on stanpumpR, please contact me at steven.shafer@stanford.edu",
     "</p><p>&nbsp;</p>",
    "</div></body></html>"
  ))
 }
