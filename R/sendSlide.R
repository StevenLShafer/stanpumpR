# Send a copy of the current plot to the designated recipient
sendSlide <- function(
  prior,
  recipient,
  plotObject,
  allResults,
  plotResults,
  isShinyLocal,
  slide,
  drugs,
  email_username,
  email_password
)
{
  prevEcho <- options("ECHO_OUTPUT_COMMENTS" = TRUE)
  on.exit(options("ECHO_OUTPUT_COMMENTS" = prevEcho[[1]]))

  outputComments(paste("Sending email to", recipient))

  if (missing(email_username) || is.null(email_username)) {
    stop("email username missing")
  }
  if (missing(email_password) || is.null(email_password)) {
    stop("email password missing")
  }
  title = prior$title
  DT <- prior$DT
  url <- prior$url

  outputComments("In function sendSlide()")

  if (!file.exists("Slides")) dir.create("Slides")
  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  DATE <- format(Sys.Date(), "%m/%d/%y")
  PPTX <- read_pptx("misc/Template.pptx")
  MASTER <- "Office Theme"

  PPTX <- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <- ph_with(PPTX, title, location = ph_location_type("title"))
  PPTX <- ph_with(PPTX, dml(code = print(plotObject)), location = ph_location_type("body"))

  PPTX <- ph_with(PPTX, DATE, location = ph_location_type ("dt"))
  PPTX <- ph_with(PPTX, slide, location = ph_location_type ("sldNum"))
  PPTX <- ph_with(PPTX, "From StanpumpR", location = ph_location_type ("ftr"))
  pptxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".pptx")

  outputComments("Saving PPTX")
  print(PPTX, target = pptxfileName)
  xlsxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".xlsx")
  pngfileName <- paste0("Slides/Preview.", slide, ".", TIMESTAMP, ".png")

  outputComments("Starting ggexport()")
  ggexport(
    plotObject + labs(title = "", caption = NULL, x = NULL, y = NULL) +
      theme(strip.text.y = element_text(
              size = 6,
              angle = 180),
            axis.text.y = element_text(
              size = 6),
            axis.text.x = element_text(
              size = 4),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            legend.text=element_text(size=4),
            legend.title = element_text(color="darkblue", size=6, face="bold")
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
  if (prior$ageUnit == "1")
  {
    ageUnit <- "years"
  } else {
    ageUnit <- "months"
  }

  if (prior$weightUnit == "1")
  {
    weightUnit <- "kilograms"
  } else {
    weightUnit <- "pounds"
  }

  if (prior$heightUnit == "1")
  {
    heightUnit <- "cms"
  } else {
    heightUnit <- "inches"
  }

  outputComments("Creating workbook")
  wb <- createWorkbook("SLS")
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
      prior$age / prior$ageUnit,
      ageUnit,
      prior$weight / prior$weightUnit,
      weightUnit,
      prior$height / prior$heightUnit,
      heightUnit,
      prior$sex
    ),
    stringsAsFactors = FALSE)
  outputComments("Writing covariates")
  addWorksheet(wb, "Covariates")
  writeData(wb, sheet = 1, covariates)

  outputComments("Writing dose table")
  addWorksheet(wb, "Dose Table")
  writeData(wb, sheet = 2, DT)

  outputComments("Writing simulation results")
  addWorksheet(wb, "Simulation Results")
  writeData(wb, sheet = 3, allResults)

  outputComments("Writing results for plotting")
  addWorksheet(wb, "Results for Plotting")
  writeData(wb, sheet = 4, plotResults)

  outputComments("Writing PK parameters")
  sheet = 5
  for (drug in sort(unique(as.character(DT$Drug))))
  {
    cat("Drug = ", drug, "\n")
    thisDrug <- which(drugDefaults_global$Drug == drug)
    cat("thisDrug = ", thisDrug, "\n")

    pkSets <- drugs[[drug]]$PK
    parameters <-   as.data.frame(
      cbind(
        v1 = map_dbl(pkSets, "v1"),
        v2 = map_dbl(pkSets, "v2"),
        v3 = map_dbl(pkSets, "v3"),
        cl1 = map_dbl(pkSets, "cl1"),
        cl2 = map_dbl(pkSets, "cl2"),
        cl3 = map_dbl(pkSets, "cl3"),
        k10 = map_dbl(pkSets, "k10"),
        k12 = map_dbl(pkSets, "k12"),
        k13 = map_dbl(pkSets, "k13"),
        k21 = map_dbl(pkSets, "k21"),
        k31 = map_dbl(pkSets, "k31"),
        lambda_1 = map_dbl(pkSets, "lambda_1"),
        lambda_2 = map_dbl(pkSets, "lambda_2"),
        lambda_3 = map_dbl(pkSets, "lambda_3"),
        ke0 = map_dbl(pkSets, "ke0"),
        p_coef_bolus_l1 = map_dbl(pkSets, "p_coef_bolus_l1"),
        p_coef_bolus_l2 = map_dbl(pkSets, "p_coef_bolus_l2"),
        p_coef_bolus_l3 = map_dbl(pkSets, "p_coef_bolus_l3"),
        e_coef_bolus_l1 = map_dbl(pkSets, "e_coef_bolus_l1"),
        e_coef_bolus_l2 = map_dbl(pkSets, "e_coef_bolus_l2"),
        e_coef_bolus_l3 = map_dbl(pkSets, "e_coef_bolus_l3"),
        e_coef_bolus_ke0 = map_dbl(pkSets, "e_coef_bolus_ke0"),
        p_coef_infusion_l1 = map_dbl(pkSets, "p_coef_infusion_l1"),
        p_coef_infusion_l2 = map_dbl(pkSets, "p_coef_infusion_l2"),
        p_coef_infusion_l3 = map_dbl(pkSets, "p_coef_infusion_l3"),
        e_coef_infusion_l1 = map_dbl(pkSets, "e_coef_infusion_l1"),
        e_coef_infusion_l2 = map_dbl(pkSets, "e_coef_infusion_l2"),
        e_coef_infusion_l3 = map_dbl(pkSets, "e_coef_infusion_l3"),
        e_coef_infusion_ke0 = map_dbl(pkSets, "e_coef_infusion_ke0"),
        ka_PO = map_dbl(pkSets, "ka_PO"),
        bioavailability_PO = map_dbl(pkSets, "bioavailability_PO"),
        tlag_PO = map_dbl(pkSets, "tlag_PO"),
        ka_IM = map_dbl(pkSets, "ka_IM"),
        bioavailability_IM = map_dbl(pkSets, "bioavailability_IM"),
        tlag_IM = map_dbl(pkSets, "tlag_IM"),
        ka_IN = map_dbl(pkSets, "ka_IN"),
        bioavailability_IN = map_dbl(pkSets, "bioavailability_IN"),
        tlag_IN = map_dbl(pkSets, "tlag_IN"),
        p_coef_PO_l1 = map_dbl(pkSets, "p_coef_PO_l1"),
        p_coef_PO_l2 = map_dbl(pkSets, "p_coef_PO_l2"),
        p_coef_PO_l3 = map_dbl(pkSets, "p_coef_PO_l3"),
        p_coef_PO_ka = map_dbl(pkSets, "p_coef_PO_ka"),
        e_coef_PO_l1 = map_dbl(pkSets, "e_coef_PO_l1"),
        e_coef_PO_l2 = map_dbl(pkSets, "e_coef_PO_l2"),
        e_coef_PO_l3 = map_dbl(pkSets, "e_coef_PO_l3"),
        e_coef_PO_ke0 = map_dbl(pkSets, "e_coef_PO_ke0"),
        e_coef_PO_ka = map_dbl(pkSets, "e_coef_PO_ka"),
        p_coef_IM_l1 = map_dbl(pkSets, "p_coef_IM_l1"),
        p_coef_IM_l2 = map_dbl(pkSets, "p_coef_IM_l2"),
        p_coef_IM_l3 = map_dbl(pkSets, "p_coef_IM_l3"),
        p_coef_IM_ka = map_dbl(pkSets, "p_coef_IM_ka"),
        e_coef_IM_l1 = map_dbl(pkSets, "e_coef_IM_l1"),
        e_coef_IM_l2 = map_dbl(pkSets, "e_coef_IM_l2"),
        e_coef_IM_l3 = map_dbl(pkSets, "e_coef_IM_l3"),
        e_coef_IM_ke0 = map_dbl(pkSets, "e_coef_IM_ke0"),
        e_coef_IM_ka = map_dbl(pkSets, "e_coef_IM_ka"),
        p_coef_IN_l1 = map_dbl(pkSets, "p_coef_IN_l1"),
        p_coef_IN_l2 = map_dbl(pkSets, "p_coef_IN_l2"),
        p_coef_IN_l3 = map_dbl(pkSets, "p_coef_IN_l3"),
        p_coef_IN_ka = map_dbl(pkSets, "p_coef_IN_ka"),
        e_coef_IN_l1 = map_dbl(pkSets, "e_coef_IN_l1"),
        e_coef_IN_l2 = map_dbl(pkSets, "e_coef_IN_l2"),
        e_coef_IN_l3 = map_dbl(pkSets, "e_coef_IN_l3"),
        e_coef_IN_ke0 = map_dbl(pkSets, "e_coef_IN_ke0"),
        e_coef_IN_ka = map_dbl(pkSets, "e_coef_IN_ka")
      ),stringsAsFactors = FALSE)
    parameters <- t(parameters)
    addWorksheet(wb, paste(drug,"PK"))
    writeData(wb, sheet = sheet, parameters, rowNames=TRUE)
    sheet <- sheet + 1
  }
  outputComments("Saving Workbook")
  saveWorkbook(wb, xlsxfileName, overwrite = TRUE)

  outputComments("Creating e-mail")
  bodyText <- paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear ",gsub("@", " at ",as.character(recipient)),":<p>&nbsp;</p>",
    "<p>Here is the simulation you requested from stanpumpR on",Sys.Date(),".</p><p>&nbsp;</p>",
    "<p>The simulation is for a ",prior$age / prior$ageUnit, " ", ageUnit, "-old ",prior$sex,
    " weighing ", prior$weight / prior$weightUnit, " ",weightUnit,
    " and ", prior$height / prior$heightUnit, " ", heightUnit, "tall.</p><p>&nbsp;</p>",
    "<p>You should be able to reload the file from ",
    "<a href=\"",url,"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>If you have any questions or suggestions, please just reply to this e-mail. This is an early release of stanpumpR. ",
    "If you encounter any errors or crashes, please also contact me at steven.shafer@stanford.edu.</p><p>&nbsp;</p>",
    "<p>stanpumpR is an open-source program. You can get the code from ",
    "<a href=\"https://www.github.net/StevenLShafer/stanpumpR\">GitHub</a>.</p><p>&nbsp;</p>",
    "<p>for using stanpumpR.</p><p>&nbsp;</p>",
    "<p>Thank you for using stanpumpR.</p><p>&nbsp;</p>",
    "<p>Sincerely,</p><p>&nbsp;</p>",
    "<p>Steve Shafer</p>",
    "<p>PS: stanpumpR is an open-source program. The code is freely availabel at  ",
    "<a href=\"https://www.github.net/StevenLShafer/stanpumpR\">GitHub</a>.",
    "Collaborators are particularly needed to \"own\" individeual drug libraries and keep the library up-to-date with the ",
    "pharmacokinetic literature. ",
    "If you are interested in collaborating on stanpumpR, please contact me at steven.shafer@stanford.edu",
     "</p><p>&nbsp;</p>",
    "</div></body></html>"
  )

  outputComments("Sending email")
  email <- send.mail(
    from = email_username,
    to = recipient,
    subject = title,
    body = bodyText,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = email_username,
      passwd = email_password,
      ssl = TRUE),
    attach.files = c(
      pptxfileName,
      xlsxfileName
    ),
    authenticate = TRUE,
    send = internetConnected # Only send from server
  )
  outputComments("Leaving sendMail()")
  return(pngfileName)
}
