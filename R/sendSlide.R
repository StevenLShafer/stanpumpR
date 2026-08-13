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
  email_password,
  smtp_host = "smtp.gmail.com",
  smtp_port = 587,
  smtp_ssl = TRUE
)
{
  tryCatch({
    prevEcho <- options("ECHO_OUTPUT_COMMENTS" = TRUE)
    on.exit(options("ECHO_OUTPUT_COMMENTS" = prevEcho[[1]]))

    outputComments("Preparing simulation email")

    if (missing(email_username) || is.null(email_username)) {
      stop("email username missing")
    }
    if (missing(email_password) || is.null(email_password)) {
      stop("email password missing")
    }

    outputDir <- tempfile(pattern = "stanpumpr-email-")
    dir.create(outputDir, mode = "0700")
    on.exit(unlink(outputDir, recursive = TRUE, force = TRUE), add = TRUE)

    emailData <- generateEmail(
      values, recipient, plotObject, allResults, plotResults, height, width,
      slide, drugs, drugDefaults, outputDir = outputDir
    )

    outputComments("Sending email")
    message <- createSimulationEmailMessage(
      from = email_username,
      to = recipient,
      subject = emailData$title,
      html = emailData$bodyText,
      attachments = c(
        emailData$pptxfileName,
        emailData$pngfileName,
        emailData$xlsxfileName
      )
    )
    smtpScheme <- if (isTRUE(smtp_ssl) && smtp_port == 465) "smtps" else "smtp"
    curl::send_mail(
      mail_from = email_username,
      mail_rcpt = recipient,
      message = message,
      smtp_server = sprintf("%s://%s:%d", smtpScheme, smtp_host, smtp_port),
      use_ssl = if (isTRUE(smtp_ssl)) "force" else "no",
      username = email_username,
      password = email_password,
      verbose = FALSE
    )
    outputComments("Leaving sendMail()")
    return(TRUE)
  }, error = function(e) {
    # SMTP/library exceptions can contain internal host, account, or transport
    # details. Do not log the exception or return it to a client.
    return("The simulation email could not be sent. Please try again later.")
  })
}

createSimulationEmailMessage <- function(from, to, subject, html, attachments) {
  if (!isEmailRecipientValid(from) || !isEmailRecipientValid(to)) {
    stop("Invalid email header address.")
  }
  if (!is.character(subject) || length(subject) != 1L || grepl("[\r\n]", subject)) {
    stop("Invalid email subject.")
  }
  if (!all(file.exists(attachments))) stop("Email attachment missing.")

  boundary <- paste0("stanpumpr-", paste(sample(c(letters, 0:9), 32L, TRUE), collapse = ""))
  wrapBase64 <- function(x) paste(strwrap(x, width = 76L), collapse = "\r\n")
  encodeFile <- function(path) {
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    wrapBase64(base64enc::base64encode(raw))
  }
  mimeTypes <- c(
    pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    png = "image/png",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  parts <- c(
    paste0(
      "--", boundary, "\r\n",
      "Content-Type: text/html; charset=UTF-8\r\n",
      "Content-Transfer-Encoding: base64\r\n\r\n",
      wrapBase64(base64enc::base64encode(charToRaw(enc2utf8(html)))), "\r\n"
    ),
    vapply(attachments, function(path) {
      ext <- tolower(tools::file_ext(path))
      mime <- unname(mimeTypes[[ext]])
      if (is.null(mime)) mime <- "application/octet-stream"
      filename <- basename(path)
      paste0(
        "--", boundary, "\r\n",
        "Content-Type: ", mime, "; name=\"", filename, "\"\r\n",
        "Content-Disposition: attachment; filename=\"", filename, "\"\r\n",
        "Content-Transfer-Encoding: base64\r\n\r\n",
        encodeFile(path), "\r\n"
      )
    }, character(1)),
    paste0("--", boundary, "--\r\n")
  )
  paste0(
    "From: stanpumpR <", from, ">\r\n",
    "To: ", to, "\r\n",
    "Subject: ", subject, "\r\n",
    "MIME-Version: 1.0\r\n",
    "Content-Type: multipart/mixed; boundary=\"", boundary, "\"\r\n\r\n",
    paste(parts, collapse = "")
  )
}

generateEmail <- function(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, drugDefaults,
                          outputDir) {
  title <- paste("stanpumpR SIMULATION (not a patient record) on", format(Sys.time()))
  DT <- values$DT
  url <- values$url

  outputComments("In function sendSlide()")

  if (missing(outputDir) || !is.character(outputDir) || length(outputDir) != 1L || !dir.exists(outputDir)) {
    stop("A private export directory must be supplied.")
  }
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
  PPTX <- officer::ph_with(
    PPTX, "stanpumpR SIMULATION — NOT A PATIENT RECORD",
    location = officer::ph_location_type("ftr")
  )
  pptxfileName <- file.path(outputDir, "stanpumpR-simulation.pptx")

  outputComments("Saving PPTX")
  print(PPTX, target = pptxfileName)

  xlsxfileName <- file.path(outputDir, "stanpumpR-simulation.xlsx")

  outputComments("Creating PNG file")

  pngfileName <- file.path(outputDir, "stanpumpR-preview.png")
  ggplot2::ggsave(
    plotObject +
      ggplot2::labs(caption = "stanpumpR SIMULATION — NOT A PATIENT RECORD") +
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
  openxlsx::writeData(
    wb, sheet = 1, x = "stanpumpR SIMULATION — NOT A PATIENT RECORD",
    startCol = 4, startRow = 1
  )

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
    "<p><strong>SIMULATION — NOT A PATIENT RECORD</strong></p><p>&nbsp;</p>",
    "<p>Here is the simulation you requested from stanpumpR on ", Sys.Date(),".</p><p>&nbsp;</p>",
    "<p>The simulation is for a ",values$age / values$ageUnit, " ", ageUnit, "-old ",htmltools::htmlEscape(values$sex),
    " weighing ", values$weight / values$weightUnit, " ",weightUnit,
    " and ", values$height / values$heightUnit, " ", heightUnit, " tall.</p><p>&nbsp;</p>",
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
