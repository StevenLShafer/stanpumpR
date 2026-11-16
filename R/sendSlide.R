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
  email_username,
  email_password
)
{
  # Everything the email attaches -- the deck, its preview and a workbook of
  # the patient's covariates, doses and results -- is written to a private
  # directory of this send's own and removed whether or not the send
  # succeeds.  Until 2026-10-31 the files went to a shared Slides/ folder in
  # the working directory, named by the session's click count and the second,
  # and were removed only after a successful send.
  dir <- tempfile("stanpumpR-slide-")
  dir.create(dir, mode = "0700")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # Whether the mail server has been asked to take the message: a send that
  # fails before then is given back to the hour's limit (forgetEmailSend()),
  # which app_server() counted it against before calling here
  connecting <- FALSE
  tryCatch({
    prevEcho <- options("ECHO_OUTPUT_COMMENTS" = TRUE)
    on.exit(options("ECHO_OUTPUT_COMMENTS" = prevEcho[[1]]), add = TRUE)

    # Not the recipient: this line is echoed to the server log
    outputComments("Preparing email")

    # One string that is not blank each, as paperReviewUsageNotify() reads
    # them: "" got as far as the mail server
    configured <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
    if (!configured(email_username)) {
      stop("Email username missing")
    }
    if (!configured(email_password)) {
      stop("Email password missing")
    }

    emailData <- generateEmail(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, dir)

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
      password = email_password,
      # Bounded: R serves every session of a worker on one thread, so a send
      # that stalls stalls them all.  emayili's defaults are five attempts and
      # curl's, no limit on the transfer and five minutes to connect; in the
      # keyboard review of 2026-11-02 a stalled send held the app for more
      # than five minutes.  See EMAIL_SMTP_* in R/constants.R.
      max_times = EMAIL_SMTP_ATTEMPTS,
      connecttimeout = EMAIL_SMTP_CONNECT_TIMEOUT,
      timeout = EMAIL_SMTP_TIMEOUT
    )
    connecting <- TRUE
    smtp(msg, verbose = FALSE)

    outputComments("Leaving sendMail()")
    return(TRUE)
  }, error = function(e) {
    # The detail -- an SMTP diagnostic, a path -- goes to the server's log
    # only; the browser is told that it failed (EMAIL_FAILURE_MESSAGE), and
    # the session's debug log, which ?debug=1 shows in the browser, is told no
    # more than that.
    base::message("stanpumpR Email Slide failed: ", conditionMessage(e))
    outputComments("Email Slide failed; the detail is in the server log")
    # (an address that cannot be read is no address to give a send back to)
    if (!connecting) try(forgetEmailSend(recipient), silent = TRUE)
    EMAIL_FAILURE_MESSAGE
  })
}

# The email's three files and its body.  The files are the ones the Export
# panel also offers for download, one at a time (R/app_server.R): each is
# written by its own function below, so that a download and an email of the
# same simulation carry the same file.
#
# dir: the private directory the files are written to (sendSlide() makes and
# removes it)
generateEmail <- function(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, dir) {
  title <- exportTitle()

  outputComments("In function sendSlide()")

  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  pptxfileName <- file.path(dir, paste0("From stanpumpR.", slide, ".", TIMESTAMP, ".pptx"))
  writeSlideDeck(plotObject, pptxfileName, title = title, slide = slide)

  pngfileName <- file.path(dir, paste0("Preview.", slide, ".", TIMESTAMP, ".png"))
  writePlotImage(plotObject, pngfileName, height = height, width = width)

  xlsxfileName <- file.path(dir, paste0("From stanpumpR.", slide, ".", TIMESTAMP, ".xlsx"))
  writeWorkbook(values, allResults, plotResults, drugs, xlsxfileName)

  outputComments("Creating e-mail")
  units <- exportUnitNames(values)
  bodyText <- generateBodyText(recipient, values, units$age, units$weight, units$height, values$url, values$comments)

  return(list(
    title = title,
    bodyText = bodyText,
    pptxfileName = pptxfileName,
    xlsxfileName = xlsxfileName,
    pngfileName = pngfileName
    )
  )
}

# The title of the deck's first slide and the email's subject
exportTitle <- function(time = Sys.time()) {
  paste("stanpumpR simulation on", format(time))
}

# The name a downloaded file is given: "stanpumpR.261115-142233.pptx", the
# time it was made (the server's clock) in the email's own format.
exportFileName <- function(extension, time = Sys.time()) {
  paste0("stanpumpR.", format(time, format = "%y%m%d-%H%M%S"), ".", extension)
}

# The names of the units the patient was entered in, for the Covariates sheet
# and the email: each of values$ageUnit, $weightUnit and $heightUnit is the
# divisor that turns the stored value into the unit shown, 1 for years,
# kilograms and centimetres.
exportUnitNames <- function(values) {
  list(
    age = if (values$ageUnit == "1") "years" else "months",
    weight = if (values$weightUnit == "1") "kilograms" else "pounds",
    height = if (values$heightUnit == "1") "cms" else "inches"
  )
}

# The PowerPoint deck, written to `file` (whose name must end in .pptx): the
# whole plot on the first slide, titled `title` and numbered `slide` (an
# email numbers it by the session's sends, as it always has; a download is 1),
# then one slide per panel (panelPlots()), numbered by its place in the deck.
writeSlideDeck <- function(plotObject, file, title = exportTitle(), slide = 1) {
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

  # The first slide holds the whole plot, as it always has; with many drugs
  # its panels are too small to read.  So each panel then gets a slide of its
  # own (panelPlots()), numbered by its place in the deck.
  panels <- panelPlots(plotObject)
  for (i in seq_along(panels))
  {
    outputComments("Adding slide for", names(panels)[i])
    PPTX <- officer::add_slide(PPTX, layout = "Title and Content", master = MASTER)
    PPTX <- officer::ph_with(PPTX, names(panels)[i], location = officer::ph_location_type("title"))
    PPTX <- officer::ph_with(PPTX, rvg::dml(ggobj = panels[[i]]), location = officer::ph_location_type("body"))
    PPTX <- officer::ph_with(PPTX, DATE, location = officer::ph_location_type ("dt"))
    PPTX <- officer::ph_with(PPTX, i + 1, location = officer::ph_location_type ("sldNum"))
    PPTX <- officer::ph_with(PPTX, "From StanpumpR", location = officer::ph_location_type ("ftr"))
  }

  outputComments("Saving PPTX")
  print(PPTX, target = file)
  invisible(file)
}

# The PNG of the whole plot, written to `file` (whose name must end in .png),
# drawn as the app draws it.  renderPlot() draws at 72 dpi, `width` by
# `height` pixels, and simulationPlot() lays the plot out for that (its
# aspect ratio sets aside the threshold labels' gap in points as pixels); the
# image is the same drawing at `scale` times the pixels and the dpi, as a
# high-resolution screen shows it, so nothing moves.  Less than `scale` only
# where that would pass what the device can draw (maxPlotDevicePixels()).
#
# Until 2026-11-15 the email's PNG was the plot redrawn at 150 dpi into the
# plot's own pixels with smaller text (a previewPlot() that has gone), which
# clipped the legend of a short plot and ran the threshold labels into the
# panels; the Export panel's Image download made it worth drawing properly.
#
# limitsize = FALSE: ggsave() refuses an image over 50 inches, and at 72 dpi
# times the scale that is any plot over 3,600 of the app's pixels, which a
# tall plot of many drugs or a wide screen (MAX_PLOT_WIDTH 4,096) passes
# (CodeRabbit, PR #731).  The bound that matters is in pixels: `scale` is
# held so the image stays within the device, and simulationPlot() refuses a
# plot taller than the device draws before it gets here.
writePlotImage <- function(plotObject, file, height, width, scale = 2) {
  outputComments("Creating PNG file")
  scale <- max(1, min(scale, maxPlotDevicePixels() / max(height, width)))
  ggplot2::ggsave(
    plotObject,
    filename = file,
    dpi = 72 * scale,
    height = height * scale,
    width = width * scale,
    units = "px",
    limitsize = FALSE
  )
  invisible(file)
}

# The Excel workbook, written to `file`: the patient's covariates, the dose
# table (with the TCI rates and scheduled repeats merged in), the simulated
# concentrations, the results as plotted, and one sheet of PK parameters per
# drug.  `values` is what app_server()'s exportValues() gathers.
writeWorkbook <- function(values, allResults, plotResults, drugs, file) {
  DT <- values$DT

  outputComments("Fixing Units for export")
  units <- exportUnitNames(values)
  ageUnit <- units$age
  weightUnit <- units$weight
  heightUnit <- units$height

  # Every Time column in the workbook is in minutes, which is what the engine
  # works in and what a script reading the sheets expects.  When the plot was
  # shown in hours, days or weeks, each of those columns is followed by the
  # same times in that unit, and the covariates sheet and the email say so.
  # NULL (a caller that predates time units) means minutes.
  timeUnit <- if (is.null(values$timeUnit)) "minutes" else values$timeUnit

  outputComments("Creating workbook")
  wb <- openxlsx::createWorkbook("SLS")
  # Each covariate one cell: what the field held, its default when it held
  # nothing, and "unreadable" when it held no one value.  The genotypes come
  # from the inputs as sent, read by a model only if a drug names them, so on
  # a run of inhaled agents alone a vector reached here, and character(0)
  # left the sheet one value short of its names, which stopped the send
  # (Carbon's third UI audit, 2026-11-15, M5).
  cell <- function(x, default) {
    if (is.null(x)) return(as.character(default))
    if (is.list(x) || length(x) != 1 || is.na(x)) return("unreadable")
    as.character(x)
  }
  covariates <- data.frame(
    Covariate = c(
      "Age",
      "Age Unit",
      "Weight",
      "Weight Unit",
      "Height",
      "Height Unit",
      "Sex",
      "Pregnant",
      "Adjust weight to fat-free mass",
      "Baseline serum osmolality (mOsm/kg)",
      "Serum creatinine (mg/dL)",
      "Gestational age at birth (weeks)",
      "Serum albumin (g/dL)",
      "Serum AST (U/L)",
      "CYP2D6",
      "CYP2C19",
      "CYP2C9",
      "CYP2B6",
      "VKORC1",
      "CYP4F2",
      "Minto / Schnider models shown",
      "Hamberg model shown",
      "Antiseizure drug interactions applied"
    ),
    Value = c(
      cell(values$age / values$ageUnit, ""),
      ageUnit,
      cell(values$weight / values$weightUnit, ""),
      weightUnit,
      cell(values$height / values$heightUnit, ""),
      heightUnit,
      cell(values$sex, ""),
      if (isTRUE(values$pregnant)) "yes" else "no",
      if (isTRUE(values$adjustToFFM)) "yes" else "no",
      cell(values$osmolality, OSMOLALITY_DEFAULT),
      cell(values$creatinine, "not entered (assumed normal)"),
      cell(values$gestationalAge, "not entered (term assumed)"),
      cell(values$albumin, "not entered (each model's reference)"),
      cell(values$ast, "not entered (each model's reference)"),
      # The phenotypes and the companion models change what was simulated,
      # so the sheet records them.  (Claude Code, 2026-10-17, from an
      # external review.)
      cell(values$cyp2d6, CYP2D6_DEFAULT),
      cell(values$cyp2c19, CYP2C19_DEFAULT),
      cell(values$cyp2c9, CYP2C9_DEFAULT),
      cell(values$cyp2b6, CYP2B6_DEFAULT),
      cell(values$vkorc1, VKORC1_DEFAULT),
      cell(values$cyp4f2, CYP4F2_DEFAULT),
      if (isTRUE(values$mintoSchnider)) "yes" else "no",
      if (isTRUE(values$hamberg)) "yes" else "no",
      if (isTRUE(values$antiseizureInteractions)) "yes" else "no"
    ))
  covariates <- rbind(covariates, physiologyCovariateRows(values$physiology,
                                                         values$temperature))
  covariates <- rbind(covariates, plotTimeSettings(values$maximum, timeUnit))
  outputComments("Writing covariates")
  openxlsx::addWorksheet(wb, "Covariates")
  openxlsx::writeData(wb, sheet = 1, covariates)

  # The TCI infusion rows and the repeats of scheduled (qd/bid/tid/qid/q4h) doses
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
  # One sheet per dosed drug, then one per alternative-model companion that
  # was simulated beside it ("propofol (Schnider) PK"; R/alternativeModels.R)
  companions <- names(drugs)[vapply(drugs, isAlternativeModelEntry, logical(1))]
  for (drug in c(sort(unique(as.character(DT$Drug))), sort(companions)))
  {
    pkSets <- drugs[[drug]]$PK
    if (is.null(pkSets)) next
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
        ka_SL = purrr::map_dbl(pkSets, "ka_SL"),
        bioavailability_SL = purrr::map_dbl(pkSets, "bioavailability_SL"),
        tlag_SL = purrr::map_dbl(pkSets, "tlag_SL"),
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
        p_coef_SL_l1 = purrr::map_dbl(pkSets, "p_coef_SL_l1"),
        p_coef_SL_l2 = purrr::map_dbl(pkSets, "p_coef_SL_l2"),
        p_coef_SL_l3 = purrr::map_dbl(pkSets, "p_coef_SL_l3"),
        p_coef_SL_ka = purrr::map_dbl(pkSets, "p_coef_SL_ka"),
        e_coef_SL_l1 = purrr::map_dbl(pkSets, "e_coef_SL_l1"),
        e_coef_SL_l2 = purrr::map_dbl(pkSets, "e_coef_SL_l2"),
        e_coef_SL_l3 = purrr::map_dbl(pkSets, "e_coef_SL_l3"),
        e_coef_SL_ke0 = purrr::map_dbl(pkSets, "e_coef_SL_ke0"),
        e_coef_SL_ka = purrr::map_dbl(pkSets, "e_coef_SL_ka"),
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
    openxlsx::addWorksheet(wb, workbookSheetName(paste(drug, "PK"), names(wb)))
    openxlsx::writeData(wb, sheet = sheet, parameters, rowNames=TRUE)
    sheet <- sheet + 1
  }
  outputComments("Saving Workbook")
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
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

# One ggplot per panel of the simulation plot, for the slides that follow the
# full plot in the PowerPoint deck (generateEmail()).
#
# simulationPlot() draws every panel -- each drug, a TCI pump rate, the % MEAC
# sum, the interaction, the events -- as one facet of a single ggplot, and
# every layer's data carries the facet in its Wrap column.  Each panel's plot
# is that ggplot with every layer's data cut down to the panel's rows, so it
# is drawn exactly as it was in the full plot (same colours, bands, labels,
# recovery overlay), alone, and with no fixed aspect ratio, so that it fills
# the slide.  A layer with no rows on the panel is left out rather than kept
# empty: ggplot draws a layer whose data is empty but whose aesthetics are
# constants (the white rectangle behind the event labels, ymin 0 to ymax 1)
# on the first panel anyway, which blanked everything below 1 on every slide.
# A layer is never changed in place: the layers are ggproto objects, which
# are environments, so a copy of the plot shares them with the original,
# which the app goes on using.  The Events panel is labels only and gets no
# plot of its own.
#
# Named by the panel's first line (the drug, "propofol TCI", "% MEAC"), the
# slide's title; the strip on the panel keeps the units.  Empty when the plot
# has no facetted layer.
panelPlots <- function(plotObject)
{
  if (is.null(plotObject)) return(list())
  layerData <- lapply(plotObject$layers, function(layer) layer$data)
  wrapped <- vapply(layerData, function(d) is.data.frame(d) && "Wrap" %in% names(d), logical(1))
  if (!any(wrapped)) return(list())

  # The facets in the plot's order: the levels of the Wrap factor, kept to
  # those that some layer draws on
  wraps <- unlist(lapply(layerData[wrapped], function(d) levels(as.factor(d$Wrap))))
  drawn <- unlist(lapply(layerData[wrapped], function(d) as.character(d$Wrap)))
  wraps <- unique(wraps)
  wraps <- wraps[wraps %in% drawn & wraps != PLOT_NAME_EVENTS]

  plots <- lapply(wraps, function(wrap) {
    layers <- lapply(plotObject$layers, function(layer) {
      d <- layer$data
      if (!is.data.frame(d) || !"Wrap" %in% names(d)) return(layer)
      d <- d[as.character(d$Wrap) == wrap, , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      ggplot2::ggproto(NULL, layer, data = d)
    })
    panel <- plotObject
    panel$layers <- Filter(Negate(is.null), layers)
    # A panel fills its own slide, so the strip goes back to its full size:
    # the combined plot shrinks it to fit a short panel (stripTextSize()).
    panel + ggplot2::theme(
      aspect.ratio = NULL,
      strip.text.y = ggplot2::element_text(size = STRIP_TEXT_MAX_SIZE, angle = 270)
    )
  })
  names(plots) <- panelTitle(wraps)
  plots
}

# A panel's strip label is "drug\n(units)"; the slide title is the first line
panelTitle <- function(wrap)
{
  sub("\n.*$", "", wrap)
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

# A worksheet name Excel accepts: at most 31 characters (openxlsx refuses a
# longer one, which failed every slide with levodopaEntacaponeIntestinalGel
# or an antiseizure "(no interactions)" companion in it; adversarial UI
# review, 2026-11, finding 12), none of the characters Excel forbids, and
# not one already in `existing` (compared as Excel does, ignoring case).  A
# name cut short ends in "~" and, if that is taken, a number.
workbookSheetName <- function(name, existing = character(0)) {
  limit <- 31L
  name <- chartr("[]:*?/\\", "_______", name)
  if (nchar(name) > limit) name <- paste0(substr(name, 1, limit - 1L), "~")
  taken <- function(x) tolower(x) %in% tolower(existing)
  if (!taken(name)) return(name)
  for (i in seq_len(999L)) {
    suffix <- paste0("~", i)
    candidate <- paste0(substr(name, 1, limit - nchar(suffix)), suffix)
    if (!taken(candidate)) return(candidate)
  }
  stop("No free worksheet name for ", name)
}
