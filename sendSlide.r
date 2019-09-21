# Send a copy of the current plot to the designated recipient
sendSlide <- function(
  recipient, 
  title, 
  plotObject, 
  DT, 
  allResults, 
  plotResults, 
  isShinyLocal,
  url,
  slide
)
{
  if (!file.exists("Slides")) dir.create("Slides")
  cat("In function sendSlide()\n")
  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  DATE <- format(Sys.Date(), "%m/%d/%y")
  PPTX <- read_pptx("Template.pptx")
  MASTER <- "Office Theme"
  
  PPTX <- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <- ph_with(PPTX, title, location = ph_location_type("title"))
  PPTX <- ph_with(PPTX, dml(code = print(plotObject)), location = ph_location_type("body"))
  
  PPTX <- ph_with(PPTX, DATE, location = ph_location_type ("dt"))
  PPTX <- ph_with(PPTX, slide, location = ph_location_type ("sldNum"))
  PPTX <- ph_with(PPTX, "From StanpumpR", location = ph_location_type ("ftr"))
  pptxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".pptx")
  print(PPTX, target = pptxfileName)
  xlsxfileName <- paste0("Slides/From stanpumpR.", slide, ".", TIMESTAMP, ".xlsx")
  #  pngfileName <- paste0("Slides/Preview.", slide, ".", TIMESTAMP, ".png")
  wb <- createWorkbook("Fred")
  addWorksheet(wb, "Dose Table")
  addWorksheet(wb, "Simulation Results")
  addWorksheet(wb, "Results for Plotting")
  writeData(wb, sheet = 1, DT)
  writeData(wb, sheet = 2, allResults)
  writeData(wb, sheet = 3, plotResults)
  saveWorkbook(wb, xlsxfileName, overwrite = TRUE)
  #  write.xlsx(allResults, file = xlsxfileName)
  bodyText <- paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear ",gsub("@", " at ",as.character(recipient)),":<p>&nbsp;</p>",
    "<p>Here is the simulation you requested from stanpumpR on",Sys.Date(),".</p><p>&nbsp;</p>",
    "<p>You should be able to reload the file from ",
    "<a href=\"",url,"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>If you have any questions or suggestions, please just reply to this e-mail. This is an early release. ",
    "If you encounter any errors or crashes, please also contact me at steven.shafer@stanford.edu.</p><p>&nbsp;</p>",
    "<p>Thank you for using stanpumpR.</p><p>&nbsp;</p>",
    "<p>Sincerely,</p><p>&nbsp;</p>",
    "<p>Steve Shafer</p>",
    "</div></body></html>"
  )
  
  email <- send.mail(
    from = "stanpumpR@gmail.com",
    to = recipient,
    subject = "Figure and Data from stanpumpR",
    body = bodyText,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com", 
      port = 465,
      user.name = "stanpumpR@gmail.com",
      passwd = config$password,
      ssl = TRUE),
    attach.files = c(
      pptxfileName, 
      xlsxfileName
    ),
    authenticate = TRUE,
    send = internetConnected # Only send from server
  )
  return()
}
