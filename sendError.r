# sendError: function to send me an error message whenever Shiny crashes
sendError <- function(
  url,
  errorMessage 
)
{
  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  bodyText <- paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear Steve:<p>&nbsp;</p>",
    "<p>I have some bad news. An error was encountered with stanpumpR.", 
    "The error is:</p>", 
    "<p>", errorMessage, "</p><p>&nbsp;</p>",
    "<p>You should be able to reload the file from ",
    "<a href=\"",url,"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>Sorry, </p><p>&nbsp;</p>",
    "<p>The Shiny Server Gnome</p>",
    "</div></body></html>"
  )
  email <- send.mail(
    from = "stanpumpR@gmail.com",
    to = "stanpumpR@gmail.com",
    subject = "Error on Shiny Server",
    body = bodyText,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com", 
      port = 465,
      user.name = "stanpumpR@gmail.com",
      passwd = config$password,
      ssl = TRUE),
    authenticate = TRUE,
    send = internetConnected # Only send if connected to internet
  )
  return()
}

