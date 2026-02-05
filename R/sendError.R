# sendError: function to send me an error message whenever Shiny crashes
sendError <- function(
  url,
  errorMessage
)
{
  config <- .sprglobals$config

  TIMESTAMP <- format(Sys.time(), format = "%y%m%d-%H%M%S")
  bodyText <- paste0(
    "<html><head><style><!-- p 	{margin:0in;	font-size:12.0pt;	font-family:\"Times New Roman\",\"serif\"	} --></style>",
    "<body><div>",
    "<p>&nbsp;</p>",
    "<p>Dear ",config$maintainer_name,":<p>&nbsp;</p>",
    "<p>I have some bad news. An error was encountered with stanpumpR.</p>",
    "<p>The error is:</p>",
    "<p>", errorMessage, "</p><p>&nbsp;</p>",
    "<p>You should be able to reload the file from ",
    "<a href=\"",url,"\">stanpumpR</a>.</p><p>&nbsp;</p>",
    "<p>Sorry, </p><p>&nbsp;</p>",
    "<p>The Shiny Server Gnome</p>",
    "</div></body></html>"
  )
  email <- mailR::send.mail(
    from = config$email_username,
    to = config$maintainer_email,
    subject = "Error on Shiny Server",
    body = bodyText,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 587,
      user.name = config$email_username,
      passwd = config$email_password,
      ssl = TRUE),
    authenticate = TRUE,
    send = TRUE # internetConnected # Only send if connected to internet
  )
  return()
}
