# Separate hour from minute in hh:ss format. Return number of minutes
hourMinute <- function (x)
{
  x <- strsplit(x,":")
  return(unlist(as.numeric(x[[1]][1]) * 60 + as.numeric(x[[1]][2])))
}
