# Separate hour from minute in hh:ss format. Return number of minutes
# Used only in clockTimeToDelta
hourMinute <- function(x)
{
    px <- lubridate:parse_date_time(x, "HM", quiet=TRUE)
    if (!is.na(px)) px <- 60*lubridate::hour(px) + lubridate::minute(px)
    return(px)
}
