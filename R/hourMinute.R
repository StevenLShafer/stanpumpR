library(lubridate)
# Separate hour from minute in hh:ss format. Return number of minutes
# Used only in clockTimeToDelta
hourMinute <- function(x)
{
    px <- parse_date_time(x, "HM", quiet=TRUE)
    if (!is.na(px)) px <- 60*hour(px) + minute(px)
    return(px)
}
