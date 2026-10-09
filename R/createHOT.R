# timeFormat: the format the Time strings are in (timeFormat() in
# R/utils-time.R).  It is stamped on the widget, and rhandsontable sends the
# widget's settings back with every edit, so the server can tell an edit made
# in a grid drawn before a change of time unit from one made after it.
createHOT <- function(doseTable, drugDefaults, timeFormat = NULL)
{
  rownames(doseTable) <- 1:nrow(doseTable)
  HOT <- rhandsontable::rhandsontable(
    doseTable,
    `overflow-y` = 'scroll',
    rowHeaders = NULL,
    renderAllRows = TRUE,
    stretchH = "all"
  ) %>%
    rhandsontable::hot_col(
      col = "Drug",
      type = "autocomplete",
      source = sortDrugNames(drugDefaults$Drug),
      strict = TRUE,
      halign = "htLeft",
      valign = "vtMiddle",
      allowInvalid = FALSE
    ) %>%
    rhandsontable::hot_col(
      col = "Time",
      halign = "htRight"
    ) %>%
    rhandsontable::hot_col(
      col = "Dose",
      # Text, not numeric: a numeric column parses a pasted entry itself,
      # before hookSanitize() (inst/www/hot_funs.js) sees it, and read
      # "1,000" as 1 and "1,5" as 1.5.  The hook reads it as written.
      type = "text",
      halign = "htRight"
    ) %>%
    rhandsontable::hot_col(
      col = "Units",
      type = "dropdown",
      source = list(""),
      strict = TRUE,
      halign = "htLeft",
      valign = "vtMiddle",
      allowInvalid = FALSE
    )

  if (!is.null(timeFormat)) HOT$x$timeFormat <- timeFormatString(timeFormat)

  # Disable context menu options that aren't relevant
  HOT$x$contextMenu$items <- HOT$x$contextMenu$items[grepl("row", names(HOT$x$contextMenu$items))]

  # Set units on a per drug basis
  for (i in 1:nrow(doseTable))
  {
    cell <- list(row = i - 1, col = 3)
    if (!is.na(doseTable$Drug[i]) && doseTable$Drug[i] != "")
    {
      cell$source <- as.list(unlist(drugDefaults$Units[drugDefaults$Drug == doseTable$Drug[i]]))
    } else {
      cell$source <- as.list(c(""))
    }
    HOT$x$cell <- c(HOT$x$cell, list(cell))
  }

  HOT <- addHotHooks(
    HOT, filterKeys = TRUE, sanitize = TRUE,
    afterChange = "hookDoseTableUpdate",
    afterBeginEditing = "hookSelectAllDrugText",  # pressing Enter in a drug cell
    afterSelectionEnd = "hookSelectAllDrugText"   # clicking into a drug cell with mouse
  )

  return(HOT)
}
