createHOT <- function(doseTable,drugDefaults)
{
  rownames(doseTable) <- 1:nrow(doseTable)
  HOT <- rhandsontable::rhandsontable(
    doseTable,
    overflow = 'visible',
    rowHeaders = NULL,
    selectCallback = FALSE,
    renderAllRows = TRUE
  ) %>%
    rhandsontable::hot_col(
      col = "Drug",
      type = "dropdown",
      source = drugDefaults$Drug,
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
      type = "numeric",
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
    ) %>%
    rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
    rhandsontable::hot_cols(colWidths = c(120, 70, 70, 120))

  # Set units on a per drug basis
  for (i in 1:nrow(doseTable))
  {
    cell <- list(row = i - 1, col = 3)
    if (!is.na(doseTable$Drug[i]) && doseTable$Drug[i] != "")
    {
      cell$source <- unlist(drugDefaults$Units[drugDefaults$Drug == doseTable$Drug[i]])
    } else {
      cell$source <- c("")
    }
    HOT$x$cell <- c(HOT$x$cell, list(cell))
  }

  HOT <- addHotHooks(HOT, filterKeys = TRUE, sanitize = TRUE, afterChange = "hookDoseTableUpdate")

  return(HOT)
}
