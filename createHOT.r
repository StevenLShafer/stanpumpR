# Create the handsontable that has the dosing information
createHOT <- function(doseTable)
{
  HOT <- rhandsontable(
    doseTable,
    overflow = 'visible',
    rowHeaders = NULL,
    #      width = 550,
    height = 220,
    selectCallback = TRUE
    #    highlightCol = TRUE, 
    #    highlightRow = TRUE
  ) %>%
    hot_col(
      col = "Drug",
      type = "dropdown",
      source = drugList,
      strict = TRUE,
      halign = "htLeft",
      valign = "vtMiddle",
      allowInvalid=FALSE
    ) %>%
    hot_col(
      col = "Time",
      type="autocomplete",
      halign = "htRight",
      allowInvalid = TRUE,
      strict = FALSE
    ) %>%
    hot_col(
      col = "Dose",
      type = "autocomplete",
      halign = "htRight",
      allowInvalid = TRUE,
      strict = FALSE
    ) %>%
    hot_col(
      col = "Units",
      type = "dropdown",
      source = units,
      strict = TRUE,
      halign = "htLeft",
      valign = "vtMiddle",
      allowInvalid=FALSE
    ) %>%
    #        hot_validate_numeric(cols = c("Minute"), min = 0, max = 1440) %>%
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
    hot_rows(rowHeights = 10) %>%
    hot_cols(colWidths = c(120,70,70,120))
  HOT$sizingPolicy$viewer$padding <- 20 # no effect
  HOT$sizingPolicy$browser$padding <- 20 # no effect
  HOT$sizingPolicy$viewer$defaultHeight <- 80 # no effect
  return(HOT)
}
