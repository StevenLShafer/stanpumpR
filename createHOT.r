# Create the handsontable that has the dosing information
createHOT <- function(doseTable,drugDefaults)
{
  .rowNamesDF(doseTable, make.names=TRUE) <- 1:nrow(doseTable) 
  HOT <- rhandsontable(
    doseTable,
    overflow = 'visible',
    rowHeaders = NULL,
    #width = 550,
    height = 400,
    selectCallback = FALSE
    #    highlightCol = TRUE, 
    #    highlightRow = TRUE
    ) %>%
    hot_col(
      col = "Drug",
      type = "dropdown",
      source = drugDefaults$Drug,
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
  
  # Set units on a per drug basis
  for (i in 1:nrow(doseTable))
   {
    cell <- list(row = i - 1, col = 3)
    if (doseTable$Drug[i] != "")
      {
        cell$source <- unlist(drugDefaults$Units[drugDefaults$Drug == doseTable$Drug[i]])
      } else {
        cell$source <- c("")
      }
    if (length(cell$source) == 1)
    {
      cell$readOnly <- TRUE
    } else {
      cell$readOnly <- FALSE
    }
    HOT$x$cell <- c(HOT$x$cell, list(cell))
    
    # Must disable contect menu because inset rows / delete rows reproducibly crashes
    # hot_to_r with  Error in .rowNamesDF<-: invalid 'row.names' length. Same error for
    # both insert and delete row functions
    HOT$x$contextMenu <- NULL #= list(items = c("row_above", "row_below"))
  }
  #########################
  return(HOT)
}
