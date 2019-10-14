# Create the handsontable that has the dosing information
# Note to Dean: This is the routine that creates the handsontable. I will still need this
# in the server. However, as entries are changed by the user, I want the JavaScript validation
# code keep the handsontable in a format that is compatible with this. 

# The key thing is having the dose units agree with the name of the drug.

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
  
  # Note to Dean: This is where I set individual drug dosing units. They come from the 
  # drugDefaults table. drugDefaults$Unit consists of a list of possible dosing units. It is
  # unique for each drug. rHandsontable does not provide a routine for setting unique
  # select input choices for each cell, but I was able to figure out how to do it from
  # the source code. This will need to be implemented in a JavaScript version of handontable,
  # so that when the user enters a new line, or changes the drug name, a new set of dose
  # units choices are put into the list of available dosing units in column 4, the dose 
  # units  

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
