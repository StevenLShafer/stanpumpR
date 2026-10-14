# timeFormat: the format the Time strings are in (timeFormat() in
# R/utils-time.R).  It is stamped on the widget, and rhandsontable sends the
# widget's settings back with every edit, so the server can tell an edit made
# in a grid drawn before a change of time unit from one made after it.
# drugChoices: the drug names offered in the Drug column's autocomplete, which
#   depends on the "Show illicit drugs" opt-in (visibleDrugNames()); NULL offers
#   every drug, as it always did.  The per-drug unit lists still come from the
#   full drugDefaults, so a drug already in the table (e.g. restored from a
#   bookmark) keeps its units even when the opt-in would hide it from the list.
# The Drug column shows the names of illicit drugs (ILLICIT_DRUG_CATEGORY) in
#   red, read in the renderer from the drug_defaults global the UI injects.
createHOT <- function(doseTable, drugDefaults, timeFormat = NULL, drugChoices = NULL)
{
  if (is.null(drugChoices)) drugChoices <- drugDefaults$Drug
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
      source = sortDrugNames(drugChoices),
      strict = TRUE,
      halign = "htLeft",
      valign = "vtMiddle",
      allowInvalid = FALSE,
      # Draw the cell as usual (the autocomplete arrow), then colour an illicit
      # drug's name red.  The illicit set is derived once from the drug_defaults
      # global the UI injects (app_ui()), by Category.
      renderer = htmlwidgets::JS(paste0(
        "function(instance, td, row, col, prop, value, cellProperties) {",
        "  Handsontable.renderers.AutocompleteRenderer.apply(this, arguments);",
        "  try {",
        "    if (!window._illicitDrugSet) {",
        "      window._illicitDrugSet = {};",
        "      if (window.drug_defaults) {",
        "        window.drug_defaults.forEach(function(d) {",
        "          if (d.Category === '", ILLICIT_DRUG_CATEGORY, "') window._illicitDrugSet[d.Drug] = true;",
        "        });",
        "      }",
        "    }",
        "    td.style.color = (value && window._illicitDrugSet[value]) ? '#C00000' : '';",
        "  } catch (e) {}",
        "  return td;",
        "}"
      ))
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
