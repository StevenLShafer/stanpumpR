drugUnitsExpand <- function(units) {
  strsplit(units, ",")
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}
load('R/sysdata.rda')
drugDefaults_global$Units <- drugUnitsExpand(drugDefaults_global$Units)
