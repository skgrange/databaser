#' Squash the global variable notes when building a package. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(".", "table_name", "name", "type", "values", "row_count")
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
