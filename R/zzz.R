#' Squash the global variable notes when building a package. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "table_name", "name", "type", "values", "row_count", "size", "datname"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
