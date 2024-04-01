#' Pseudo-function to re-export \strong{dplyr}'s common functions. 
#' 
#' @name dplyr functions
#'
#' @importFrom dplyr select rename mutate filter arrange distinct summarise 
#'     do group_by ungroup rowwise do left_join inner_join everything bind_rows 
#'     pull as_tibble tibble if_else slice across relocate sym join_by between
#' 
NULL


#' Pseudo-function to re-export \strong{magrittr}'s pipe. 
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


#' @importFrom dplyr db_has_table
#' 
#' @export
dplyr::db_has_table


# #' @importFrom dplyr db_drop_table
# #' 
# #' @rdname db_list_tables
# #' 
# #' @export
# dplyr::db_drop_table
