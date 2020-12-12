# I know this is bad form, but it would be super repetitive otherwise
library(dplyr)

#' Summarise Group Data
#'
#' @param data a data frame
#' @param group_id a string or vector of strings of column names to group by
#' @param obs a string or vector of strings of column names to summarise
#' @param fun a function to summarise with or a list of functions
#'
#' @param ... any additional arguments for the function
#'
#' @return a dataframe containing the summary of the obs variable
#' @export
#'
#' @examples
#' summarise_group_data(mtcars, "gear", "mpg", mean)
#' summarise_group_data(mtcars, "gear", c("disp","mpg"), mean)

summarise_group_data <- function(data, group_id = NULL, obs, fun, ...) {

  # check for valid column names  check for valid column names
  if(!valid_g_d_inputs(group_id, obs)) {
    stop("Error: group_id must be of type char or NULL and obs must be of type char")
  }

  #create output table
  my_summary <- data %>%
    group_by(.dots = group_id) %>%
    summarise_at(.vars = obs, .funs = fun)

  # OLD: renamed columns, but frankly, it looks better without when using multiple functions
  #names(my_summary) <- c(group_id, obs)

  # return table
  return(my_summary)

}

#' Summarize group data
#'
#' Identical to summarise_group_data. View documentation for that function.
#' @export
summarize_group_data <- summarise_group_data
