#' Regress group data
#'
#' @param data a dataframe
#' @param group_id a string or list of strings representing the group id column
#' @param obs a string representing the column name of the observed variable
#' @param cov a string or list of strings representing covariates
#' @param include_intercept boolean. whether to include intercept, TRUE by default
#' @param ... other arguments
#'
#' @return dataframe containing parameters for linear regression
#' @export
#'
#' @examples
#' regress_group_data(mtcars, "gear", "mpg", "disp")
regress_group_data <- function(data, group_id = NULL, obs, cov,
                               include_intercept = TRUE, ...) {

  # check for valid column names of type char
  if(!valid_g_d_inputs(group_id, obs)) {
    stop("Error: group_id must be of type char or NULL, obs and cov must be of type char")
  }

  # create formula based on obs and cov
  my_formula <- create_formula(obs, cov, include_intercept)


  #use dpylr, broom and lm to make a data frame where rows are groups and
  # columns are coefficients
  regres <- data %>%
    group_by(.dots = group_id) %>%
    do(lin_fit = broom::tidy(lm(formula = my_formula, data = .))) %>%
    tidyr::unnest(lin_fit) #this is what hadley would have wanted

  if(!include_intercept) {
    regres$`(Intercept)` <- NA
  }

  # reshape in the way we have to for class
  reform <- tidyr::pivot_wider(regres, names_from = term, values_from = estimate) %>%
    group_by(.dots = group_id) %>%
    summarise_at(.vars = c('(Intercept)', cov), .funs = mean, na.rm = T)
}
