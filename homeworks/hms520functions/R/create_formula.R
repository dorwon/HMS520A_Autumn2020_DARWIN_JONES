#' Create formula from variable names
#'
#' @description create a formula type object from lists of variable names
#' for use in the lm() function. Called by regress_group_data() and may not be useful for end users.
#'
#' @param ind_var string of independent(y) variable name
#' @param dep_var string or list of strings of dependent(X1, X2, ...) variable names
#' @param intercept boolean. When FALSE intercept is 0
#'
#' @return formula object for use in lm() and similar functions
#' @export
#'
#' @examples
#' create_formula(ind_var = "happiness", dep_var = c("cats","vacation_time"))
#' > "happiness ~ cats + vacation_time"
create_formula <- function(ind_var, dep_var, intercept = TRUE) {

  # concatenate variables, add symbols (+, ~), convert to formula
  my_form <- append(ind_var, dep_var)

  len <- length(dep_var)
  my_form <- append(my_form, "~", 1)



  if( len <= 1) {

    #If intercept == FALSE, add 0
    if(!intercept) {
      my_form <- append(my_form, "+ 0")
    }

    return(as.formula(paste(my_form, collapse = " ")))
  }

  for(i in seq(3,(len * 2 - 1),2)) {
    my_form <- append(my_form, "+", i)
  }

  #If intercept == FALSE, add 0
  if(!intercept) {
    my_form <- append(my_form, "+ 0")
  }

  return(as.formula(paste0(my_form, collapse = "")))

}
