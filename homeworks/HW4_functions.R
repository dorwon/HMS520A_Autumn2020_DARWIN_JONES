# Just the functions from HW4

library(dplyr)
library(broom)
library(tidyr)

# 1: create a summarise_group_data function which returns a
# dataframe with group_id columns and summary statistics


summarise_group_data <- function(data, group_id, obs, fun, ...) {
  
  #check if group_id and obs are strings
  #if(!is.character(group_id) | !is.character(obs)) {
  # stop("group_id and obs must be character type for this function")
  #}
  
  #create output table
  my_summary <- data %>% 
    group_by(.dots = group_id) %>% 
    summarise_at(.vars = obs, .funs = fun)
  
  #rename columns to look nicer
  names(my_summary) <- c(group_id, obs)
  
  # return table
  return(my_summary)
  
}

# problem 2 regress group data

# this function makes a formula from strings (independant var, dep vars)
# and has an include intercept option

create_formula <- function(ind_var, dep_var, intercept = TRUE) {
  
  # concatanate variables, add symbols (+, ~), convert to formula
  my_form <- append(ind_var, dep_var)
  
  len <- length(dep_var)
  
  my_form <- append(my_form, "~", 1)
  
  if( len <= 1) {
    
    if(intercept == FALSE) {
      my_form <- append(my_form, "+ 0")
    }
    
    return(as.formula(paste(my_form, collapse = " ")))
  }
  
  for(i in seq(3,(len * 2 - 1),2)) {
    my_form <- append(my_form, "+", i)
  }
  
  
 #If intercept == FALSE, add 0
  if(intercept == FALSE) {
    my_form <- append(my_form, "+ 0")
  } 
  
  print(my_form)
  
  return(as.formula(paste0(my_form, collapse = "")))
  
}

# regress data
regress_group_data <- function(data, group_id, obs, cov,
                               include_intercept = TRUE, ...) {
  
  # create formula based on obs and cov
  my_formula <- create_formula(obs, cov, include_intercept)
  
  #use dpylr, broom and lm to make a data frame where rows are groups and 
  # columns are coefficients
  regres <- data %>% 
    group_by(.dots = group_id) %>% 
    do(lin_fit = tidy(lm(formula = my_formula, data = .))) %>%
    unnest(lin_fit) #this is what hadley would have wanted
  
  # reshape in the way we have to for class
  
  if(include_intercept == FALSE) {
    vars = cov
  } else {
    vars = c('(Intercept)', cov)
  }
  
  reform <- pivot_wider(regres, names_from = term, values_from = estimate) %>% 
    group_by(.dots = group_id) %>% 
    summarise_at(.vars = vars, .funs = mean, na.rm = T) 
}