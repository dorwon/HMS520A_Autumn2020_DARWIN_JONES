# Darwin Jones
# HMS 520
# 11-07-2020
# Homework 4

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
    return(as.formula(my_form))
  }
  
  for(i in seq(3,(len * 2 - 1),2)) {
  my_form <- append(my_form, "+", i)
  }
  
  
  #If intercept == FALSE, add 0
  if(intercept == FALSE) {
    my_form <- append(my_form, "+ 0")
  }
  
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
  reform <- pivot_wider(regres, names_from = term, values_from = estimate) %>% 
    group_by(.dots = group_id) %>% 
    summarise_at(.vars = c('(Intercept)', cov), .funs = mean, na.rm = T) 
}


# Problem 3
# work with and clean data
#get who tb report and world bank pop data
df_tb <- tidyr::who

df_pop <- read.csv(file = "API_SP.POP.TOTL_DS2_en_csv_v2_1637443.csv", skip = 4)

# follow tidy data to clean df_tb
df_tb_clean <- df_tb %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% #fix column names
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>% #2 passes to seperate key into new, type, sex, and age
  separate(sexage, c("sex", "age"), sep = 1) %>%
  select(-new, -iso2, -iso3) #remove redundant columns

# clean (pivot longer) the df_pop data, delete
df_pop_clean <- df_pop %>%
  pivot_longer(
    cols = X1960:X2020,
    names_to = "year",
    values_to = "pop",
    values_drop_na = TRUE
  ) %>%
  mutate(
    year = substring(year, 2) #remove X from year 
  ) %>%
  select(Country.Name, year, pop) %>%
  mutate(year = as.numeric(year)) #make years numeric instead of char

# inner join and compute incidence ratio of TB
# ir = cases/pop

df_tb_pop <- inner_join(df_pop_clean, df_tb_clean, by = c('year' = 'year', 
                                                          "Country.Name" = 'country'))

df_tb_inc <- df_tb_pop %>% 
  group_by(Country.Name, year) %>%
  transmute(
    total_cases = sum(cases),
    incidence_ratio = total_cases/pop
  ) %>%
  distinct()

#3.4 use the regression function to obtain coefficients
# for i_r ~ 1 + year

coefs <- regress_group_data(df_tb_inc,group_id =  "Country.Name",obs = "incidence_ratio", cov = "year",include_intercept = TRUE)
