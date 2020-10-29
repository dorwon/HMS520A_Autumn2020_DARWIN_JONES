# Darwin Jones
# HMS 520
# 10-28-2020
# Homework 3

# Problem 1: Create a my_sum function with argument x and na.rm


my_sum <- function(x, na.rm = TRUE) {
  
  # Check if x is an atomic vector of ints or doubles
  if(!is.atomic(x) | !(is.double(x) | is.integer(x))) {
    stop("Error: x must be an atomic vector which contains integers or doubles")
  }
  
  # if na.rm is true, remove nas
  if(na.rm == TRUE){
    x <- x[!is.na(x)]
  }
  # if the vector is length 0, return 0 and throw error
  if(length(x) == 0){
    print("Warning: x is length 0")
    return(0)
  }
  
  # compute sum using for loop
  mySum = 0
   for(i in x) {
     mySum = mySum + i
   }
  
  return(mySum)
}

# Make a my_mean function. use my_sum in the function

my_mean <- function(x, na.rm = TRUE) {
  
  my_sum(x, na.rm = na.rm) / length(x)
}

# create a my_var function which computes variance

my_var <- function(x, na.rm = TRUE) {
  
  # find the mean
  mn <- rep(my_mean(x, na.rm = na.rm), length(x))
  
  #find and square the difference between the mean and each data point
  dif <- (mn - x)^2
  
  #sum the result and divide by the length of x - 1
  sum(dif) / (length(x) - 1)
  
}

# Problem 2 fibbonacci function which returns a vector with the corresponding amount of the fibbonacci sequence (a_0 = 0)

fib <- function(n, starting_values = c(0,1)) {
  
  # check for an integer > 0
  if(!is.integer(n) | n < 0) {
    print(!is.integer(n) | n < 0)
    stop("error: n must be an integer")
  }
  
  # If n = 0:1, return predecided A_0 A_1
  if(n < 2) {
    return(starting_values[1:(n+1)])
  }
  
  # calculate fibbonacci using while loop
  fib <- starting_values
  
  while(n > 1) {
    fib <- c(fib, sum(tail(fib, n = 2)))
    n = n - 1
  }
  
  return(fib)
}

# create a count function which can take in 2 vectors and return the counts of each number
count <- function(vec, x) {
  
  # throw error if vec is not atomic
  if(!is.atomic(vec)) {
    stop("vec must be an atomic vector")
  }
  
  # See the number of times each value in x is in vec and return that vector
  counts <- rep(0L, times = length(x))
  
  for(i in x) {
    counts[match(i, x)] <- length(vec[vec == i])
  }
  
  return(counts)
}

# Create a my_unique function with arguements vec
# and return_counts == FALSE

my_unique <- function(vec, return_counts = FALSE) {
  
  # If return_counts == false, use existing unique function
  if(return_counts == FALSE) {
    
    unique(vec)
 
    } else {
    
      list(unique(vec), count(vec = vec, x = unique(vec)))
      
  }
}

# Create binomial_fun with arguments s (sum of events) and n (samples)
# and return the estimation of the mean and standard deviation of 
# the estimation

binomial_fun <- function(s, n) {
  
  #calulate p^
  p_hat <- (1 / n) * s
  
  # calculate sqrt of v_hat, the standard deviation
  v_hat <- (p_hat * (1 - p_hat)) / n
  
  std_dev <- sqrt(v_hat)
  
  return(c(mean = p_hat, standard_dev = std_dev))
  
}

# create polynomial fit, starting with a linear fit

lin_fit <- function(x, y) {
  lm(y~x)$coefficients
}

# now make a poly fit function

poly_fit <- function(x, y, degree = 1) {

  model <- lm(y ~ poly(x, degree, raw = TRUE))
  
  coefficients(model)
}

# create a poly_pred with x and coeff as arguments
# and returns the dependent vectors

poly_pred <- function(x, coef) {
  print(coef)
  poly(x, degree = (length(coef)-1), coefs = coef, raw = TRUE, simple = TRUE)
}