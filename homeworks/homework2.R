# Darwin Jones
# HMS 520
# Learning activity 2

# Problem 1

# Create a double atomic vector, with values c(1, 2, ..., 100)?
a1 <- seq(1,100,1)

# Create a double atomic vector with all elements to be 10 and length to be 100.
a2 <- seq(from = 10, to = 10, length.out = 100)

# Create a double atomic vector with elements c(1, 2, 3, 4, 5) repeat 10 times. E.g., c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, ...).
a3 <- as.double(rep(1:5,10))

# Create a double atomic vector with elements c(1, 2, 3, 4, 5) each repeat 10 times. E.g., c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, ...).
a4 <- as.double(rep(1:5, each = 10))

# Create a double atomic vector with elements c(0, 0.01, 0.02, ..., 1).
a5 <- seq(0,1,0.01)


# Problem 2

# What is the type of rivers? (Atomic vector or list?) If it is an atomic vector, 
# what is the element type of the vector? (double, integer, logic, or string?)
print("rivers is a double atomic vector")

# Create an atomic vector that contains the length, sum, mean, median, variance, standard deviation,
# minimum and maximum of log-transformed rivers. Make sure each element in the vector has a reasonable name.
rivs_l <- log(rivers)

loged_summary <- c(n = length(rivs_l), sum = sum(rivs_l), mean = mean(rivs_l), 
             med = median(rivs_l), var = var(rivs_l), st_dev = sd(rivs_l),
             min = min(rivs_l), max = max(rivs_l))

# Trim 10 largest and 10 smallest elements in rivers and create the same summary vector in 2.
trimmed <- sort(rivers)[11:(length(rivers)-10)]

trimmed_summary <- c(n = length(trimmed), sum = sum(trimmed), mean = mean(trimmed), 
             med = median(trimmed), var = var(trimmed), st_dev = sd(trimmed),
             min = min(trimmed), max = max(trimmed))

#Problem 3 In this problem, we try to create a list, modify, add, and delete the items in the list.
#Create a list u, with two items x = c(5, 6, 7, 8) and y = c("a", "b", "c", "d").
u <- list(x = c(5,6,7,8), y = c("a", "b", "c", "d"))

#Modify y in u such that it has numerical values c(1, 2, 3, 4).
u$y <- c(1:4)

#What is the best way to compute the mean of all elements in x and y?
u_mean <- mean(x = c(mean(u$x), mean(u$y)))

#Add x2 = x^2 and log_x = log(x) into the list.
u$x2 <- u$x^2
u$log_x <- log(u$x)

#Remove log_x from the list.
u$log_x <- NULL

# Problem 4 In this problem, we will work on the creation of a matrix.
# Create three vectors x <- c(1, 2, 3), y <- c(4, 5, 6) and z <- c(7, 8, 9). Combine these three vectors as row vectors into a matrix.
x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)

A <- rbind(x,y,z)

#Combine x, y and z as column vectors into a matrix.
B <- cbind(x,y,z)

#Create a vector a <- c(1, 2, ..., 12). "Reshape" this vector into a 4 by 3 matrix in the column order,
a <- c(1:12)

MatA <- matrix(a, nrow = 4)

#Reshape" this vector into a 4 by 3 matrix in the row order,

MatB <- matrix(a, nrow = 4, byrow = TRUE)

# Problem 5In this problem, we will work through some matrix operations.
# Create matrices A <- matrix(c(1, 2, ..., 12), nrow = 3) and B <- matrix(c(1, 2, ..., 16), 
# nrow = 4)and C <- matrix(c(16, 15, ..., 1), nrow = 4)
A <- matrix(c(1:12), nrow = 3)
B <- matrix(c(1:16), nrow = 4)
C <- matrix(c(16:1), nrow = 4)
#What does B*C do? Try to explain the arithmetic.
BC <- B*C
print("B*C does element wise multiplication. Each element is multiplied by corresponding element in other matrix")

#What does A %*% B do?
AB <- A %*% B
print("A %*% B does matrix multiplication and creates a matrix product")


#Compute the sum of the diagonal elements of B
sum_B <- sum(diag(B))