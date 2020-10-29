A <- matrix(rnorm(12), nrow = 4)

# row mean 
rowmean <- list()
for (val in 1:4) {
  rowmean[(val)] <- mean(A[val,])
}

#column mean
colmean <- list()
for (val in 1:3) {
  colmean[val] <- mean(A[,val])
}

# using apply
rowm <- apply(A, 1, mean)
colm <- apply(A, 2, mean)

#appender

append_er <- function(word) {
  paste0(word, "er")
}

actions = list("eat", "work", "sleep")

# append to each thing in the list using for loops
for (i in 1:length(actions)) {
  print(append_er(actions[i]))
}

# use lapply

sud <- lapply(actions, append_er)
  