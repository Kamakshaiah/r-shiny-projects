
# simulating na data
mydm <- matrix(round(abs(rnorm(90, 0, 1)*10), 2), 30, 3)
mydf <- data.frame(mydm)

for (i in 1:ncol(mydf)){
  mydf[, i][rbinom(30, 1, 0.2)==1] <- NA
}
sum(is.na(mydf))

# mean substitution
mydf$X1[is.na(mydf$X1)]
mydf$X1[is.na(mydf$X1)] <- mean(mydf$X1, na.rm = TRUE)
mydf$X1

mydf$X2
is.na(mydf$X2)
sum(is.na(mydf$X2))
mydf$X2[is.na(mydf$X2)] <- mean(mydf$X2, na.rm = TRUE)

# logic for mean substitution
for (i in 1:ncol(mydf)){
  mydf[, i][is.na(mydf[, i])] <- mean(mydf[, i], na.rm = TRUE)
}

# function
meanSubstitution <- function(data){
  for (i in 1:ncol(mydf)){
    mydf[, i][is.na(mydf[, i])] <- mean(mydf[, i], na.rm = TRUE)
  }
  return(mydf)
}