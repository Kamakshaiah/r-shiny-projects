mydf <- matrix(round(runif(90, 1, 5), 0), 30, 3)
colnames(mydf) <- c("x1", "x2", "x3")
apply(mydf, 2, mean)
mydfch <- cbind.data.frame(cov(mydf), apply(mydf, 1, mean))
colnames(mydfch) <- c("x1", "x2", "x3", "f")
head(mydfch)
library(lavaan)

lavmodel <- '
  f =~ x3 + x2 + x1
'
lavout <- cfa(lavmodel, mydf)
summary(lavout)
