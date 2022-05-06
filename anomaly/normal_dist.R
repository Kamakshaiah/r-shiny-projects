# NORMAL DIST.

# P(x = 0)

pnorm(0)

# CDF

seq(0, 3, 1)
pnorm(seq(0, 3, 1))

pnorm(c(1.64, 1.96, 2.53))

# Intervals

# P(-1 <= x <= 1)

pnorm(-1)
pnorm(1)
pnorm(1) - pnorm(-1)

# visual 

x <- rnorm(100)

c.x <- c(-1, seq(-1, 1, 0.1), 1)
c.y <- c(0, dnorm(seq(-1, 1, 0.1)), 0)
curve(dnorm(x), xlim = c(-3, 3))
polygon(c.x, c.y, col = "skyblue")

# 90 percentile 
# P(-1.64 <= x <= 1.64)

pnorm(1.64) - pnorm(-1.64)

# 95 percentile
# P(-1.96 <= x <= 1.96)

pnorm(1.96) - pnorm(-1.96) # 95th percentile

c.x <- c(-1.96, seq(-1.96, 1.96, 0.1), 1.96)
c.y <- c(0, dnorm(seq(-1.96, 1.96, 00.1)), 0)
curve(dnorm(x), xlim = c(-3, 3))
polygon(c.x, c.y, col = "skyblue")

text(1.96, 0.2, "this is 1.96 std", cex = 0.75)
arrows(2, 0.2, 1.96, 0.05, length = 0.5, code = 2, lty = 2, col = "red", cex = 0.5)

# 99 percentile
# P(-2.53 <= x <= 2.53)

pnorm(2.58) - pnorm(-2.58)

# sample distribution

x <- abs(round(rnorm(100)*100, 0))

hist(x, freq = FALSE); lines(density(x))
abline(v = mean(x), col = "red", lwd = 2, lty = 2)
text(200, 0.008, paste("Mean =", mean(x)))
arrows(150, 0.007, 75, 0.005, length = 0.2, code = 2, col = "red")

# P(x < mean(x))

pnorm(mean(x), mean(x), sd(x))

# P(x <= 200)

1 - pnorm(200, mean(x), sd(x))

vec <- abs(round(rnorm(100)*100, 0))

x <- (seq(-3, 3, 0.1)*sd(vec))+mean(vec)
y <- dnorm(x, mean(x), sd(x))

plot(x, y, type = "l")
abline(v = mean(vec), col = "green", lty = 2, lwd = 2)

# P(x >= 200)

1- pnorm(200, mean(vec), sd(vec))
#abline(v = (1 - qnorm(0.0183, mean(vec), sd(vec))))
abline(v = qnorm(pnorm(200, mean(vec), sd(vec)), mean(vec), sd(vec)), lty = 2, lwd = 2, col = "red")

polygon(c( x[x >=200], 200), c(y[x >= 200], 0), col = "red") # the area shaded represents 0.01831919 percent of area under the curve

# junk

qnorm(0.025) # 95th percentile value i.e. 1.96
-qnorm((1-0.95)/2)