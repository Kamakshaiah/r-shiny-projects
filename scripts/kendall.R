# data simulation

tsdat <- abs(round(rnorm(100), 2))
tsdat.ts <- as.ts(tsdat)
scatter.smooth(tsdat.ts)

mkttsdat <- seqMK(tsdat.ts)
plot(mkttsdat$prog, type="l", ylim = c(-3, 3)); lines(mkttsdat$retr)
# No trend in the sample

library(Kendall)
MannKendall(tsdat.ts)
SeasonalMannKendall(tsdat.ts.exp)
# Test fails. There might be positive trend in the population (H0 rejected)