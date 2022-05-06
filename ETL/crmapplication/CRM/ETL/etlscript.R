
# DATA SETS
# http://archive.ics.uci.edu/ml/machine-learning-databases/00352/ (MARKET BASKET)
# TUTORIALS (MARKET BASEKT)
# https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce

x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows


# marekt basket analysis

# https://www.statmethods.net/stats/frequencies.html

