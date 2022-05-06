x <- matrix(NA, length(dflist), length(dflist[[1]]))
for (i in 1:length(dflist)){
  for (j in 1:length(dflist[[1]])){
    x[i, j] <- dflist[[i]][j]
  }
}

x
