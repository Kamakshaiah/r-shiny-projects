# outliers <- function(d = rnorm(100), y = 90, ...){
#   
#   args <- list(d, type)
# 
#     if (is.null(args[['d']])){
#     d <- round(rnorm(100)*100, 2)
#   }
#   
#   
#   c1 <- quantile(d, 0.75)
#   c2 <- ll
#   c3 <- ul
#   
#   ul <- mean(d) + 1.96*sd(d)
#   ll <- mean(d) - 1.96*sd(d)
#   
#   y <- matrix(NA, length(d), 1)
#   
#   for (i in 1:length(x)){
#     if (d[i] <= ll | d[i] >= ul){
#       y[i] <- d[i]
#     }
#   }
#   
#   if (!is.null(args[['type']])){
#     plot(d, type = "l"); points(y, col = "red", pch = 19); abline(h=mean(d), col = "red");abline(h=ul, col = "green"); abline(h=ll, col = "green"); text(c1, ul, paste("UL=", round(ul, 2))); text(c1, ll, paste("LL=", round(ll, 2)))
#   } else {
#     plot(d); points(y, col = "red", pch = 19); abline(h=mean(d), col = "red");abline(h=ul, col = "green"); abline(h=ll, col = "green"); text(c1, ul, paste("UL=", round(ul, 2))); text(c1, ll, paste("LL=", round(ll, 2)))
#   }
#   
#   
# }

# edit 1

outliers <- function(x, cl = 0.95, plot = TRUE, out = FALSE, na.remove = FALSE, complete = FALSE){
  
  # Missing data treatment, considered as outliers
  if (na.remove == TRUE){
    for (i in 1:length(x)){
      if(is.na(x[i])){
        x[i] <- 0
      }
    }
  } else if (complete == TRUE){
    x <- complete.cases(x)
  }
  
  z <- -qnorm((1-cl)/2)
  
  ul <- mean(x) + z * sd(x)
  ll <- mean(x) - z * sd(x)
  
  
  xc <- length(x) - quantile(x, 0.1) 
  y <- matrix(NA, length(x), 1)
  
  for (i in 1:length(x)){
    if (x[i] <= ll | x[i] >= ul){
      y[i] <- x[i]
    }
  }
  
  if(out == TRUE){
    print(y[!is.na(y)])  
  }
  
  
  if(plot == TRUE){
    plot(x, type = "l"); points(y, col = "red", pch = 19); abline(h = ul, col = "green"); abline(h=ll, col = "green"); abline(h=mean(x), col = "red"); text(xc, ul, paste("UL=", round(ul, 2))); text(xc, ll, paste("LL=", round(ll, 2)))
    
  }  
  
}

# df <- read.csv(file.choose())
# df <- df[, -1]
# 
# outliers(complete.cases(df[, 7]))
#  
# outliers(df[, 7], cl = 0.999, out = TRUE, complete = TRUE)
# outliers(df[, 7], cl = 0.999, out = TRUE, na.remove = TRUE)

# yna = matrix(NA, length(df[, 7]), 1 )
# yzero = matrix(NA, length(df[, 7]), 1 )
# 
# for (i in 1:length(df[, 7])){
#   if (is.na(df[, 7][i])){
#     yna[i] <- df[, 7][i]
#   } else if(df[, 7][i] == 0){
#     yzero[i] <- df[, 7][i]
#   }
# }

# Regression 

# outliersInReg <- function(x, plot = FALSE, desc = FALSE, out = FALSE){
#   
#   
#   fit <- lm(x ~ ., data = as.data.frame(x))
#   cd <- cooks.distance(fit)
#   
#   if (desc==TRUE){
#     print(summary(fit))
#   }
#   
#   if (out == TRUE){
#     print(cd[cd > 4*mean(cd, na.rm = T)])
#   }
#   
#   if(plot==TRUE){
#     plot(cd); abline(h = 4*mean(cd, na.rm = T), col ="red"); text(x = 1:length(cd)+1, y = cd, labels = ifelse(cd > 4*mean(cd, na.rm = T), names(cd), ""), col = "red")
#     
#   }
#   
# }

cookOutliers <- function(x, result = FALSE, plot = FALSE){
  
  
  fit <- lm(x ~ ., data = as.data.frame(x))
  cd <- cooks.distance(fit)
  
  out <- list(desc = summary(fit), outliers = cd[cd > 4*mean(cd, na.rm = T)])
  
  if (result){
    return(list(desc = summary(fit), outliers = cd[cd > 4*mean(cd, na.rm = T)]))
  } 
  
 if (plot){
   plot(cd); abline(h = 4*mean(cd, na.rm = T), col ="red"); text(x = 1:length(cd)+1, y = cd, labels = ifelse(cd > 4*mean(cd, na.rm = T), names(cd), ""), col = "red")
   
 }
   
}

# using CI in regression

x <- rnorm(30)
y <- rnorm(30)

fit <- lm(y ~ x, data = cbind.data.frame(x, y))

newdat <- as.data.frame(seq(min(x), max(x), length.out = 30))

conf_int <- predict(fit, newdat, interval = "confidence", level = 0.95)

plot(x, y, xlab="x", ylab="y", main="Regression")
abline(fit, col="blue")
matlines(newdat, conf_int[,2:3], col = "pink", lty=2)

# function

regOutliers <- function(x = rnorm(100), y= rnorm(100), newdf=as.data.frame(seq(min(x), max(x), length.out = 100)), ci = 0.999, plot = FALSE, out = FALSE){
  
  fit <- lm(y ~ x, data = cbind.data.frame(x, y))
  
  conf_int <- predict(fit, newdf, interval = "confidence", level = ci)
  
  if(plot){
    plot(x, y, xlab="x", ylab="y", main="Regression"); text(x+0.2, y, row.names(cbind.data.frame(x, y)), cex = 0.5, col = "red")
    abline(fit, col="red")
    matlines(newdf, conf_int[,2:3], col = "blue", lty=2)
    
  }
  if(out){
    return(conf_int)
  }
}