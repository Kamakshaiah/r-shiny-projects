library(pso)

psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)), lower=-5,upper=5,control=list(abstol=1e-8))

o <- psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)),
             lower=-5,upper=5,control=list(abstol=1e-8,trace=1,REPORT=1,
                                           trace.stats=TRUE))

plot(o$stats$it,o$stats$error,log="y",xlab="It",ylab="Error")
points(o$stats$it,sapply(o$stats$f,min),col="blue",pch=2)