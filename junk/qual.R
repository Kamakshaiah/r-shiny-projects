set.seed(1234)
fdo = facDesign(k = 3, centerCube = 4) #fdo - factorial design object
names(fdo) = c("Factor 1", "Factor 2", "Factor 3") #optional
lows(fdo) = c(80, 120, 1) #optional
highs(fdo) = c(120, 140, 2) #optional
summary(fdo)

yield = simProc(x1 = 120, x2 = 140, x3 = 2)

yield = c(simProc(120, 140, 1),simProc(80,140, 1),simProc(120,140, 2),
          simProc(120,120, 1),simProc(100,130, 1.5),simProc(100,130, 1.5),
          simProc(80,120, 2),simProc(100,130, 1.5), simProc(100,130, 1.5),
          simProc(120,120, 2),simProc(80,140, 2), simProc(80,120, 1))

response(fdo) = yield

effectPlot(fdo, classic = TRUE)
