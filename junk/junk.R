library(lavaan)
library(psych)
library(sem)

anildf <- read.csv(file.choose())

faout <- fa(cor(anildf[, 7:60]), 7)
fa.diagram(faout)

semmodel <- structure.sem(faout)
semout <- sem(model = semmodel, data = anildf[, 7:60])
sem.diagram(semout)
