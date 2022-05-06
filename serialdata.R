library(serial)

con <- serialConnection(name = "testcon", port = "pts/1")

# let's open the serial interface

open(con)

# write some stuff
#write.serialConnection(con,"Hello World!")

# read, in case something came in
n = 10
x <- matrix(NA, 10, 1)
for (i in 1:n){
x[i] <- read.serialConnection(con)
}
# close the connection
close(con)
# }