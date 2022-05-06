for (i in 1:10){
  x[i] <- scan(file="/dev/pts/1",n=1,what="character")
}

print(x)
