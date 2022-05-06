f <- file("/dev/pts/2", open="r")
nObs = 10
Temperature <- rep(NA, nObs)
for(i in 1:nObs){
  Temperature[i] <- scan(f, n=1, quiet=TRUE)
  Sys.sleep(1)
}
close(f)