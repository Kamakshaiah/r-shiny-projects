y = 0
while(TRUE){
  x0=sample(1:2, size = 1)
  y = append(x0, y)
  write.csv(y, paste0("sampled", gsub("[^0-9]","",Sys.time()),".csv"), row.names = FALSE)
  Sys.sleep(0.5) # Suspend execution of R expressions. The time interval to suspend execution for, in seconds.
  
}