library(shiny)
library(data.table)
library(ggplot2)
library(gridExtra)
library(readr)

# IsThereNewFile=function(){  #  cheap function whose values over time will be tested for equality;
#   #  inequality indicates that the underlying value has changed and needs to be 
#   #  invalidated and re-read using valueFunc
#   filenames <- list.files(pattern="*.csv", full.names=TRUE)
#   length(filenames)
# }
# ReadAllData=function(){ # A function that calculates the underlying value
#   filenames <- list.files(pattern="*.csv", full.names=TRUE)
#   read_csv(filenames[length(filenames)])
# }

serial_read = function(){
  
  for (i in 1:5){
    x[i] <- scan(file="/dev/pts/1",n=1,what="character")
  }
  return(x)
}

serial_collect = function(){
  
    serial_read()
    
}

function(input, output, session) {
  
  sampled_data <- reactivePoll(5, session,serial_read, serial_collect)    
  # 10: number of milliseconds to wait between calls to checkFunc
  output$text<-renderPrint({     
    dat=  sampled_data()
    #len = dim(dat)[1]
    print(tail(dat))
      })
  
  output$plot1 <- renderPlot({
    dat=  sampled_data()
    len = dim(dat)[1]
    plot(dat)
    
    
  })
}

