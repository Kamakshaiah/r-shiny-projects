
extract <- function(){
  etlsales <- read.csv("C:/Users/kamakshaiah/Documents/data/etlsales.csv")
  etlcost <- read.csv("C:/Users/kamakshaiah/Documents/data/etlcost.csv")
  return(list(etlsales, etlcost))
}

transform <- function(x, y){
  etlsales <- read.csv("C:/Users/kamakshaiah/Documents/data/etlsales.csv")
  etlcost <- read.csv("C:/Users/kamakshaiah/Documents/data/etlcost.csv")
  out <- merge(etlsales, etlcost)
  return(out)
  
}

load <- function(out){
  etlsales <- read.csv("C:/Users/kamakshaiah/Documents/data/etlsales.csv")
  etlcost <- read.csv("C:/Users/kamakshaiah/Documents/data/etlcost.csv")
  out <- merge(etlsales, etlcost)
  write.csv(out, "C:/Users/kamakshaiah/Documents/data/etl.csv")
}

etlsales <- read.csv("C:/Users/kamakshaiah/Documents/data/etlsales.csv")
etlcost <- read.csv("C:/Users/kamakshaiah/Documents/data/etlcost.csv")
out <- merge(etlsales, etlcost)
write.csv(out, "C:/Users/kamakshaiah/Documents/data/etl.csv")


# tabPanel("Reports",
#          sidebarLayout(
#              sidebarPanel(),
#              mainPanel(
#                  div(
#                      helpText("Upgrade to advanced version.")
#                  )
#              )
#          )
# ),
# tabPanel("Contact",
#          sidebarLayout(
#              sidebarPanel(),
#              mainPanel(
#                  div(
#                      helpText("Contact:"),
#                      helpText("Dr. M. Kamakshaiah,"),
#                      helpText("+9177573730"),
#                      helpText("kamakshaiah.m@gmail.com"),   
#                  )
#              )
#          )
#          
#          )