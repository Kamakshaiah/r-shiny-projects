library(shiny)
library(ggplot2)  # for the diamonds dataset
library(DT)

fields <- c("id", "slno", "part", "make", "location", "quantity", "price")


ui <- fluidPage(
  
  
  navbarPage(title = "BSECS-RSfWMS",
             tabPanel("Input Form", 
                      sidebarLayout(
                        sidebarPanel("Consignment",
                                     textInput("id", "Part ID"),
                                     textInput("slno", "SL. No. "),
                                     textInput("part", "Part Name"),
                                     textInput("make", "Make"),
                                     textInput("location", "Location"),
                                     textInput("quantity", "Quantity"),
                                     textInput("price", "Price"), 
                                     actionButton("submit", "Submit")
                                     ),
                        mainPanel(
                          tableOutput("consignmenttable")
                        )
                        
                      )
                      ), 
             tabPanel("Stock Search",
                      DT::dataTableOutput("responses", width = 300)
                      ), 
             
             tabPanel("Dashboards"),
             tabPanel("Contact",
               sidebarLayout(
                 sidebarPanel("Contact Information"), 
                 mainPanel(
                   htmlOutput("contact")
                 )
               )
               
             )
    
  )
  
)

server <- function(input, output) {
  
  formdata <- reactive({
    data <- sapply(fields, function(x) input[[x]])
  })
  
  observeEvent(input$submit, {
    saveData(formdata())
  })
  
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })
  
  saveData <- function(data){
    data <- as.data.frame(t(data))
    if(exists("responses")){
      responses <<-rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function(){
    if(exists("responses")){
      responses
    }
  }
  
  consTable <- reactive({
    rowNames <- c("ID", "SL_No", "Part", "Location", "Quantity", "Price", "Total")
    total <- as.numeric(input$quantity) * as.numeric(input$price)
    rowData <- c(input$id, input$slno, input$part, input$location, input$quantity, input$price, total)
    data <- cbind(rowNames, rowData)
    return(data)
  })
  
  output$consignmenttable <- renderTable({
    consTable()
    
  })
  
  output$contact <- renderText({
    str1 <- paste("BSECS") 
    str2 <- paste("contact@bse-cs.com") 
    str3 <- paste("+919848396972")
    str4 <- paste("166, Vayushakti Nagar, Dammaiguda, Hyderabad, Telangana State, India 500083")
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
  
  
}

shinyApp(ui, server)
