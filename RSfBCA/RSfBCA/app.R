library(shiny)
library(shinyLP)



ui <- fluidPage(
   
   navbarPage('RSfBCA',
              
              tabPanel('About', 
                       jumbotron('RSfBCA', 'RSfBCA is server level application for Blockchian applications. The application right now supports numerical analyses on coin-markets data', button = TRUE, buttonLabel = a(href='https://github.com/Kamakshaiah', 'Know More!'))
                       ),
          
              tabPanel('Data'
                
              
              ),
              tabPanel('Analysis')
              
              )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
 
}

# Run the application 
shinyApp(ui = ui, server = server)

