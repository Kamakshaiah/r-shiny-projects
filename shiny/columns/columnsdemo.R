library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6,
           
             sliderInput("obs", "Number of observations:",
                         min = 1, max = 1000, value = 500) 
           
           
    ),
    column(6,
           
             plotOutput("distPlot")
           
    )
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui, server)
