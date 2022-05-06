library(shiny)

ui <- fluidPage(
  withMathJax(),
    sidebarLayout(
      sidebarPanel(
        sliderInput("input1", "Input", min = 10, max = 100, value = 15)
      ), 
      mainPanel(
        div(
          withMathJax(
            helpText("this is simple $$\\alpha$$ formula")
          ) 
          
        ),
        div(
          uiOutput("text")
        ),
        
        div(
          plotOutput("plot1")
        )
      )
    )
  )


server <- function(input, output, session) {
  histo <- reactive({
    hist(input$input1, freq = FALSE)
  })
  
output$text <- renderPrint({
  withMathJax(paste("This is simple $$\\sqrt{2}$$ function"))
})  
output$plot1 <- renderPlot({
 
  histo()
})
  
  }

shinyApp(ui, server)

