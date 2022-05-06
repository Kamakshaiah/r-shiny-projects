library(shiny)

shinyApp(
shinyUI(fluidPage(
  
  titlePanel("Display a PDF"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Generate PDF")
    ),
    
    mainPanel(
      uiOutput("pdfview")
    )
  )
)),

shinyServer(function(input, output) {
  
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="foo.pdf")
    })
  })
})
)