library(shiny)

server <- function(input, output) {
  output$line_report_PDF <- renderUI({
  PDFfile="test.pdf"
  tags$iframe(
    src=PDFfile,
    width="100%",
    height="800px")
})
}

# from ui.R:
ui <- tabPanel("Line Report",
           basicPage(
             imageOutput("line_report_PDF")
           )
  )


shinyApp(ui=ui, server=server)