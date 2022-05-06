library(shiny)
library(knitr)

ui <- basicPage(
  textInput('value1', 'Value', value = '10'),
  textInput('value2', 'Value', value = '10'),
  downloadButton('report')
)

server <- function(input, output) {
  output$report = downloadHandler(
    filename = function(){'myreport.pdf'},
    
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
}

shinyApp(ui, server)
