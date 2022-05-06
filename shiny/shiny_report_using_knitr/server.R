library(knitr)

shinyServer(function(input, output) {
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy('input.pdf', file, overwrite = TRUE)
    },
    
    contentType = 'application/pdf'
  )
  # output$pdfview <- renderUI({
  #   tags$iframe(style="height:600px; width:100%", src="myreport.pdf")
  # })
  # tags$iframe(src="myreport.pdf", width="600", height="900")
  browseURL(file.path(paste(getwd(), "/", "input.pdf", sep = "")))
})
