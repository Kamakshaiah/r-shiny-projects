
library(shiny)

shinyApp(
shinyServer(function(input, output,session){
  
  
}),


# Simple shiny layout for demo sake
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h5("use case - embed a pdf user guide in the app - embed as a local pdf or from web URL")
    ), 
    mainPanel(
      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("Reference", 
                 tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                             src="https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf")),
        tabPanel("Summary"),
        tabPanel("Plot")
      )
    ))
  
))
)
