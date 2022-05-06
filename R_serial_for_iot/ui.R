library(shiny)
fluidPage(
  tags$h2("Visualizing Streaming Data with Shiny",
          style="color:blue;text-align:center"),
  tags$h4("Simulated Data"),
  verbatimTextOutput("text"),
  tags$h4("Plot for Simulated Data"),
  plotOutput("plot1")
)

