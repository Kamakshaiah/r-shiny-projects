library(shiny)

shinyUI(basicPage(
  
  textInput('firstname', 'First name', value = 'Jimmy'),
  textInput('lastname', 'Last name', value = 'John'),
  downloadButton('report')
  
))
