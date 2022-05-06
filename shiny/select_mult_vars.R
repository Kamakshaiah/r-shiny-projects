library(shiny)

ui <- fluidPage(
  sidebarPanel(uiOutput('select_value')),
  mainPanel(uiOutput('input_value'))
)

server <- function(input , output){
  descriptive_data <- mtcars
  # initial value for weights and to keep track of value
  weightages <- rep(0, ncol(descriptive_data))
  # set names to simplify recover/storing value
  names(weightages) <- names(descriptive_data)

  output$select_value <- renderUI({
    div(
      selectInput('var_name', 'choose variables',
        names(descriptive_data), multiple = TRUE),
      actionButton("get", "Get weightages"),
      tableOutput('table')
    )
  })

  output$input_value <- renderUI({
    var_name <- input$var_name
    if (!is.null(var_name)) {
      # lapply will return a list
      lapply(1:length(var_name), function(k) { 
          numericInput(paste0("var", k), 
            paste('input weightage for', 
            # assign stored value
            var_name[k]), weightages[[var_name[k]]])
      })
    }    
  })

  observeEvent(input$get, {
    # to avoid create one observer per numeric input
    # we use a action button to trigger the recovering 
    # of weights.
    var_name <- input$var_name
    if (!is.null(var_name)) {
      for(k in 1:length(var_name)) { 
        # only recover/update value is the numeric input exists
        if (!is.null(input[[paste0("var", k)]]))
          weightages[[var_name[k]]] <<- input[[paste0("var", k)]]
      }
    }
    # show current weights 
    output$table <- renderTable(data.frame(
                        variable = names(descriptive_data),
                        weightages))

  })

}

shinyApp(ui = ui , server = server)
