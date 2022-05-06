library(shiny)
library(shinydashboard)        
library(nortest)
library(mvnormtest)

ui <- fluidPage(
  
  navbarPage(title = "SimpleSEM",
             tabPanel("DataSets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('downloaddatset', "Download"),
                          hr(),
                          radioButtons("trans1", "Transformation:", choices = c("Not-Required", "log", "inverselog", "exponential", "lognormal", "standardize"))                        ), 
                        
                        mainPanel(tableOutput("tab1"))
                        )
                      
             ), 
             
             navbarMenu("Statistical Anlaysis",
               tabPanel("Summary Statistics",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE)
                            
                          ), 
                          mainPanel(
                            fluidRow(
                              h3("Summary Statistics"),
                              div(
                                verbatimTextOutput("summar")
                              )
                            )
                          )
                        )
               ), 
               tabPanel("Frequency Tables",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols2", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                            selectInput("cols3", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE)
                          ), 
                          mainPanel(
                            fluidRow(
                              h3("Frequency Tables"),
                              div(
                                verbatimTextOutput("freq_tables")
                              )
                            )
                          )
                        )
                        
               ), 
               
               tabPanel("Plots",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "BarPlot", "Scatter", "Pie" )),
                            selectInput("cols6", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                            textInput("xaxisname", "Write X Axis Name"),
                            textInput("yaxisname", "Write Y Axis Name"),
                            textInput("title", "Write Title For the Graph")
                          ), 
                          mainPanel(
                            h3("Plots"),
                            fluidRow(
                              plotOutput("plot")
                            )
                          )
                        )
                        
               ),
               
               
               tabPanel("Statistical Tests", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols7", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                            selectInput("cols8", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE),
                            radioButtons("normaltest", "Select Method:", choices = c("A-D-Test", "Shapiro", "KS-Test", "MV-Shapiro")),
                            hr(),
                            helpText("For more details visit:"),
                            a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", "Anderson–Darling test"), br(),
                            a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", "Shapiro–Wilk test"), br(),
                            a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", "Kolmogorov–Smirnov test"), br(),
                            
                            hr()
                          ), 
                          mainPanel(
                            h3("Statistical Tests"),
                            fluidRow(
                              div(
                                plotOutput("qqp")
                              ),
                              div(
                                verbatimTextOutput("normtest")
                              )
                            )
                          )
                          
                        )
                        
               ),
               
               tabPanel("Correlation", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            radioButtons("cormethod", "Select Method:", choices = c("KarlPearson", "Spearman", "Kendals")),
                            hr(),
                            helpText("For Details Visit:"),
                            a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                            hr()
                          ), 
                          mainPanel(
                            h3("Correlation"),
                            verbatimTextOutput("cor_t")
                          )
                          
                        )
                        
               ),
               tabPanel("Regression & ANOVA", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols11", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            selectInput("cols12", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            radioButtons("regmethod", "Select Method:", choices = c("Fit", "Summary", "ANOVA")), 
                            hr(),
                            helpText("For more information please visit"),
                            a(href="https://en.wikipedia.org/wiki/Simple_linear_regression", "Simple Linear Regression"),
                            hr()
                          ), 
                          mainPanel(
                            h3("Regression & ANOVA"),
                            fluidRow(
                              div(
                                verbatimTextOutput("regout")
                              ),
                              div(
                                plotOutput("regplot")
                              )
                            )
                          )
                          
                        )
                        
               ),
               
               tabPanel("MANOVA", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cols13", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            selectInput("cols14", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                            radioButtons("manmethod", "Choose Method:", choices = c("Fit", "Summary")),
                            hr(),
                            helpText("For more information please visit"),
                            a(href="https://en.wikipedia.org/wiki/Multivariate_analysis_of_variance", "MANOVA"),
                            hr(),
                            helpText("Right now MANOVA supports only two dependent varibales"),
                            hr()
                          ), 
                          mainPanel(
                            h3("MANOVA"),
                            fluidRow(
                              div(
                                verbatimTextOutput("manovaout")
                              ),
                              div(
                                plotOutput("manovaplot")
                              )
                            )
                          )
                          
                        )
                  )
               ),
             navbarMenu("SEM",
               tabPanel("EFA & SEM",
                        sidebarLayout(
                          sidebarPanel(
                            textInput("semlvvars", "Write Latent Variables:", placeholder = "F1+F2+...+F3"),
                            selectInput("semmvvars", "Select Manifest Variables:", choices = "", selected = "", multiple = TRUE)
                          ),
                          mainPanel(
                            fluidRow(
                              div(
                                plotOutput("semplot")
                              ),
                              div(
                                textOutput("semoutput")
                              )
                            )
                          )
                        )
                        
                        
               ),
               tabPanel("CFA & SEM",
                        sidebarLayout(
                          sidebarPanel(
                            textInput("semlvvars", "Write Latent Variables:", placeholder = "F1+F2+...+F3"),
                            selectInput("semmvvars", "Select Manifest Variables:", choices = "", selected = "", multiple = TRUE)
                          ),
                          mainPanel(
                            fluidRow(
                              div(
                                plotOutput("semplot")
                              ),
                              div(
                                textOutput("semoutput")
                              )
                            )
                          )
                        )
                        
                        
               )
             ),  
             
               
             
               tabPanel("Contact", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information to contact"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
              )
          )
      )




server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
    }
  )
  
  logno <- reactive({
    df <- data_input()
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    for(i in 1:length(df[, input$cols])){
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- dlnorm(df[, input$cols][[i]][j]) 
      }
    }
    return(t(x))
  })
  
  standout <- reactive({
    df <- data_input()
    
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    
    if(!is.list(df[, input$cols])){
      df[, input$cols] <- list(df[, input$cols])
    }
    
    for(i in 1:length(df[, input$cols])){
      
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- df[, input$cols][[i]][j]-mean(df[, input$cols][[i]])/sd(df[, input$cols][[i]])
      }
    }
    return(t(x))
    
    })
  
 output$tab1 <- renderTable(
   {
     df <- data_input()
     
     if (input$indata == "Full"){
       print(df)
     } else if(input$trans1 == "Not-Required"){
       data <- df[, input$cols]
       print(data)
     } else if(input$trans1 == "log"){
       data <- log(df[input$cols])
       print(data)
     } else if(input$trans1 == "inverselog"){
       data <- 1/log(df[input$cols])
       print(data)
     } else if(input$trans1 == "exponential"){
       data <- exp(df[input$cols])
       print(data)
     } else if(input$trans1 == "lognormal"){
       logno()
     } else if(input$trans1 == "standardize"){
       standout()
     }
     
   }
 )

 
 output$downloaddatset <- downloadHandler(
   
   filename <- function(){
     paste("data-", Sys.Date(), ".csv", sep = "")
   },
   
   content <- function(file){
     df <- data_input()
     write.csv(df[, input$cols], file, row.names = TRUE)
   }
   
 )
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
   }
 )
 
 summ <- reactive({
   var1 <- data_input()[,input$cols1]
 
   su <- summary(var1)
   return(su)
 })
 
 output$summar <- renderPrint({
   summ()
   })
 
 # frequency tab
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols2", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols3", choices = names(data_input()))
 }
 )
 
 freq <- reactive({
   var1 <- data_input()[,input$cols2]
   var2 <- data_input()[,input$cols3]
   fre <- table(var1, var2)
   return(fre)
 })
 
 output$freq_tables <- renderPrint({
   freq()
 })
 
 # Cross tabulation
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols4", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols5", choices = names(data_input()))
 }
 )

cross <- reactive({
   var1 <- data_input()[,input$cols4]
   var2 <- data_input()[,input$cols5]
   
   cro <- chisq.test(var1, var2)
   return(cro)
 })
 
 output$chi_t <- renderPrint({
   cross()
   
 })
 
 # Plots 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
 )
 
 output$plot <- renderPlot({
   df <- data_input()
   if(input$plotoption == "Histogram"){
     hist(df[, input$cols6], freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(df[, input$cols6]), col = "red", lwd = 1.5)
   } else if(input$plotoption == "BarPlot"){
     barplot(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
   } else if(input$plotoption == "Scatter"){
     scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
   } else {
     pie(table(df[, input$cols6]))
   }
 })
 
 # Statistical Tests
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols7", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols8", choices = names(data_input()))
 }
 )
 
 output$qqp <- renderPlot({
   df <- data_input()
   qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
 })
 
 adt <- reactive({
   df <- data_input()
   var <- df[, input$cols7]
   ad <- ad.test(var)
   return(ad)
 })
 
 sht <- reactive({
   df <- data_input()
   var <- df[, input$cols7]
   sh <- shapiro.test(var)
   return(sh)
 })
 
 kst <- reactive({
   df <- data_input()
   var1 <- df[, input$cols7]
   var2 <- df[, input$cols8]
   ks <- ks.test(var1, var2)
   return(ks)
 })
 
 mvst <- reactive({
   df <- data_input()
   var1 <- df[, input$cols7]
   var2 <- df[, input$cols8]
   return(mshapiro.test(t(as.data.frame(var1, var2))))
 })
 
 output$normtest <- renderPrint({
   
   if(input$normaltest == "A-D-Test"){
       print(adt())
   } else if(input$normaltest == "Shapiro"){
     print(sht())
   } else if(input$normaltest == "KS-Test"){
     print(kst())
   } else if(input$normaltest == "MV-Shapiro"){
     print(mvst())
   }
   
   }
 
 )
 # correlation & regression 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
 }
 )
 
 cortest <- reactive({
   var1 <- data_input()[,input$cols9]
   var2 <- data_input()[,input$cols10]
   if (input$cormethod == "KarlPearson"){
     return(cor.test(var1, var2, method = "pearson"))
   } else if(input$cormethod == "Spearman"){
     return(cor.test(var1, var2, method="spearman"))
   } else {
     return(cor.test(var1, var2, method="kendall"))
   }
  }
  )

 output$cor_t <- renderPrint({
   cortest()
 })
 
 # Regression
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
 }
 )
 
 reganal <- reactive({
   df <- data_input()
   var1 <- df[, input$cols11]
   var2 <- df[, input$cols12]
   rego <- lm(var1 ~ var2, data = df)
   return(list(fit = rego, fitsum = summary(rego), anov = anova(rego)))
   
 })
 
 output$regout <- renderPrint({
   if (input$regmethod == "Fit"){
     reganal()$fit
   } else if(input$regmethod == "Summary"){
     reganal()$fitsum
   } else if(input$regmethod == "ANOVA"){
     reganal()$anov
   }
 })
 
 output$regplot <- renderPlot({
   df <- data_input()
   var1 <- df[, input$cols11]
   var2 <- df[, input$cols12]
   plot(var1, var2); abline(lm(var1 ~ var2, data = df), col = "red", lwd=2)
 })
 
 # MANOVA
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols13", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols14", choices = names(data_input()))
 }
 )
 
 manovaform <- reactive({
   df <- data_input()
   #formula <- as.formula(paste(cbind(df[, input$cols13]), '~', df[, input$cols14])) 
   manform <- as.formula(paste("cbind(unlist(rbind(df[, input$cols13]))[1:length(df[, input$cols14])], unlist(rbind(df[, input$cols13]))[length(df[, input$cols14])+1:length(df[, input$cols14])*2])", "~", "df[, input$cols14]"))
   return(manform)
 })
 
 manovaanal <- reactive({
   df <- data_input()
   manout <- manova(manovaform(), data = df)
   return(manout)
 })
 
 output$manovaout <- renderPrint({
 if(input$manmethod=="Fit"){
   manovaanal()
 } else if(input$manmethod == "Summary"){
   summary(manovaanal())
 }
   
 })
 
output$manovaplot <- renderPlot({
  df <- data_input()
  var1 <- df[, input$cols13]
  var2 <- df[, input$cols14]
  plot(data.frame(var1, var2))
}) 


 
 # Contact Information 
 
 output$text1 <- renderText({
   str1 <- paste("Dr. M. Kamakshaiah") 
   str2 <- paste("kamakshaiah.m@gmail.com") 
   str3 <- paste("+919177573730")
   #str4 <- paste("166, Vayushakti Nagar, Dammaiguda, Hyderabad, Telangana State, India 500083")
   HTML(paste(str1, str2, str3, sep = '<br/>'))
 })
  
}



shinyApp(ui, server)
