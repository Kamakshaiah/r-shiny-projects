library(shiny)
library(cluster)
library(MASS)
library(RSQLite)

ui <- fluidPage(
    
    navbarPage("CRM Analytics through RShiny (CRMAtRS)",
               
               navbarMenu("Transformations",
                          
                          tabPanel("Forms",
                                   fluidRow(
                                       column(4, 
                                              hr(),
                                              helpText("Customer Data"),
                                              hr(),
                                              textInput("transid", "Transaction ID"),
                                              textInput("transdate", "Transaction Date"),
                                              textInput("transcusname", "Customer Name")
                                       ), 
                                       column(4,
                                              helpText("Socioeconomic Data"),
                                              textInput("transgender", "Gender"),
                                              textInput("transemployment", "Employment"),
                                              textInput("transfamily", "Family Size"),
                                              textInput("transeducation", "Education Level"),
                                              textInput('transsalary', "Salary")
                                       ),
                                       column(4,
                                              helpText("Purchase Details"),
                                              textInput("transamount", "Purchase Amount"),
                                              textInput("transunits", "Number of Items"),
                                              textInput("transcategory", "Category"),
                                              textInput('transrepeat', "Customer Type")
                                              
                                       )
                                       
                                   ),
                                   hr(style="border-color: black;"),
                                   fluidRow(
                                       column(4,
                                              div("Form Table",
                                                  tableOutput("formtable")   
                                              ),
                                              div(
                                                  actionButton("submitdata", "Submit")
                                              )
                                              ),
                                       column(4,
                                              div("Transformations",
                                                  helpText("ETL"),
                                                  textInput("formdbtransvar", "Select Common Variable"),
                                                  radioButtons("formdbtransoption", "Select Option", c("No Option", "Table 1", "Table 2", "Merge", "Union", "Intersect", "Load")),
                                                  textInput("formdbloadtable", "Name for Load Table")
                                              )
                                              
                                            ),
                                       column(4,
                                              div("Transformation Results", tableOutput("formdbtab"))
                                       )
                                   ),
                                    
                                   
                          ),
                          
                          tabPanel("Database Manipulation",
                                   sidebarLayout(
                                       sidebarPanel(
                                           fileInput("file4", "Import File"),
                                           hr(),
                                           radioButtons("dboption", "Select Option", choices = c("No Option", "Create DB", "Add Table", "Append Table", "List Tables", "DB Query")),
                                           textInput("tablename", "Define Table Name"),
                                           textInput("dbquery", "Write SQL Coomand Here")
                                       ),
                                       mainPanel(
                                           div(
                                               tableOutput("dbtable")
                                           ),
                                           div(
                                               verbatimTextOutput("dboutput")
                                           )
                                       )
                                   )  
                          ),
                          
                          tabPanel("Data Warehouse",
                                   sidebarLayout(
                                       sidebarPanel(
                                           fileInput("file", "Import File"),
                                           fileInput("file1", "Import File"),
                                           hr(),
                                           
                                           radioButtons("datasets", "Select Datasets", choices = c("No Data", "dataset1", "dataset2", "Extract")),
                                           radioButtons("trans", "Transformation methods", choices = c("No Transformation", "Merge", "Union", "Intersection")),
                                           textInput("cols", "Write Columns"),
                                           hr(),
                                           downloadButton("downloadfile", "Download File")
                                       ),
                                       
                                       mainPanel(
                                           tableOutput("table")
                                       )  
                                   )
                                   
                          )
                          
               ),
               
               navbarMenu("Analytics", 
                          tabPanel("Dataset",
                                   sidebarLayout(
                                       sidebarPanel(
                                           fileInput("file3", "Upload File")
                                           
                                       ),
                                       mainPanel("Upload dataset",
                                                 tableOutput("crmdata")
                                       )
                                   )
                          ),
                          tabPanel("Multidimensional Scaling", 
                                   sidebarLayout(
                                       sidebarPanel(
                                           
                                           radioButtons("mdsoption", "Select Option", choices = c("No Option", "Fit", "Plot")),
                                           hr(),
                                           helpText("Visit", a(href="https://en.wikipedia.org/wiki/Multidimensional_scaling", "MDS"), "for more detials")
                                       ),
                                       mainPanel("Multidimensional Scaling",
                                                 div(
                                                     verbatimTextOutput("mdsout")
                                                 ),
                                                 div(
                                                     plotOutput("mdsplot")
                                                 )
                                       )
                                   )
                          ), 
                          tabPanel("Customer Segmentation",
                                   sidebarLayout(
                                       sidebarPanel(
                                           radioButtons("clusoption", "Select Option", choices = c("No Option", "Fit", "Plot")),
                                           hr(),
                                           helpText("Visit", a(href="https://en.wikipedia.org/wiki/Cluster_analysis", "CA"), "for more detials")
                                       ),
                                       mainPanel("Customer Segmentation",
                                                 div(
                                                     verbatimTextOutput("clusout")
                                                 ),
                                                 div(
                                                     plotOutput("clusplot")
                                                 )
                                       )
                                   )
                          ),
                          tabPanel("Customer Churn Prediction", 
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
                                       mainPanel("Customer Churn Analysis",
                                                 # h3("Regression & ANOVA"),
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
                          tabPanel("Market Basket Analysis",
                                   sidebarLayout(
                                       sidebarPanel(
                                           selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                           selectInput("cols2", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                           selectInput("cols3", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                           radioButtons("mbaoption", "Select Option", choices = c("Description", "Cross Table", "LogLinearModel", "Mutual Independence", "Partial Independence", "Conditional Independence")),
                                           hr(),
                                           helpText("Visit ", a(href="https://en.wikipedia.org/wiki/Association_rule_learning", "Association Rule Mining"), "for more details.")
                                       ),
                                       mainPanel("Market Basket Analysis",
                                                 div(
                                                     helpText("A market basket or commodity bundle is a fixed list of items. Market Basket Analysis is also knonw as 'Affinity analysis' is a data analysis and data mining technique that discovers co-occurrence relationships among activities performed by (or recorded about) specific individuals or groups. ")
                                                 ),
                                                 div(
                                                     helpText("This application doesn't support 'RSCL' methodology; only supports few independent tests only. For 'RSCL' upgrade to advanced version.")
                                                 ),
                                                 div(
                                                     verbatimTextOutput("mbaout")
                                                 )
                                       )
                                   )
                          )
                          
               ),
               tabPanel("Reports",
                        helpText("Upgrade to advanced version. Go to 'contact' section.")
               ),
               tabPanel("Contact",
                        helpText("Dr. M. Kamakshaiah"),
                        helpText("+919177573730"),
                        helpText("kamakshaiah.m@gmail.com")
               )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    formdata <- reactive({
        
        formdatarows <- c("transid", "transdate", "transcusname", "transgender", "transemployment", "transfamily", "transeducation", "transsalary", "transamount", "transunits","transcategory", "transrepeat")
        formdata <- c(input$transid, input$transdate, input$transcusname, input$transgender, input$transemployment, input$transfamily, input$transeducation, input$transsalary, input$transamount, input$transunits, input$transcategory, input$transrepeat)
        out <- cbind(formdatarows, formdata)
        return(data.frame(out))
    })
    
    output$formtable <- renderTable({
        formdatarows <- c("transid", "transdate", "transcusname", "transgender", "transemployment", "transfamily", "transeducation", "transsalary", "transamount", "transunits","transcategory", "transrepeat")
        formdata <- c(input$transid, input$transdate, input$transcusname, input$transgender, input$transemployment, input$transfamily, input$transeducation, input$transsalary, input$transamount, input$transunits, input$transcategory, input$transrepeat)
        print(cbind(formdatarows, formdata))
    })
    
    observeEvent(input$submitdata, {
        formdf <- formdata()
        conn <- dbConnect(RSQLite::SQLite(), "formdata.db")
        dbWriteTable(conn, input$transcusname, formdf, overwrite = TRUE)
        dbDisconnect(conn)
        
    })
    
    formdbtrans <- reactive({
        conn <- dbConnect(RSQLite::SQLite(), "formdata.db")
        tabs <- dbListTables(conn)
        tab1 <- data.frame(dbGetQuery(conn, paste("select * from", tabs[1])))
        tab2 <- data.frame(dbGetQuery(conn, paste("select * from", tabs[2])))
        
        if (input$formdbtransoption == "Table 1"){
            return(tab1)
        } else if (input$formdbtransoption == "Table 2"){
            return(tab2)
        } else if (input$formdbtransoption == "Merge"){
            out <- merge(tab1, tab2, by = c(input$formdbtransvar))
            return(out)
        } else if (input$formdbtransoption == "Union"){
            out <- union(tab1, tab2)
            return(out)
        } else if (input$formdbtransoption == "Intersect"){
            out <- intersect(tab1, tab2)
            return(out)
        } else if (input$formdbtransoption == "Load"){
            out <- merge(tab1, tab2, by = c(input$formdbtransvar))
            dbWriteTable(conn, input$formdbloadtable, out, overwrite = TRUE)
            return(print("Transformation Loaded Successfully!"))
            
        }
        
        dbDisconnect(conn)
        
    }
    )
    
    output$formdbtab <- renderTable({
        if (input$formdbtransoption == "Table 1"){
            print(formdbtrans())
        } else if (input$formdbtransoption == "Table 2"){
            print(formdbtrans())
        } else if (input$formdbtransoption == "Merge"){
            print(formdbtrans())
        } else if (input$formdbtransoption == "Union"){
            print(formdbtrans())
        } else if (input$formdbtransoption == "Intersect"){
            print(formdbtrans())
        } else if (input$formdbtransoption == "Load"){
            print(formdbtrans())
        }   
    })
    
    dbdataset <- reactive({
        infile <- input$file4
        req(infile)
        data.frame(read.csv(infile$datapath))
        
    })
    
    output$dbtable <- renderTable({
        dbdata <-dbdataset() 
        print(dbdata)
    })
    
    
    
    dbmanipulation <- reactive({
        dbdata <- dbdataset()
        conn <- dbConnect(RSQLite::SQLite(), "test.db")
        
        if (input$dboption == "Create DB"){
            print("The DB has been creted!")
        } else if(input$dboption == "Add Table"){
            dbWriteTable(conn, input$tablename, dbdata, overwrite = TRUE)
            print("The data table has been written to 'test.db'." )
        } else if(input$dboption == "Append Table"){
            dbWriteTable(conn, input$tablename, dbdata, append = TRUE)
            print("The data table has been appended to 'test.db'." )
        } else if (input$dboption == "List Tables"){
            dbListTables(conn)
        } else if (input$dboption == "DB Query"){
            dbGetQuery(conn, input$dbquery)
        } 
        
        
    })
    
    output$dboutput <- renderPrint({
        if (input$dboption == "Create DB"){
            dbmanipulation()
        } else if(input$dboption == "Add Table"){
            dbmanipulation()
        } else if(input$dboption == "Append Table"){
            dbmanipulation()
        } else if (input$dboption == "List Tables"){
            print(dbmanipulation())
        } else if (input$dboption == "DB Query"){
            print(dbmanipulation())
        } else if (input$datasets == "Extract"){
            dbdf <- dbGetQuery(conn, input$dbquery)
        }
    })
    
    datasetone <- reactive({
        infile <- input$file
        req(infile)
        data.frame(read.csv(infile$datapath))
        
    })    
    
    datasettwo <- reactive({
        infile <- input$file1
        req(infile)
        data.frame(read.csv(infile$datapath))
        
    })
    
    transform <- reactive({
        
        if(input$trans == "Merge"){
            dfone <- datasetone()
            dftwo <- datasettwo()
            out <- merge(dfone, dftwo, by = c(input$cols))
            # out <- typeof(input$cols)
            return(out)
        } else if(input$trans == "Union"){
            dfone <- datasetone()
            dftwo <- datasettwo()
            out <- union(dfone[, input$cols], dftwo[, input$cols])
            # out <- typeof(input$cols)
            return(out)
        } else if(input$trans == "Intersection"){
            dfone <- datasetone()
            dftwo <- datasettwo()
            out <- intersect(dfone[, input$cols], dftwo[, input$cols])
            # out <- typeof(input$cols)
            return(out)
        }
        
        
    }
    )
    
    
    output$table <- renderTable({
        dfone <- datasetone()
        dftwo <- datasettwo()
        if (input$datasets == "dataset1"){
            print(dfone)
        } else if (input$datasets == "dataset2"){
            print(dftwo)
        } else if (input$trans == "Merge"){
            print(transform())
        } else if (input$trans == "Union"){
            print(transform())
        } else if (input$trans == "Intersection"){
            print(transform())
        }
    })
    
    output$downloadfile <- downloadHandler(
        filename = function() {
            paste(transform, ".csv", sep = "")
        },
        content = function(file) {
            
            dfone <- datasetone()
            dftwo <- datasettwo()
            
            if (input$datasets == "dataset1"){
                write.csv(transform(), file, row.names = FALSE)
            } else if (input$datasets == "dataset2"){
                write.csv(transform(), file, row.names = FALSE)
            } else if (input$trans == "Merge"){
                write.csv(transform(), file, row.names = FALSE)
            } else if (input$trans == "Union"){
                write.csv(transform(), file, row.names = FALSE)
            } else if (input$trans == "Intersection"){
                write.csv(transform(), file, row.names = FALSE)
            }
            
        }
    )
    
    datasetthree <- reactive({
        infile <- input$file3
        req(infile)
        data.frame(read.csv(infile$datapath))
        
    })    
    
    output$crmdata <- renderTable({
        dataset <- datasetthree()
        print(datasetthree())
    })
    
    # MULTIDIMENSIONAL SCALING 
    
    mds <- reactive({
        dataset <- datasetthree()
        d <- dist(dataset)
        out <- cmdscale(d,eig=TRUE, k=2)
        
        if (input$mdsoption == "Fit"){
            return(out)
        } else if (input$mdsoption == "Plot"){
            x <- out$points[,1]
            y <- out$points[,2]
            plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
                 main="Metric MDS", type="n")
            abline(h=0, v=0, col = "red")
            text(x, y, labels = row.names(dataset), cex=.7)
        }
    })
    
    output$mdsout <- renderPrint(
        if (input$mdsoption == "Fit"){
            print(mds())
        }
    )
    
    output$mdsplot <- renderPlot({
        if (input$mdsoption == "Plot"){
            mds()
        }
    })
    
    # CLUSTER ANALYSIS
    
    clusanal <- reactive({
        dataset <- datasetthree()
        out <- kmeans(na.omit(dataset), 2)
        
        if(input$clusoption == "Fit"){
            return(out)
        } else if(input$clusoption == "Plot"){
            clusplot(na.omit(dataset), out$cluster, color=TRUE, shade=TRUE,
                     labels=2, lines=0)
        }
    })
    
    output$clusout <- renderPrint({
        if(input$clusoption == "Fit"){
            print(clusanal())
        }
    }
    )
    
    output$clusplot <- renderPlot({
        if(input$clusoption == "Plot"){
            clusanal()
        }
    })
    
    # CHURN PREDICTION
    
    observeEvent(input$file3, {
        updateSelectInput(session, inputId = "cols11", choices = names(datasetthree()))
        updateSelectInput(session, inputId = "cols12", choices = names(datasetthree()))
    }
    )
    
    reganal <- reactive({
        df <- datasetthree()
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
        df <- datasetthree()
        var1 <- df[, input$cols11]
        var2 <- df[, input$cols12]
        plot(var1, var2); abline(lm(var1 ~ var2, data = df), col = "red", lwd=2)
    })
    
    # MARKET BASKET ANALYSIS
    
    observeEvent(input$file3, {
        updateSelectInput(session, inputId = "cols1", choices = names(datasetthree()))
        updateSelectInput(session, inputId = "cols2", choices = names(datasetthree()))
        updateSelectInput(session, inputId = "cols3", choices = names(datasetthree()))
    })
    
    mba <- reactive({
        dataset <- datasetthree()
        
        var1 <- dataset[, input$cols1]
        var2 <- dataset[, input$cols2]
        var3 <- dataset[, input$cols3]
        
        mytable <- table(var1, var2, var3)
        
        if (input$mbaoption == "Cross Table"){
            return(ftable(mytable))
        } else if (input$mbaoption == "LogLinearModel"){
            out <- xtabs(~ var1+var2+var3, data = dataset)
            return(out)
        } else if (input$mbaoption == "Mutual Independence"){
            out <- loglm(~var1+var2+var3, mytable)
            return(out)
        } else if (input$mbaoption == "Partial Independence"){
            out <- loglm(~ var1+var2+var3+var2*var3, mytable)
            return(out)
        } else if (input$mbaoption == "Conditional Independence"){
            out <- loglm(~var1+var2+var3+var1*var3+var2*var3, mytable)
            return(out)
        }    
    })
    
    output$mbaout <- renderPrint({
        
        if (input$mbaoption == "Cross Table"){
            print(mba())
        } else if (input$mbaoption == "LogLinearModel"){
            print(mba())
        } else if (input$mbaoption == "Mutual Independence"){
            print(mba())
        } else if (input$mbaoption == "Partial Independence"){
            print(mba())
        } else if (input$mbaoption == "Conditional Independence"){
            print(mba())
        }
    })
    
}

shinyApp(ui = ui, server = server)
