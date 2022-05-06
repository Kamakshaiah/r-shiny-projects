library(shiny)
library(shinydashboard)
library(shinyLP)
library(RJSONIO)
library(jsonlite)
library(nortest)
library(mvnormtest)
library(MASS)
library(reshape2)
library(DT)
library(corrplot)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(TTR)
library(quadprog)
library(digest)


options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  
  navbarPage("BCMtRS",
             tabPanel("Home",
                      jumbotron(paste("Hi Welcome to BCCMAtRS"), paste("This is web application for Blockchain & Cryptocurrency Coin Market Analysis! Supports testing Blockchians and performing analysis on Cryptocurrency Coin Market Data."), buttonLabel = "Click Me" ),
                      helpText("This app supports both JSON and CSV file formats. Use JSON files for 'Statistical Analysis' menu; CSV files for 'CryptoCurrency Analysis' menu. "),
                      a(href="https://github.com/Kamakshaiah", "Know More here")
             ),
             
             tabPanel("Data Sets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose JSON File", accept=c('.json')),
                          fileInput("file2", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          hr(),
                          helpText("Upload and use JSON files to perform 'Statistical Analysis'. Upload and use CSV files to perform 'CryptoCurrency Analysis'. CSV file must be in OHLC file format including few other columns, such as 'Currency',   'Date', Open', 'High, 'Low', 'Close, 'Volume', 'Market.Cap'"),
                          hr(),
                          helpText("For sample data sets visit:"),
                          a(href="https://api.coinmarketcap.com/v1/ticker/", "CoinMarketCap"), br(),
                          a(href="https://coinmetrics.io/data-downloads/", "COINMETRICS"),
                        ), 
                        
                        mainPanel(
                            div(DTOutput("tab1")),
                          div(tableOutput("tab2"))
                        )
                      )
                      
             ), 
             navbarMenu("Statistical Analysis",
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
                                     radioButtons("cormethod", "Select Method:", choices = c("Covariance", "KarlPearson", "Spearman", "Kendals")),
                                     hr(),
                                     helpText("For Details Visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Covariance & Correlation"),
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
                                 
                        )
                        
             ),
             
             tabPanel("Blockchain",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("create", "Create:", c("No Option", "example", "Hashed Block", "PoW", "New Block", "Genesis Block", "Build Blocks")),
                          textInput("bcoption", "Show Me", value = "", placeholder = "No. of Blocks")
                        ),
                        mainPanel(
                          verbatimTextOutput("bcoutput")
                        )
                      )
             ),
             navbarMenu("CryptoCurrency Analysis", 
                        tabPanel("Analysis of Close  Market Cap",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # fileInput("file2", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                                     helpText("Make Data"),
                                     radioButtons("makedata", "Select:", c("No Option", "Show Data", "Make Close",  "Logivity", "Correlations", "Top Five Coins", "Make Market Cap.", "Show Newest Currency")),
                                     textInput("corcriteria", "Criteria for Correlation", value = "180"), #  c("No Option", "correlation", "bitcoin", "first-five", "first-five-negative")
                                     hr(),
                                     helpText("Market Cap Analysis"),
                                     hr(),
                                     radioButtons("markcapoption", "Market Cap. Option", c("No Option", "Summary")),
                                     textInput("marketcapanalytics", "Market Cap. Analysis:", value = "200"), # c("No Option", "mean-cap")
                                     radioButtons("plot", "Selct Option:", c("No Plot", "Plot")),
                                     textInput("plotname", "Coin Name", value = "", placeholder = "Write the coin name here.")
                                     
                                   ), 
                                   mainPanel(
                                     div(
                                       verbatimTextOutput("ccoutput")
                                     ),
                                     div(
                                       plotOutput("plot1")
                                     )
                                   )
                                 )
                                 
                                 
                        ), 
                        tabPanel("Top 10 Crypto Currency Analysis", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("topteninput", "Select:", c("No Option", "Make TS", "Log Return", "First Ten Means", "First Ten Variance", "First Ten Std. Dev.", "CVaR", "Efficient Frontier")),
                                     radioButtons("tsplot", "Plot Type", c("No Plot", "TS Plot", "Currency Wise Vol. Charts", "Correlation Chart", "Kernel Density", "EF Plot")),
                                     textInput("coinname", "Coins", value="24"),
                                     hr(),
                                     helpText("Results are displayed only for first ten currencies")
                                   ),
                                   mainPanel(
                                     div(
                                       verbatimTextOutput("toptenoutput")
                                     ),
                                     div(plotOutput("tsplots"))
                                   )
                                 )
                        )
             ),
             
             tabPanel("Reports",
                      helpText("Upgrade to Advanced Version")
             ),
             tabPanel("Cotact",
                      helpText("Dr. M. Kamakshaiah"),
                      helpText("+919848396972"),
                      helpText("kamakshaiah.m@gmail.com")
             )
  )
  
)

server <- function(input, output, session){
  
  # data input section starts here 
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    as.data.frame(fromJSON(input$file1$datapath))
  } 
  )
  
  data_input_1 <- reactive({
    infile <- input$file2
    req(infile)
    data.frame(read.csv(infile$datapath))
  })
  
  # JSON file input section
  
  output$tab1 <- renderDT({
    df <- data_input()
    print(df)
  })
  
  output$tab2 <- renderTable({
    df <- data_input_1()
    print(head(df))
  })
  # data input section ends here 
  
  # SUMMARY STATISTICS
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    su <- summary(as.numeric(var1))
    suout <- list(summary=su, type = typeof(var1))
    return(suout)
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
    var1 <- as.numeric(data_input()[,input$cols4])
    var2 <- as.numeric(data_input()[,input$cols5])
    
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
    var1 <- df[, input$cols6]
    
    if(input$plotoption == "Histogram"){
      hist(as.numeric(var1), freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(as.numeric(var1), col = "red", lwd = 1.5))
      
    } else if(input$plotoption == "BarPlot"){
      barplot(as.numeric(var1), xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else if(input$plotoption == "Scatter"){
      scatter.smooth(as.numeric(var1), xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else {
      pie(table(var1))
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
    qqnorm(as.numeric(df[, input$cols7])); qqline(as.numeric(df[, input$cols7]))
  })
  
  adt <- reactive({
    df <- data_input()
    var <- as.numeric(df[, input$cols7])
    ad <- ad.test(var)
    return(ad)
  })
  
  sht <- reactive({
    df <- data_input()
    var <- as.numeric(df[, input$cols7])
    sh <- shapiro.test(var)
    return(sh)
  })
  
  kst <- reactive({
    df <- data_input()
    var1 <- as.numeric(df[, input$cols7])
    var2 <- as.numeric(df[, input$cols8])
    ks <- ks.test(var1, var2)
    return(ks)
  })
  
  mvst <- reactive({
    df <- data_input()
    var1 <- as.numeric(df[, input$cols7])
    var2 <- as.numeric(df[, input$cols8])
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
    var1 <- as.numeric(data_input()[,input$cols9])
    var2 <- as.numeric(data_input()[,input$cols10])
    
    if (input$cormethod == "Covariance"){
      return(cov(var1, var2))
    } else if (input$cormethod == "KarlPearson"){
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
  
  # simple linear regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
  }
  )
  
  lmout <- reactive({
    df <- data_input()
    var1 <- as.numeric(df[, input$cols11])
    var2 <- as.numeric(df[, input$cols12])
    out <- lm(var1 ~ var2, data = df)
    return(out)
  })
  
  output$regout <- renderPrint({
    if(input$regmethod == "Fit"){
      lmout()
    } else if(input$regmethod == "Summary"){
      summary(lmout())
    } else if (input$regmethod == "ANOVA"){
      anova(lmout())
    }
  })
  
  output$regplot <- renderPlot({
    par(mfrow=c(2,2))
    plot(lmout())
  })
  
  # BLOCKCHAIN 
  
  block_example <- list(index = 1,
                        timestamp = "2018-01-05 17.00 MST",
                        data = "some data",
                        previous_hash = 0,
                        proof = 9,
                        new_hash = NULL)
  
  #Function that creates a hashed "block"
  
  hash_block <- function(block){
    block$new_hash <- digest(c(block$index,
                               block$timestamp,
                               block$data,
                               block$previous_hash), "sha256")
    return(block)
  }
  
  ### Simple Proof of Work Alogrithm
  
  proof_of_work <- function(last_proof){
    proof <- last_proof + 1
    
    # Increment the proof number until a number is found that is divisable by 99 and by the proof of the previous block
    while (!(proof %% 99 == 0 & proof %% last_proof == 0 )){
      proof <- proof + 1
    }
    return(proof)
  }
  
  #A function that takes the previous block and normally some data (in our case the data is a string indicating which block in the chain it is)
  
  gen_new_block <- function(previous_block){
    
    #Proof-of-Work
    new_proof <- proof_of_work(previous_block$proof)
    
    #Create new Block
    new_block <- list(index = previous_block$index + 1,
                      timestamp = Sys.time(),
                      data = paste0("this is block ", previous_block$index +1),
                      previous_hash = previous_block$new_hash,
                      proof = new_proof)
    
    #Hash the new Block
    new_block_hashed <- hash_block(new_block)
    
    return(new_block_hashed)
  }
  
  # Define Genesis Block (index 1 and arbitrary previous hash)
  
  block_genesis <-  list(index = 1,
                         timestamp = Sys.time(),
                         data = "Genesis Block",
                         previous_hash = "0",
                         proof = 1)
  
  build_block_chain <- function(){
    # Building the Blockchain
    
    blockchain <- list(block_genesis)
    previous_block <- blockchain[[1]]
    
    # How many blocks should we add to the chain after the genesis block
    num_of_blocks_to_add <- as.numeric(input$bcoption)
    
    # Add blocks to the chain
    for (i in 1: num_of_blocks_to_add){
      block_to_add <- gen_new_block(previous_block) 
      blockchain[i+1] <- list(block_to_add)
      previous_block <- block_to_add
      
      print(cat(paste0("Block ", block_to_add$index, " has been added", "\n",
                       "\t", "Proof: ", block_to_add$proof, "\n",
                       "\t", "Hash: ", block_to_add$new_hash)))
    }
  }
  
  
  bcfunct <- reactive({
    
    if (input$create == "example"){
      return(block_example)
    } else if (input$create == "Hashed Block"){
      hash_block(block_example)
    } else if (input$create == "PoW"){
      proof_of_work(block_example$proof)
    } else if (input$create == "New Block"){
      gen_new_block(block_example)
    } else if (input$create == "Genesis Block"){
      return(block_genesis)
    } else if (input$create == "Build Blocks"){
      build_block_chain()
    } 
    
  })
  
  output$bcoutput <- renderPrint({
    
    
    if (input$create == "example"){
      print(bcfunct())
    } else if (input$create == "Hashed Block"){
      print(bcfunct())
    } else if (input$create == "PoW"){
      print(bcfunct())
    } else if (input$create == "New Block"){
      print(bcfunct())
    } else if (input$create == "Genesis Block"){
      print(bcfunct())
    } else if (input$create == "Build Blocks"){
      print(bcfunct())
    }
    
    
  })
  
  
  # CRYPTOCURRENCY MENU
  
  makeclose <- reactive({
  
    dat <- data_input_1()
    
    close.raw <- reshape(dat[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    
    close.dat <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    colnames(close.dat) <- sub("Close.", "", colnames(close.dat))
    
    if (input$makedata == "Show Data"){
      return(head(dat))
    }
    
    if (input$makedata == "Make Close"){
      return(names(close.dat))
    }
  
    # longivity of coins
    
    length.col <- colSums(!is.na(close.dat[,-1]))
    new_cur <- sort(length.col)[1]
    
    longi <- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
    
    if (input$makedata == "Logivity"){
      return(longi)
    }
    
    # correlation
    
    close.cor <- close.dat[,colSums(!is.na(close.dat)) >= as.numeric(input$corcriteria)]
    corr <- cor(close.cor[,-1], use = "pairwise.complete")
    
    corr.bit <- corr[1:ncol(corr),"bitcoin", drop=FALSE]
    
    if (input$makedata == "Correlations"){
      return(corr.bit)
    }
    
    # top-five
    
    corr.order <- corr.bit[order(corr.bit, decreasing=T),,drop=FALSE]
    top.five.df <- data.frame(name=corr.order[2:6,0], cor=corr.order[2:6,1])
    
    if (input$makedata == "Top Five Coins"){
      return(top.five.df)
    }
    
    # market cap analysis
    
    markcap <- dat[c(1,2,8)]
    markcap$Market.Cap[markcap$Market.Cap == "-"] <- NA
    
    markcap.raw <- reshape(markcap, timevar= "Currency", idvar = "Date", direction = "wide")
    markcap.raw[,"Market.Cap.Currency"] <- NULL
    
    markcap <- sapply(markcap.raw, function(z){as.numeric(gsub(",","", z))})
    markcap <- data.frame(markcap[-nrow(markcap),])
    markcap <- markcap[,colSums(!is.na(markcap)) >= as.numeric(input$marketcapanalytics)]
    
    colnames(markcap) <- sub("Market.Cap.", "", colnames(markcap))
    
    if (input$makedata == "Make Market Cap."){
      return(markcap)
    }
    
    if (input$markcapoption == "Summary"){
      return(summary(markcap))
    }
    
    if (input$markcapoption == "Means"){
      return(apply(!is.na(markcap), 2, mean))
    }
    
    if (input$plot == "Plot"){
      plot(markcap[, input$plotname])
    }
    
    
    
  })
  
   
  
  plots <- reactive({
    
    
  })
  
  output$ccoutput <- renderPrint({
    
    if (input$makedata == "Show Data"){
      print(makeclose())
    }
    
    if (input$makedata == "Make Close"){
      print(makeclose())
    }
    
    if (input$makedata == "Logivity"){
      print(makeclose())
    }
    
    if (input$makedata == "Correlations"){
      print(makeclose())
    }
    
    if (input$makedata == "Top Five Coins"){
      print(makeclose())
    }
    
    if (input$makedata == "Make Market Cap."){
      print(makeclose())
    }
    
    if (input$markcapoption == "Summary"){
      print(makeclose())
    }
    
    if (input$markcapoption == "Summary"){
      print(makeclose())
    }
    
    if (input$markcapoption == "Means"){
      print(makeclose())
    }
       
    
  })
  
  output$plot1 <- renderPlot({
    if (input$plot == "Plot"){
      makeclose()
    }
  })
  
  # top 10 analysis
  
  toptenanalysis <- reactive({
    
    dat <- data_input_1()
    
    close.raw <- reshape(dat[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    
    close.dat <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    colnames(close.dat) <- sub("Close.", "", colnames(close.dat))
    
    library(lubridate)
    dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y") #, locale = "eng")
    #close.dat$Date<- dates
    
    #Delete the last row since it has no information
    
    rownames(close) <- dates[-length(dates)]
    
    if (input$topteninput == "Make TS"){
      return(head(close))
    }
    
    if (input$tsplot == "TS Plot"){
      plot(as.ts(close[, as.numeric(input$coinname)]), col="red")
    }
    
    if (input$topteninput == "Log Return"){
      out <- CalculateReturns(as.ts(close[, as.numeric(input$coinname)]), method = "log")
      return(list(message= "only first few records", log.ret=data.frame(head(out))))
    }
    
    if (input$topteninput == "First Ten Means"){
      out <- apply(close[, 2:10], 2, function(x) mean(x, na.rm = TRUE))
      return(list(message= "means for first ten coins", means=data.frame(out)))
    }
    
    if (input$topteninput == "First Ten Variance"){
      out <- apply(close[, 2:10], 2, function(x) var(x, na.rm = TRUE))
      return(list(message= "variances for first ten coins", variances=data.frame(out)))
    }
    
    if (input$topteninput == "First Ten Std. Dev."){
      out <- apply(close[, 2:10], 2, function(x) sd(x, na.rm = TRUE))
      return(list(message= "Std. Dev. for first ten coins", sigmas=data.frame(out)))
    }
    
    if (input$topteninput == "CVaR"){
      out <- CVaR(close[, 2:10], 2, function(x) CVaR(x))
      return(list(message= "CVaR for first ten coins", CVARs=data.frame(out)))
    }
    
    if (input$tsplot == "Currency Wise Vol. Charts"){
      
      volt <- volatility(as.numeric(na.omit(close[, as.numeric(input$coinname)])), calc = "close")
      plot(volt , main = paste("Volt. Chart for ", names(close)[as.numeric(input$coinname)] ), ylab="Volatility")
    }
    
    if (input$tsplot == "Correlation Chart"){
      chart.Correlation(close[2:10])
    }
    
    if (input$tsplot == "Kernel Density"){
      plot(density(na.omit(close[, as.numeric(input$coinname)])), main = paste("Volt. Chart for ", names(close)[as.numeric(input$coinname)]))
    }
    
    # efficient frontier
    
    # close.ef <- na.omit(close[, as.integer(input$coinname)])
    # mean_vect <- colMeans(as.matrix(close.ef))
    # cov_mat <- cov(close.ef)
    # sd_vect <- sqrt(diag(cov_mat))
    # 
    # M <- length(mean_vect)
    # 
    # library(quadprog)
    # Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
    # muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
    # sdP <- muP # set up storage for std dev's of portfolio returns
    # weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
    # for (i in 1:length(muP)){
    #   bvec <- c(1, muP[i]) # constraint vector
    #   result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2) 
    #   sdP[i] <- sqrt(2*result$value) 
    #   weights[i,] <- result$solution
    # }
    # 
    # ind.min <- which.min(sdP)
    # #Expected return
    # muP[ind.min]
    # 
    # #Expected standard deviation
    # sdP[ind.min]
    # 
    # #Proportions
    # weight <- data.frame(proportion = weights[ind.min,]) #, row.names = colnames(close[, -1]))
    
    if (input$topteninput == "Efficient Frontier"){
      #return(weight)
      return(typeof(input$coinname))
    }
    
    if (input$tsplot == "EF Plot"){
      plot(100*sdP, 100*muP, type="l", xlim=c(0,250), ylim=c(0,35), 
           main="Efficient Frontier", ylab="Expected Return(%)", xlab="Standard deviation(%)")
    }
    
  })
  
  output$toptenoutput <- renderPrint({
  
    if (input$topteninput == "Make TS"){
      print(toptenanalysis())
    }
    
    if (input$topteninput == "Log Return"){
      print(toptenanalysis())
    }
    
    if (input$topteninput == "First Ten Means"){
      print(toptenanalysis())
    }
    
    if (input$topteninput == "First Ten Variance"){
      print(toptenanalysis())
    }
    if (input$topteninput == "First Ten Std. Dev."){
      print(toptenanalysis())
    }
    if (input$topteninput == "CVaR"){
      print(toptenanalysis())
    }
    
    if (input$topteninput == "Efficient Frontier"){
      print(toptenanalysis())
    }
  })
  
  
  
  output$tsplots <- renderPlot({
    if (input$tsplot == "TS Plot"){
      toptenanalysis()
    }
    if (input$tsplot == "Correlation Chart"){
      toptenanalysis()
    }
    if (input$tsplot == "Currency Wise Vol. Charts"){
      toptenanalysis()
    }
    if (input$tsplot == "Kernel Density"){
      toptenanalysis()
    }
    if (input$tsplot == "EF Plot"){
      toptenanalysis()
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)

