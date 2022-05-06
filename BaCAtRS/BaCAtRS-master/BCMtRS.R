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

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  
  navbarPage("BCMtRS",
             tabPanel("Home",
                      jumbotron(paste("Hi Welcome to BCMtRS"), paste("This is web application for Blockchain & Cryptocurrency Market Analysis! Right now BCMtRS supports Cryptocurrency Market Data Analysis."), buttonLabel = "Click Me" ),
                      a("Know More here", href="www.github.com/Kamakshaiah")
             ),
             tabPanel("Data Sets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose JSON File", accept=c('.json')),
                          fileInput("file2", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv'))
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
             navbarMenu("Crypto Currency Analysis", 
                        tabPanel("Analysis of Close & Market Cap",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file2", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                                     radioButtons("makedata", "Select:", c("No Option", "Show Data", "Make Close", "Make Market Cap.", "Show Newest Currency", "Logivity")),
                                     hr(),
                                     helpText("Analysis"),
                                     hr(),
                                     textInput("analytics", "Correlation Analysis:", c("No Option", "correlation", "bitcoin", "first-five", "first-five-negative")),
                                     textInput("marketcapanalytics", "Market Cap. Analysis:", c("No Option", "mean-cap")),
                                     radioButtons("plot", "Selct Option:", c("No Plot", "Bar Plot"))
                                     
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
                                     radioButtons("topteninput", "Select:", c("No Option", "Make TS", "Long Return", "Mean", "Variance", "Std. Dev.", "CVaR", "Portfolio Selection")),
                                     radioButtons("tsplot", "Plot Type", c("No Plot", "TS Plot", "Volatility Chart", "Currency Wise Vol. Charts", "Correlation Chart", "Kernel Density", "Efficient Frontier"))
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
             tabPanel("Contact",
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
  
  # CRYPTOCURRENCY MENU
  
  data_input_1 <- reactive({
    infile <- input$file2
    req(infile)
    data.frame(read.csv(infile$datapath))
  })
  
  makeclose <- reactive({
    
    df <- data_input_1()
    close.raw <- reshape(df[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    
    close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    colnames(close) <- sub("Close.", "", colnames(close))
    
    length.col <- colSums(!is.na(close[,-1]))
    new_cur <- sort(length.col)[1]
    
    longi <- table(cut(length.col, c(0, 180, 365, 2*365, 3*365, 4*365, Inf), right = T))
    
    # correlation
    
    close.180 <- close[,colSums(!is.na(close)) >= 180]
    corr <- cor(close.180[,-1], use = "pairwise.complete")
    
    bitcor <- corr[1:ncol(corr),"bitcoin", drop=FALSE]
    
    # top-five
    
    corr.bit.dec.order <- corr.bit[order(corr.bit, decreasing=T),,drop=F]
    top.five.df <- data.frame(name=corr.bit.dec.order[2:6,0], cor=corr.bit.dec.order[2:6,1])
    
    # top-five -ve
    
    corr.bit.inc.order <- corr.bit[order(corr.bit, decreasing=F),,drop=F]
    top.five.neg <- data.frame(name=corr.bit.inc.order[1:5,0], cor=corr.bit.inc.order[1:5,1])
    
    # Market Cap Analysis
    
    markcap <- data[c(1,2,8)]
    markcap$Market.Cap[markcap$Market.Cap == "-"] <- NA
    
    markcap.raw <- reshape(markcap, timevar= "Currency", idvar = "Date", direction = "wide")
    markcap.raw[,"Market.Cap.Currency"] <- NULL
    
    markcap <- sapply(markcap.raw, function(z){as.numeric(gsub(",","", z))})
    markcap <- data.frame(markcap[-nrow(markcap),])
    markcap <- markcap[,colSums(!is.na(markcap)) >= 200]
    
    colnames(markcap) <- sub("Market.Cap.", "", colnames(markcap))
    
    # mean market cap
    
    mean.cap <- data.frame(mean.cap=colMeans(markcap, na.rm = T))
    mean.cap.10.name <- rownames(mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F])[1:10]
    mean.cap.10.value <- mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F][1:10,]
    mean.cap.10 <- data.frame(name=mean.cap.10.name, mean.market.cap=mean.cap.10.value)
    
    
    if (input$makedata == "Make Close"){
      return(close)
    } else if(input$makedata == "Show Newest Currency"){
      return(new_cur)
    } else if (input$makedata == "Logivity"){
      return(longi)
    } else if (input$analytics == "correlation"){
      return(head(corr))
    } else if(input$analytics == "bitcoin"){
      return(head(bitcor))
    } else if(input$analytics == "first-five"){
      return(top.five.df)
    } else if(input$analytics == "first-five-negative"){
      return(top.five.neg)
    } else if (input$makedata == "Make Market Cap."){
      return(head(markcap))
    } else if (input$marketcapanalytics == "mean-cap"){
      return(mean.cap.10)
    }
  })
  
  plots <- reactive({
    df <- data_input_1()
    close.raw <- reshape(df[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    
    close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    colnames(close) <- sub("Close.", "", colnames(close))
    
    close.180 <- close[,colSums(!is.na(close)) >= 180]
    
    if(input$analytics == "correlation"){
      corr <- cor(close.180[,-1], use = "pairwise.complete")
      corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
               title = "Correlation matrix (ordered by hierarchical clustering)",
               mar = c(0,1,2,0))
    } else if(input$analytics == "bitcoin"){
      corr <- cor(close.180[,-1], use = "pairwise.complete")
      corr.bit <- corr[1:ncol(corr),"bitcoin", drop=FALSE]
      corrplot(t(corr.bit), diag = FALSE, tl.col = "black", tl.cex = 0.7, mar = c(0,1,2,0))
    } else if (input$marketcapanalytics == "mean-cap" & input$plot == "Bar Plot"){
      
      
      barplot(mean.cap.10[,2], names.arg = mean.cap.10[,1],las=2 , cex.names=0.9,
              main="Average market capital in top 10 Cryptocurrencies")
    }
    
    
  })
  
  output$ccoutput <- renderPrint({
    
    if (input$makedata == "Show Data"){
      df <- data_input_1()
      return(head(df))
    } else if (input$makedata == "Make Close"){
      out <- list(No_of_Currencies = length(names(makeclose())), names = names(makeclose()))
      print(out)
    } else if (input$makedata == "Show Newest Currency"){
      print(makeclose())
    } else if (input$makedata == "Logivity"){
      print(makeclose())
    } else if (input$analytics == "correlation"){
      print(makeclose())
    } else if(input$analytics == "bitcoin"){
      print(makeclose())
    } else if(input$analytics == "first-five"){
      print(makeclose())
    } else if(input$analytics == "first-five-negative"){
      print(makeclose())
    } else if (input$makedata == "Make Market Cap."){
      return(makeclose())
    } else if (input$marketcapanalytics == "mean-cap"){
      return(makeclose())
    }
    
  })
  
  output$plot1 <- renderPlot({
    if (input$analytics == "correlation"){
      plots()  
    } else if (input$analytics == "bitcoin"){
      plots()
    } else if (input$marketcapanalytics == "mean-cap" & input$plot == "Bar Plot"){
      plots()
    }
  })
  
  # top 10 analysis
  
  toptenanalysis <- reactive({
    df <- data_input_1()
    
    close.raw <- reshape(df[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
    close.raw[,"Close.Currency"] <- NULL
    
    close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
    colnames(close) <- sub("Close.", "", colnames(close))
    
    close.180 <- close[,colSums(!is.na(close)) >= 180]
    
    dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y")
    close$Date<- dates
    close <- close[-nrow(close),]
    
    if (input$topteninput == "Make TS"){
      return(head(close))
    }
    
    # long return
    
    rownames(close) <- close$Date
    close.xts <- as.xts(close)
    
    price10 <- as.xts(close[ , mean.cap.10.name])
    ret10.xts <- CalculateReturns(price10, method="log")
    ret10 <- data.frame(ret10.xts)
    
    if (input$topteninput == "Long Return"){
      return(head(ret10))
    }
    
    options(digits = 3)
    meanout <- data.frame(mean.percent = sort(apply(ret10[,1:ncol(ret10)], 2, 
                                                    function(x) mean(x, na.rm=TRUE)), decreasing = T))*100
    if (input$topteninput == "Mean"){
      return(meanout)
    }
    
    options(digits = 3)
    varout <- data.frame(variance.percent = sort(apply(ret10[,1:ncol(ret10)], 
                                                       2, function(x) sd(x, na.rm=TRUE)), decreasing = T))*100
    if (input$topteninput == "Variance"){
      return(varout)
    }
    
    options(digits = 3)
    stdout <- data.frame(variance.percent = sqrt(sort(apply(ret10[,1:ncol(ret10)], 
                                                            2, function(x) sd(x, na.rm=TRUE)), decreasing = T)))*100
    if (input$topteninput == "Std. Dev."){
      return(stdout)
    }
    
    cvarout <- CVaR(ret10)
    
    if (input$topteninput == "CVaR"){
      return(cvarout)
    }
    
    # Deterministic optimisation method
    
    ret10.CF <- na.omit(ret10)
    mean_vect <- colMeans(ret10.CF)
    cov_mat <- cov(ret10.CF)
    sd_vect <- sqrt(diag(cov_mat))
    
    M <- length(mean_vect)
    
    
    Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
    muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
    sdP <- muP # set up storage for std dev's of portfolio returns
    weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
    
    for (i in 1:length(muP)){
      bvec <- c(1, muP[i]) # constraint vector
      result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2) 
      sdP[i] <- sqrt(2*result$value) 
      weights[i,] <- result$solution
    }
    
    ind.min <- which.min(sdP)
    options(digits = 3)
    
    weight <- data.frame(proportion = weights[ind.min,], row.names = colnames(ret10.CF))
    
    #Expected return
    
    portselect <- list(min.value = muP[ind.min], std.dev = sdP[ind.min], proportions = weight)
    
    if (input$topteninput == "Portfolio Selection"){
      return(portselect)
    }
    
  })
  
  output$toptenoutput <- renderPrint({
    if (input$topteninput == "Make TS"){
      print(toptenanalysis())
    } else if (input$topteninput == "Long Return"){
      print(toptenanalysis())
    } else if (input$topteninput == "Mean"){
      print(toptenanalysis())
    } else if (input$topteninput == "Variance"){
      print(toptenanalysis())
    } else if (input$topteninput == "Std. Dev."){
      print(toptenanalysis())
    } else if (input$topteninput == "CVaR"){
      print(toptenanalysis())
    } else if (input$topteninput == "Portfolio Selection"){
      print(toptenanalysis())
    } else if (input$topteninput == "Portfolio Selection"){
      print(toptenanalysis())
    }
  })
  
  
  
  output$tsplots <- renderPlot({
    if (input$tsplot == "TS Plot"){
      price10 <- as.xts(close[ ,mean.cap.10.name])
      plot.xts(price10, main="Price")
      
    } else if (input$tsplot == "Volatility Chart"){
      price10 <- as.xts(close[ ,mean.cap.10.name])
      ret10.xts <- CalculateReturns(price10, method="log")
      vol30 <- xts(apply(ret10.xts, 2, runSD,n=30), index(ret10.xts))*sqrt(252)
      plot.xts(vol30)
    } else if (input$tsplot == "Currency Wise Vol. Charts"){
      
      price10 <- as.xts(close[ ,mean.cap.10.name])
      ret10.xts <- CalculateReturns(price10, method="log")
      vol30 <- xts(apply(ret10.xts, 2, runSD,n=30), index(ret10.xts))*sqrt(252)
      par(mfrow=c(2,1))
      for(i in 1:ncol(vol30)){
        print(plot(vol30[,i], main=colnames(vol30)[i]))
      }
    } else if (input$tsplot == "Correlation Chart"){
      
      price10 <- as.xts(close[ ,mean.cap.10.name])
      ret10.xts <- CalculateReturns(price10, method="log")
      ret10 <- data.frame(ret10.xts)
      
      chart.Correlation(ret10)
    } else if (input$tsplot == "Kernel Density"){
      price10 <- as.xts(close[ ,mean.cap.10.name])
      ret10.xts <- CalculateReturns(price10, method="log")
      ret10 <- data.frame(ret10.xts)
      par(mfrow=c(2,1)); 
      
      for(i in 1:ncol(ret10)){
        plot(density(ret10[,i], na.rm = T), main=colnames(ret10)[i])
      }
    } else if (input$tsplot == "Efficient Frontier"){
      
      price10 <- as.xts(close[ ,mean.cap.10.name])
      ret10.xts <- CalculateReturns(price10, method="log")
      ret10 <- data.frame(ret10.xts)
      ret10.CF <- na.omit(ret10)
      mean_vect <- colMeans(ret10.CF)
      cov_mat <- cov(ret10.CF)
      sd_vect <- sqrt(diag(cov_mat))
      
      M <- length(mean_vect)
      
      
      Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
      muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
      sdP <- muP # set up storage for std dev's of portfolio returns
      weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
      for (i in 1:length(muP)){
        bvec <- c(1, muP[i]) # constraint vector
        result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2) 
        sdP[i] <- sqrt(2*result$value) 
        weights[i,] <- result$solution
      }
      
      plot(100*sdP, 100*muP, type="l", xlim=c(0,250), ylim=c(0,35), 
           main="Efficient Frontier", ylab="Expected Return(%)", xlab="Standard deviation(%)")
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)

