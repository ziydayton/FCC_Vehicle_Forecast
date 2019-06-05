server <- function(input, output) {
  
  hideTab(inputId = "tabs", target = "Introduction")
  hideTab(inputId = "tabs", target = "Data Select")
  hideTab(inputId = "tabs", target = "Data View")
  hideTab(inputId = "tabs", target = "Forecasting Methods & Fit")
  hideTab(inputId = "tabs", target = "Forecast & Risk Window")
  
  plotsObject <- reactiveValues()
  
  newref <- reactive({
    input$ref_num
  })
  
  observeEvent(input$go, {
    isolate(input$pswd)
    if (input$pswd == "password") {
      hideTab(inputId = "tabs", target = "Password Entry")
      showTab(inputId = "tabs", target = "Introduction")
      showTab(inputId = "tabs", target = "Data Select")
      showTab(inputId = "tabs", target = "Data View")
      showTab(inputId = "tabs", target = "Forecasting Methods & Fit")
      showTab(inputId = "tabs", target = "Forecast & Risk Window")
    }
  })
  
  fitMethod <- reactive({
    input$rbFit
  })
  
  observeEvent( list(input$ref_num, input$rbFit), {
    
    if ((input$ref_num == "") | is.null(input$ref_num)) {return()}
    
    withProgress(message = 'Performing analysis', style = "notification", value = 0.0, {
      
      incProgress(0.05, detail = paste("Updating global variables"))
      get.set.global.vars(x = input$ref_num)
      
      incProgress(0.05, detail = paste("Reading vehicle data"))
      
      raw.usage.data <<- read.csv(file = paste0("data/", newref(), "_Usage.csv"),
                                 stringsAsFactors = FALSE)
      
      incProgress(0.05, detail = paste("Building data frames and tables"))
      plotsObject$tbl1 <- raw.usage.data
      usage.data <- FixMonthlyDateDataFrame(df = raw.usage.data)
      usage.data <- SpliceDataFrame(df = usage.data, elem = "Miles",
                                    elem.data = usage.data[, 2])
      day.usage.data <- ConsolidateDailyUseData(df = usage.data,
                                                start.date = FOC.date)
      month.usage.data <- ConsolidateMonthlyUseData(df = day.usage.data)
      ts.day <- ts(day.usage.data[, 2],
                   frequency = 365,
                   start = c(lubridate::year(min(day.usage.data[,1])),
                             lubridate::month(min(day.usage.data[,1])),
                             lubridate::day(min(day.usage.data[,1]))))
      
      incProgress(0.1, detail = paste("Generating data plots"))
      plotsObject$pl1 <- SimpleDataPlot(df = day.usage.data,
                                        main.title = paste("Daily Data Plot"),
                                        x.lab = paste("Time (Year)"),
                                        y.lab = paste("Daily Use (Miles)"))
      ts.month <- ts(month.usage.data[,2],
                     frequency = 12,
                     start = c(lubridate::year(min(month.usage.data[,1])),
                               lubridate::month(min(month.usage.data[,1]))))
      plotsObject$pl2 <- SimpleDataPlot(df = month.usage.data,
                                        main.title = "Monthly Data Plot",
                                        x.lab = "Time (Year)",
                                        y.lab = "Monthly Use (Miles)")
      incProgress(0.05, detail = paste("Finding length of time to forecast"))
      int.h <<- ceiling(lubridate::time_length(lubridate::interval(max(month.usage.data[,1]),
                                                                  EOL.date),
                                              unit = "months"))
      cum.day.usage.data <- GenerateCumData(df = day.usage.data, init = FOC.mileage)
      ts.day.cum <- ts(cum.day.usage.data[, 2],
                       frequency = 365,
                       start = c(lubridate::year(min(cum.day.usage.data[,1])),
                                 lubridate::month(min(cum.day.usage.data[,1])),
                                 lubridate::day(min(cum.day.usage.data[,1]))))
      cum.month.usage.data <<- GenerateCumData(df = month.usage.data, init = FOC.mileage)
      ts.month.cum <<- ts(cum.month.usage.data[,2],
                          frequency = 12,
                          start = c(lubridate::year(min(cum.month.usage.data[,1])),
                                    lubridate::month(min(cum.month.usage.data[,1]))))
      plotsObject$pl3 <- SimpleDataPlot(df = cum.month.usage.data,
                                        main.title = "Cumulative Monthly Data Plot",
                                        x.lab = "Time (Year)",
                                        y.lab = "Monthly Use (Miles)")
      ##Need to ensure training data does not reduce data set below 3 complete cycles
      incProgress(0.05, detail = paste("Building model training dataframes"))
      if (length(ts.month.cum) < 36) {
        ts.month.train <<- subset(ts.month.cum, end = (length(ts.month.cum) - 1))
      } else {
        ts.month.train <<- subset(ts.month.cum, end = (length(ts.month.cum) - 12))
      } ##/if length(ts.month.cum)
      
      ##FIT METHOD(S) SECTION
      incProgress(0.05, detail = paste("Generating Forecast"))
      fitMethods <- GenerateMultipleFitMethods()
      if (input$rbFit == 0) {  #DEFAULT case
        ##begin of auto deterine fit methods
        print(paste0("Current Fit Index: ", input$rbFit, "."))
        
        ##Begin Auto Fit
        fits <- GenerateMultipleFits(ts.df = ts.month.train, h.int = 12)
        fits.summary <- GenerateFitSummary(fit.x = fits,
                                           ts.x = ts.month.cum,
                                           fit.x.methods = fitMethods)
        plotsObject$tbl2 <- data.table(fits.summary)
        normFits <- NormMatrix(df = fits.summary)
        plotsObject$tbl3 <- data.table(normFits)
        orderNormFits <- AvgRank(df = normFits)
        plotsObject$tbl4 <- data.table(orderNormFits)
        plotsObject$tbl5 <- orderNormFits
        fit.index <<- as.numeric(gsub("Fit", "", orderNormFits$Fit[1]))
        
        incProgress(0.1, detail = paste("Generating Final Fit"))
        fit.choice <<- GenerateMultipleFits(ts.df = ts.month.cum, h.int = int.h, 
                                            incl.fit = fit.index)
        ##end of auto determine fit
      } else {
        
        ##begin user selected fit methods
        print(paste0("Current Fit Index: ", input$rbFit, "."))
       
        fit.index <<- as.numeric(input$rbFit)
        
        fits <- GenerateMultipleFits(ts.df = ts.month.train, h.int = 12,
                                     incl.fit = fit.index)
        
        fitSummary <- GenerateFitSummary(fit.x = fits,
                                         ts.x = ts.month.cum,
                                         fit.x.methods = fitMethods,
                                         multiFit = FALSE,
                                         indexNum = fit.index)
        plotsObject$tbl2 <- data.table(fitSummary)
        normFits <- NormMatrix(df = fitSummary)
        plotsObject$tbl3 <- data.table(normFits)
        orderNormFits <- AvgRank(df = normFits)
        plotsObject$tbl4 <- data.table(orderNormFits)
        plotsObject$tbl5 <- orderNormFits
    
        incProgress(0.1, detail = paste("Generating Final Fit"))
        fit.choice <<- GenerateMultipleFits(ts.df = ts.month.cum, h.int = int.h, 
                                            incl.fit = fit.index)
        
      }
      
      plotsObject$pl4 <- DataFitPlot(df = cum.month.usage.data,
                                     fm = fit.choice,
                                     ID = paste0(input$ref_num, " - Fit Plot"),
                                     subt = "Monthly Cumulative Miles with Fit",
                                     cap = paste0("Method: ", fit.choice$method),
                                     ylab = "Monthly Miles (Cumulative)",
                                     llab = "Monthly Miles",
                                     t.period = "days")
      
      plotsObject$pl5 <- DataFitForecastPlot(df = cum.month.usage.data, 
                                                              fm = fit.choice,
                                                              ID = paste0(input$ref_num, " - Forecast Plot"),
                                                              subt = "Monthly Cumulative Miles with Fit and Forecast (with 95% FI)",
                                                              cap = paste0("Method: ", fit.choice$method),
                                                              ylab = "Monthly Miles (Cumulative)",
                                                              llab = "Monthly Miles",
                                                              t.period = "months")
        
      
      
      
      
      incProgress(0.1, detail = paste("Generating Final Fit"))
      plotdata <- V1_ComplexPlotData(df = cum.month.usage.data,
                                     fm = fit.choice,
                                     end.use = EOL.mileage)
      
      f1 <- DataFitForecastPlotwEOL(df = plotdata,
                                      ID = paste0(input$ref_num, " - Forecast Plot with EOL Criteria"),
                                      subt = "Cumulative Monthly Miles with Fit and Forecast (with 95% FI)",
                                      cap = NULL,
                                      ylab = "Monthly Miles (Cumulative)",
                                      llab = "Monthly Mile",
                                      t.period = "months",
                                      end.use = EOL.mileage)
      
      plotsObject$pl6 <- f1
      key.points <- GeneratePoints_V1(df = plotdata, end.use = EOL.mileage,
                                      end.time = EOL.date, full = TRUE)
      plotdata2 <- GenerateRiskPlotData(df = plotdata, point = key.points,
                                        end.use = EOL.mileage, end.time = EOL.date)
      plotsObject$pl7 <- GenerateRiskPlot_V1(df = plotdata2, 
                                             key.dates = key.points)
      fin.key.points <<- GeneratePoints_V1(df = plotdata, end.use = EOL.mileage,
                                          end.time = EOL.date, full = FALSE)
      
      plotdata3 <- GenerateRiskPlotData(df = plotdata, point = fin.key.points,
                                        end.use = EOL.mileage, end.time = EOL.date)
      plotsObject$pl8 <- GenerateRiskPlot_V1(df = plotdata3, 
                                             key.dates = fin.key.points,
                                             ID = paste0(input$ref_num, " - Replacement Decision Space"),
                                             subt = paste0("Risk in Time - Relating key dates to EOL criteria for ", input$ref_num),
                                             cap = paste0("Bumper Number: ", input$ref_num))
      plotsObject$txt1 <- GetTextOfKeyDates(date.vec = fin.key.points)
      
      incProgress(0.05, detail = paste("COMPLETE"), Sys.sleep(1.0))
      
    }) ##/withProgress
  }) ##/observeEvent
  

  output$dataTableHeader1 <- renderText({
    paste0("Vehicle Data")
  })
  
  output$fleet <- renderDataTable({
    fleet.data
  })
  
  output$dataTableHeader2 <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    paste0("Usage data for ", input$ref_num)
  })
   
  output$usage <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl1
  })
  
  output$plot1 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl1
  }) ##/output$plot1
  
  output$plot2 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl2
  }) ##/output$plot2
  
  output$plot3 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl3
  }) ##/output$plot4
  
  output$fit.summary <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl2 
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable  
  ) ##/output$fits.summary
  
  output$fit.ranking <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl3
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable
  ) ##/output$fits.rank
  
  output$final.rank <- renderDataTable({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$tbl4
  }, options = list(searching=0, ordering=0, processing=0, paging=0, info=0) #Removes extra stuff on datatable
  ) ##/output$final.rank
  
  output$best.fit <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    fitrank <- plotsObject$tbl5
    paste0("The forecasting method applied: ", fitrank$Name[1],
           " [", fitrank$Fit[1], "]")
  }) ##/output$best.fit
  
  output$plot4 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$pl4
  }) ##/output$plot4

  
  output$plot5 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    suppressWarnings(print(plotsObject$pl5))
  }) ##/output$plot5  
  
  output$plot6 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    suppressWarnings(print(plotsObject$pl6))
  }) ##/output$plot6
  
  
  output$plot7 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    # print("Plot 7 to Strip and Combine")
    suppressMessages(
      f2 <- StripGGPlot(plot = plotsObject$pl6, 
                      ID = paste0(input$ref_num, " - Aligning Forecast to Replacement Decision Space (Risk in Time)"))
    )
    r2 <- plotsObject$pl7
    suppressWarnings(
      gridExtra::grid.arrange(f2, r2,
                              nrow = 2,
                              heights = c(75,25))
      )
  })##/output$plot7
  
  output$plot8 <- renderPlot({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    # print("Plot 8 Risk Window")
    plotsObject$pl8
  }) ##/output$plot8
  
  output$text1 <- renderText({
    if(is.null(input$ref_num) | input$ref_num == "") {return()}
    plotsObject$txt1
  }) ##/output$text1

} ##/server