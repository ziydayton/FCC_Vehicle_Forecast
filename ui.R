


ui = fluidPage(
  
 
  
  # Application title
  title = "Vehicle Replacement Forecast",
  
  h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center"),
  
  div(id = "header", align = "center", 
      img(src='Army.svg', align = "left",width = 100, height = 100 ),
      img(src='FuturesAndConceptsCenter.png', align = "right", width = 115, height = 110 ),
      h1("Vehicle Replacement Forecast")),
  
  tabsetPanel(id = "tabs",
    tabPanel("Password Entry",
             h1(),
             fluidRow(
               column(4, offset = 4,
                    wellPanel(
                      h4(),
                      passwordInput("pswd", "Enter 'password':"),
                      actionButton("go", "Go")
                      ) ##/wellPanel
                    )),
             fluidRow(
               h1(),
             h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
             )
             ), ##/tabPanel "Password Entry"
            
    tabPanel("Introduction",
      div(id = "intro", 
        wellPanel(h2("Purpose"),
  
                h4("This app was developed to forecast when a person should consider replacing their car based on end of life criteria for mileage and time. The app take the a daily vehicle usage log as an  input and transforms it into cumulative monthly usage for forecasting.  These forecasts establish a reasonable expectation as to when the vehicle should be replaced."),
                h1(" "),
                hr(style="border-color: black;"),
                h2("Instructions"),
                h4(
                  tags$b("Step 1"),
                  " (Required): In the next tab, ",
                  tags$i("Data Select"),
                  ", use the drop down to select the vehicle of interest by bumper number."),
                h4(
                  tags$b("Step 2"),
                  "(Optional): The ",
                  tags$i("Data View"), 
                  " tab displays the usage data visually."),
                h4(
                  tags$b("Step 3"),
                  " (Optional): The ", 
                  tags$i("Forecasting Methods & Fit"), 
                  " tab provides information on the forecasting methods examined, summaries and rankings of forecasting methods, and initial visualizations."),
                h4(
                  tags$b("Step 4"), 
                  " (Required): The ",
                  tags$i("Forecast & Risk Window"),
                  " tab displays the answer.  It contains visualizations that summarize the time series forecast, the development of the risk window to forecast replacement, and the key dates to consider replacement."),
                h1()),
      wellPanel(h2("Package information"),
                h4("The following software/packages, with versions, were used in the development of this application."), 
                h5("R 3.4.4"),   
                h5("RStudio 1.1.456"),
                h5("data.table 1.11.8"),
                h5("dplyr 0.7.8"),
                h5("forecast 8.4"),
                h5("ggplot2 3.1.0"),
                h5("grid 3.4.4"),
                h5("gridExtra 2.3"),
                h5("lubridate 1.7.4"),
                h5("scales 1.0.0"),
                h5("shiny 1.0.5"),
                h5("stats 3.4.4"),
                h5("zoo 1.8-4")
                
      ) ##/wellPanel
  ),
  h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
    ), ##/tabPanel - Introduction
  
  tabPanel("Data Select",
           wellPanel(h4("In this section, the actual vehicle is identified by bumper number to differentiate from other low density vehicles."),
                     h4("The analysis begins with the selection of the vehicle by bumper number.  The complete list of vehicles is in the first table below. This table of low density vehicles includes other information such as procurement dates and end of lifecycle (EOL) criteria specific to the vehicle."),
                     h4("The second table appears after making a vehicle choice.  The second table displays the vehicle use log that is the foundational raw data for the analysis."),
                     h4("Please note that the majority of the data analysis is performed when a vehicle is selected below.  The app may pause, with a progress bar appearing.  This is normal.")),
                     
                     selectInput("ref_num", h4(em(p(strong("Please select a bumper number."))), style = "color: green" ), 
                                 choices = c(Choose="", as.list(fleet.data$Bumper_Num)),
                                 selectize = TRUE),
                     
                     wellPanel(
                       wellPanel(
                         h3(textOutput("dataTableHeader1")), 
                                         style = "background-color: lightskyblue" 
                         ),
                         dataTableOutput("fleet"), align = "center"),
                     
                     wellPanel(
                       wellPanel(
                         h3(textOutput("dataTableHeader2")),  
                         style = "background-color: lightskyblue"
                         ),
                       dataTableOutput("usage"), align = "center"),
           
           h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
           ), ##/tabPanel - Data SELECT

  tabPanel("Data View",
    wellPanel(
      h4("Visualizations of the data are displayed below.  The following plots provide a quick idea of what the data looks like, providing an indication of frequency, variance, and sparsity."),
      h4("The first two plots are paired by use per day and month, respectively."),
      h4("For the third plot the data is transformed to a running total or cumulative use by month over time.  This cumulative data is used to forecast the usage.")
    ),
    
    wellPanel(
      wellPanel(
        h3("Daily and Monthly Data"), 
        style = "background-color: lightskyblue" ),
        plotOutput("plot1"), align = "center"),
        
      wellPanel(
        plotOutput("plot2"), align = "center"),
        wellPanel(
          wellPanel(
            h3("Cumulative Monthly Data"), 
                               style = "background-color: lightskyblue" ),
                     plotOutput("plot3"), align = "center"),
    h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
  ), ##/tabPanel - Data View
  
  tabPanel("Forecasting Methods & Fit", 
    wellPanel(
      wellPanel(
        h4("There are 10 forecast methods explored, ranging from linear regression to more complex machine learning techniques such as Seasonal Trend-Loess, Holt Winters, and ARIMA."),
        style = "background-color: lightskyblue", align = "center" ),
        h4("The selection below defaults to use all 10 forecast methods and apply one of the best forecast fits.  This selection enables an analyst to apply other forecast methods.  Be advised that not all forecast methods are applicable to all data sets and/or a sub-optimal answer is highly possible.  Use individual methods with caution and prudent review.")
      ), ##/wellPanel
          
      radioButtons("rbFit", "Manually Select Forecast Method (Optional):",
                  choiceNames = c("Auto fit - Test all (DEFAULT)",
                                 GenerateMultipleFitMethods()),
                  choiceValues = c(0:10),
                  selected = 0,
                  inline = TRUE
      ),

      wellPanel(                     
        tags$div("The forecast methods and the resulting accuracy metrics (expected performance) are displayed in the tables below.  These metrics include:",
          tags$br(),
          tags$br(),
             tags$ul(
               tags$li("ME - Mean error is the average of all the errors in a set."),
               tags$li("RMSE - Root-mean-squared error (RMSE) is used to measure the differences between values predicted by a model the values observed.  Also known as root-mean-squared deviation (RMSD)."),
               tags$li("MAE - Mean absolute error is a measure of difference between two continuous variables."),
               tags$li("MPE - Mean percentage error (MPE) is the computed average of percentage errors by which forecasts of a model differ from actual values of the quantity being forecast.") ,
               tags$li("MAPE - Mean absolute percentage error is a measure of prediction accuracy of a forecasting method. Also known as mean absolute percentage deviation (MAPD). Heuristic goal < 20%."),
               tags$li("MASE - Mean absolute scaled error is a measure of the forecast accuracy."),
               tags$li("ACF1 - Autocorrelation of errors at lag 1 is a measure that determines the correlation between past and future data points in a time series.  Autocorrelation values are the most obvious way to measure the linearity of the dependence between the current values of the time series versus the lagged values of the time series."), 
             type = "disc"),
             tags$br(),
             tags$br()
             ),
           
           wellPanel(h3("Fit & Accuracy Summary Table"),
                     style = "background-color: lightskyblue", align = "center" ),
                     
           dataTableOutput("fit.summary"),
           tags$div(tags$br(),
                    tags$br()
                    ),
           wellPanel(h3("Normalized Fit & Accuracy Summary Table"),
                     style = "background-color: lightskyblue", align = "center" ),
           dataTableOutput("fit.ranking"),
           wellPanel(h3("Ordered List of Average Normalized Fit & Accuracy"),
                     style = "background-color: lightskyblue", align = "center" ),
           tags$div(h3("The 'best fit' (the best model to use for the forecast) is selected by the lowest average ranks of the accuracy metrics."),
                    tags$br()
                    ),
           dataTableOutput("final.rank"),
           wellPanel(h3(textOutput("best.fit")),
                     style = "background-color: lightgreen", align = "center" )
           ),
           wellPanel(wellPanel(h3("Initial Fit & Forecast Plots"),
                               style = "background-color: lightskyblue", align = "center" ),
                     plotOutput("plot4")
                     ),
           wellPanel(plotOutput("plot5")
           ),
    h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
  ), ##/tabPanel - Forecast Methods
  
  tabPanel("Forecast & Risk Window",
    wellPanel(
      wellPanel(
        h3("Forecast (to EOL criteria)"),
        style = "background-color: lightskyblue", 
        align = "center" 
      ),
      h4("The usage data is forecast to determine when EOL conditions are met.  The visualization below displays the usage data (green bars), the fit of the usage data (black line), the forecast (blue line), the forecast interval (gray shading), and the EOL criteria for usage and time (red lines)."),
      h4("The intersection of the forecast (blue line) with the EOL criteria (red lines) indicates the most likely date of replacement.  The earliest intersection of the forecast interval (gray shading) indicates when the earliest projected replacement date could occur.  Rather than a specific date, the solution could include a range of dates, or window."),
      
      plotOutput("plot6"),
      
      tags$br(),
      tags$br(),
      
      wellPanel(
        h3("Developing the Forecast Window"),
        style = "background-color: lightskyblue", 
        align = "center" 
      ),
      
      h4("The plot below is a transition visualization, aligning the forecast plot with a color band that includes the key dates.  The key dates are identified to inform the solution of when replacement is needed.  The dates associated with these points [and meaning of colors] are:"),
      
      tags$ul(
        tags$li("The FOC date of the vehicle -- the beginning of the usage data. [Green - The vehicle does not require replacement.]"),
        tags$li("The last known point of data. [Green - The vehicle does not require replacement.]"),
        tags$li("The first instance of the 95% forecast interval intersecting the EOL criteria. [Yellow - Earliest forecasted replacement point, consider replacement.] "),
        tags$li("The first instance of the forecast mean intersecting either of the EOL criteria. [Red - Projected most likely replacement point, must replace.]"), 
        style = "disc"),
      tags$br(),
      tags$br(),
      
      plotOutput("plot7"),
      
      tags$br(),
      tags$br(),
      
      wellPanel(
        h3("Forecast Window"),
        style = "background-color: lightskyblue", 
        align = "center" 
      ),
      h4("The plot below is the forecast window of when to plan for replacement. The forecast window begins at the end of the known data, which is also the beginning of the forecast.  This color band ends with the forecast meeting the EOL criteria.  As above, key dates are established.  Specifically for this visual, the key dates [and colors] are:"),
      
      tags$ul(
        tags$li("The last known point of data. [Green - The vehicle does not require replacement.]"),
        tags$li("The first instance of the 95% forecast interval intersecting the EOL criteria. [Yellow - Earliest forecasted replacement point, consider replacement.] "),
        tags$li("The first instance of the forecast mean intersecting either of the EOL criteria. [Red - Projected most likely replacement point, must replace.]"), 
        style = "disc"),
      tags$br(),
      
      div(plotOutput("plot8", width = "40%"), align = "center"),
      
      tags$br(),       
      wellPanel(
        h3(textOutput("text1")),
        style = "background-color: lightgreen", 
        align = "center"
      )
      
    ), ##/wellPanel
    h4(p(strong("UNCLASSIFIED")), style = "color: green", align = "center")
  ) ##/tabPanel - Forecast & Risk Window
  
  ), ##/tabSetPanel
  
  tagList(
    singleton(
      tags$head(
        tags$style(type="text/css", "tfoot {display:none;}")
      )
    )
  ),
  dataTableOutput('customtable') 
  ) ##/ ui