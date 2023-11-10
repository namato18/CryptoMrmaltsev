library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(aws.s3)
library(plotly)
library(shinyWidgets)
library(shinyalert)
library(binance)
library(purrr)
library(shinymanager)
library(fresh)
library(shinybusy)
library(rvest)
library(stringr)
library(jsonlite)
library(shinythemes)
library(padr)

Color.DT = function(df){
  
  if(is.null(df)){
    return(NULL)
  }else{
    dt.colored = datatable(df,
                           rownames = FALSE,
                           extensions = c("Buttons","FixedHeader"),
                           style = "bootstrap",
                           options = list(paging = FALSE,fixedHeader = TRUE, searching = FALSE, dom = 'Bfrtip', buttons = c('csv'))) %>%
      formatStyle("Signal",
                  backgroundColor = styleEqual(c("DON'T BUY SIGNAL", "BUY SIGNAL"), c('darkred','lightgreen')))
    
    return(dt.colored)
  }
}

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

api.key = "HKYWSDAZQS14QKVB7KY1AKTQURYMEPFFZU"


# mytheme <- create_theme(
#   theme = "cyborg",
#   bs_vars_color(
#     brand_primary = "#75b8d1",
#     brand_success = "#c9d175",
#     brand_info = "#758bd1",
#     brand_warning = "#d1ab75",
#     brand_danger = "#d175b8"
#   ),
#   bs_vars_navbar(
#     default_bg = "#75b8d1",
#     default_color = "#d175b8",
#     default_link_color = "#d175b8",
#     default_link_active_color = "#d175b8"
#   )
# )

mytheme <- create_theme(
  
  adminlte_color(
    light_blue = "#17202a",
    
  ),
  adminlte_sidebar(
    width = "400px",
    light_bg = "#FFF",
    dark_bg = "#FFF",
    dark_hover_bg = "#586473",
    dark_color = "#FFF"
  ),
  adminlte_global(
    content_bg = "#001321",
    box_bg = "#001A2C",
    info_box_bg = "#D8DEE9"
    
  )
  
)

# token.names.df.comb = readRDS("tickers/token.names.df.comb.rds")

str1 = readRDS('tickers/str1.rds')
str2 = readRDS('tickers/str2.rds')

str1 = str1[-61]
str2 = str2[-61]

fx.pair.names = readRDS("tickers/fx.pair.names.rds")

coin_decimals = readRDS('coin_decimals.rds')

stock1 = readRDS('tickers/stock1.rds')

checkbox_list = setNames(str1, str1)
stock.names = setNames(stock1,stock1)

# token.names.list = setNames(token.names.df.comb$tokens, token.names.df.comb$names)

possibly_spot_new_order = possibly(spot_new_order, otherwise = 'ERROR')
possibly_s3read_using = possibly(s3read_using, otherwise = 'ERROR')

# df.master.comb = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData", object = "df.master.comb.rds")
df.comb.all = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData", object = "df.comb.all.v2.rds")

# Define UI
ui <- secure_app(
  theme=shinytheme("cyborg"),
  dashboardPage(
    title = "MLModel",
    dashboardHeader(title = tags$a(tags$text("Markets Prophet"),
                                   tags$img(src="logo_123_nobg.png",height="40",width="40"),
                                   style="color: white"),
                    tags$li(
                      class = "dropdown",
                      tags$style(HTML("
          .navbar-custom-menu{float:left !important;}
          .sidebar-menu{display:flex;align-items:baseline;}"))
                    ),
                    
                    tags$li(
                      class = "dropdown",
                      sidebarMenu(
                        id = "tablist",
                        menuItem(text = "", tabName = "create", icon = icon("house")),
                        menuItem("", tabName = 'predictMultiple', icon = icon('money-bill-trend-up')),
                        menuItem("", tabName = 'predictNextWeek', icon = icon('chart-line')),
                        # menuItem("", tabName = 'alphaVantageBacktest', icon = icon('calendar')),
                        menuItem("", tabName = "forexFactoryBacktest", icon = icon('calendar'))
                        # menuItem("", tabName = "etherscan", icon = icon("searchengin"))
                      )
                    )
                    
    ),
    # dashboardHeader(title = shinyDashboardLogo(
    #   theme = "poor_mans_flatly",
    #   boldText = "Markets",
    #   mainText = 'Prophet',
    #   badgeText = NULL
    # ),
    # titleWidth = 300
    #                 ),
    dashboardSidebar(
      disable = TRUE
      # sidebarMenu(
      #   menuItem(text = "", tabName = "create", icon = icon("house")),
      #   menuItem("", tabName = 'predictMultiple', icon = icon('money-bill-trend-up')),
      #   menuItem("", tabName = 'predictNextWeek', icon = icon('chart-line')),
      #   menuItem("", tabName = "etherscan", icon = icon("searchengin"))
      #   # menuItem("Build TradingView Model", tabName = 'inputCoin', icon = icon('upload')),
      #   # menuItem("Binance", tabName = "binance", icon = icon('sack-dollar')),
      #   # menuItem("Binance Automation", tabName = "automation", icon = icon('robot'))
      #   
      #   
      #   # menuItem("Most Likely Outcome", tabName = "likely")
      #   
      # )
    ),
    dashboardBody(
      use_theme(mytheme),
      # shinyDashboardThemes(
      #   theme = "poor_mans_flatly"
      # ),
      tabItems(
        tabItem(tabName = "create",
                fluidPage(
                  shinyjs::useShinyjs(),
                  tags$head(tags$style('body {color:white;}')),
                  tags$head(tags$style('table {color:white;}')),
                  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
                  add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "bottom-right"),
                  # setBackgroundColor(color="black",shinydashboard = TRUE),
                  # setBackgroundImage(src = "green2.jpg",shinydashboard = TRUE),
                  # verbatimTextOutput("auth_output"),
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  # HTML('<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_blank">
                  #        <input type="hidden" name="cmd" value="_s-xclick">
                  #        <input type="hidden" name="hosted_button_id" value="2MGB68YUJEB5Q">
                  #        <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_subscribeCC_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!" target="_blank">
                  #        <img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1" target="_blank">
                  #        </form>'),
                  
                  box(title = "Creating a Backtesting Model", solidHeader = TRUE, status = "primary", width = 12,
                      paste0("On this tab you can use the sliders to modify how the predictive model is created. First you need to select a timeframe ",
                             "and coin that you're interested in predicting. ","The first slider is used ",
                             "to select the percentage increase in the timeframe that you've selected. The second slider is used ",
                             "to select how confident you want the model to be in order to classify a 'BUY'. The model will make a prediction on a scale from ",
                             "0-1, the closer to 1 the prediction is, the more confident the model is that your selected percentage increase will happen in your selected timeframe.")
                  ),
                  br(),
                  
                  
                  column(width = 6,
                         box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                             selectInput("selectType", "Pick Which Type to Predict", choices = list("Stocks" = "Stocks",
                                                                                                    "Crypto" = "Crypto",
                                                                                                    "Forex" = "Forex")),
                             selectInput("timeframe","Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                        "1 Hour" = "1hour",
                                                                                        "4 Hour" = "4hour",
                                                                                        "8 Hour" = "8hour",
                                                                                        "1 Day" = "1day")),
                             selectInput("select","Pick a crypto to predict", choices = checkbox_list),
                             br(),
                             sliderInput("slider1","Select Percentage Increase", min = 0.1, max = 1, step = 0.1, value = 0.1),
                             sliderInput("slider2", "Confidence Score 'BUY' Threshold", min = 0.1, max = 1, step = 0.02, value = 0.5),
                             # strong("Note: IT IS STRONGLY RECOMMENDED TO PLACE YOUR TAKE PROFIT TO THE SAME VALUE AS YOUR TARGET PERCENTAGE INCREASE"),
                             # br(),
                             # paste0("Metrics by default are calculated based on the candles closing value. ",
                             #        "You can use the TP (take profit) input field to specify your TP. ",
                             #        "Setting a TP can limit your gains, but can also limit your losses! ",
                             #        "Leaving the TP value at 0 will set the metrics to be calculated based on candles closing value."),
                             # numericInput('tp',"Set TP % (must be positive)", value = 0, min = 0),
                             # numericInput("sl","Set SL (must be negative)", value = 0, max = 0),
                             actionButton('action1', label = "Generate"),
                             br(),
                             br()
                         ),
                         box(title = "Histogram", width = NULL, status = "primary", solidHeader = TRUE,
                             paste0("Ideally, we'd like there to be a near 0 probability or a near 1 probability for all predictions. ",
                                    "Values that are more in the middle can give us an unclear prediction."),
                             plotOutput("modelPlot")
                         ),
                         box(title = "Sentiment Backtest Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                             valueBoxOutput(outputId = "precisionBoxSentiment", width = 12),
                             # valueBoxOutput(outputId = "recallBox", width = 6),
                             # valueBoxOutput(outputId = "f1Box", width = 4),
                             valueBoxOutput(outputId = "totalDataSentiment", width = 6),
                             valueBoxOutput(outputId = "predictedHitsSentiment", width = 6)
                             
                         )
                         # box(title = "Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                         #   # infoBoxOutput("OverallAccuracy", width = 6),
                         #   infoBoxOutput("Buy", width = 6),
                         #   infoBoxOutput("SumPercentage", width = 6),
                         #   # infoBoxOutput("DontBuy", width = 6),
                         #   infoBoxOutput("Predictions", width = 6),
                         #   infoBoxOutput("Hits", width = 6)
                         #   
                         # ),
                  ),
                  column(width = 6,
                         box(title = "Metric Descriptions", width = NULL, status = "primary", solidHeader = TRUE,
                             strong("Precision: "),paste0("A measure of how accurate the model is at hitting it's target when it gives a buy signal.",
                                                          " The equation goes as: (true positives) / (true positives + false positives)"),
                             br(),
                             br(),
                             # strong("Recall: "),paste0("A measure of what percentage of buy signals WERE classified correctly compared to how many SHOULD have been classified as a buy signal",
                             #                           " The equation goes as: (true positives) / (true positives + false negatives)"),
                             # br(),
                             # br(),
                             # strong("F1 Score: "),paste0("A value from 0 to 1, 1 being the model classifies every observation correctly. The equation goes as: 2 * (precision * recall) / (precision + recall)"),
                             # br(),
                             # br(),
                             strong("Number of Candles Backtested: "), paste0("The amount of historical data that has been tested for the selected time period"),
                             br(),
                             br(),
                             strong("Predicted Buy Signals: "), paste0("How many times did the model predict a buy signal on the backtested data")
                         ),
                         box(title = "Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                             valueBoxOutput(outputId = "precisionBox", width = 12),
                             # valueBoxOutput(outputId = "recallBox", width = 6),
                             # valueBoxOutput(outputId = "f1Box", width = 4),
                             valueBoxOutput(outputId = "totalData", width = 6),
                             valueBoxOutput(outputId = "predictedHits", width = 6)
                             
                         ),
                         box(width = NULL, title = "Backtest", status = "primary", solidHeader = TRUE,
                             strong(h4("Variable Info:")),
                             strong('Actual:'),
                             paste0("If the next candle actually hit the target percentage increase, this will be 'HIT TARGET', otherwise 'MISSED TARGET'. ",
                                    "The color will be GREEN if a profit could have been made and RED if a loss could have been made."),
                             br(),
                             strong("Actual High:"),
                             paste0("This was the next candles high"),
                             br(),
                             strong("Actual Low:"),
                             paste0("This was the next candles low"),
                             br(),
                             strong("Actual Close:"),
                             paste0("This was the next candles close"),
                             br(),
                             strong("Confidence Score:"),
                             paste0("This is the confidence the model has that the next candle would reach the target percentage increase (on a scale of 0 to 1)"),
                             br(),
                             strong("Signal:"),
                             paste0("If the 'Confidence Score' is higher than the selected prediction BUY threshold, this will be 'DID BUY', otherwise 'DIDN'T BUY'"),
                             br(),
                             br(),
                             dataTableOutput("table1")
                         )

                         
                  )
                  
                  
                )
        ),
        # tabItem(tabName = "predict",
        #         fluidRow(
        #           strong(h3("About:")),
        #           strong("Note that you must create a model on the previous tab before predicting tomorrow!"),
        #           br(),
        #           paste0("This tab will simply generate a prediction using the model you created in the 'Creating a Model' Tab. ",
        #                  "Remember that the probability is on a scale of 0-1, where a larger value represents a higher probability of your ",
        #                  "model's prediction coming true."),
        #           br(),
        #           br(),
        #           box(title = "Predict Tomorrow", status = "primary", solidHeader = TRUE,
        #             textInput("open","Open"),
        #             textInput("close","Close"),
        #             textInput("low","Low"),
        #             textInput("high","High"),
        #             actionButton("action2","Predict"),
        #             br(),
        #             br(),
        #             infoBoxOutput("predict", width = NULL)
        #           )
        #         )
        #         ),
        tabItem(tabName = "predictMultiple",
                fluidRow(
                  add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "bottom-right"),
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  box(title = "Predict Next Candle (Multiple):", status = "primary", solidHeader = TRUE,width = 12,
                      paste0("On this tab you can generate predictions for multiple coins! Simply use the check boxes to select which coins you'd like to predict.",
                             " If you'd like to export these results, simply press the 'csv' button on top of the table below.")
                  ),
                  br(),
                  br(),
                  box(title = "Predict Multiple", status = "primary", solidHeader = TRUE, height = 600,
                      # actionButton('selectall','Select All'),
                      selectInput("selectTypeMult", "Pick Which Type to Predict", choices = list("Stocks" = "Stocks",
                                                                                                 "Crypto" = "Crypto",
                                                                                                 "Forex" = "Forex")),
                      selectizeInput('checkGroup',label = 'Select Coin(s)', choices = checkbox_list, multiple = TRUE, options = list(maxItems = 4)),
                      # checkboxGroupInput('checkGroup', label = 'Select Coin(s)',
                      #                    choices = checkbox_list,
                      #                    selected = 'btcusd'),
                      selectInput("timeframePredict","Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                        "1 Hour" = "1hour",
                                                                                        "4 Hour" = "4hour",
                                                                                        "8 Hour" = "8hour",
                                                                                        "1 Day" = "1day")),
                      sliderInput("slider3", "Select Prediction 'BUY' Threshold", min = 0.1, max = 1, step = 0.05, value = 0.5),
                      actionButton("action4","Predict"),
                      br(),
                      br(),
                      strong(paste0("****** NOTE THAT I AM NOT RESPONSIBLE FOR FINANCIAL LOSS OR GAIN. PLACE TRADES AT YOUR OWN RISK. ",
                                    "IT IS GOOD TO USE THIS TOOL TO HELP YOU MAKE DECISIONS SUPPORTED BY OTHER EVIDENCE. ******")),
                      br(),
                      br()
                      
                      
                  ),
                  box(title = "Candlestick Chart", status = "primary", solidHeader = TRUE, height = 600,
                      br(),
                      selectInput('candlestickInput','Choose Asset to View (options updated after predictions are made)', choices = NULL),
                      plotlyOutput('candlestickPlot')
                      
                  ),
                  
                  box(title = "Predictions", status = "primary", solidHeader = TRUE, width =12,
                      br(),
                      strong(textOutput('timer')),
                      br(),
                      strong(h4("Variable Info:")),
                      strong('Coin:'),
                      paste0("The coin being predicted"),
                      br(),
                      strong('Price.Change:'),
                      paste0("The price change that's being predicted"),
                      br(),
                      strong('CS HT (Confidence Score Hit Target):'),
                      paste0("The confidence score that the Price.Change will be hit next candle"),
                      br(),
                      strong('CS BH (Confidence Score Break Previous High):'),
                      paste0("The confidence score that the Price.Change will break through the previous high"),
                      br(),
                      strong('Prev.High:'),
                      paste0("The previous candles high"),
                      br(),
                      strong('CS BL (Confidence Score Break Previous Low):'),
                      paste0("The confidence score that the Price.Change will break through the previous low"),
                      br(),
                      strong('Prev.Low:'),
                      paste0("The previous candles low"),
                      br(),
                      strong('Signal:'),
                      paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold")
                      
                  ),
                  box(title = "Prediction 1", status = "primary", solidHeader = TRUE, width=6,
                      dataTableOutput("multipleOutput1")
                  ),
                  box(title = "Prediction 2", status = "primary", solidHeader = TRUE, width=6,
                      dataTableOutput("multipleOutput2")
                  ),
                  box(title = "Prediction 3", status = "primary", solidHeader = TRUE, width=6,
                      dataTableOutput("multipleOutput3")
                  ),
                  box(title = "Prediction 4", status = "primary", solidHeader = TRUE, width=6,
                      dataTableOutput("multipleOutput4")
                  ),
                  box(title = "Sentiment Analysis", status = "primary", solidHeader = TRUE, width=12,
                      strong("NOTE THAT SENTIMENT ANALYSIS ONLY DISPLAYS PREDICTIONS FOR THE 1 DAY TIMEFRAME"),
                      br(),
                      br(),
                      column(width = 6,
                             valueBoxOutput("fearGreedRating", width = 6),
                             valueBoxOutput("OneHrSent", width = 6)
                             ),
                      column(width = 6,
                             valueBoxOutput("EightHrSent", width = 6),
                             valueBoxOutput("TwenFourHrSent", width = 6)
                             ),
                      br(),
                      br(),
                      
                      column(width = 6,
                             box(title = "Sentiment 1", status = "primary", solidHeader = TRUE, width=12,
                                 dataTableOutput("sentimentOutput1")
                             ),
                             box(title = "Sentiment 3", status = "primary", solidHeader = TRUE, width=12,
                                 dataTableOutput("sentimentOutput3")
                             )
                      ),
                      column(width = 6,
                             box(title = "Sentiment 2", status = "primary", solidHeader = TRUE, width=12,
                                 dataTableOutput("sentimentOutput2")
                             ),
                             box(title = "Sentiment 4", status = "primary", solidHeader = TRUE, width=12,
                                 dataTableOutput("sentimentOutput4")
                             )
                      )
                      
                      
                      
                  )
                  
                )
        ),
        
        tabItem(tabName = "inputCoin",
                fluidRow(
                  add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "bottom-right"),
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  strong(h1("Generate Model using TradingView Data:")),
                  box(width = 12,
                      paste0("On this tab you can generate a predictive model for data that you input from TradingView. You need to export TradingView data ",
                             "with no indicators on the chart. The 'Time Format' must also be set to ISO time. Name the exported file follwing the format <coinsymbol>.csv. For example, BTCUSD data would simply be BTCUSD.csv. Once you've exported the TradingView data",
                             " you simply drag that file into the input below. A timeframe must also be selected.")
                  ),
                  br(),
                  br(),
                  box(title = "Build Model from TradingView", status = "primary", solidHeader = TRUE,
                      fileInput('tvDataDrop', label = 'Input TradingView Data Here'),
                      selectInput("tvTimeFrame","Pick a Timeframe", choices = list("4 Hour" = "4hour",
                                                                                   "8 Hour" = "8hour",
                                                                                   "1 Day" = "1day",
                                                                                   "1 Week" = "7day",
                                                                                   "1 Month" = '1month')),
                      sliderInput("tvSlider","Select Percentage Increase", min = 1, max = 5, step = 1, value = 1),
                      actionButton('action6','Predict')
                      
                  ),
                  box(title = "Predict Next Candle", status = "primary", solidHeader = TRUE, width =12,
                      strong(h4("Variable Info:")),
                      strong('Coin:'),
                      paste0("The coin being predicted"),
                      br(),
                      strong('Price.Change:'),
                      paste0("The price change that's being predicted"),
                      br(),
                      strong('Confidence.Score.HIT.TARGET:'),
                      paste0("The confidence score that the Price.Change WILL be hit next candle"),
                      br(),
                      strong('Confidence.Score.MISS.TARGET:'),
                      paste0("The confidence score that the Price.Change WILL NOT be hit next candle"),
                      br(),
                      strong('Signal:'),
                      paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold"),
                      br(),
                      withSpinner(dataTableOutput('TVPrediction'))
                  )
                  
                )
        ),
        
        tabItem(tabName = "predictNextWeek",
                fluidRow(
                  add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "bottom-right"),
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  column(width = 6,
                         box(title = "Predict Next 7 Days/Weeks:", status = "primary", solidHeader = TRUE, width = 12,
                             paste0("On this tab you may pick a crypto to forecast for the next 7 days/weeks! The machine learning model utilizes the past 14 candles of data ",
                                    "to predict the next 7 candles price movements!"),
                             br(),
                             br(),
                             selectInput("selectTypeWeek", "Pick Which Type to Predict", choices = list("Stocks" = "Stocks",
                                                                                                        "Crypto" = "Crypto",
                                                                                                        "Forex" = "Forex")),
                             selectInput('selectTimeFrame', 'Pick a Timeframe', choices = list('7 Days' = 'daily',
                                                                                               '7 Weeks' = 'weekly')),
                             selectInput('selectNextWeek', "Select a Coin", choices = checkbox_list),
                             actionButton("action5", "Predict"),
                             br(),
                             strong('Note: Previous data is displayed in BLUE while forecasted data is displayed in RED'),
                             br()
                             
                         )
                         
                         
                  ),
                  column(width = 6,
                         dataTableOutput("forecastWeekOutput")
                         
                  ),
                  
                  
                  plotOutput("nextWeekOutput")
                  
                  
                )
        ),
        
        tabItem(tabName = "binance",
                fluidRow(
                  
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  strong(h1("Binance Integration")),
                  box(width=10,
                      paste0("This tab offers you the capability of performing trades on Binance directly through this interface.")
                  ),
                  box(title = "Inputs", status = "primary", solidHeader = TRUE,
                      selectInput('selectCoinBinance', "Select a Coin", choices = checkbox_list, selected = 'BTCUSDT'),
                      br(),
                      selectInput('selectTypeBinance', 'Market or Limit', choices = list('Market' = 'MARKET',
                                                                                         "Limit" = 'LIMIT'),
                                  selected = 'Market'),
                      br(),
                      selectInput('selectSideBinance', 'Buy or Sell', choices = list("Buy" = "BUY",
                                                                                     "Sell" = "SELL"),
                                  selected = 'Buy'),
                      br(),
                      # sliderInput("takeProfitBinance", "Set Take Profit %",min = 0, max = 20, step = 0.1, value = 0),
                      # br(),
                      # sliderInput("stopLossBinance", "Set Stop Loss %",min = 0, max = 20, step = 0.1, value = 0),
                      # br(),
                      numericInput("tradeQuantity", "Quantity", value = 0, min = 0, step = 0.1),
                      textOutput('decimalsAllowed'),
                      br(),
                      sliderInput('percentSliderBinance', 'Percentage of USDT balance',value = 0,min = 0, max = 100, step = 0.1)
                  ),
                  box(title = "Live Price", status = "primary", solidHeader = TRUE,
                      actionButton(inputId = 'getLivePrice', label = 'Refresh Live Price'),
                      br(),
                      br(),
                      textOutput('livePrice')
                  ),
                  box(title = "Spot Account Balances", status = "primary", solidHeader = TRUE,
                      dataTableOutput('spotAccountBalances')
                  ),
                  actionBttn(inputId = 'submitBinance',
                             label = 'Submit',
                             icon = icon('money-bill-trend-up'),
                             style = 'pill',
                             color = 'warning',
                             size = 'lg',
                             block = TRUE),
                  br(),
                  dataTableOutput('binancePredictionTable')
                  
                  
                )
        ),
        tabItem(tabName = "automation",
                fluidRow(
                  # img(src='ai3.png', width = 125, height = 125, align = 'right' ),
                  strong(h1("Binance Automation")),
                  box(width=10,
                      paste0("This tab allows you to start and stop automation. Use the inputs to set up your automation criteria.")
                  ),
                  box(title = "Inputs", status = "primary", solidHeader = TRUE,width=4,
                      selectInput("timeframeAutomation","Pick a Timeframe to Automate", choices = list("1 Hour" = "1hour",
                                                                                                       "4 Hour" = "4hour")),
                      br(),
                      selectInput('checkGroupBinance',label = 'Select Coin(s) to Automate', choices = checkbox_list, multiple = FALSE, selected = 'BTCUSDT'),
                      br(),
                      sliderInput('sliderAutomationTarget', 'Select Target Percentage Increase', min = 0.2, max = 3, value = 1, step = 0.2),
                      br(),
                      sliderInput('sliderBalanceUsed', 'Select Percentage of USDT Balance to Use', min = 1, max = 100, value = 1, step = 1),
                      br(),
                      sliderInput("takeProfitBinanceAutomation", "Set Take Profit %",min = 0, max = 20, step = 0.1, value = 0),
                      br(),
                      sliderInput("stopLossBinanceAutomation", "Set Minimum Stop Loss as % of Take Profit",min = 0, max = 100, step = 1, value = 33),
                      br(),
                      sliderInput("confidenceThresholdAutomation", "Required Confidence Score to Buy", min = 0.1, max = 1, step = 0.02, value = 0.9)
                      
                      
                  ),
                  box(title = "Spot Account Balances", status = "primary", solidHeader = TRUE,
                      dataTableOutput('spotAccountBalancesAutomation')
                  ),
                  # box(title = "Current Automation Running", status = "primary", solidHeader = TRUE,width=8,
                  #     dataTableOutput("currentAutomation")
                  # ),
                  br(),
                  box(title = "Current Automation Running", status = "primary", solidHeader = TRUE,width=8,
                      # selectInput('selectActiveAutomation', "Select a Coin", choices = checkbox_list),
                      dataTableOutput("activeAutomationInfo")
                  ),
                  # box(title = "Volume % Change From Mean 5min Volume Over Past 2 Hours", status = "primary", solidHeader = TRUE,width=4,
                  #   gaugeOutput("volumeGauge")
                  # ),
                  actionBttn(inputId = 'submitBinanceAutomation',
                             label = 'Begin Automation',
                             icon = icon('robot'),
                             style = 'pill',
                             color = 'success',
                             size = 'lg',
                             block = TRUE),
                  br(),
                  actionBttn(inputId = 'cancelBinanceAutomation',
                             label = 'Cancel Automation',
                             icon = icon('robot'),
                             style = 'pill',
                             color = 'danger',
                             size = 'lg',
                             block = TRUE),
                  br()
                  # box(title = "Trades Placed", status = "primary", solidHeader = TRUE,width=12,
                  #     selectInput('selectTradesPlaced', "Select a Coin", choices = checkbox_list),
                  #     dataTableOutput("tradesPlaced")
                  # )
                  
                )),
        # tabItem(tabName = "alphaVantageBacktest",
        #         fluidRow(
        #           tags$head(
        #             tags$style(type="text/css"
        #             ),
        #             tags$link(rel = "stylesheet", type = "text/css", href = "stylev1.css")
        #           ),
        #           box(title = "Backtest News Inputs", status = "primary", solidHeader = TRUE, width = 6,
        #               # selectInput(inputId = "newsTopic",label = "Select a News Type", choices = list("Blockchain" = "blockchain",
        #               #                                                                                "Earnings" = "earnings",
        #               #                                                                                "IPO" = "ipo",
        #               #                                                                                "Mergers and Acquisitions" = "mergers_and_acquisitions")),
        #               dateRangeInput("dateRange", label = "Select a Date Range", start = "2023-01-01" , end = "2023-09-09"),
        #               selectInput(inputId = "assetTypeAV", label = "Select an Asset Type", choices = list("All" = "ALL",
        #                                                                                                   "Forex" = "FOREX",
        #                                                                                                   "Crypto" = "CRYPTO",
        #                                                                                                   "Stock" = "STOCK")),
        #               actionButton("generateBacktestData","Generate Backtest")
        #           ),
        #           box(title = "News Feed", status = "primary", solidHeader = TRUE, width = 12,
        #               dataTableOutput("newsFeed")
        #           ),
        #           column(width = 6,
        #                  box(title = "Backtest Bullish News", status = "primary", solidHeader = TRUE, width = NULL,
        #                      dataTableOutput("tableBull"),
        #                      plotlyOutput("pieChartBull")
        #                  )
        #           ),
        #           column(width = 6,
        #                  box(title = "Backtest Bearish News", status = "primary", solidHeader = TRUE, width = NULL,
        #                      dataTableOutput("tableBear"),
        #                      plotlyOutput("pieChartBear")
        #                  )
        #           )
        #         )
        # ),
        tabItem(tabName = "forexFactoryBacktest",
                fluidRow(
                  add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "bottom-right"),
                  
                  tags$head(
                    tags$style(type="text/css"
                    ),
                    tags$link(rel = "stylesheet", type = "text/css", href = "stylev1.css")
                  ),
                  column(width = 4,
                         box(title = "Backtest News Inputs", status = "primary", solidHeader = TRUE, width = NULL,
                             selectInput("newsRegion", label = "Select a News Region", choices = list("USD" = "USD",
                                                                                                      "EUR" = "EUR",
                                                                                                      "NZD" = "NZD",
                                                                                                      "AUD" = "AUD",
                                                                                                      "CAD" = "CAD",
                                                                                                      "CHF" = "CHF",
                                                                                                      "CNY" = "CNY",
                                                                                                      "GBP" = "GBP",
                                                                                                      "JPY" = "JPY"
                             )),
                             selectInput("newsTopic", "Select a Topic to Examine", choices = list("Growth" = "Growth",
                                                                                                  "Inflation" = "Inflation",
                                                                                                  "Employment" = "Employment",
                                                                                                  "Central Bank" = "Central Bank",
                                                                                                  "Bonds" = "Bonds",
                                                                                                  "Housing" = "Housing",
                                                                                                  "Consumer Surveys",
                                                                                                  "Business Surveys" = "Business Surveys",
                                                                                                  "Speeches" = "Speeches")),
                             
                             dateRangeInput("dateRangeFF", label = "Select a Date Range", start = "2020-01-01" , end = "2023-09-09"),
                             selectInput("assetType", "Select an Asset Type", choices = list(
                               "USDCAD" = "USDCAD",
                               "GBPUSD" = "GBPUSD",
                               "AUDUSD" = "AUDUSD",
                               "EURUSD" = "EURUSD",
                               "USDCHF" = "USDCHF",
                               "USDJPY" = "USDJPY",
                               "CHFJPY" = "CHFJPY",
                               "CNHJPY" = "CNHJPY",
                               "NZDJPY" = "NZDJPY",
                               "ETHUSDT" = "ETHUSDT",
                               "BTCUSDT" = "BTCUSDT",
                               "BNBUSDT" = "BNBUSDT",
                               "LINKUSDT" = "LINKUSDT",
                               "SOLUSDT" = "SOLUSDT",
                               "INJUSDT" = "INJUSDT",
                               "SNXUSDT" = "SNXUSDT",
                               "DYDXUSDT" = "DYDXUSDT",
                               "MATICUSDT" = "MATICUSDT")),
                             selectInput("timeframeFF","Select a Timeframe to Analyze", choices = list("5 Minutes" = "5min",
                                                                                                       "30 Minutes" = "30min",
                                                                                                       "1 Hour" = "60min")),
                             selectInput("selectImpact", "Select an Impact", choices = list("All" = "All",
                                                                                            "Red" = "red",
                                                                                            "Orange" = "ora",
                                                                                            "Yellow" = "yel")),
                             actionButton("generateBacktestFF", "Generate Backtest")
                             
                         )
                  ),
                  column(width = 4,
                         plotlyOutput("pieChart1")
                  ),
                  column(width = 4,
                         plotlyOutput("pieChart2")
                  ),
                  
                  box(title = "Backtest Selected Daterange by Month", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("subCategory","Select a Sub-Category to Filter", choices = list("All" = "All")),
                      actionButton("applySubCategory", "Apply Filter"),
                      br(),
                      br(),
                      dataTableOutput("ffBacktestTable")
                  ),
                  box(title = "Time Series Plot", status = "primary", solidHeader = TRUE, width = 12,
                      plotlyOutput("timeSeriesPlot")
                  )
                  
                )
        )
        
        # tabItem(tabName = "etherscan",
        #         add_busy_spinner(spin = "circle", color = "blue", height = "100px", width="100px", position = "bottom-right"),
        #         
        #         column(width = 6,
        #                box(title = "Select a Coin to Investigate", status = "primary", solidHeader = TRUE,width=NULL,
        #                    selectInput(inputId = "selectTopCoin", "Select a Coin", choices = token.names.list)
        #                ),
        #                box(title = "Holder Info", status = "primary", solidHeader = TRUE,width=NULL,
        #                    dataTableOutput("holderInfo")
        #                )
        #         ),
        #         
        #         box(title = "Detailed Holder Info", status = "primary", solidHeader = TRUE,width=12,
        #             "select a holder wallet before clicking the button below. Wait ~5 seconds before grabbing new data.",
        #             br(),
        #             actionButton("generateHolderInfo", "Grab Detailed Holder Information"),
        #             dataTableOutput("detailedHolderInfo")
        #         ),
        #         valueBoxOutput("balance", width = 4),
        #         valueBoxOutput("status", width = 4),
        #         valueBoxOutput("dynamics", width = 4),
        #         valueBoxOutput("SevenDayChange", width = 4),
        #         valueBoxOutput("ThirtyDayChange", width = 4),
        #         valueBoxOutput("percentHeld", width = 4)
        #         
        #         
        #         
        #         
        # )
      )
    )
    
    
  )
)

# Define server logic
server <- function(input, output, session) {
  
  source("DogeCoinML.R")
  
  
  dateTime = reactiveVal(Sys.time())
  output$timer = renderText(paste0("Time reamining in this candle: ",dateTime()))
  
  
  userpass.df =  s3read_using(FUN = readRDS, bucket = "cryptomlbucket/mlprophet_users", object = "userpass.df.rds")
  usernames = userpass.df$user
  passwords = userpass.df$password
  
  credentials <- data.frame(
    user = usernames,
    password = passwords,
    stringsAsFactors = FALSE
  )
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)$user
  })
  
  
  
  observe({
    invalidateLater(1000, session)
    isolate({
      if(is.null(input$timeframePredict)){
        
      }else{
        dateTime(getTimeRemaining2(input$timeframePredict, input$selectTypeMult))
      }
      # dateTime(dateTime()-1)
    })
    
    # if(is.null(reactiveValuesToList(res_auth)$user)){
    #   
    # }else if(reactiveValuesToList(res_auth)$user == 'nick'){
    #   # MINE
    #   
    #   secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
    #   api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"
    #   binance::authenticate(key = api_key,secret = secret)
    #   binance::base_url("https://api.binance.us")
    #   
    # }else if(reactiveValuesToList(res_auth)$user == 'gentlemam1'){
    #   #Gentlemam
    #   secret = "9qhPtPDePdBJnWL5zThAxqrUWXNcv37NYbyDHdkDctoJZGa0CZS6IyPqmqOdIh3i"
    #   api_key = "wZpij1rDxXsrnyRyuNmuaoLPsVSgJKvmmgt0rzi44GZB03za9GBFqeB6chXi1p0T"
    #   binance::authenticate(key = api_key,secret = secret)
    #   binance::base_url("https://api.binance.com")
    # }else if(reactiveValuesToList(res_auth)$user == 'gentlemam2'){
    #   #Gentlemam
    #   secret = "KECWzTynzt47MdHyFdY28l06G43odgzjXyOKf52VaiA4mEs7x68MTRHpLNl2XH0E"
    #   api_key = "3VSV3sbcbDS5DFnYHnpqqKZwQOjFG5hiFXEB7r6Kaev0wTBDQlvyEpOLFZgAhZZD"
    #   binance::authenticate(key = api_key,secret = secret)
    #   binance::base_url("https://api.binance.com")
    # }else if(reactiveValuesToList(res_auth)$user == 'gentlemam3'){
    #   #Gentlemam
    #   secret = "xghtE9HU3aNHkMojdVe3jxgAzBu5Xz0EqiuAoifbM9b0rY09KjZntuSJzsCj5gvC"
    #   api_key = "HbKcjXOHLS0yseTvMnwX7jxltI0ugk2ZXoiYZHeDRZr9b2XWbiCBkOODsPu6xpSp"
    #   binance::authenticate(key = api_key,secret = secret)
    #   binance::base_url("https://api.binance.com")
    # }
    # output$spotAccountBalances = renderDataTable(datatable(spot_account_balances()))
    # output$spotAccountBalancesAutomation = renderDataTable(datatable(spot_account_balances()))
    # output$livePrice = renderText(round(as.numeric(binance::market_price_ticker(input$selectCoinBinance)$price), digits = 4))
    
    # x = aws.s3::get_bucket_df("cryptomlbucket")
    
    # x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
    # coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
    # 
    #   
    #   df.coins.running = data.frame(User = character(),
    #                                 Timeframe = character(),
    #                                 Coins = character(),
    #                                 Target = character(),
    #                                 Confidence = character(),
    #                                 Percentage = character(),
    #                                 TakeProfit = character(),
    #                                 StopLoss = character(),
    #                                 Active = character())
    #   for(z in 1:length(coins.running)){
    #     dfx = possibly_s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user), object = paste0(coins.running[z],".rds"))
    #     df.coins.running = rbind(df.coins.running, dfx)
    #   }
    #   # if(length(coins.running) != 0){
    #   y = data.frame(Coins = coins.running)
    #   if(length(coins.running > 0)){
    #     output$activeAutomationInfo = renderDataTable(datatable(df.coins.running))
    #   }else{
    #     output$activeAutomationInfo = NULL
    #   }
    #   # output$currentAutomation = renderDataTable(datatable(y))
    #   # updateSelectInput(session = session, inputId = 'selectTradesPlaced', choices = y$Coins, selected = y$Coins[1])
    #   # updateSelectInput(session = session, inputId = 'selectActiveAutomation', choices = y$Coins, selected = y$Coins[1])
    #   
    #   #}
    # 
    # 
    
    
  })
  
  
  # user = res_auth$user
  
  # .GlobalEnv = environment()
  # Read in functions
  
  output$decimalsAllowed = renderText(paste0(coin_decimals$decimals[coin_decimals$symbol == input$selectCoinBinance], " decimal places allowed."))
  
  output$timeRemaining = renderText(paste0("Please note there is ",getTimeRemaining(input$timeframePredict)," before the current candle closes! displayed predictions are for the current candle!"))
  output$TVPrediction = NULL
  
  observeEvent(input$action3, {
    showModal(modalDialog("Predicting Most Likely...", footer = NULL))
    on.exit(removeModal())
    all.bst.names = list.files(path = "bsts", pattern = ".rds")
    all.bst.numbers = str_match(string = all.bst.names, pattern = "bst_(.*)\\.")[,2]
    all.bst.path = list.files(path = "bsts", pattern = ".rds", full.names = TRUE)
    all.bst = lapply(all.bst.path, readRDS)
    assign('all.bst.numbers',all.bst.numbers,.GlobalEnv)
    assign('all.bst',all.bst,.GlobalEnv)
    
    predict.best(0.3, all.bst, all.bst.names)
    
    all.predictions = round(all.predictions, digits = 4)
    max.pred = which(all.predictions == max(all.predictions))
    max.bst = all.bst.numbers[max.pred]
    
    output$mostLikely = renderText(paste0("The most probable outcome over the next 24 hours is a change of ",max.bst,"% or more."))
    output$percentChance = renderText(paste0(max(all.predictions)," Probability Predicted"))
  })
  
  
  observeEvent(input$action1, {
    tags$head(tags$style("#modalGen .modal-header {background-color: #17202a; text-align: center}"))
    showModal(tags$div(id="modalGen",modalDialog(title= tags$div(HTML(paste(
      "cyl = ",
      tags$span("Generating Model...", style="color:black")
    ))),
    footer = NULL)))
    on.exit(removeModal())
    if(input$slider1 == 0){
      
    }else{
      createModel(input$selectType,input$slider1, input$slider2, input$select, input$timeframe, input$slider1)
      senti = BacktestSentiment(input$selectType,input$slider1, input$slider2, input$select, input$timeframe)
    }
    
    output$precisionBox = renderValueBox({
      shinydashboard::valueBox(value = paste0(precision,"%"), subtitle = "Precision Score", icon = icon("bullseye"))
    })
    output$totalData = renderValueBox({
      shinydashboard::valueBox(value = nrow(compare), subtitle = "Number of Candles Backtested", icon = icon("vials"))
    })
    output$predictedHits = renderValueBox({
      shinydashboard::valueBox(value = nrow(compare[compare$decision == 1,]), subtitle = "Predicted Buy Signals", icon = icon("vials"))
    })
    output$precisionBoxSentiment = renderValueBox({
      shinydashboard::valueBox(value = paste0(senti$precision,"%"), subtitle = "Precision Score", icon = icon("bullseye"))
    })
    output$totalDataSentiment = renderValueBox({
      shinydashboard::valueBox(value = nrow(senti$df.examine), subtitle = "Number of Candles Backtested", icon = icon("vials"))
    })
    output$predictedHitsSentiment = renderValueBox({
      shinydashboard::valueBox(value = nrow(senti$df.examine[senti$df.examine$prediction == 1,]), subtitle = "Predicted Buy Signals", icon = icon("vials"))
    })
    
    
    
    
    compare2$Actual.Percent.High = paste0(compare2$Actual.Percent.High,"%")
    compare2$Actual.Percent.Low = paste0(compare2$Actual.Percent.Low,"%")
    compare2$Actual.Percent.Close = paste0(compare2$Actual.Percent.Close,"%")
    
    compare2$Signal[compare2$Signal == 1] = "DID BUY"
    compare2$Signal[compare2$Signal == 0] = "DIDN'T BUY"
    
    compare2$Actual[compare2$Actual == 0] = 'MISSED TARGET'
    compare2$Actual[compare2$Actual == 1] = 'HIT TARGET'
    
    colnames(compare2) = c("Actual","Actual High","Actual Low","Actual Close","Confidence Score", "Signal","profit")
    
    table1.colored = datatable(compare2, rownames = FALSE, options = list(pageLength = 20,
                                                                          columnDefs = list(list(targets = 6, visible = FALSE))
                                                                          
    ), style = "bootstrap") %>%
      formatStyle('Actual','profit',
                  backgroundColor = styleEqual(c(0,1), c('darkred','lightgreen'))) %>%
      formatStyle('Signal',
                  backgroundColor = styleEqual(c("DIDN'T BUY","DID BUY"), c('darkred','lightgreen')))
    
    
    
    output$table1 = renderDataTable(table1.colored)
    # output$modelPlot = renderPlot(hist(compare$Confidence.Score))
    output$modelPlot = renderPlot(ggplot(data = compare, aes(x = pred)) + geom_histogram(colour = "blue", alpha = 0.3))
  })
  
  observeEvent(input$action2, {
    predict.tomorrow(0.3, input$select)
    output$textToday = renderText(paste0("Probability of: ",round(predict.now, digits = 4)))
    output$predict = renderInfoBox({
      infoBox("Predicted Probability",round(predict.now, digits = 4))
    })
  })
  
  observeEvent(input$action4, {
    tags$head(tags$style("#modalPred .modal-header {background-color: #17202a; text-align: center}"))
    showModal(tags$div(id="modalPred",modalDialog(title= tags$div(HTML(paste(
      "cyl = ",
      tags$span("Generating Predictions...", style="color:black")
    ))),
    footer = NULL)))
    on.exit(removeModal())
    x = input$checkGroup
    updateSelectInput(session = session, inputId = 'candlestickInput', choices = x, selected = head(x,1))
    
    if(input$selectTypeMult == "Crypto" | input$selectTypeMult == "Stocks"){
      predict.tomorrow.multiple(input$selectTypeMult,input$checkGroup, input$timeframePredict, input$slider3)
    }else{
      predict.next.bh.bl.tar(input$checkGroup, input$timeframePredict, input$slider3)
    }
    
    returned.sentiment = PerformSentimentAnalysis(input$checkGroup, input$slider3, input$selectTypeMult)
    
    x = FearGreedToday()
    output$fearGreedRating = renderValueBox(shinydashboard::valueBox(value = toupper(x$fear.greed.rating), subtitle = "Today's Fear/Greed Rating"))
    # output$fearGreedScore = renderValueBox(shinydashboard::valueBox(value = round(x$fear.greed.score,2), subtitle = "Today's Fear/Greed Score"))
    output$OneHrSent = renderValueBox(shinydashboard::valueBox(value = returned.sentiment$one.hr.sentiment, subtitle = "1 Hour Sentiment"))
    output$EightHrSent = renderValueBox(shinydashboard::valueBox(value = returned.sentiment$eight.hr.sentiment, subtitle = "8 Hour Sentiment"))
    output$TwenFourHrSent = renderValueBox(shinydashboard::valueBox(value = returned.sentiment$twenfour.hr.sentiment, subtitle = "24 Hour Sentiment"))
    
    
    # dt.returned.sentiment = Color.DT(returned.sentiment$df.to.return)
    # output$sentimentTable = renderDataTable(dt.returned.sentiment)
    
    
    dt.colored1 = Color.DT(predictions.df.indi1)
    dt.colored2 = Color.DT(predictions.df.indi2)
    dt.colored3 = Color.DT(predictions.df.indi3)
    dt.colored4 = Color.DT(predictions.df.indi4)
    
    dt.colored11 = Color.DT(predictions.df.indi11)
    dt.colored22 = Color.DT(predictions.df.indi22)
    dt.colored33 = Color.DT(predictions.df.indi33)
    dt.colored44 = Color.DT(predictions.df.indi44)
    
    
    
    output$multipleOutput1 = renderDataTable(dt.colored1)
    # output$multipleOutput1 = renderDataTable(datatable(predictions.df.indi1,
    #                                                     rownames = FALSE,
    #                                                     extensions = c("Buttons","FixedHeader"),
    #                                                     style = "bootstrap",
    #                                                     options = list(paging = FALSE,fixedHeader = TRUE, searching = FALSE, dom = 'Bfrtip', buttons = c('csv'))) %>%
    #   formatStyle("Signal",
    #               backgroundColor = styleEqual(c("DON'T BUY SIGNAL", "BUY SIGNAL"), c('darkred','lightgreen')))
    # )
    
    
    if(is.null(dt.colored2)){
      shinyjs::hide("multipleOutput2")
    }else{
      shinyjs::show("multipleOutput2")
      output$multipleOutput2 = renderDataTable(dt.colored2)
    }
    if(is.null(dt.colored3)){
      shinyjs::hide("multipleOutput3")
    }else{
      shinyjs::show("multipleOutput3")
      output$multipleOutput3 = renderDataTable(dt.colored3)
    }
    if(is.null(dt.colored4)){
      shinyjs::hide("multipleOutput4")
    }else{
      shinyjs::show("multipleOutput4")
      output$multipleOutput4 = renderDataTable(dt.colored4)
    }
    
    output$sentimentOutput1 = renderDataTable(dt.colored11)
    
    
    
    if(is.null(dt.colored22)){
      shinyjs::hide("sentimentOutput2")
    }else{
      shinyjs::show("sentimentOutput2")
      output$sentimentOutput2 = renderDataTable(dt.colored22)
    }
    if(is.null(dt.colored33)){
      shinyjs::hide("sentimentOutput3")
    }else{
      shinyjs::show("sentimentOutput3")
      output$sentimentOutput3 = renderDataTable(dt.colored33)
    }
    if(is.null(dt.colored44)){
      shinyjs::hide("sentimentOutput4")
    }else{
      shinyjs::show("sentimentOutput4")
      output$sentimentOutput4 = renderDataTable(dt.colored44)
    }
    
    output$binancePredictionTable = renderDataTable(dt.colored)
    output$candlestickPlot = renderPlotly(createCandlePlot(input$candlestickInput))
  })
  
  observeEvent(input$action5, {
    # showModal(modalDialog("Generating predictions...", footer = NULL))
    output$nextWeekOutput = renderPlot(predict_week(tolower(input$selectNextWeek), input$selectTimeFrame, input$selectTypeWeek))
    output$forecastWeekOutput = renderDataTable(datatable(week.forecast.df, style = "bootstrap", rownames = FALSE))
    
    # on.exit(removeModal())
    
  })
  
  observeEvent(input$action6, {
    tags$head(tags$style("#modalPred2 .modal-header {background-color: #17202a; text-align: center}"))
    showModal(tags$div(id="modalPred2",modalDialog(title= tags$div(HTML(paste(
      "cyl = ",
      tags$span("Generating Predictions...", style="color:black")
    ))),
    footer = NULL)))
    on.exit(removeModal())
    
    if(is.null(input$tvDataDrop)){
      return(NULL)
    }else{
      df = input$tvDataDrop
    }
    
    output$TVPrediction = renderDataTable(build.TV.model(df, input$tvTimeFrame))
    # output$TVPrediction = renderDataTable(predictions.df.comb)
    
  })
  
  observeEvent(input$selectall, {
    updateCheckboxGroupInput(session = session, 'checkGroup',choices = checkbox_list, selected = checkbox_list)
    
  })
  
  # observeEvent(input$getLivePrice, {
  #   output$livePrice = renderText(round(as.numeric(binance::market_price_ticker(input$selectCoinBinance)$price), digits = 4))
  # })
  
  # observeEvent(input$submitBinance, {
  #   x = possibly_spot_new_order(
  #     order_type = input$selectTypeBinance,
  #     symbol = input$selectCoinBinance,
  #     side = input$selectSideBinance,
  #     quantity = input$tradeQuantity,
  #     test = FALSE
  #   )
  #   if(x[1] == 'ERROR'){
  #     shinyalert("Order Not Placed",
  #                "Check to see if you used to many decimals or if the minimum order requirements have not been met!",
  #                type = 'error')
  #   }else{
  #     shinyalert("Success",
  #                "Your order was successfully placed!",
  #                type = 'success')
  #   }
  # 
  #   output$spotAccountBalances = renderDataTable(datatable(spot_account_balances()))
  # })
  
  # observeEvent(input$percentSliderBinance, {
  #   current_balance = spot_account_balances()
  #   free_usdt = current_balance$free[current_balance$asset == 'USDT']
  #   percentage = (input$percentSliderBinance / 100)
  #   quantity_usdt = free_usdt * percentage
  #   
  #   current_coin_price = round(as.numeric(binance::market_price_ticker(input$selectCoinBinance)$price), digits = 4)
  #   quantity_coin = round(quantity_usdt / current_coin_price, digits = coin_decimals$decimals[coin_decimals$symbol == input$selectCoinBinance])
  #   updateNumericInput(session = session, inputId = 'tradeQuantity',label = 'Quantity',value = quantity_coin, min = 0, step = 0.1)
  # })
  # 
  
  # observeEvent(input$checkGroupBinance, {
  #   vol1 = riingo_crypto_latest(input$checkGroupBinance, resample_frequency = '5min')
  #   vol1 = vol1[-1,]
  #   vol2 = riingo_crypto_prices(input$checkGroupBinance,start_date = Sys.Date() - 2,end_date = Sys.Date(), resample_frequency = '5min')
  # 
  #   vol = rbind(vol2, vol1)
  #   vol = vol[nrow(vol)-25:nrow(vol),]
  #   m.vol = mean(vol$volume)
  #   vol.now = vol$volume[(nrow(vol)-1)]
  # 
  #   vol.compare = (vol.now/m.vol * 100) - 100
  # 
  #   output$volumeGauge = renderGauge({
  #     gauge(vol.compare,
  #           min = -100,
  #           max = 100,
  #           sectors = gaugeSectors(
  #             success = c(20, 100),
  #             warning = c(-20, 20),
  #             danger = c(-100, -20)))
  #   })
  # })
  # 
  
  
  # observeEvent(input$submitBinanceAutomation, {
  #   
  #   x = data.frame(User = reactiveValuesToList(res_auth)$user,
  #                  Timeframe = input$timeframeAutomation,
  #                  Coins = input$checkGroupBinance,
  #                  Target = input$sliderAutomationTarget,
  #                  Confidence = input$confidenceThresholdAutomation,
  #                  Percentage = input$sliderBalanceUsed,
  #                  TakeProfit = input$takeProfitBinanceAutomation,
  #                  StopLoss = (input$stopLossBinanceAutomation / 100) * input$takeProfitBinanceAutomation,
  #                  Active = TRUE
  #                  )
  #   saveRDS(x, file = paste0(tempdir(), "/x.rds"))
  # 
  #   aws.s3::put_folder(reactiveValuesToList(res_auth)$user ,bucket = "cryptomlbucket/Automation")
  # 
  #   put_object(
  #     file = file.path(tempdir(), "x.rds"),
  #     object = paste0(input$checkGroupBinance,".rds"),
  #     bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user)
  #   )
  #   
  #   x = aws.s3::get_bucket_df("cryptomlbucket")
  #   
  #   x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
  #   coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
  #   if(length(coins.running) != 0){
  #     y = data.frame(Coins = coins.running)
  #     # output$currentAutomation = renderDataTable(datatable(y))
  #   }
  #   
  #   # updateSelectInput(session = session, inputId = 'selectTradesPlaced', choices = y$Coins, selected = y$Coins[1])
  #   # updateSelectInput(session = session, inputId = 'selectActiveAutomation', choices = y$Coins, selected = y$Coins[1])
  #   
  #   x = aws.s3::get_bucket_df("cryptomlbucket")
  #   
  #   x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
  #   coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
  #   
  # 
  #     df.coins.running = data.frame(User = character(),
  #                                   Timeframe = character(),
  #                                   Coins = character(),
  #                                   Target = character(),
  #                                   Confidence = character(),
  #                                   Percentage = character(),
  #                                   TakeProfit = character(),
  #                                   StopLoss = character(),
  #                                   Active = character())
  #     for(z in 1:length(coins.running)){
  #       dfx = possibly_s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user), object = paste0(coins.running[z],".rds"))
  #       df.coins.running = rbind(df.coins.running, dfx)
  #     }
  #     if(length(coins.running > 0)){
  #       output$activeAutomationInfo = renderDataTable(datatable(df.coins.running))
  #     }else{
  #       output$activeAutomationInfo = NULL
  #     }    
  #   
  # 
  #   
  #   shinyalert("Success",
  #              "Your Automation Was Successfully Started!",
  #              type = 'success')
  # })
  # 
  # observeEvent(input$cancelBinanceAutomation, {
  #   
  #   # x = data.frame(User = reactiveValuesToList(res_auth)$user,
  #   #                Timeframe = input$timeframeAutomation,
  #   #                Coins = input$checkGroupBinance,
  #   #                Target = input$sliderAutomationTarget,
  #   #                Confidence = input$confidenceThresholdAutomation,
  #   #                Percentage = input$sliderBalanceUsed,
  #   #                TakeProfit = input$takeProfitBinanceAutomation,
  #   #                StopLoss = input$stopLossBinanceAutomation,
  #   #                Active = FALSE
  #   # )
  #   # saveRDS(x, file = paste0(tempdir(), "/x.rds"))
  #   # 
  #   # aws.s3::put_folder(reactiveValuesToList(res_auth)$user ,bucket = "cryptomlbucket/Automation")
  #   # 
  #   # put_object(
  #   #   file = file.path(tempdir(), "x.rds"),
  #   #   object = paste0(input$checkGroupBinance,".rds"),
  #   #   bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user)
  #   # )
  #   aws.s3::delete_object(object = paste0(input$checkGroupBinance,".rds"), bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user))
  #   x = aws.s3::get_bucket_df("cryptomlbucket")
  #   
  #   x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
  #   coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
  #   # if(length(coins.running) != 0){
  #     y = data.frame(Coins = coins.running)
  #     # output$currentAutomation = renderDataTable(datatable(y))
  #     # updateSelectInput(session = session, inputId = 'selectTradesPlaced', choices = y$Coins, selected = y$Coins[1])
  #     # updateSelectInput(session = session, inputId = 'selectActiveAutomation', choices = y$Coins, selected = y$Coins[1])
  #     
  #     x = aws.s3::get_bucket_df("cryptomlbucket")
  #     
  #     x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
  #     coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
  # 
  #       df.coins.running = data.frame(User = character(),
  #                                     Timeframe = character(),
  #                                     Coins = character(),
  #                                     Target = character(),
  #                                     Confidence = character(),
  #                                     Percentage = character(),
  #                                     TakeProfit = character(),
  #                                     StopLoss = character(),
  #                                     Active = character())
  #       for(z in 1:length(coins.running)){
  #         dfx = possibly_s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user), object = paste0(coins.running[z],".rds"))
  #         df.coins.running = rbind(df.coins.running, dfx)
  #       }
  #       if(length(coins.running > 0)){
  #         output$activeAutomationInfo = renderDataTable(datatable(df.coins.running))
  #       }else{
  #         output$activeAutomationInfo = NULL
  #       }      
  # 
  #   # }
  #   
  #   shinyalert("Success",
  #              "Your Automation Was Successfully Stopped!",
  #              type = 'success')
  # })
  # # observeEvent(input$selectTradesPlaced, {
  # #   y = binance::spot_trades_list(symbol=input$selectTradesPlaced)
  # #   if(!is.null(y)){
  # #     y = y %>%
  # #       select(symbol, time, price, qty, commission, commission_asset, side)
  # #     output$tradesPlaced = renderDataTable(datatable(y))
  # #   }
  # # 
  # # })
  # 
  # # observeEvent(input$selectActiveAutomation, {
  # #   x = aws.s3::get_bucket_df("cryptomlbucket")
  # #   
  # #   x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
  # #   coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
  # # 
  # #     df.coins.running = data.frame(User = character(),
  # #                                   Timeframe = character(),
  # #                                   Coins = character(),
  # #                                   Target = character(),
  # #                                   Confidence = character(),
  # #                                   Percentage = character(),
  # #                                   TakeProfit = character(),
  # #                                   StopLoss = character(),
  # #                                   Active = character())
  # #     for(z in 1:length(coins.running)){
  # #       dfx = possibly_s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user), object = paste0(coins.running[z],".rds"))
  # #       df.coins.running = rbind(df.coins.running, dfx)
  # #     }
  # #     output$activeAutomationInfo = renderDataTable(datatable(df.coins.running))
  # #   
  # # })
  # 
  observeEvent(input$timeframe,{
    if(input$selectType == "Crypto" | input$selectType == "Stocks"){
      if(input$timeframe == "15min" | input$timeframe == "1hour"){
        updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min = -1, max = 1, step = 0.1, value = 0.1)
      }else if(input$timeframe == "2hour"){
        updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min =-2, max = 2, step = 0.2, value = 0.2)
      }else{
        updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min = -3, max = 3, step = 0.2, value = 0.2)
      }
    }else{
      updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min = -0.5, max = 0.5, step = 0.05, value = 0.05)
    }
    
  })
  
  observeEvent(input$selectType, {
    if(input$selectType == "Stocks"){
      updateSelectInput(inputId = "select", label = "Pick a Stock to Predict", choices = stock.names)
      updateSelectInput(inputId = "timeframe",label = "Pick a Timeframe", choices = list("Daily" = "daily",
                                                                                         "Weekly" = "weekly"))
    }
    if(input$selectType == "Crypto"){
      updateSelectInput(inputId = "select", label = "Pick a Crypto to Predict", choices = checkbox_list)
      updateSelectInput(inputId = "timeframe",label = "Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                         "1 Hour" = "1hour",
                                                                                         "4 Hour" = "4hour",
                                                                                         "8 Hour" = "8hour",
                                                                                         "1 Day" = "1day"))
    }
    if(input$selectType == "Forex"){
      updateSelectInput(inputId = "select", label = "Pick a Forex Pair to Predict", choices = fx.pair.names)
      updateSelectInput(inputId = "timeframe",label = "Pick a Timeframe", choices = list("1 Hour" = "1hour",
                                                                                         "4 Hour" = "4hour",
                                                                                         "8 Hour" = "8hour",
                                                                                         "1 Day" = "1day"))
    }
  })
  
  observeEvent(input$selectTypeMult, {
    if(input$selectTypeMult == "Stocks"){
      updateSelectizeInput(inputId = "checkGroup", label = "Pick a Stock to Predict", choices = stock.names, options = list(maxItems = 4))
      updateSelectInput(inputId = "timeframePredict",label = "Pick a Timeframe", choices = list("Daily" = "daily",
                                                                                                "Weekly" = "weekly"))
    }
    if(input$selectTypeMult == "Crypto"){
      updateSelectizeInput(inputId = "checkGroup", label = "Pick a Crypto to Predict", choices = checkbox_list, options = list(maxItems = 4))
      updateSelectInput(inputId = "timeframePredict",label = "Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                                "1 Hour" = "1hour",
                                                                                                "4 Hour" = "4hour",
                                                                                                "8 Hour" = "8hour",
                                                                                                "1 Day" = "1day"))
    }
    if(input$selectTypeMult == "Forex"){
      updateSelectizeInput(inputId = "checkGroup", label = "Pick a Forex Pair to Predict", choices = fx.pair.names, options = list(maxItems = 4))
      updateSelectInput(inputId = "timeframePredict",label = "Pick a Timeframe", choices = list("1 Hour" = "1hour",
                                                                                                "4 Hour" = "4hour",
                                                                                                "8 Hour" = "8hour",
                                                                                                "1 Day" = "1day"))
    }
  })
  
  observeEvent(input$selectTypeWeek, {
    if(input$selectTypeWeek == "Forex"){
      updateSelectInput(inputId = "selectNextWeek", label = "Pick a Forex Pair to Predict", choices = fx.pair.names)
    }
    if(input$selectTypeWeek == "Stocks"){
      updateSelectInput(inputId = "selectNextWeek", label = "Pick a Stock to Predict", choices = stock.names)
    }
    if(input$selectTypeWeek == "Crypto"){
      updateSelectInput(inputId = "selectNextWeek", label = "Pick a Crypto to Predict", choices = checkbox_list)
    }
    
  })
  
  # observeEvent(input$selectTopCoin, {
  #   
  #   holder.info = GetTopHolders(input$selectTopCoin)
  #   assign("holder.info",holder.info,.GlobalEnv)
  #   
  #   output$holderInfo = renderDataTable({
  #     datatable(holder.info, rownames = FALSE, style = "bootstrap", selection = "single")
  #   })
  # })
  # 
  # observeEvent(input$generateHolderInfo, {
  #   
  #   print(paste0("Coin Code: ",input$selectTopCoin))
  #   print(paste0("Holder Code: ",holder.info$holder.wallet[input$holderInfo_rows_selected]))
  #   
  #   
  #   print("success")
  #   holder.coin.df = GetHolderInfo(input$selectTopCoin, holder.info$holder.wallet[input$holderInfo_rows_selected], 30)
  #   print("past function")
  #   
  #   if(length(holder.coin.df) > 0){
  #     in.trades = sum(as.numeric(holder.coin.df$actualValue[holder.coin.df$in.out == "in"]))
  #     out.trades = sum(as.numeric(holder.coin.df$actualValue[holder.coin.df$in.out == "out"]))
  #     
  #     sum.trades = in.trades - out.trades
  #   }
  #   
  #   
  #   
  #   
  #   if(exists("seven.day.df")){
  #     
  #     if(nrow(seven.day.df) > 0){
  #       # seven day metrics
  #       in.trades.seven = sum(as.numeric(seven.day.df$actualValue[seven.day.df$in.out == "in"]))
  #       out.trades.seven = sum(as.numeric(seven.day.df$actualValue[seven.day.df$in.out == "out"]))
  #       
  #       sum.trades.seven = in.trades.seven - out.trades.seven
  #     }else{
  #       sum.trades.seven = 0
  #     }
  #   }else{
  #     sum.trades.seven = 0
  #   }
  #   
  #   
  #   if(status == "active"){
  #     
  #     output$detailedHolderInfo = renderDataTable(datatable(holder.coin.df, style = "bootstrap", rownames = FALSE, selection = "none"))
  #     
  #     output$balance = renderValueBox(
  #       shinydashboard::valueBox(value = holder.info$quantity[input$holderInfo_rows_selected], subtitle = "Balance (coin)", color = "orange", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     
  #     output$status = renderValueBox(
  #       shinydashboard::valueBox(value = "Active", subtitle = "Status", color = "green", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     
  #     
  #     if(sum.trades >= 0){
  #       output$ThirtyDayChange = renderValueBox(
  #         shinydashboard::valueBox(value = paste0("+",sum.trades), subtitle = "30 Day Coin Change", color = "green", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #       output$dynamics = renderValueBox(
  #         shinydashboard::valueBox(value = "Coins In", subtitle = "Coin Movement Over 30 Days", color = "green", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #     }else{
  #       output$ThirtyDayChange = renderValueBox(
  #         shinydashboard::valueBox(value = paste0(sum.trades), subtitle = "30 Day Coin Change", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #       output$dynamics = renderValueBox(
  #         shinydashboard::valueBox(value = "Coins Out", subtitle = "Coin Movement Over 30 Days", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #     }
  #     
  #     if(sum.trades.seven >= 0){
  #       output$SevenDayChange = renderValueBox(
  #         shinydashboard::valueBox(value = paste0("+",sum.trades.seven), subtitle = "7 Day Coin Change", color = "green", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #     }else{
  #       output$SevenDayChange = renderValueBox(
  #         shinydashboard::valueBox(value = paste0(sum.trades.seven), subtitle = "7 Day Coin Change", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #       )
  #     }
  #     
  #     output$percentHeld = renderValueBox(
  #       shinydashboard::valueBox(value = holder.info$percentage[input$holderInfo_rows_selected], subtitle = "Percentage of Circulating Supply Held", color = "orange", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #   }else{
  #     output$status = renderValueBox(
  #       shinydashboard::valueBox(value = "Not Active", subtitle = "Status", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     
  #     
  #     output$ThirtyDayChange = renderValueBox(
  #       shinydashboard::valueBox(value = paste0("No Activity"), subtitle = "30 Day Coin Change", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     output$SevenDayChange = renderValueBox(
  #       shinydashboard::valueBox(value = paste0("No Activity"), subtitle = "7 Day Coin Change", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     output$dynamics = renderValueBox(
  #       shinydashboard::valueBox(value = "No Activity", subtitle = "Coin Movement Over 30 Days", color = "red", href = "https://rstudio.github.io/shinydashboard/structure.html#shinydashboard::valueBox")
  #     )
  #     
  #     
  #   }
  #   
  #   
  #   
  #   
  #   
  # })
  
  observeEvent(input$generateBacktestData, {
    figs = Backtest.AV(df.comb.all, input$dateRange[1] , input$dateRange[2] , input$newsTopic, input$assetTypeAV)
    output$pieChartBull = renderPlotly(figs$fig.bull)
    output$pieChartBear = renderPlotly(figs$fig.bear)
    output$tableBull = renderDataTable(datatable(figs$tbl.bull, style = "bootstrap"))
    output$tableBear = renderDataTable(datatable(figs$tbl.bear, style = "bootstrap"))
    output$newsFeed = renderDataTable(datatable(figs$news.feed, style = "bootstrap", options = list(scrollX = TRUE)))
    
  })
  
  observeEvent(input$generateBacktestFF, {
    newsRegion = input$newsRegion
    newsTopic = input$newsTopic
    dateRangeFF = input$dateRangeFF
    assetType = input$assetType
    timeframeFF = input$timeframeFF
    impactFF = input$selectImpact
    
    returned.data = BackTestFF(newsRegion,newsTopic,dateRangeFF,assetType,timeframeFF,impactFF)
    fig.pie1 = CreatePie(newsRegion,newsTopic,dateRangeFF,assetType,timeframeFF, impactFF)
    fig.pie2 = returned.data$pie2
    
    output$ffBacktestTable = DT::renderDT(server = FALSE, {
      datatable(returned.data$df.summarized, style = "bootstrap", selection = list(mode = "multiple", selected = 1),
                extensions = 'Buttons',
                options = list(dom = "Bfrtip",
                               buttons = "csv")) %>%
        formatStyle('Result',
                    backgroundColor = styleEqual(c("Miss","Beat"), c('darkred','lightgreen')))
    }
    )
    output$pieChart1 = renderPlotly(fig.pie1)
    output$pieChart2 = renderPlotly(fig.pie2)
    updateSelectInput(session = session, inputId = "subCategory", label = "Select a Sub-Category to Filter", choices = returned.data$unique.sub.topics)
  })
  
  observeEvent(input$applySubCategory, {
    newsRegion = input$newsRegion
    newsTopic = input$newsTopic
    dateRangeFF = input$dateRangeFF
    assetType = input$assetType
    timeframeFF = input$timeframeFF
    sub.category = input$subCategory
    impactFF = input$selectImpact
    
    returned.data = BackTestFF(newsRegion,newsTopic,dateRangeFF,assetType,timeframeFF,impactFF,sub.category)
    
    
    output$ffBacktestTable = DT::renderDT(server = FALSE, {
      datatable(returned.data$df.summarized, style = "bootstrap", selection = list(mode = "multiple",selected = 1),
                extensions = 'Buttons',
                options = list(dom = "Bfrtip",
                               buttons = c('csv')))%>%
        formatStyle('Result',
                    backgroundColor = styleEqual(c("Miss","Beat"), c('darkred','lightgreen')))
      
    })
    
  })
  
  observeEvent(input$ffBacktestTable_rows_selected, {
    newsRegion = input$newsRegion
    newsTopic = input$newsTopic
    dateRangeFF = input$dateRangeFF
    assetType = input$assetType
    timeframeFF = input$timeframeFF
    sub.category = input$subCategory
    impactFF = input$selectImpact
    
    plot = CreateTimeseries(newsRegion,newsTopic,dateRangeFF,assetType,timeframeFF,impactFF,sub.category,input$ffBacktestTable_rows_selected)
    
    output$timeSeriesPlot = renderPlotly(plot)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
