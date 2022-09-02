# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(lubridate)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

library(tidyverse)

library(fs)
library(quantmod)

source(file = "stock_analysis_functions.R")

# UI ----
ui <- fluidPage(title = "Stock Analyzer",

    # 1.0 HEADER ----
    div(
      h1("Stock Analyzer")
    ),
  
    # 2.0 APPLICATION UI -----
    div(
      column(
        width = 4,
        wellPanel(
          # Add content here
          pickerInput(label = h4("Stock Index"),
                      inputId = "indices",
                      selected = "sp500",
                      choices = c("DAX", "SP500", "DOW", "NASDAQ"),
                      multiple = F,
                      options = pickerOptions(actionsBox = FALSE, liveSearch = TRUE, size = 10)),
          uiOutput("indices"),
          dateRangeInput(inputId = "date_range",
                         label   = h4("Date Range"),
                         start   = "2020-07-01",
                         end     = today(),
                         min     = "2020-07-01",
                         max     = today(),
                         startview = "year"),
          actionButton(inputId='analyze' , 'Analyze', icon = icon('download')),
          hr(),
          # Sidebar to demonstrate various slider options ----
          sliderInput("mavg_short", "Moving Average Short:",
                          min = 5, 
                          max = 40,
                          value = 20,
                          step=1),
          sliderInput("mavg_long", "Moving Average Long:",
                          min = 50, 
                          max = 120,
                          value = 50,
                          step = 1)
      )
    ),

      column(
        width = 8,
        div(
          textOutput(outputId = 'symbol'),
          # Add content here
          div(h4(id = 'plot_header')),
          plotlyOutput(outputId = 'plotly_plot')
          
        )
      )
      ),

    # 3.0 ANALYST COMMENTARY ----
      column(
        width = 12,
        div(
          # Add content here
          h2('Analyst Commentry'),
          verbatimTextOutput(outputId = "commentary")
        )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  output$indices <- renderUI({
    choices = get_stock_list(input$indices) %>% purrr::pluck("label")
    pickerInput(label = h4("Stocks"),
                inputId = "stock_selection",
                choices = choices,
                selected = "AAPL, Apple Inc.",
                multiple = F,
                options = pickerOptions(actionsBox = FALSE, liveSearch = TRUE, size = 10))
  })

  stock_symbol <- eventReactive(input$analyze, {
    input$stock_selection
  }, ignoreNULL = TRUE)
  
  output$plot_header <- renderText({stock_symbol()})
  output$symbol  <-  renderText({stock_symbol()})
  
  stock_data_tbl <- reactive({
    
      stock_symbol() %>% get_symbol_from_user_input() %>%
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)
    
  })
  
  stock_chart_plot <- reactive({
    stock_data_tbl() %>% plot_stock_data()
    
  })
  
  output$plotly_plot <- renderPlotly({stock_chart_plot()})
  
  
  stock_commentry <- reactive({
    stock_data_tbl() %>% generate_commentary( user_input =  get_symbol_from_user_input( stock_symbol()))
    
  })
  
  output$commentary <- renderText({stock_commentry()})

}

# RUN APP ----
shinyApp(ui = ui, server = server)