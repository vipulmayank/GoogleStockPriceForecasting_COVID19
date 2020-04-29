library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(DT)
# Importing the dataset for analysis -------------------------------------

forecasting_summary <- read.csv("D:/College/Courses/Spring 2020/Forecasting and Time Series/Final Project/GoogleStockPriceForecasting/ForecastingSummary.csv",
                                stringsAsFactors = FALSE)




ui <- fluidPage(
    
    theme = shinytheme("darkly"),
    
    titlePanel("Google Stock Price Forecasting"),
    
    sidebarLayout(
    
    sidebarPanel( dateInput("Date",
                  label = "Select Date",
                  min = Sys.Date(),
                  max = Sys.Date() + 64)
    ),
    
    mainPanel(
        DTOutput("forecast"),
        br(),
        br(),
        textOutput("previous_change_text"),
        textOutput("previous_change"),
        br(),
        textOutput("future_change_text"),
        textOutput("future_change"),
        br(),
        br(),
        textOutput("Text"),
        plotlyOutput("plot_trend")
        
    )
    )
)

server <- function(input, output) {
    
    summary_forecast <- reactive({
        forecasting_summary %>% filter(Date >= (input$Date - 1) & Date <= (input$Date + 1)) %>% mutate("Average Value" = round(rowMeans(select(., -c(Date))),2)) 
        
    })
    
    output$forecast <- renderDT({
        summary_forecast() %>% datatable(rownames = FALSE, options = list(dom = 't'), width = 10) %>% 
            formatStyle(c(1:5), color = "black")
        })
    
    output$previous_change <- renderText({
        round((((summary_forecast()[2,5] - summary_forecast()[1,5])/summary_forecast()[2,5]) * 100), 2)
        
    })
    
    output$future_change <- renderText({
        round((((summary_forecast()[2,5] - summary_forecast()[3,5])/summary_forecast()[2,5]) * 100), 2)
        
    })
    
    output$plot_trend <- renderPlotly({
        fig <- plot_ly(forecasting_summary, x = ~Date, y = ~Arima.Forecasting , type = 'scatter', mode = 'lines', name = "ARIMA Forecasting")
        fig <- fig %>% add_trace(y = ~KNN.Forecasting,mode = 'lines', name = "KNN Forecating") 
        fig <- fig %>% add_trace(y = ~NN.Forecasting,mode = 'lines', name = "Neural Network Forecating") 
        fig <- fig %>% layout(xaxis = list(title = "Time"),
                              yaxis = list(title = "Stock Price"))
    })
    
    output$Text <- renderText({
        "Forecast of Google Stock Price For May and June"
    })
    
    output$previous_change_text <- renderText({
        "Percentage change from Previous day"
    })
    
    output$future_change_text <- renderText({
        "Percentage change from Next day"
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
