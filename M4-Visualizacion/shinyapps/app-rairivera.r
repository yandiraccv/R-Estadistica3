library(shiny)
library(plotly)
library(dplyr)
library(forecast)

AYBP <- c(691.9,707.0,715.2,752.9,803.2,768.2,818.9,863.7,904.6,938.1,995.2,943.5,953.8,1012.0,1070.9,1138.2,1283.9,1403.2,1403.9,1234.9,1147.9,1127.7,1171.0,1168.6,1222.4,1272.9,1328.6)
b1 <- data.frame(AYBP)
AyBts <- ts(b1, start = 1990, frequency = 1)
AyBtsa <- auto.arima(AyBts)
AyBF <- forecast(AyBtsa,3)

ui <- fluidPage(
  titlePanel("ARIMA ANTIGUA Y BARBADOS"),
  plotOutput("plot"),
  verbatimTextOutput("event"),
  plotOutput("plot2"),
  tableOutput("modelo")
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    plot.ts(AyBts)
  })
  
  output$plot2 <- renderPlot({
    plot.ts(AyBF)
  })
  
  output$modelo <- renderTable({
    summary(AyBtsa)
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "PIB" else d
  })
}

shinyApp(ui, server)
