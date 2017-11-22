# Taller shinyapps - Escuela de R-Estadística
# Cristian Benalcázar

library(shinydashboard)
library(shiny)
library(dplyr)
load(url("https://github.com/RUsersGroup-Ecuador/R-Estadistica3/raw/master/datos.RData"))

ui <- dashboardPage(
  dashboardHeader(title = "Consulta Cias"),
  dashboardSidebar(
    radioButtons("tabla", "Seleccionar tabla", 
                 choices = c("CIAS"#, "ACCIONISTAS", "REPRESENTANTES"
                             ),
                 selected = "CIAS"),
    selectInput("tipo", "Tipo de consulta", choices = c("Número de registros", "RUC")),
    conditionalPanel("input.tipo!='RUC'",
                     numericInput("n", "Numero de registros", value = 2,min = 1, max = 220092)
                     ),
    conditionalPanel("input.tipo=='RUC'",
                     textInput("ruc", "RUC")
    )
  ),
  dashboardBody(
    conditionalPanel("input.tipo!='RUC'",
                     textOutput("n")
                     ),
    dataTableOutput("tabla")
  )
)

server <- function(input, output) { 
  
  n <- reactive(input$n)
  output$n <- renderText(paste("Numero de registros:", n()))
  data <- reactive({
  if(input$tipo=='RUC'){
    cias %>% filter(RUC==input$ruc)
  } else cias[c(1:n()),]
  })
  
  output$tabla <- renderDataTable(data())
  
  }

shinyApp(ui, server)
