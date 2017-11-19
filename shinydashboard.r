## app.R ##
library(shiny)
library(shinydashboard)
#library(flexdashboard)
library(shiny)
library(DBI)
library(RPostgreSQL)

ui <- dashboardPage(
  dashboardHeader(title = "Consulta postgres"),
  dashboardSidebar(
    radioButtons("tabla", "Seleccionar tabla", 
                 choices = c("CIAS", "ACCIONISTAS", "REPRESENTANTES"),
                 selected = "CIAS"),
    numericInput("n", "Numero de registros", value = 2,min = 1, max = 220092),
    dateInput("fecha", "Fecha")
  ),
  dashboardBody(
    
    textOutput("n"),
    dataTableOutput("tabla")
    
  )
)

server <- function(input, output) { 
  
  n <- reactive(input$n)
  
  output$n <- renderText(paste("Numero de registros:", n()))
  

  data <- reactive({
    db <- dbConnect(drv=dbDriver("PostgreSQL"), 
                    dbname = "SC",
                    host = "localhost",port = 5432, 
                    user = "postgres",
                    password = "sintiempo")
    # nombre_tabla <- if(input$tabla=='CIAS') {"companias_15092016"} else{
    #   "accionistas_15092016"
    # }
    nombre_tabla <-switch(input$tabla,
                          "CIAS" = "companias_15092016",
                          "ACCIONISTAS" = "accionistas_15092016",
                          "REPRESENTANTES" = "representantes_15092016")
    
    query <- paste("SELECT * FROM", nombre_tabla, " LIMIT ", n())
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data})
  
  output$tabla <- renderDataTable(data())
  
  }

shinyApp(ui, server)

