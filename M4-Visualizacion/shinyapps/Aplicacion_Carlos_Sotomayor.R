## app.R ##
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(readxl)

url <- "https://github.com/csotomayor2/Taller3/blob/master/LISTA.xlsx?raw=true"
destfile <- "lista.xlsx"
curl::curl_download(url, destfile)
lista <- read_excel(destfile)

lista$PROMEDIO<-as.numeric(lista$PROMEDIO)
lista$CURSO<-as.factor(lista$CURSO)
lista$CUALITATIVO<-as.factor(lista$CUALITATIVO)
lista$COMPORTAMIENTO<- as.factor(lista$COMPORTAMIENTO)

ui <- dashboardPage(
    dashboardHeader(title = "U.E.CAMBRIDGE"),
    dashboardSidebar(selectInput("CURSO",label = "Seleccione el Curso",
                                        choices = c("Primero","Segundo","Tercero",
                                            "Cuarto","Quinto","Sexto","Septimo",
                                            "Octavo","Noveno","Decimo")
                                        ),
                     selectInput("variable", "Seleccione la Variable a Graficar ", 
                                 choices =c( "CUALITATIVO", "COMPORTAMIENTO") )
                     ),
    
    dashboardBody(dataTableOutput("tabla"),
                  highchartOutput("grafico"),
                  tableOutput("ren"))
)


server <- function(input, output) {
    
    
    data <- reactive({
        
        data <- lista %>% filter(CURSO==input$CURSO)
        
        data})
    
    output$tabla <- renderDataTable(data())
    
    
    rendimiento <- reactive({
        data <- lista%>%filter(CURSO==input$CURSO)%>%
            group_by(get(input$variable)) %>%
            summarize(PROMEDIO=n())
        colnames(data)<-c("class", "n")
        data
    })
    
    output$grafico <- renderHighchart({
        hchart(rendimiento(), "pie", hcaes(x = class, y = n)) %>%
            hc_add_theme(hc_theme_538())
    })
    
    output$ren <- renderTable(rendimiento())
}

# Run the application 
shinyApp(ui = ui, server = server)
