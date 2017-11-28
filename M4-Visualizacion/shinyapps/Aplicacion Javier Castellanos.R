###################################
# TALLER 2                        #
# AUTOR: JAVIER CASTELLANOS REYES #
###################################

####################
# Cargar librerias #
####################

library(shiny)
library(shinydashboard)
library(dplyr)
library(highcharter)

##################
# Cargar archivo #
##################

load('balance.sf.RData')
nombre.entidad <- unique(balance.sf$NOMBRE_ENTIDAD)

######
# ui #
######

ui <- dashboardPage(
  
  dashboardHeader(title = "INFORMACIÓN DEL SISTEMA FINANCIERO"),
  
  dashboardSidebar(
    checkboxGroupInput("tipoifi", "Tipo de Entidad", 
                 choices = c("Bancos Tradicionales","Bancos Microfinanzas","Cooperativas"),
                 selected = "Bancos Tradicionales"),
  
    fluidRow(
      column(1,offset = 1,
             actionButton("control", "Ejecutar"))
    )
  ),
  
  dashboardBody(
    
    tabsetPanel(
      
      tabPanel("CARTERA TOTAL",
               fluidRow(
               column(6, 
                      highchartOutput("grafcarteratotal")),
               column(4, offset = 1,
                      dataTableOutput("tabcarteratotal"))
               )
      ),
      
      tabPanel("CARTERA POR SEGMENTO DE CARTERA",
               fluidRow(
                 column(5,
                        highchartOutput("grafconsumo")),
                 column(5, offset = 1,
                        highchartOutput("grafmicro"))
               ),
               fluidRow(
                 column(5,
                        highchartOutput("grafcomercial")),
                 column(5, offset = 1,
                        highchartOutput("grafvivienda"))
               )
      ),
      
      navbarMenu("INDICADORES",
                 tabPanel("INDICADORES DE MOROSIDAD",
                          fluidRow(
                            column(5,
                                   highchartOutput("grafmorageneral")),
                            column(5, offset = 1,
                                   highchartOutput("grafmoratotal"))
                          )
                          ),
                 tabPanel("INDICADORES DE EFICIENCIA",
                          fluidRow(
                            column(5,
                                   highchartOutput("grafpyg")),
                            column(5, offset = 1,
                                   highchartOutput("grafeficiencia"))
                          )
                         )
      )
    )
  )
)


##########
# server #
##########

server <- function(input, output) { 
  
  tipo <- eventReactive(input$control,{
    input$tipoifi
  })
  
  carteratotal <- reactive({
      base.datos <- balance.sf %>%
      filter(
      TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(sum(cartera.total))
      colnames(base.datos)<-c("Tipo", "Monto")
      base.datos    
     })
  
  consumo <- reactive({
    base.datos <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(sum(consumo.total))
    colnames(base.datos)<-c("Tipo", "Monto")
    base.datos    
  })
  
  micro <- reactive({
    base.datos <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(sum(micro.total))
    colnames(base.datos)<-c("Tipo", "Monto")
    base.datos    
  })
  
  comercial <- reactive({
    base.datos <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(sum(comercial.total))
    colnames(base.datos)<-c("Tipo", "Monto")
    base.datos    
  })
  
  vivienda <- reactive({
    base.datos <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(sum(vivienda.total))
    colnames(base.datos)<-c("Tipo", "Monto")
    base.datos    
  })
  
  mora.general <- reactive({
    base.datos <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(mora.general))
    colnames(base.datos)<-c("Tipo", "Mora")
    base.datos    
  })
   
  mora.total <- reactive({
    base.consumo <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(mora.consumo))
    colnames(base.consumo)<-c("Tipo", "Mora")
    base.consumo$Cartera <- "consumo"
    
    base.micro <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(mora.micro))
    colnames(base.micro)<-c("Tipo", "Mora")
    base.micro$Cartera <- "micro"
    
    base.comercial <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(mora.comercial))
    colnames(base.comercial)<-c("Tipo", "Mora")
    base.comercial$Cartera <- "comercial"
    
    base.vivienda <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(mora.vivienda))
    colnames(base.vivienda)<-c("Tipo", "Mora")
    base.vivienda$Cartera <- "vivienda"
    
    base.total <- rbind(base.consumo,base.micro,base.comercial,base.vivienda)
    base.total
  })
  
  pyg <- reactive({
    margen.intermediacion <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(porc.margen.intermediacion))
    colnames(margen.intermediacion)<-c("Tipo", "Valor")
    margen.intermediacion$Indicador <- "Margen_Intermediacion"
    
    utilidad.neta <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(porc.utilidad.neta))
    colnames(utilidad.neta)<-c("Tipo", "Valor")
    utilidad.neta$Indicador <- "Utilidad_Neta"
    
    base.pyg <- rbind(margen.intermediacion,utilidad.neta)
    base.pyg
  })
  
  eficiencia <- reactive({
    ingresos.servicios <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(porc.ingresos.servicios))
    colnames(ingresos.servicios)<-c("Tipo", "Valor")
    ingresos.servicios$Indicador <- "Ingresos_Servicios"
    
    provisiones <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(porc.provisiones))
    colnames(provisiones)<-c("Tipo", "Valor")
    provisiones$Indicador <- "Provisiones"
    
    gastos.operacion <- balance.sf %>%
      filter(
        TIPO_ENTIDAD %in% tipo()
      ) %>%
      group_by(TIPO_ENTIDAD) %>%
      summarise(mean(porc.gastos.operacion))
    colnames(gastos.operacion)<-c("Tipo", "Valor")
    gastos.operacion$Indicador <- "Provisiones"
    
    base.eficiencia <- rbind(ingresos.servicios,provisiones,gastos.operacion)
    base.eficiencia
  })
  
  output$grafcarteratotal <- renderHighchart({
    hchart(carteratotal(), "pie", hcaes(x = Tipo, y = Monto)) %>%
      hc_title(text = "Cartera Total / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_538())
  })
   
  output$tabcarteratotal <- renderDataTable(carteratotal())

  output$grafconsumo <- renderHighchart({
    hchart(consumo(), "column", hcaes(x = Tipo, y = Monto)) %>%
      hc_title(text = "Cartera de Consumo / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$grafmicro <- renderHighchart({
    hchart(micro(), "column", hcaes(x = Tipo, y = Monto)) %>%
      hc_title(text = "Cartera de Microcrédito / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$grafcomercial <- renderHighchart({
    hchart(comercial(), "column", hcaes(x = Tipo, y = Monto)) %>%
      hc_title(text = "Cartera Comercial / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$grafvivienda <- renderHighchart({
    hchart(vivienda(), "column", hcaes(x = Tipo, y = Monto)) %>%
      hc_title(text = "Cartera de Vivienda / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$grafmorageneral <- renderHighchart({
    hchart(mora.general(), "pie", hcaes(x = Tipo, y =  Mora)) %>%
      hc_title(text = "Indicador de Morosidad General / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_darkunica())
  })
  
  output$grafmoratotal <- renderHighchart({
    hchart(mora.total(), "bar", hcaes(x = Tipo, y = Mora, group = Cartera)) %>%
      hc_title(text = "Indicador de Morosidad por Tipo de Cartera / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_google())
  })
  
  output$grafpyg <- renderHighchart({
    hchart(pyg(), "bar", hcaes(x = Tipo, y = Valor, group = Indicador)) %>%
      hc_title(text = "Indicadores del Estado de Pérdidas y Ganancias / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_google())
  })
  
  output$grafeficiencia <- renderHighchart({
    hchart(eficiencia(), "bar", hcaes(x = Tipo, y = Valor, group = Indicador)) %>%
      hc_title(text = "Indicadores de Eficiencia / promedio enero 2008 - septiembre 2017") %>%
      hc_add_theme(hc_theme_google())
  })
}

##########################
# Ejecutar la aplicacion #
##########################

shinyApp(ui, server)