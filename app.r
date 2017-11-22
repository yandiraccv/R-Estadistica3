library(shiny)
library(xlsx)
library(dplyr)
load(url("https://github.com/Crisben/RC-MCCTH/blob/master/buses1.rdata?raw=true"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
   # Application title
   titlePanel("App"),
   fluidRow(
     column(3,
            numericInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            actionButton("control", "Boton"),
            
            textAreaInput("text",
                          "Ingrese su nombre",placeholder = "ingrese su nombre"),
            selectInput("Sector",label = "Ingrese el sector",choices = c("Sur","Norte","Centro"),
                        selected = "Seleccionar"),
            conditionalPanel("input.selec=='Plot2'",
                             downloadButton("descarga","descargar")
                             ),
            selectInput("Ano", label = "Seleccion Ano", c(1996:2017), selected = "Seleccionar"),
            selectInput("variable", "Seleccione la variable a observar", 
                        choices =c( "Tipo", "Cooperativa") )
            
            
            
            
     ),
   
     column(9,
           tabsetPanel(
              tabPanel("Plot",
                       textOutput("nombre"),
                       plotOutput("bins")
                       ),
              tabPanel("Plot1",
                       tableOutput("tabla"),
                       verbatimTextOutput("Sector")
                       ),
              tabPanel("Plot2",
                       highchartOutput("graf"),
                       tableOutput("bus")
                       ),id = "selec"
            )
            )
            
     # Sidebar with a slider input for number of bins 
   )   
      # Show a plot of the generated distribution
       
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nombre<- renderText({
    paste0("Este es su nombre ",input$text)
  })

  bins <- eventReactive(input$control,{
    input$bins
  })
  
  Sector <- eventReactive(input$control,{
    input$Sector
  })
  
  output$Sector <- renderText({
    paste0("Su sector es ", Sector())
  })
     
   output$bins <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = bins() + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$tabla <- renderTable({
     mtcars[c(1:bins()),]
   })
   
   output$bus <- renderTable(dataano())
   
   exportar <- function(datos, file){        
     wb <- createWorkbook(type="xlsx")
     
     # Define some cell styles
     # Title and sub title styles
     TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE)
     
     SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=12,
                                             isItalic=TRUE, isBold=FALSE)
     
     # Styles for the data table row/column names
     TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
     
     TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
       Alignment(vertical="VERTICAL_CENTER",wrapText=TRUE, horizontal="ALIGN_CENTER") +
       Border(color="black", position=c("TOP", "BOTTOM"), 
              pen=c("BORDER_THICK", "BORDER_THICK"))+Fill(foregroundColor = "lightblue", pattern = "SOLID_FOREGROUND")
     
     sheet <- createSheet(wb, sheetName = "Información aTM")
     
     # Helper function to add titles
     xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
       rows <- createRow(sheet, rowIndex=rowIndex)
       sheetTitle <- createCell(rows, colIndex=1)
       setCellValue(sheetTitle[[1,1]], title)
       setCellStyle(sheetTitle[[1,1]], titleStyle)
     }
     
     # Add title and sub title into a worksheet
     xlsx.addTitle(sheet, rowIndex=4, 
                   title=paste("Fecha:", format(Sys.Date(), format="%Y/%m/%d")),
                   titleStyle = SUB_TITLE_STYLE)
     
     xlsx.addTitle(sheet, rowIndex=5, 
                   title="Elaborado por: ATM",
                   titleStyle = SUB_TITLE_STYLE)
     
     # Add title
     xlsx.addTitle(sheet, rowIndex=7, 
                   paste("Información -", input$data_select),
                   titleStyle = TITLE_STYLE)
     
     # Add a table into a worksheet
     addDataFrame(datos,
                  sheet, startRow=9, startColumn=1,
                  colnamesStyle = TABLE_COLNAMES_STYLE,
                  rownamesStyle = TABLE_ROWNAMES_STYLE,
                  row.names = FALSE)
     
     # Change column width
     setColumnWidth(sheet, colIndex=c(1:ncol(datos)), colWidth=20)
     
     # image
     #addPicture("/www/r.png", sheet, scale=0.28, startRow = 1, startColumn = 1)
     
     # Save the workbook to a file...
     saveWorkbook(wb, file)
   }
   
   output$descarga <- downloadHandler(
     filename = function() { paste("Información ATM.xlsx") },
     content = function(file) {
       exportar(data, file)}
   )
   
   dataano <- reactive({
     data <- data%>%filter(`Año de Fabricacion`==input$Ano)%>%
                         group_by(get(input$variable)) %>%
                         summarize(Sector=n())
     colnames(data)<-c("class", "n")
     data
     })
   
   output$graf <- renderHighchart({
     hchart(dataano(), "pie", hcaes(x = class, y = n)) %>%
       hc_add_theme(hc_theme_538())
   })
   
   #input <- list()
   #input$Ano=2017
   
}

# Run the application 
shinyApp(ui = ui, server = server)

