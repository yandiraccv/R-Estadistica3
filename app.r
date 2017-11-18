library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   fluidRow(
     column(3,
            "sidebar",
            
            
            helpText("tetete"),
            
            
            numericInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            actionButton("control", "Boton"),
            
            textAreaInput("text",
                          "Ingrese su nombre",placeholder = "ingrese su nombre"),
            selectInput("Sector",label = "Ingrese el sector",choices = c("Sur","Norte","Centro"),
                        selected = "Seleccionar")
            
            
     ),
   
     column(9,
            "main",
            textOutput("nombre"),
            plotOutput("bins"),
            tableOutput("tabla"),
            verbatimTextOutput("Sector")
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
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

