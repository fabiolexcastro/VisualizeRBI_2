
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(shiny, shinydashboard, DT, data.table, tidyverse, readxl, ggplot2, plotly)

# Create user interface ---------------------------------------------------
ui <- dashboardPage(
    
    title = 'Shiny example',
    skin = 'black',
    
    dashboardHeader(title = 'Visalizacion'),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem('Lectura de datos', tabName = 'Reader', icon = icon('eye')),
            menuItem('Visualización de datos', tabNames = 'Viewer', icon = icon('arrow-right'))
        )
    ), 
    
    dashboardBody(
        
        tabItems(
            
            tabItem(
                tabName = 'Reader',
                fluidRow(
                    box(
                        title = 'Configuración',
                        width = 3,
                        fileInput(inputId = 'Data', 
                                  label = 'Cargar datos aquí', 
                                  multiple = FALSE, 
                                  accept = c('.csv', '.txt', '.xlsx', '.xls'),
                                  buttonLabel = 'Buscar archivo',
                                  placeholder = 'No ha cargado archivos')
                    )
                ),
                
                fluidRow(
                    
                    tabBox(
                        title = 'Vista previa',
                        width = 12,
                        tabPanel(
                            title = 'Estructura',
                            verbatimTextOutput('estructura')
                        ),
                        tabPanel(
                            title = 'Datos crudos',
                            DTOutput('datoscrudos')
                        )
                    )
                    
                ),
            
            tabItem(tabName = 'Viewer')
            
            )
        
        ) 
   
    ) 
     
)

server <- function(input, output){
    
    datos_crudos <- reactive({
        datos <- fread(input$Data$datapath) #input$Data, debe ser el nombre igual al de la linea fileInput (inputId)
        return(datos)  
    })
    
    output$datoscrudos <- renderDT({
        return(datos_crudos())
    }, options = list(scrollX = TRUE))
    
    output$estructura <- renderPrint({
        return(str(datos_crudos()))
    })
    
}
    
shinyApp(ui, server)
    
