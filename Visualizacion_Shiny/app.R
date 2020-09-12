
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(shiny, shinydashboard, DT, data.table, tidyverse, readxl, ggplot2, plotly, tools)

# Create user interface ---------------------------------------------------
ui <- dashboardPage(
    
    title = 'Shiny example',
    skin = 'black',
    
    dashboardHeader(title = 'Visalizacion'),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem('Lectura de datos', tabName = 'Reader', icon = icon('eye')),
            menuItem('Visualización de datos', tabName = 'Viewer', icon = icon('arrow-right')),
            uiOutput('controles_ejex')
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
                                  placeholder = 'No ha cargado archivos'),
                        uiOutput('controles_excel')
                        
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
                    
                )
            
            ),
            
        tabItem(
            tabName = 'Viewer', 
            box(
                title = 'Mi grafica',
                plotOutput(outputId = 'grafica'),
                width = 9,
                collapsible = TRUE,
                collapsed = FALSE
                )
            )
        
        ) 
   
    ) 
     
)

server <- function(input, output){
    
    output$controles_excel <- renderUI({
        
        validate(
            
            need(
                !is.null(input$Data$datapath),
                'Esperando por archivo'
            )
            
        )
        
        if(tolower(file_ext(input$Data$datapath)) %in% c('xlsx', 'xls')){
            
            n <- excel_sheets(path = input$Data$datapath)
            sliderInput('HojasExcel', label = 'Hoja excel', min = 1, max = length(n), value = 1, step = 1)
            
        } 
        
    })
        
    datos_crudos <- reactive({
        
        validate(
            
            need(
                !is.null(input$Data$datapath),
                'Esperando por archivo'
            )
        )
        
        if(tolower(file_ext(input$Data$datapath)) %in% c('xlsx', 'xls')){
            
            datos <- read_excel(input$Data$datapath, sheet = input$HojasExcel)
            
        } else {
            
            datos <- fread(input$Data$datapath) #input$Data, debe ser el nombre igual al de la linea fileInput (inputId) 
            
        }
        
        return(datos)  
        
    })
    
    output$datoscrudos <- renderDT({
        
        return(datos_crudos())
        
    }, options = list(scrollX = TRUE))
    
    output$estructura <- renderPrint({
        
        return(str(datos_crudos()))

    })
    
    output$controles_ejex <- renderUI({
        
        selectInput(inputId = 'eje_x', label = 'Eje X', 
                    choices = c('Ninguna', colnames(datos_crudos())),
                    selected = 'Ninguna',
                    multiple = FALSE)
        
          
    })
    
    output$grafica <- renderPlot({
        
        datos <- as.data.frame(datos_crudos())
        
        validate(
            
            need(input$eje_x != 'Ninguna', message = 'Seleccione la variable a graficar en el eje X')
            
        )
        
        if(is.numeric(datos[,input$eje_x])){
            
            gg <- ggplot(data = datos, aes_string(x = input$eje_x)) +
                geom_density(col = 'darkblue', fill = 'darkblue')
            
        } else if(is.character(datos[,input$eje_x]) | is.factor(datos[,input$eje_x])){
            
            tab <- table(datos[,input$eje_x]) %>% 
                as.data.frame()
            colnames(tab) <- c(input$eje_x, 'Total')
            
            gg <- ggplot(data = tab, aes_string(x = input$eje_x, y = 'Total')) +
                geom_bar(stat = 'identity')
            
        } 
        
        return(gg)
        
    })
    
}
    
shinyApp(ui, server)
    
