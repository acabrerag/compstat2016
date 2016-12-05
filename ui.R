library(shiny)


shinyUI(fluidPage(
    titlePanel("Alain Cabrera"),
    sidebarLayout(
        sidebarPanel(
            selectInput("opcion", "Tarea:", choices = c("Tarea 1", "Tarea 2","Tarea 4"), selected = 'Tarea 2'),
            
 #----------------------------------.
 conditionalPanel(
              'input.opcion === "Tarea 4"',
              
              selectInput("dep", "Variable Dependiente:", choices = colnames(swiss), selected = 'Fertility'), 
              checkboxGroupInput("checkGroup", label = "Variables Independientes", 
                                 choices = colnames(swiss),
                                 selected = 1),
            sliderInput('N4', label = 'Numero de simulaciones', value = 50, min = 1, max = 2000, step = 1),
              sliderInput('alpha4', label = 'Longitud de la cadena', min = 100, max = 2000, value =1000, step = 10),actionButton('Calcular', 'Calcular')),
            
            
#-------------------------------------------------------------------------------            
            conditionalPanel(
              'input.opcion === "Tarea 2"',
            
            numericInput('n', label = 'Dimension de la funcion', value = 2, min = 1, max = 5),
            textInput(
              inputId="expresionMC", 
              label="Funcion g(x)",
              value="function(x) 12*x[1]^2/(1+x[2]^2)"
            ),
            sliderInput('a', label = 'Limites de integracion', min = -6, max = 6, value = c(0,1), step = 0.5),
            #sliderInput('N', label = 'Numero de simulaciones', value = 100, min = 1, max = 1000, step = 100),
            selectInput("N", "Numero de simulaciones:", choices = c(10,100,1000,10000), selected = 100), 
            
          sliderInput('alpha', label = 'Significancia del intervalo', min = 0.001, max = 0.1, value = 0.05, step = 0.001)),
            
  #------------------------------------------------------------------------------------------------------------          
            conditionalPanel(
                'input.opcion === "Tarea 1"',
                sliderInput('nsims', label = 'Numero de simulaciones', value = 1000, min = 10, max = 2000, step = 1),
                sliderInput('nbins', label = 'Numero de clases', value = 5, min = 2, max = 10, step = 1),

                  numericInput("lambda",
                               "Parametro:",
                               value = 1,step=.1),
                  downloadButton('downloadData', 'Download')
                  
              
            )
            
            
        ),
  #------------------------------------------------------------------------------
        mainPanel(
          conditionalPanel(condition="input.opcion=='Tarea 1'",
            tabsetPanel(type = 'tabs',
                        tabPanel("Histograma", plotOutput("distPlot") , h1('Prueba de bondad de ajuste'), 
                                 p(textOutput("pvalChi"),textOutput("pvalK"))),
                        tabPanel("Observaciones", h4('A continuacion se presentan las primeras 10 simulaciones obtenidas.')
                                                     ,h4('Para descargar un archivo csv con todas las simulaciones, favor de oprimir el boton Download.'),tableOutput("table")),
                        tabPanel("Descriptivos",h4('A continuacion se presentan los estadisticos descriptivos de la muestra obtenida.') ,verbatimTextOutput("summary"),
                                 h4('Los valores teoricos son: '),  h5(textOutput('media')),h5(textOutput('q1')),h5(textOutput('mediana')) ,h5(textOutput('q3')))
        ))
        ,
        
        conditionalPanel(condition="input.opcion=='Tarea 4'",
                         tabsetPanel(type = 'tabs',
                           tabPanel('Gráfico de dispersión',h4('Esta app permite estimar el valor de una integral sobre un hiper-cubo.')
                                    
                           ),
                           
                           tabPanel('Histogramas',h4('Esta app permite estimar el valor de una integral sobre un hiper-cubo.')
                                    
                           ),
                           
                           
                           tabPanel('Densidad',h4('Esta app permite estimar el valor de una integral sobre un hiper-cubo.')
                                    
                           ),
                           tabPanel('Datos', h4('Esta app permite estimar el valor de una integral sobre un hiper-cubo.'))))
        
        ,
        
 conditionalPanel(condition="input.opcion=='Tarea 2'",
                   tabsetPanel(
                     #         tabPanel('Hist', plotOutput('random_hist')),
                     tabPanel('Estimación',
                              h5('DESCRIPCIÓN: Esta app permite estimar el valor de una integral múltiple sobre un hiper-cubo via método de Montecarlo y Regla del Trapecio.'),
                              h5('NOTA: La función g(x) toma como variables a las entradas del vector x, es decir, x[1] corresponde a la variable x, x[2] a la variable y.')
                              ,h5('EJEMPLO: En dimensión 2, function(x) 12*x[1]^2/(1+x[2]^2) representaría: '),
                              withMathJax("$$\\int_{0}^{1}\\int_{0}^{1}\\frac{12x^2}{1+y^2}dxdy=\\pi$$"),
                              h4('RESULTADO:'),
                              withMathJax(uiOutput('estim_MC')),
                              plotOutput('plot', width = '8in', height = '3in'),
                              checkboxInput('ribbon', 'Mostrar intervalos de confianza', value = TRUE),
                              checkboxInput('trap', 'Comparar con método del trapecio', value = TRUE),
                              h4('ADVERTENCIA: No se recomienda hacer la comparación con el método del trapecio para dimensiones altas ni para número de simulaciones (cortes uniformes) altos.')
                              

                     ),
                     tabPanel('Datos', dataTableOutput('data'))
                   )
                   
                   
                   
                   )


        
        
)
)))


