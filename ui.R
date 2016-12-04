library(shiny)


shinyUI(fluidPage(
  titlePanel("Alain Cabrera"),
  sidebarLayout(
    #--------------------------------------------NOMBRE TAREAS-----------------------------------------------      
    sidebarPanel(
      selectInput("opcion", "Tarea:", choices = c("Simulador Exponencial", "Region de rechazo",
                                                  "Integrales-Montecarlo"), selected = 'Integrales-Montecarlo'),
      
      #--------------------------<<<<<<<<<<<<<------------INPUTS----------------->>>>>>>>>>>>>>>>>>>>>>---------------------------------------      
      
      #-----------------------------------TAREA 3---------------------------------------------------------          
      
      conditionalPanel(
        'input.opcion === "Integrales-Montecarlo"',
        numericInput('n', label = 'Dimension de la funcion', value = 1, min = 1, max = 5),
        textInput(
          inputId="expresionMC", 
          label="Funcion g(x)",
          value="mean(x)"
        ),
        sliderInput('a', label = 'Limites de integracion', min = -6, max = 6, value = c(0,2), step = 0.01),
        
        sliderInput('N', label = 'Numero de simulaciones', value = 50, min = 1, max = 2000, step = 1),
        sliderInput('alpha', label = 'Significancia del intervalo', min = 0.001, max = 0.1, value = 0.05, step = 0.001),
        actionButton('reset_input', 'Reset input')),
      
      
      #------------------------------------TAREA 2------------------------------------------------------------------------          
      
      conditionalPanel('input.opcion === "Region de rechazo"', withMathJax(),
                       
                       h2("Aceptacion-Rechazo"),
                       textInput(
                         inputId="expresion1", 
                         label="$$\\mbox{Funcion } f$$",
                         value="function(x) 2*x"
                       ),
                       selectInput(
                         inputId="expresion2", 
                         label="$$\\mbox{Funcion } g$$",
                         choices=c("Uniforme(xmin, xmax)"="unif", "Exponencial(1) truncada a (xmin,xmax)"="exp", "Normal(0,1) truncada a (xmin,xmax)"="norm")
                       ),
                       sliderInput("xmin", "xmin", min=-30, max=30, value=0),
                       sliderInput("xmax", "xmax", min=-30, max=20, value=1),
                       sliderInput("M", "$$M$$", min=0.1, max=100, value=1),
                       numericInput("nsim", "Numero de simulaciones", value=100)
                       
      )
      
      
    ),
    
    #--------------------------<<<<<<<<<<<<<------------OUTPUTS----------------->>>>>>>>>>>>>>>>>>>>>>---------------------------------------      
    
    mainPanel(
      
      
      #------------------------------------TAREA 1------------------------------------------------------------------------          
      conditionalPanel(condition="input.opcion=='Simulador Exponencial'",
                       tabsetPanel(type = 'tabs',
                                   tabPanel("Histograma", plotOutput("distPlot") , h1('Prueba de bondad de ajuste'), 
                                            p(textOutput("pvalChi"),textOutput("pvalK"))),
                                   tabPanel("Observaciones", h4('A continuacion se presentan las primeras 10 simulaciones obtenidas.')
                                            ,h4('Para descargar un archivo csv con todas las simulaciones, favor de oprimir el boton Download.'),tableOutput("table")),
                                   tabPanel("Descriptivos",h4('A continuacion se presentan los estadisticos descriptivos de la muestra obtenida.') ,verbatimTextOutput("summary"),
                                            h4('Los valores teoricos son: '),  h5(textOutput('media')),h5(textOutput('q1')),h5(textOutput('mediana')) ,h5(textOutput('q3')))
                       )),
      
      
      
      #------------------------------------TAREA 2------------------------------------------------------------------------          
      conditionalPanel(condition="input.opcion=='Region de rechazo'",
                       tabsetPanel(
                         tabPanel("Grafica de la region",h2('Region de rechazo'), h4('Objetivo: Simular observaciones de la distribución f'), 
                                  h4('Descripción del algoritmo:'), 
                                  withMathJax()
                                  , h5("$$\\cdot\\mbox{Generar independientemente }U\\sim Unif[0,1]\\mbox{ y  }Y\\mbox{ de acuerdo a la densidad g. }$$")
                                  , h5("$$\\cdot\\mbox{Si }U\\leq\\frac{f(y)}{Mg(Y)}\\mbox{ se acepta el conjunto  }X=Y\\mbox{. En otro caso, volver al caso anterior y volver a empezar. }$$")
                                  , h5("$$\\mbox{ El conjunto resultante }X\\mbox{  se distribuye de acuerdo a la función de densidad }f$$")
                                  , plotOutput("Grafica")),
                         tabPanel("Resultados", p("Tasa de exito", textOutput("tasa_exito")),h1('Prueba de bondad de ajuste'),plotOutput("hist_sim") ,
                                  sliderInput("nbins", "nbins", value=20, min=10, max=100)
                         )
                       ))
      
      #------------------------------------TAREA 3------------------------------------------------------------------------          
      , conditionalPanel(condition="input.opcion=='Integrales-Montecarlo'",
                         tabsetPanel(
                           #         tabPanel('Hist', plotOutput('random_hist')),
                           tabPanel('Grafica',h4('Esta app permite estimar el valor de una integral sobre un hiper-cubo.'),
                                    withMathJax(uiOutput('estim_MC')),
                                    plotOutput('plot', width = '8in', height = '3in'),
                                    checkboxInput('ribbon', 'Mostrar intervalos de confianza', value = TRUE)
                                    
                           ),
                           tabPanel('Datos', dataTableOutput('data'))
                         )
                         
      )
      
      
      
      
    )
  )))


