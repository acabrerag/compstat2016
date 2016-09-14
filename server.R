# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)
Sys.setlocale(locale="C")


library(shiny)



# use the below options code if you wish to increase the file input limit, in this example file input limit
# is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input,output,session){
  require(ggplot2)

  x <- reactive({
    x    <-runif(input$bins,0,1)   # Old Faithful Geyser data
    x=-1*(input$lambda)*log(x)
  })
  
  lambda <- reactive({
    lambda=input$lambda*1
  })
  output$distPlot <- renderPlot({
    
    bins <- seq(min(x()), max(x()), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x(), breaks = 20, col = 'orange', border = 'white',main='Histograma de las observaciones',ylab='Frecuencia')
    # estimate the parameters
    # Generate a summary of the data
    output$summary <- renderPrint({
      summary(x())
    })
    output$media<- renderText({
      paste0('\nMedia: ', lambda())
    })
    output$mediana <- renderText({
      paste0('\nMediana: ', round(log(2)*lambda(),3))
    })
    output$q1 <- renderText({
      paste0('\n1er Cuantil: ', round(log(4/3)*lambda(),3))
    })
    output$q3 <- renderText({
      paste0('\n3er Cuantil: ', round(log(4)*lambda(),3))
    })
    
    
    output$pvalK <- renderPrint({
      require(MASS)
      # data generation
      # estimate the parameters
      fit2 <- fitdistr(x(), "exponential")
      # goodness of fit test
      ks.test(x(), "pexp", fit2$estimate) 
    })
    output$table <- renderTable({

      data.frame(x())
    })
    output$pvalChi <- renderPrint({
      require(vcd)
      require(MASS)
      
      breaks <- c(seq(0,10,by=1))
      
      O <- table(cut(x(),breaks=breaks))
      p <- diff(pexp(breaks))
      chisq.test(O,p=p, rescale.p=T)
    })
    output$table <- renderTable({
      data.frame(x()[0:10])
    })
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(x(), file)
        
      })})
  
  
 #-------------------------------------------------------------------------------------------- 
  #Tarea 2 aceptacion-rechazo
  fun1 <- reactive({
    texto <- paste("aux <- ", input$expresion1)
    eval(parse(text=texto))
    aux
  })
  
  funMC <- reactive({
    texto <- paste("aux <- ", input$expresionMC)
    eval(parse(text=texto))
    aux
  })
  
  fun2 <- reactive({
    switch(input$expresion2,
           "unif"= function(x) 1*(x>0 && x<1),
           "exp"= function(x) dexp(x)*(x>0),
           "norm" = function(x) dnorm(x)
    )
  })
  
  output$Grafica <- renderPlot({
    x <- seq(input$xmin, input$xmax, length.out=100)
    y1 <- sapply(x, fun1())
    y2 <- input$M*sapply(x, fun2())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    # tarea: investigar sapply, lapply, apply, tapply, mapply
    plot(x, y1, type="l", col="blue", ylab="y", ylim=plot_limit)
    lines(x, y2, col="red")
    legend("topright", c("f", "M*g"), col=c("blue", "red"), lty = 1)
  })
  
  # 
  
  simulaciones <- reactive({
    num_aceptados <- 0
    num_intentos <- 0
    sim_Y <- switch(input$expresion2,
                    "unif"= function() runif(1),
                    "exp"= function() rexp(1),
                    "norm" = function() rnorm(1)
    )
    # print(sim_Y)
    valor_aceptados <- numeric(input$nsim)
    while(num_aceptados < input$nsim){
      Y <- sim_Y()
      U <- runif(1)
      if(Y >= input$xmin && Y<=input$xmax && U <= (fun1()(Y))/(input$M*(fun2()(Y)))){
        num_aceptados <- num_aceptados + 1
        valor_aceptados[num_aceptados] <- Y
      }
      num_intentos <- num_intentos + 1
    }
    # print(valor_aceptados)
    list(valor=valor_aceptados, tasa_exito=input$nsim/num_intentos)
  })
  
  output$tasa_exito <- renderText({
    simulaciones()$tasa_exito
  })
  
  output$hist_sim <- renderPlot({
    hist(simulaciones()$valor, main="Histograma de las simulaciones", breaks=input$nbins)
  })
  
  
  #Tarea 3--------------------------------------------------------------------------
  I_MC <- reactive({
  I <- numeric(input$N)
    phix <- I
    lower <- I
    upper <- I
    s <- I
    for(i in 1:input$N){
      x <- runif(input$n, 0, 1)
      x<-x*(input$a[2]-input$a[1] )+input$a[1] 
      phix[i] <-sapply(x, funMC())
      s[i] <- sd(phix[1:i])
      I[i] <- (input$a[2]-input$a[1] )**input$n*mean(phix[1:i])
      #Intervalos de confianza
      lower[i] <- I[i] - s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
      upper[i] <- I[i] + s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
    }
    out <- data.frame(I_MC=I, lower, upper, sd=s)
  })
  data <- reactive({
    cbind(nsim=1:input$N, I_MC())
  })
  

 
  output$estim_MC <- renderUI({
    n <- nrow(data())
    #paste0('\nEstimacion con ', n,' simulaciones : ',round(data()$I_MC[n], 3))
    texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}")
    alain<-input$n
    alain=alain-1
    for (i in 1:alain){
      texto=paste(texto,"\\int_{",input$a[1],"}^{",input$a[2],"}")
    }
    texto=paste(texto,"g(x)dx\\approx")
    if(alain==0){
      texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}g(x)dx\\approx")}
    return (withMathJax(paste("$$\\mbox{La estimacion con }",n,"\\mbox{ simulaciones es: }",texto,round(data()$I_MC[n], 3),"$$")))
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes(x=nsim)) +
      geom_line(aes(y=I_MC), color='blue', size=1)
    if(input$ribbon){
      p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), fill='orange', alpha=0.5)
    }
      p +
      labs(x='Numero de simulaciones',
           y='I',
           title='Estimacion de la integral')
  })
  output$data <- renderDataTable(round(data(), 3))
  
  observeEvent(input$reset_input, {
    updateNumericInput(session, "n", value = 1)
    updateSliderInput(session, "N", value = 50)
    updateSliderInput(session, "a", value = c(0,2))
    updateSliderInput(session, "alpha", value = 0.05)
    updateCheckboxInput(session, "ribbon", value = TRUE)
  })
  
  
  
  

})








