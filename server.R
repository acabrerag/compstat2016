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
 
#-----------------------------------------
  
  
  x <- reactive({
    x    <-runif(input$nsims,0,1)   # Old Faithful Geyser data
    x=-1*(input$lambda)*log(x)
  })
  
  lambda <- reactive({
    lambda=input$lambda*1
  })
  nbins <- reactive({
    
       nbins=input$nbins-1
  })
  
  output$distPlot <- renderPlot({
    
    
    # draw the histogram with the specified number of bins
    hist(x(), breaks = nbins(), col = 'orange', border = 'white',main='Histograma de las observaciones',ylab='Frecuencia')
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

  
  #Tarea 2--------------------------------------------------------------------------
  
  I_trap <- reactive({
    I<-sapply(1:input$N, function(k){
      int_trap_mult(funMC(), rep(input$a[1],input$n), rep(input$a[2],input$n), k)
    })
    
    out<-data.frame(I_trap=I)
    
  })
  int_trap <- function(fx, a, b){
    N <- length(fx)
    (b - a)/2*mean(fx[-N] + fx[-1])
  }
  int_trap_mult <- function(f, a, b, N=20){
    n <- length(a)
    x <- seq(a[1], b[1], l=N)
    if(n <= 1){
      fx <- sapply(x, f)
    } else{
      fx <- numeric(N)
      for(i in 1:length(x)){
        g <- function(y){
          f(c(x[i], y))
        }
        fx[i] <- int_trap_mult(g, a[-1], b[-1], N=N)
      }
    }
    I <- int_trap(fx, a[1], b[1])
    return(I)
  }
  
  
  
  
  funMC <- reactive({
    texto <- paste("aux <- ", input$expresionMC)
    eval(parse(text=texto))
    aux
  })

  I_MC <- reactive({
    
    
  I <- numeric(input$N)
    phix <- I
    s <- I
    for(i in 1:input$N){
      x <- runif(input$n, 0, 1)
      x<-x*(input$a[2]-input$a[1] )+input$a[1] 
      phix[i] <-eval(parse(text=substring(input$expresionMC,13)))
      s[i] <- sd(phix[1:i])
      I[i] <- (input$a[2]-input$a[1] )**input$n*mean(phix[1:i])
     
    }
    out<-data.frame(I_MC=I,sd=s)
  })
  invento<-function(){
    I<-I_MC()$I_MC
    s<-I_MC()$sd
    lower <- I
    upper <- I
    for(i in 1:input$N){
    lower[i] <- I[i] - s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)
    upper[i] <- I[i] + s[i]/sqrt(i)*qnorm(1 - input$alpha/2, 0, 1)   
    }

    out <- data.frame(I_MC=I, lower, upper, sd=s)
    
  }
  data <- reactive({
    if (input$trap){cbind(nsim=1:input$N,invento(),I_trap())
}
    else {cbind(nsim=1:input$N,invento())}
  })
  

 
  output$estim_MC <- renderUI({
    n <- nrow(data())
    texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}")
    alain<-input$n
    alain=alain-1
    for (i in 1:alain){
      texto=paste(texto,"\\int_{",input$a[1],"}^{",input$a[2],"}")
    }
    texto=paste(texto,"g(x)dx\\approx")
    if(alain==0){
      texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}g(x)dx\\approx")}
    
    if (input$trap){
      return (withMathJax(paste("$$\\mbox{La estimacion de montecarlo con }",n,"\\mbox{ simulaciones es: }",texto,round(data()$I_MC[n], 3), "\\mbox{\n La estimacion del trapecio es }",texto,round(data()$I_trap[n], 3),"$$")))
      
    }
    return (withMathJax(paste("$$\\mbox{La estimacion con }",n,"\\mbox{ simulaciones es: }",texto,round(data()$I_MC[n], 3),"$$")))
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes(x=log10(nsim))) +
      geom_line(aes(y=I_MC), color='blue', size=1)
    if(input$trap){p<-p+geom_line(aes(y=I_trap), color='red', size=1)}
    if(input$ribbon){p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), fill='orange', alpha=0.5)}
      p + labs(x='Número de simulaciones en logaritmo base 10', y='Estimación de la integral')
  })
  output$data <- renderDataTable(round(data(), 3))
  

  

})








