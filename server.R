# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)
Sys.setlocale(locale="C")

library(shiny)

shinyServer(function(input,output,session){
  
  #Librerias
  require(ggplot2)
  library(Rcpp)
  library(ggplot2)
  library(dplyr)
  require(vcd)
  require(MASS)
  
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
  
  funMC <- function(){
    texto <- paste("aux <- ", input$expresionMC)
    eval(parse(text=texto))
    aux
  }

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
  
  output$estim_trap <- renderUI({
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
      return (withMathJax(paste("$$\\mbox{\n La estimacion del trapecio es }",texto,round(data()$I_trap[n], 3),"$$")))
    }
    return ("")
  })
  
  output$estim_MC <- renderUI({
    n <- nrow(data())
    texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}")
    alain<-input$n
    alain=alain-1
    for (i in 1:alain){texto=paste(texto,"\\int_{",input$a[1],"}^{",input$a[2],"}") }
    texto=paste(texto,"g(x)dx\\approx")
    if(alain==0){texto=paste("\\int_{",input$a[1],"}^{",input$a[2],"}g(x)dx\\approx")}
    return (withMathJax(paste("$$\\mbox{La estimacion de montecarlo con }",n,"\\mbox{ simulaciones es: }",texto,round(data()$I_MC[n], 3),"$$")))
  })
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes(x=log10(nsim))) +
      geom_line(aes(y=I_MC), color='blue', size=1)
    if(input$trap){p<-p+geom_line(aes(y=I_trap), color='red', size=1)}
    if(input$ribbon){p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), fill='orange', alpha=0.5)}
      p + labs(x='Numero de simulaciones en logaritmo base 10', y='Estimacion de la integral')
  })
  output$data <- renderDataTable(round(data(), 3))
  #--------------------------------------Tarea 1
  output$plotVs <- renderPlot({
    d <- density(x())
    plot(density(rexp(100000,input$lambda)), col="green" ,main="Densidad Teorica vs Densidad Empirica",xlab="x", ylab="f(x)")
    lines(d, col="red")
  })
  
  x <- reactive({
    x    <-runif(input$nsims,0,1)   # Old Faithful Geyser data
    x=-1*(input$lambda)*log(x)
  })
  
  lambda <- reactive({
    lambda=input$lambda*1
  })
  
  nbins <- reactive({nbins=input$nbins})
  
  output$distPlot <- renderPlot({
    hist(x(), nclass = nbins(), col = 'orange', border = 'white',main='Histograma de las observaciones',ylab='Frecuencia',xlab='Valor observado') 
    })
  
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
    breaks <- c(seq(0,10,by=1))
    O <- table(cut(x(),breaks=breaks))
    p <- diff(pexp(breaks))
    chisq.test(O,p=p, rescale.p=T)
  })
  
  output$table <- renderTable({
    data.frame(x()[0:10])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, 'sim.csv', sep='') },
    content = function(file) {
      write.csv(x(), file)
    })
  
  
  #-------------------------------------------------------------------------------------------- 
  
  
  # Tarea 4
  
  
  #-----------------------------------------
 
    cosaMH<- eventReactive(input$Calcular,{
      base<<-iris[c("Sepal.Length" ,"Sepal.Width"  ,"Petal.Length", "Petal.Width")]
      colnames(base)<-c(1,2,3,4)
      y <- base[,c(input$dep)]
      cosas<<-c(input$checkGroup)
      x <- base[cosas]
      variables<<-length(cosas)

      #Distribuciones a priori
      # beta_j ~ N(0,100)
      prior.beta <<- function(x) dnorm(x, 0, 100)
      prior.sigma <<- function(x) dexp(x, 0.1)
      
      
      # logposterior distribution
      cppFunction('
                  double objdens(NumericMatrix X, NumericVector y, NumericVector theta, double sigma){
                  double lkh, logprior, yhat;
                  int m=X.nrow(), p=X.ncol();
                  NumericVector aux(m);
                  // Compute loglikelihood
                  lkh=0;
                  for (int i=0; i<m; i++){
                  aux = X(i,_)*theta;
                  yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
                  lkh += -.5/pow(sigma,2)*pow(y[i] - yhat,2);
                  }
                  // Compute logprior
                  logprior = 0.0;
                  for(int j=0; j<p; j++){
                  logprior += R::dnorm(theta[j], 0.0, 100, true); // Aqu?? la inicial!!
                  }
                  logprior += R::dgamma(sigma, 5.0, 0.01, true);
                  // Log of target density
                  return lkh + logprior;
                  }')
objdens(as.matrix(x), y, 1:3, 1)

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta, double sigma){
            int nparam = theta.size();
            double jump = 0.1; 
            NumericVector newtheta(nparam+1);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            newtheta[nparam] = R::rnorm(sigma, jump);
            if(newtheta[nparam] <= 0){
            newtheta[nparam] = 0.0001;
            }
            return newtheta;
            }')
proposal(c(1,2,3), 1)

# 3) METROPOLIS

sourceCpp("BayesianMHLinReg.cpp")

nsim <- input$N4
init <- rep(0,ncol(x)+1) # Take intercept into account
sigma_init <- 1
mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal,
                         cbind(1,as.matrix(x)), y) # 1 for intercept
estims <<- mh.samp$theta
estims_sigma <<- mh.samp$sigma
str(mh.samp)


#Estimadores puntuales 
betahat <- apply(estims, 2, mean)
betasd <- apply(estims, 2, sd)
sigmahat <- mean(estims_sigma)
sigmasd <- sd(estims_sigma)

#Intervalos de probabilidad
alpha <- 0.05
intervals <- lapply(1:(ncol(x)+1), function(i){
  quantile(estims[ ,i], c(alpha/2, 1-alpha/2)) %>%
    t %>%
    as.data.frame
}) %>%
  rbind_all
interval_sigma <- quantile(estims_sigma, c(alpha/2, 1-alpha/2)) %>%
  t %>%
  as.data.frame

Comparison <<- data.frame(betahat, betasd, intervals)
colnames(Comparison) <- c('Estimate', 'sd', colnames(intervals))
Comparison <- rbind(Comparison, c(Estimate=sigmahat, sd=sigmasd, interval_sigma))
rownames(Comparison)[1:length(betahat)] <- paste0('beta',0:length(betahat))
rownames(Comparison)[length(betahat)+1] <- 'sigma'
out<-data.frame(betaHat=betahat)
tabla<<-Comparison

    })
    
    
    output$value <- renderPrint({
      cosaMH()
      data.frame(tabla)
      
    })
      output$tablechain <- renderDataTable({
        cosaMH()
        data.frame(estims)
      })
      
      output$tablehat <- renderDataTable({
        cosaMH()
        data.frame(tabla)
      })
      output$ploH <- renderPlot({
        cosaMH()
        par(mfrow=c(3,2))
        for(j in 1:length(cosas)){
          hist(estims[ ,j], prob=TRUE,  breaks=20, col="lightblue",
               main="Histograma de simulaciones",ylab="Densidad",xlab=paste("beta",j-1))
          
        }
        hist(estims_sigma, prob=TRUE,  breaks=20, col="lightblue",
             main="Histograma de simulaciones para sigma")
        
      })  
      
      output$iris<-renderPlot({
        pairs(iris[c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')])      
      })
      
      output$ploPP <- renderPlot({
        par(mfrow=c(3,2))
        for(j in 1:length(cosas)){
          #hist(estims[ ,j], prob=TRUE,  breaks=20, col="lightblue",
          #    main="Histogram and Posterior(blue) vs Prior(red) of the Mean")
          # plot(prior.beta, xlim=c(min(estims[ ,j]),max(estims[ ,j])), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
          plot(density(estims[ ,j]),main=paste("Densidad Posterior(azul) vs Prior(rojo) de beta_",j-1), xlim=c(-3,1+max(estims[ ,j])),col="darkblue", lwd="2")
          lines(density(rnorm(1000, mean = 0,sd = 1)),col="darkred")
        }
        
        plot(density(estims_sigma), col="darkblue",main="Densidad Posterior(azul) vs Prior(rojo) de sigma", lwd="2",xlim=c(-3,max(estims_sigma)))
        lines(density(rnorm(1000, mean = 0,sd = 1)),col="darkred")
        par(mfrow=c(1,1))
      })  
      
      output$tableMH2 <- renderUI({
        cosaMH()
        texto<-"$$Y="
        texto=paste(texto,Comparison$betahat[1])
        num<-variables
        for (i in 1:num){
          signo='+'
          if(Comparison$betahat[i+1]<0){signo=''}
          texto=paste(texto,signo,Comparison$betahat[i+1],"X_{",i,"}")
        }
        return (withMathJax(paste(texto,"$$")))
      })
      
      output$tableMH <- renderUI({
        cosaMH()
        texto<-"$$Y=\\beta_{0}"
        num<-variables
        for (i in 1:num){
          texto=paste(texto,"+\\beta_{",i,"} X_{",i,"}")
        }
        return (withMathJax(paste(texto,"$$")))
      })
      output$tableHat <- renderTable({
        
        data.frame(cosaMH())
      })
      
})