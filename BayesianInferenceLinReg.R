library(Rcpp)
library(ggplot2)
library(dplyr)

data(iris)
y <- iris$Sepal.Length
x <- iris[c('Sepal.Width','Petal.Length','Petal.Width')]

# BAYESIAN APPROACH

# beta_j ~ N(0,10)
#Distribuciones a priori
prior.beta <- function(x) dnorm(x, 0, 100)
prior.sigma <- function(x) dexp(x, 0.1)

plot(prior.beta, col="darkblue", xlim=c(-50,50), lwd="2", main="Prior for mean", ylab="density")
plot(prior.sigma, col="darkblue", xlim=c(0,20), lwd="2", main="Prior for mean", ylab="density")

# 1) logposterior distribution (up to a constant)
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
            logprior += R::dnorm(theta[j], 0.0, 100, true); // Aquí la inicial!!
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

nsim <- 10000
init <- rep(0,ncol(x)+1) # Take intercept into account
sigma_init <- 1
mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal,
                         cbind(1,as.matrix(x)), y) # 1 for intercept
estims <- mh.samp$theta
estims_sigma <- mh.samp$sigma
str(mh.samp)

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,nrow(estims),by=5)
plot(estims[pts, ], type="l")
#textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)

### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
par(mfrow=c(3,2))
for(i in 1:4){
  acf(estims[ , i])
}
acf(estims_sigma)
par(mfrow=c(1,1))
# burnin and subsampling
burnin <- 100
estims <- estims[-(1:burnin), ]
estims_sigma <- estims_sigma[-(1:burnin)]
thinning <- 15
pts <- seq(1, nsim-burnin, by=thinning)
# OBS: thinning IS actually useful here
estims <- estims[pts, ]
estims_sigma <- estims_sigma[pts]
par(mfrow=c(3,2))
for(i in 1:4){
  acf(estims[ , i])
}
acf(estims_sigma)
par(mfrow=c(1,1))

# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE
par(mfrow=c(3,2))
for(j in 1:4){
  hist(estims[ ,j], prob=TRUE,  breaks=20, col="lightblue",
       main="Histogram and Posterior(blue) vs Prior(red) of the Mean")
  plot(prior.beta, xlim=c(min(estims[ ,j]),max(estims[ ,j])), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
  lines(density(estims[ ,j]), col="darkblue", lwd="2")
}

hist(estims_sigma, prob=TRUE,  breaks=20, col="lightblue",
     main="Histogram and Posterior(blue) vs Prior(red) of the Mean")
plot(prior.beta, xlim=c(min(estims_sigma),max(estims_sigma)), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims_sigma), col="darkblue", lwd="2")
par(mfrow=c(1,1))

# Pointwise estimations of coefficients
betahat <- apply(estims, 2, mean)
betasd <- apply(estims, 2, sd)
sigmahat <- mean(estims_sigma)
sigmasd <- sd(estims_sigma)

# CERTAINTY INTERVALS
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

# COMPARISON OF ALL RESULTS
Comparison <- data.frame(betahat, betasd, intervals)
colnames(Comparison) <- c('Estimate', 'sd', colnames(intervals))
Comparison <- rbind(Comparison, c(Estimate=sigmahat, sd=sigmasd, interval_sigma))
rownames(Comparison)[1:length(betahat)] <- paste0('beta',1:length(betahat))
rownames(Comparison)[length(betahat)+1] <- 'sigma'
Comparison
