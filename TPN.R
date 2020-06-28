rm(list=ls())
#------------------------------------------------
# Data simulation
#------------------------------------------------
# install.packages("devtools")
#devtools::install_github("medewitt/twopiece")
library(twopiece)

N <- 1000 # Sample size
set.seed(123) # Seed
# Simulated sample from a twopiece normal using the twopiece parameterisation
y <- rtp3(N,10,3,1,FUN=rnorm,param="tp")

# Data list
datasim <- list(y = y, N = N)

#------------------------------------------------
# Posterior simulation
#------------------------------------------------
library(rstan)

fit1 <- stan(
  file = "TPN.stan",  # Stan program
  data = datasim,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 11000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)

# Summaries of the posterior sample
print(fit1, pars=c("mu", "sigma1", "sigma2"), probs=c(.1,.5,.9))

# Trace plots and histograms (thinned samples)
index = seq(1,10000, by = 5) # thinning index
# thinned samples
mup <- extract(fit1, "mu")$mu[index]
sigma1p <- extract(fit1, "sigma1")$sigma1[index]
sigma2p <- extract(fit1, "sigma2")$sigma2[index]

# traceplots
plot(mup, type="l", ylab = expression(mu), cex.axis = 1.5, cex.lab = 1.5)
plot(sigma1p,type="l", ylab = expression(sigma[1]), cex.axis = 1.5, cex.lab = 1.5)
plot(sigma2p,type="l", ylab = expression(sigma[2]), cex.axis = 1.5, cex.lab = 1.5)

# histograms
hist(mup, breaks = 30, probability = T, 
     main = "", xlab = expression(mu), cex.axis = 1.5, cex.lab = 1.5)
points(density(mup), type="l", col="red", lwd = 2)
box()

hist(sigma1p, breaks = 30, probability = T, 
     main = "", xlab = expression(sigma[1]), cex.axis = 1.5, cex.lab = 1.5)
points(density(sigma1p), type="l", col="red", lwd = 2)
box()

hist(sigma2p, breaks = 30, probability = T, 
     main = "", xlab = expression(sigma[2]), cex.axis = 1.5, cex.lab = 1.5)
points(density(sigma2p), type="l", col="red", lwd = 2)
box()

#------------------------------------------------
# Posterior predictive density function
#------------------------------------------------
dpred <- Vectorize(function(x){
  val <- vector()
  for(i in 1:length(index)) val[i] <- dtp3(x,mup[i],sigma1p[i],sigma2p[i], FUN=dnorm, param ="tp")
  return(mean(val))
})

hist(y,probability = T, breaks = 20, xlim = c(0,15),
     main="Predictive PDF", xlab = "y", cex.axis = 1.5, cex.lab = 1.5)
curve(dpred, 0, 15, n = 250, add = T, col="red", lwd = 2 )
box()
