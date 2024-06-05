rm(list=ls())
# set working directory to location of the current script
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

################################################################################
# Inverse CDF
################################################################################
# draw 10k samples from a beta(2,2) distribution
n_samples <- 10000

# get 10k random values from U(0,1)
probs <- runif(n_samples)

# Inverse CDF
sample <- data.frame(x= qbeta(probs, 2, 2))

# plot it
sample %>% 
  ggplot()+
  geom_histogram(aes(x=x, y=..density..), fill="goldenrod")+
  stat_function(fun = dbeta, color="blue", args = list(2,2))

################################################################################
# Accept-reject method
################################################################################
# draw 50k samples from a N(15,3)
n_samples = 50000

# plot the distribution
x = seq(0,30,0.1)
fx = dnorm(x, 15, 3)
data.frame(x=x, y=fx) %>% 
  ggplot()+
  geom_line(aes(x,y), col="goldenrod")

# choose a distribution for g(x) - let's use a normal with a larger variance N(15, 7.5)
gx = dnorm(x, 15, 7.5)

data.frame(x=x, y1=fx, y2=gx) %>% 
  ggplot()+
  geom_line(aes(x,y1), col="blue")+
  geom_line(aes(x,y2), col="goldenrod")

# scale g(x) by M = 3
M = 2.6
M_gx = M * gx

data.frame(x=x, y1=fx, y2=M_gx) %>% 
  ggplot()+
  geom_line(aes(x,y1), col="blue")+
  geom_line(aes(x,y2), col="goldenrod")

# create a vector to store samples
samples <- c()
count <- 0
while(length(samples)<n_samples){
  count <- count + 1
  # sample at random from x
  x <- runif(1, 2, 28)
  print(paste0("x: ", x))
  
  # calculate fx
  fx <- dnorm(x, 15, 3)
  
  #calculate Mgx
  Mgx <- 3 * dnorm(x, 15, 7.5)
  
  # calculate acceptance prob
  prob <- fx / Mgx
  print(paste0("prob: ", prob))
  
  # draw random sample from U(0,1)
  q <- runif(1)
  print(paste0("q: ", q))
  
  # accept or reject sample
  if(q<prob){
    samples <- append(samples, x)
    print("accept")
  }else{
    print("reject")
  }
  print("-----------------")
}

print(paste0("count: ", count))

# plot it
data.frame(x=samples) %>% 
  ggplot()+
  geom_histogram(aes(x=x, y=..density..), fill="goldenrod", bins=100)+
  stat_function(fun = dnorm, color="blue", args = list(15,3))

################################################################################
# MCMC.metropolis method
################################################################################

# Code an MCMC sampler in R to draw 10000 samples from a N(10, 2) distribution
# over the closed domain of [0,20].
# Include a burn in period of 500 samples and use a uniform distribution to draw candidates from.

n_samples <- 100000
count <- 0
samples <- c()
burn_in <- 1000

# generate first point at random

# plot it
x = seq(0,20,0.1)
fx = dnorm(x, 10, 2)
data.frame(x=x, y=fx) %>% 
  ggplot()+
  geom_line(aes(x,y), col="goldenrod")

x <- runif(1,0,20)

while(length(samples)<(n_samples+burn_in)){
  count = count + 1
  print(paste0("x: ", x))
  
  # calculate fx
  fx <- dnorm(x, 10, 2)
  print(paste0("fx: ", fx))
  
  #generate candidate point at random from the uniform distribution
  x.star <- runif(1, x-2, x+2)
  print(paste0("x.star: ", x.star))
  
  # calculate f(x.star)
  fx.star <- dnorm(x.star, 10, 2)
  print(paste0("fx.star: ", fx.star))
  
  # accept or reject sample
  if(fx.star>fx){
    samples <- append(samples, x.star)
    print("accept because higher density")
    x <- x.star
  }else{
    # draw random sample from U(0,1)
    q <- runif(1)
    print(paste0("q: ", q))
    if(q<(fx.star/fx)){
      samples <- append(samples, x.star)
      print("accept because random sample")
      x <- x.star
    }else{
    print("reject candidate")
    }
  }
  print("-----------------")
}

print(paste0("count: ", count))

# remove burn-in
samples <- samples[(burn_in+1):length(samples)]

# plot the sample
data.frame(x=samples) %>% 
  ggplot()+
  geom_histogram(aes(x=x, y=..density..), fill="goldenrod", bins=100)+
  stat_function(fun = dnorm, color="blue", args = list(10,2))

