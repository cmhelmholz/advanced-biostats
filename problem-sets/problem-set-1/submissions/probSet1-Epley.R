## Problem Set 1
## Ben Epley


rm(list=ls()) #clear the global environment to prevent unintentional conflicts
#setwd("C:/Users/17036/Desktop/RstatBos/R.workDirectory/advBiostat/") #set working directory to minimize time spent specifying file paths
library(magrittr) #load bc i like to use the pipe operator (%>%) on occasion

## 1
# Poisson
# any of the counting numbers, including 0
# lambda (real numbers > 0)
# discrete
# number of sunflowers found in each cell of a 100m x 100m grid

# Negative binomial
# any of the counting numbers, including 0
# k (number of successes)
# discrete
# number of days a machine works before breaking down (<- my favorite example from Wikipedia)

# Student's t
# all real numbers
# v > 0 (degrees of freedom)
# continuous
# body-temperature measurements from humans (I remember learning avg human body temp is actually ~98.25 from Whitlock & Schluter)



## 2
#(a)
# when variance > mean

#(b) 
# Undetected overdispersion carries the risk of using a distribution model that is innapropriate for the data.
# For example, using the PDF for normally distributed data on data that is overdispersed will not properly respresent the original population.

#(c)
# The negative binomial distribution can handle cases where the mean and variance are not equal, whereas the Poisson distribution cannot



## 3
set.seed(1693) #set the seed for reproduciblitly

kurtosis <- function(x){ #kurtosis function from Sam
  mu <- mean(x)
  var <- var(x)
  N <- length(x)
  numer <- (x-mu)^4
  denom <- var^2
  kurt <- (1/N)*sum(numer/denom)
  return(kurt)
}

#(a)
myData1 <- rnorm(10000, 3, sqrt(1.5)) #create a vector of 10,000 draws from a normal distribution w/ mean=3 and var=1.5 (sd=sqrt(var))
myKurt1 <- kurtosis(myData1) #use the kurtosis function to calculate kurtosis
print(myKurt1)

#(b)
myData2 <- rnorm(10000, 12, sqrt(50)) 
myKurt2 <- kurtosis(myData2)
print(myKurt2)

#(c)
# the expected mean = 0

#(d)
tvar <- function(df){ #function that calculates the variance of a t distribution given the degrees of freedom (df)
  return(df/(df-2))
}
  
#(e)
tvar10 <- tvar(10) #calculate the variance of a t-distr. w/ 10 df
print(tvar10)

#(f)
bigData1 <- rnorm(100000, 0, sqrt(tvar10))
bigKurt1 <- kurtosis(bigData1)
xsKurt1 <- bigKurt1 - 3 #calculate excess kurtosis by subtracting 3
print(xsKurt1)

#(g)
bigT1 <- rt(100000, 10)
bigKurt2 <- kurtosis(bigT1)
xsKurt2 <- bigKurt2 - 3
print(xsKurt2)

sortedT <- sort(bigT1) #sort the data in preparation for using dt()
plot(sortedT, dt(sortedT, 10), type = 'l') #plot the t-distributed data

#(h)
sortedNorm <- sort(bigData1)
lines(sortedNorm, dnorm(sortedNorm, 0, sqrt(tvar10)), col = "red") #plot the normally distributed data on top of the t-distributed data

plot(sortedT, dt(sortedT, 10), type = 'l', ylim=c(0, 0.001)) #plot with adjusted window size for viewing the "tails" of each distribution
lines(sortedNorm, dnorm(sortedNorm, 0, sqrt(tvar10)), col = "red")

#(i)
#the t-distribution "expects" (> density in the tails) the presence of outliers more than the normal distribution



## 4

#(a)
rawDucks <- read.csv("ducks.csv") #import the data
duckData <- rawDucks[,2] #remove the index column

#(b)
# data is heavily skewed
# distribution should not contain negative values
# data is defined on 0 to 1
# distribution should not contain negative values
# distribution should not contain negative values
# mean != 0 (not even close in this case)

#(c)
PDF1 <- density(duckData, n = 10000) #use the density() function to grab a density vector of the data
ii <- which.min(abs(PDF1$x - 7)) #find two indices, one above and one below the value of 7
print(PDF1$x[ii])
iU <- ii #index for the upper value
iL <- iU - 1 #index for the lower value
print(c(PDF1$x[iL],PDF1$x[iU]))

upper <- PDF1$y[iU] #probability density of the upper value
print(upper) #an estimate of the probability density of a 7% increase
lower <- PDF1$y[iL] #probability density of the lower value
height <- (upper + lower)/2 #calculate height of the rectangle estimating the area under the curve
width <- PDF1$x[iU] - PDF1$x[iL] #calculate the width of the rectangle

# (d)
appProb <- height * width #an estimate of the probability of a 7% increase
print(appProb)

# (e)

#####NOTE VVVVV
#the following for loop takes like 20 minutes to finish so I have the confidence interval written as a comment at the very end

bootProb7 <- numeric()
for(i in 1:100000){
  boot <- sample(duckData, 100, replace = TRUE) #sample the data 100 times w/ replacements
  PDFboot <- density(boot, n = 10000) #create a density vector of the sampled data
  iU <- which(PDFboot$x >= 7)[1] #index for the upper value
  iL <- tail(which(PDFboot$x < 7)[-1], n=1) #index for the lower value
  upper <- PDFboot$y[iU] #probability density of upper value
  lower <- PDFboot$y[iL] #probability density of the lower value
  height <- (upper + lower)/2 #calculate height of the rectangle estimating the area under the curve
  width <- PDFboot$x[iU] - PDFboot$x[iL] #calculate the width of the rectangle
  bootProb7[i] <- height * width #store the estimated probability
}

CI95 <- quantile(bootProb7, probs = c(0.025, 0.975), na.rm = TRUE) #calculate the 95% confidence interval for the estimate probability
print(CI95)

#95% Confidence interval for the probability of a 7% increase:
#        2.5%        97.5% 
#1.224290e-08    2.119617e-05 





