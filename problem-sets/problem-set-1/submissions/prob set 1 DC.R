# Problem Set I: Probability Distributions & Bootstrapping
# Delaney Costante


## Question 1

#Poisson
#natural numbers from 0 to positive infinity
#lambda (real number > 0)
#discrete
#how many individuals of a sessile species are found within a quadrat

#Negative binomial
#integers from 0 to positive infinity
#k (natural numbers from zero to positive infinity)
#discrete
#likelihood of finding a mobile species in a patch of suitable habitat

#t
#real numbers from negative infinity to positive infinity
#v (real numbers > 0)
#continuous
#length of individuals of a given species


## Question 2

#(a): If the mean and variance are not equal (e.g. if there are several outliers).

#(b): If the variance is greater than you assume it to be, you won't be able to accurately 
#     calculate the probability that a specific value occurs within the distribution or 
#     falls between a specific range within the distribution.

#(c): Negative binomial doesn't require that the mean be equal to the variance. 


## Question 3

#(a):
x = rnorm(10000, mean = 3, sd = 1.5) #creating the specified vector from the normal distribution

#the kurtosis formula from the assignment
kurtosis <- function(x){ # x is the vector of sample data 
  mu <- mean(x) 
  var <- var(x) 
  N <- length(x) 
  numer <- (x-mu)^4 # x-mu subtracts mu from each element of x 
  denom <- var^2 
  kurt <- (1/N)*sum(numer/denom) 
  return(kurt) 
  } 

kurtosis(x) #viewing the calculated kurtosis of x
# returns 3.057518

#(b):
y = rnorm(10000, mean = 12, sd = 50)
kurtosis <- function(y){ # y is the vector of sample data 
  mu <- mean(y) 
  var <- var(y) 
  N <- length(y) 
  numer <- (y-mu)^4 # y-mu subtracts mu from each element of y
  denom <- var^2 
  kurt <- (1/N)*sum(numer/denom) 
  return(kurt) 
} 
kurtosis(y) 
# returns 2.998169

#(c): mean = 0 when v = 10 (when v > 1)

#(d): 
tvar <- function(v) {    
  variance <- (v/(v-2)) #the formula for variance in a t distribution
  return(variance)
  }

#(e):
v = 10
tvar <- function(v) {
  variance <- (v/(v-2))
  return(variance)
}
tvar(10)
# returns 1.25

#(f):
f = rnorm(100000, mean = 0, sd = 1.25)

#(g):
g = rt(100000, 10)
kurtosis <- function(g){ 
  mu <- mean(g) 
  var <- var(g) 
  N <- length(g) 
  numer <- (g-mu)^4 
  denom <- var^2 
  kurt <- (1/N)*sum(numer/denom) 
  return(kurt) 
} 
kurtosis(g) 
# returns 3.996045
3.996045-3
# excess kurtosis is 0.996045

sorted <- sort(g) # where g is a 100,000 element vector from a t distribution with 
# nu = 10. The sort() function organizes the data into ascending 
# order, which is required when plotting a line graph in R 
plot(sorted, dt(sorted, 10), type = 'l') # here the first argument of plot() is our 
# sample data, and the second argument gives 
# the density of each data point based on a 
# t distribution with nu = 10 

#(h):
lines(f, col="red")

sorted <- sort(g)
plot(sorted, dt(sorted, 10), ylim=c(0, 0.001), type = 'l')
lines(f, col="red")

#(i): The t distribution has longer tails (and more space under the tails) than the normal 
#     distribution, so there is a higher probability that a value (outlier) could occur
#     in these extreme regions (and still be within the range of the distribution).


## Question 4

#(a):
read.csv(ducks)

#(b):
#  Normal: mean not equal to zero
#  Gamma: data includes negative numbers
#  Beta: not bound between 0 and 1
#  Poisson: data includes negative numbers
#  Negative binomial: data includes negative numbers
#  t: mean not equal to zero

#(c):
density(10000, .07)


normPDF <- function(x, mu, var){
  term <- 1/(sqrt(var)*sqrt(2*3.14))
  power <- (-1/2)*((x-mu)/sqrt(var))^2
  return(term*exp(power))
}
normPDF(5, 5, 1)
normPDF(10, 5, 1)

set.seed(1693)
sample <- rnorm(10000, 5, 1)

kde <- density(sample)
plot(kde$x, kde$y, type = 'l')
kde$y[which(kde$x >= 5)[1]]

upper <- dnorm(1.251, 0, 1) 
lower <- dnorm(1.249, 0, 1) 
height <- (upper + lower)/2

width <- 1.251 - 1.249

height * width

integrate(dnorm, mean = 0, sd = 1, lower = 1.249, upper = 1.251)$value

#(d):
#Approximate the probability of a duck population experiencing a 7% population increase 
#across the 2-year study. HINT: Use the code which(dens$x >= 7)[1] to return the vector 
#index of the value closest to, but just larger than 7 where "dens" is the density object, 
#and "dens$x" returns the 10000-element vector of x values at which the density was computed 
#(based on the sample data).

mean(ducks[['x']])
# is -2.553439

var(ducks[['x']])
# is 9.803364

sd= sqrt(9.803364)
# is 3.131032

ducks <- function(x, mu, var){
  term <- 1/(sqrt(var)*sqrt(2*3.14))
  power <- (-1/2)*((x-mu)/sqrt(var))^2
  return(term*exp(power))
}
ducks(.069, -2.553439, 9.803364)
ducks(.071, -2.553439, 9.803364)

kde <- density(.069, .071)
plot(kde$x, kde$y, type = 'l')
kde$y[which(kde$x >= -2.553439)[9.803364]]

upper <- ducks(.069, -2.553439, 9.803364) 
lower <- ducks(.071, -2.553439, 9.803364) 
height <- (upper + lower)/2

width <- .071-.069

height * width

integrate(ducks, mean = -2.553439, sd = 3.131032, lower = .089695, upper = .089743)$value

#(e):
library(boot)
boot <- numeric() # vector for the sample data
boot <- ducks(ducks, 100, replace=TRUE) #run through the data 100 times
#picking up a random value, each value can be included more than once
#though the replace=TRUE keeps giving me an unused arguement error

bootMEAN <- numeric() #vector for the boot means
for(i in 1:100000){        #calculating the mean from 100000 random data samples
  boot <- ducks(ducks, 100, replace=TRUE) #but again this error message
  bootMEAN[i] <- mean(boot)
}

CI95 <- function(x){
  z <- qnorm((1-0.95)/2, lower.tail = FALSE)
  mean <- mean(x)
  sd <- sd(x)
  n <- length(x)
  lwr <- mean-z*(sd/sqrt(n))
  upr <- mean+z*(sd/sqrt(n))
  return(paste(lwr, upr, sep=', ')) 
}

