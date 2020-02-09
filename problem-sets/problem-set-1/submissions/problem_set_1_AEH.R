#########################################################
#                     Problem Set                       #
#        Probability Distributions & Bootstrapping      #
#     Part 1: The Comparative Anatomy of Distributions  #
#                    Amy Hughes                         #
#########################################################

## Question 1 ##

#Poisson
#  all real numbers greater than zero)
#  lambda (all real numbers greater than 0)
#  discrete
#  Probability of 5 hurricanes occurring per year in FL

#Negative Binomial
#  all real numbers greater than 0
#  r (all real numbers greater than 0)
#  discrete
#  Probability of finding a salamander in the 4th pond sampled

#t
#  negative infinity to positive infinity
#  v (all real numbers greater than 0)
#  continuous
#  Average body mass for grizzly bears with population sample size of 10

##############################################################################

## Question 2 ##

# a. When mean and variance are unequal
# (variance > mean)

# b. Undetected overdispersion can include values that do not most accurately
# represent the data, leading to inaccurate model fit and, potentially, untrue
# implications by the data. As PMF and PDF give probabilities for discrete and
# continuous values, respectively,overdispersion would affect these
# probability functions by creating inaccurate probabilities.

# c. Using the negative binomial distribution in place of Poisson allows for
# greater flexibility in the face of overdispersion,as it can adjust variance
# separately from mean using an additional parameter.

##############################################################################

## Question 3 ##

# a.
kurtosis <- function(x){
  mu <- mean(x)
  var <- var(x)
  N <- length(x)
  numer <- (x-mu)^4
  denom <- var^2
  kurt <- (1/N)*sum(numer/denom)
  return(kurt)
}
# copied kurtosis function given in problem set
?rnorm
# automatic rnorm = (n, mean=0, sd=1)
sample1 = rnorm(10000, 3, sqrt(1.5))
# created a normally distributed vector with
# sample size (N) = 10,000, mean = 3, sd = 1.5
kurtosis(sample1)
# ran kurtosis function using sample1 vector
# sample1 kurtosis: 3.080965

# b.
sample2 = rnorm(10000, 12, sqrt(50))
kurtosis(sample2)
# sample2 kurtosis: 2.976857

# c. t mean: 0

# d.
# variance = v/(v-2), v = 10
tvar = function(v){
  var = v/(v-2)
  return(var)
}
# created function to find variance of t-distribution

# e.
tvar(10)
# ran tvar function using v=10 parameter
# variance when v = 10: 1.25

# f.
sample3 = rnorm(100000, 0, sqrt(1.25))
s3kurt = kurtosis(sample3)
s3kurt
# sample3 kurtosis: 3.002265
excess.kurt = s3kurt - 3
excess.kurt
#excess kurtosis: 0.002265055

# g. 
?rt()
# rt(n, df, ncp)
sample4 = rt(100000, 10)
kurtosis(sample4)
# sample4 kurtosis: 3.933462
sorted = sort(sample4)
plot(sorted, dt(sorted, 10),
     type = 'l', ylim=c(0,0.001))

# h.
?lines()
#lines(value, etc.)
lines(sample3, col='red')

# i. t-distribution has parameter v to shape its curve and better account for
# uncertainty in small sample sizes. This means it is more resistant to
# influence from outliers.

################################################################################

## Question 4

# a. 
setwd("C:/Users/physi/Documents/WilliamMary/Courses/Spring2020/BioStats")
#set the working directory to biostats folder
ducks.col = read.csv('ducks.csv', header=T)
#read in the ducks.csv file
View(ducks.col)
#viewed ducks file in separate window
# to see 1st column mess
ducks = ducks.col[-c(1)]
View(ducks)
#got rid of 1st column (index)
hist(ducks$x)
plot(ducks$x)

# b. 
#normal: data is heavily skewed;  normal dist accommodates skewness of 0
#gamma: support must be greater than 0; ducks data contains negative values
#beta: support must be greater than 0; ducks data contains negative values
# and values greater than 1
#Poisson: support must be greater than 0; ducks data contains negative values;
# data is continuous (Poisson is for discrete values)
#Neg Binom: support must be greater than 0; data is continuous (neg binom is
# for discrete values)
#t: data is heavily skewed; t-dist accommodates skewness of 0 when v>3

# c.
kde.ducks = density(ducks$x, n = 10000)
#made a kde for ducks using sample size of 10,000 to improve resolution 
ducks.mean = mean(kde.ducks$x)
#mean: 2.3634
ducks.sd = sd(kde.ducks$x)
#sd: 6.491336
ducks.lower = dnorm(6.999, ducks.mean, ducks.sd)
ducks.upper = dnorm(7.001, ducks.mean, ducks.sd)
#made lower/upper density estimates for just above/below 7
ducks.height = (ducks.upper + ducks.lower)/2
#height = average of lower/upper DE's
#height: 0.04762005
ducks.width = 7.001-6.999
#width: 0.002
ducks.height * ducks.width
#probability  approx. via rectangle area: 9.524009e-05
integrate(dnorm, mean = ducks.mean, sd = ducks.sd, lower = 6.999, upper = 7.001)$value
# integral of interval aka probability of 7% increase: 9.524009e-05

# d. 
which(kde.ducks$x >= 7)[1]
# row 7063
kde.ducks$x[7063]
# at row 7063, value = 7.001039
kde.ducks$y[7063]
# 7063's y-value aka probability of 7% population increase: 0.003150547

# e.
?sample()
#sample(x, size, replace=T/F)
boot = numeric()
#made empty vector
for(i in 1:100){
  boot[i] = sample(kde.ducks$y, 1, replace=TRUE)
  }
#made for loop for y-values (aka probs)
kde.boot = density(boot,10000)
hist(kde.boot$y)
#made histogram of boostrapped values -> NOT normal dist
CI95 = function(x){
  z = qnorm((1-0.95)/2, lower.tail = FALSE)
  mean = mean(x)
  sd = sd(x)
  n = length(x)
  lwr = mean-z*(sd/sqrt(n))
  upr = mean+z*(sd/sqrt(n))
  return(paste(lwr, upr, sep=', ')) 
}
#made function to find 95% CI's
CI95(kde.boot$y)
#95% CI's: 1.53985393474812e-05, 1.78129747753348e-05
quantile(kde.boot$y, probs = c(0.025, 0.975))
#CI's: 5.514896e-07 3.977170e-05
# do NOT match!