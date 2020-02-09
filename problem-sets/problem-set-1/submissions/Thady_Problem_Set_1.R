##Problem Set 1
##Robin Thady

##1
##Poisson a: The Poisson distribution has the support of all natural numbers. 
##Poisson b: The Poisson distribution has a parameter *rate*, or the number of events that happen in a certain length of time. Rate (lambda) is equal to both the mean and the variance and can be any number > 0.
##Poisson c: The Poisson distribution uses discrete data.
##Poisson d: The Poisson distribution could be used to model the number of hurricanes that affect the east coast between July and November.

##Negative binomial a: The negative binomial distribution has the support of 0 and non-negative integers.
##Negative binomial b: The negative binomial has a parameter "r" defined as the number of failures that may occur before the experiment is stopped and must be an integer greater than 0. There is also the parameter "p" (probability of success) which is between 0 and 1, inclusive.
##Negative binomial c: The negative binomial distribution uses discrete data.
##Negative binomial d: The negative binomial distribution could be used to model the number of times a coin is flipped before 5 heads are obtained.

##t a: The t-distribution has support from negative infinity to positive infinity.
##t b: The t-distribution has a parameter *degrees of freedom*, which determines tail heaviness and must be a real number greater than 0.
##t c: The t-distrubution uses continuous data.
##t d: The t-distribution could be used to model the distribution of C:N ratios taken from a small sample of spiders.


##2a: Overdispersal occurs when observed variance is greater than lambda (expected variance).
##2b: Overdispersion may cause you to overestimate the probability of values closer to the mean and underestimate the probability of values further from the mean, as overdispersed data have more values in the tails and fewer moderate values relative to the predicted distribution.
##2c: In the Poisson distribution, lambda (variance) is the only parameter dictating the distribution's appearance. Since the mean and variance are equal, it is not possible to plot overdispersed data without entirely reshaping and repositioning the distribution, which would prevent the distribution from accurately representing the data. In the negative binomial distribution, it is not assumed that the mean equals variance, so the parameters rate and probability of success are able to create a distribution that is able to more accurately reflect the data.

##3a:
kurtosis <- function(x){ #creates a function "kurtosis"
  mu <- mean(x) #calculates mean of the argument
  var <- var(x) #calculates variance of the argument
  N <- length(x) #determines length of the argument
  numer <- (x-mu)^4 #numerator of the function = (each x value - mean)^4
  denom <- var^2 #denominator of the function = variance squared
  kurt <- (1/N)*sum(numer/denom) #kurtosis = 1/sample size * the sum of (num/denom)
  return(kurt) #prints the value calculated with "kurt"
}

vector1 <- rnorm(10000, 3, sqrt(1.5)) #creates a vector of normally distributed data: length 10,000 (10,000 draws), mean 3, variance 1.5

kurtosis(vector1) #applies kurtosis function to vector1

##3b:
vector2 <- rnorm(10000, 12, sqrt(50)) #creates a vector of normally distributed data: length 10,000 (10,000 draws), mean 12, variance 50
kurtosis(vector2) #applies kurtosis function to vector2

##3c: The mean of a t-distribution is 0 for degrees of freedom > 1.

##3d: variance = degrees of freedom / (degrees of freedom - 2)
tvar <-function(x){ #creates a function "tvar", which takes argument "x" (degrees of freedom) and calculates variance = degrees of freedom / (degrees of freedom - 2)
  df <- x
  variance <- df/(df-2)
  return(variance)
}

##3e:
tvar(10) #applies "tvar" function to degrees of freedom = 10

##3f:
vector3 <- rnorm(100000, 0, sqrt(1.25)) #creates a vector of normally distributed data: length 100,000, mean 0, variance 1.25

##3g:
sample <- rt(100000, 10) #creates a 100,000 draw sample from a t-distribution with 10 degrees of freedom
kurtosis(sample) - 3 #calculates the excess kurtosis (kurtosis - 3) of the sample

##3h:
sorted <- sort(sample) #puts the random sample in ascending order
plot(sorted, dt(sorted, 10), type = 'l')  #creates a line graph of the sorted sample
lines(sort(vector3), dnorm(sort(vector3)), col = 'red', ylim = c(0, 0.001)) #adds red lines of vector3 on the plot previously made 

##3i: The t-distribution has more weight in the tails, as it accounts for the fact that we are working with smaller samples of populations, so it is more robust to outliers that lie within the tails.

##4a
duckswhole <- read.csv('/Users/robinthady/Desktop/Advanced Biostatistics/ducks.csv') #reads the csv of the ducks data, saved as "duckswhole"
ducks <- duckswhole$x #isolates the "x" column

##4b: 
##Normal: data are skewed right, do not appear normal
##Gamma: includes negative numbers and zero
##Beta: has values other than 0 and 1
##Poisson: this is continuous
##Negative binomial: this is continuous
##t: mean is not 0, does not appear normal

##4c:
kde <- density(ducks, n = 10000)  #creates a 10,000 resolution kernel density estimate of the ducks data 
mean(kde$x) #takes the mean of the x values in kde
sd(kde$x) #takes the standard deviation of the x values in kde

upper <- dnorm(7.001, 2.3634, 6.491336) #creates an upper density estimate at 7.001, mean 2.3634, sd 6.391336
lower <- dnorm(6.999, 2.3634, 6.491336) #creates a lower density estimate at 6.999, same mean and sd as above
height <- (upper + lower)/2 #calculates the height at the average of the lower and upper density
width <- 7.001-6.999 #finds the width of the chosen interval
height * width #calculates area

integrate(dnorm, mean = 2.3634, sd = 6.491336, lower = 6.999, upper = 7.001)$value #takes the integral of the chosen interval using the same mean and sd as above

##4d:
which(kde$x >= 7)[1] #finds the index of the first x value in KDE greater than or equal to 7
kde$y[7063] #finds the y value (probability) that corresponds to the first x value greater than 7

##4e:
boot <- numeric() #creates an empty numeric called boot which will be filled with the next step
for(i in 1:100000) #creates a "for" loop that samples from kde$x with replacement 100,000 times
  boot[i] <- sample(kde$y, 1, replace=TRUE)

KDEboot <- density(boot, 10000) #calculates KDE of the boot vector
hist(KDEboot$y) #creates a histogram of the y-values (probabilities) of the bootstrapped data

#not normally distributed so the following confidence interval function is unlikely to be correct since z-scores must come from normal data

CI95 <- function(x){ #creates a function "CI95" that calculates a 95% confidence interval based on the z-scores of the argument
  z <- qnorm((1-0.95)/2, lower.tail = FALSE) #calculates the z-score using the quantile function
  mean <- mean(x) #finds the mean of the argument, saves it as mean
  sd <- sd(x) #finds the standard deviation of the argument, saves it as sd
  n <- length(x) #finds the length/sample size of the argument, saves it as n
  lwr <- mean-z*(sd/sqrt(n)) #finds the lower bound using the mean minus (z-score times SEM)
  upr <- mean+z*(sd/sqrt(n)) #finds the upper bound using the mean plus (z-score times SEM)
  return(paste(lwr, upr, sep=', ')) #returns the lower and upper bounds separated by a comma and a space
}

CI95(kde$y) #applies CI95 function to the y-values (probabilities) of kde

quantile(kde$y, probs = c(0.025, 0.975)) #checks if bootstrapped CIs match up with expected values, which they don't, possibly because not normally distributed