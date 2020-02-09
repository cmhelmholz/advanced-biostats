## Problem Set 1: Probability Distributions & Bootstrapping
## Timothy Boycott


## Part I: The Comparative Anatomy of Distributions


## Question 1
# Poisson Distribution:
# (A) Support: A set of natural numbers starting from 0.
# (B) Parameters (values): lambda (a set of positive real numbers (R+)).
# (C) Data type: Discrete.
# (D) Example Data: Events occurring in a specified interval (time, distance, area, volume, etc.), if 
#     these events occur with a known constant mean rate and independently of the time since the last 
#     event.
# Negative binomial:
# (A) Support: A set of countable natural numbers from 0 to infinity.
# (B) Parameters (values): r (integers (can be extended to real numbers) greater than 0), p (a pair of 
#     real numbers [0,1]).
# (C) Data type: Discrete.
# (D) Example Data: Number of successes in a sequence of independent and identically distributed Bernoulli 
#     trials (a random experiment with exactly two possible outcomes) before a specified (non-random) 
#     number of failures (denoted r) occurs.
# t:
# (A) Support: A set of real numbers from negative infinity to positive infinity.
# (B) Parameters (values): v (all real numbers greater than 0).
# (D) Data type: Continuous.
# (C) Example Data: Data from a normally distributed population in situations where the sample size is 
#     small and the population standard deviation is unknown.


## Question 2
# (A) If the sample data from a Poisson distribution has a variance different from (greater than)
#     the mean, then these data are considered overdispersed.
# (B) Data which are overdispersed have greater variability than predicted by the probability distribution 
#     from which they are assumed to be drawn. The variance, as a descriptor of the distribution, is a 
#     componenet of probability mass functions and probability density functions. If the observed and 
#     predicted variances differ, estimations from the PMS/PDF's will be inaccurate and inappropriate. 
#     This would be reflected in understated standard errors.
# (C) The Poisson distribution has one free parameter and does not allow for the variance to be adjusted 
#     independently of the mean, which is inapprpriate for overdispersed data, characterized by having a 
#     viarance different from the mean. The negative binomial distribution is completely characterized by 
#     two parameters, thereby providing an additional free parameter, which can be used to adjust the 
#     variance independently of the mean.


## Question 3
# (A) 
sqrt(1.5)
# = 1.225
samp <- rnorm(10000, 3, 1.225) # Vector named "samp" created using the rnorm function, which here takes 10000 
                               # draws from a normal distribution with a mean of 3 and a variance of 1.5
                               # variance (sigma^2) being 1.5, gives a standard deviation (sigma) of 1.225,
                               # which is the third argument of the rnorm function.
var(samp)                      # Thus, the variance of the samp data is ~1.5
# = 1.52
kurtosis <- function(samp){      # samp is the vector of sample data. The custom function executes all
                                 # processes contained in the curly brackets {}
  mu <- mean(samp)               # mu is the mean of the samp data
  var <- var(samp)               # var is the variance of the samp data
  N <- length(samp)              # N is the number of values in the samp data
  numer <- (samp-mu)^4           # samp-mu subtracts mu from each element of samp. 
  denom <- var^2                 # 
  kurt <- (1/N)*sum(numer/denom) # equation for kurtosis, using predefined numerator and denominator operations
  return(kurt)
}
kurtosis(samp) # This line executes the kurtosis function on the samp vector to give a value, in this 
               # particular sample, of 3.102

# (B)
sqrt(50)
# 7.071
samp2 <- rnorm(10000, 12, 7.071) # New vector with mean 12 and variance 50 (standard deviation = 7.071)
var(samp2)
# = 50.84
kurtosis(samp2) # Kurtosis of new vector gives a value, in this sample, of 2.936

# (C) The mean of a t-distribution when v > 1 is equal to zero. Otherise, it is undefined.

# (D)
tvar <- function(v){   # v is the single parameter of a t distribution. The function executes code contained
  variance <- v/(v-2)  # in the curly brackets {}. Here, variance is calculated with the appropriate formula
  return(variance)     # fr the t distribution.
}

# (E)
tvar(10) # Executing the tvar function when v=10 provides a variance of 1.25

# (F)
sqrt(1.25) # variance from (e) is = 1.25. rnorm function takes the argument of standard deviation 
           # (=sqrt(varaince)) = sqrt(1.25) = 1.118
samp3 <- rnorm(100000, 0, 1.118) # mean from (C) is 0
mean(samp3)                      # This verifies that the mean of this sample from the normal distriution
                                 # is near 0
var(samp3)                       # This verifies that the variance of this sample from the normal 
                                 # distribution is near 1.25

# (G)
samp4 <- rt(100000, 10) # df argument (= parameter v) is set as 10
mean(samp4)             # This verifies that the mean of this sample from the t distriution is near 0
var(samp4)              # This verifies that the variance of this sample from the t distr is near 1.25

kurtosis(samp4)         # This calculates kurtosis of samp4 (t-distr sample)
# = 4.04
4.04-3                  # The excess kurtosis in this sample is given by 1 - its kurtosis
                        # = 1.04 is the excess kurtosis of this t-distribution sample

# (H)
sortedsamp4 <- sort(samp4) # where samp4 is a 100,000 element vector from a t distribution with
                           # nu = 10. The sort() function organizes the data into ascending
                           # order, which is required when plotting a line graph in R
plot(sortedsamp4, dt(sortedsamp4, 10), type = 'l') # here the first argument of plot() is our
                                                   # sample data, and the second argument gives
                                                   # the density of each data point based on a
                                                   # t distribution with nu = 10
sortedsamp3 <- sort(samp3) # This again sorts the data in ascending order
lines(sortedsamp3, dnorm(sortedsamp3, 0, 1.118), col="red") # The lines function adds the normal 
                                                            # distribution data: the first argument
                                                            # references the sorted data, the second
                                                            # argument references the density of each 
                                                            # data point based on a normal dist with
                                                            # mean 0 and standard deviation 1.118
                                                            # (variance = 1.25)
plot(sortedsamp4, dt(sortedsamp4, 10), type = 'l', ylim=c(0, 0.001)) # the ylim argument sets the limits
lines(sortedsamp3, dnorm(sortedsamp3, 0, 1.118), col="red")          # of the y axis, to better visualize
                                                                     # the difference in densities of the
                                                                     # tails of the two distributions

# (I) The t-distribution has wider tails than the normal distribution; there is more density in values at
#     the tail ends of the t-distribution compared with the normal distribution. This means that the 
#     t-distribution's proability density function is better able to estimate the probability of values 
#     which fall further from the mean than is the normal distribution. 


## Question 4

# (A)
ducks <- read.csv(file = "C:/Users/timot/OneDrive/Documents/W&M/Spring 2020/Advanced Biostats/Problem sets/ducks.csv",header=T)
  # The dataframe is named "ducks" and the read.csv function uses the first argument to read in the 
  # file from the file path, the second argument specifies that the dataframe contians headers for 
  # the data in the first row.
print(ducks)
vducks <- ducks[,2] # Here a vector is created by subsetting the dataframe, taking all row data and only 
                    # data from the second column, thereby excluding the first index column.
print(vducks)

# (B)
# Normal: These data are not from a normal distribution as they do not resemble a bell-shaped frequency 
#         distribution, with the spread of data even roughly symmetrical about the mean/central tendency.
# Gamma: These data are not from a gamma distribution as they contain negative values. The gamma 
#        distribution has support only in a set of real numbers from 0 to positive infinity. 
# Beta: These data are not from a beta distribution as they contain values outside the support of this 
#       distribution (i.e. negative values and values greater than 1). The beta distribution has support 
#       in the set of real numbers from 0 to 1. 
# Poisson: These data are not from a Poisson distribution as they contain values outside the support of 
#          this distribution. The Poisson distribution has support in the set of natural numbers starting 
#          from 0. These data contain both negative values and non-natural numbers. 
# Negative binomial: These data are not from a Negative binomial distribution as they contain values 
#                    outside the support of this distribution. The Negative binomial distribution has 
#                    support in the set of countable natural numbers from 0 to infinity. These data 
#                    contain both negative values and non-natural numbers.
# t: Similar to the normal distribution, these data are not from a t distribution as they do not resemble 
#    a bell-shaped frequency distribution, with the spread of data symmetrical about the mean/central 
#    tendency. Furthermore, these data do not have a mean of 0, which is most often true for the t 
#    distribution. 

# (C)
kde <- density(vducks, n=10000) # Here, the density function computes kernel density estimates for the 
                                # sample data. The n=10000 argument specifies the number of equally spaced
                                # points at which the density is estimated. This amount renders a 
                                # high-resolution density vector. The kde list contains x = all values 
                                # over which kde is estimated, and y = estimated density
kde$y[which(kde$x >= 7)[1]]     # Here, $ indexes specific elements of the kde list. We ask for the 
                                # estimated density (y) values and the values for which estimations were 
                                # made (kde$x). The which statement then selects those elements of kde$x 
                                # which are >/= 7, and then only the first instance of that index.
# 0.00315  This is the probability density that a population would undergo a 7% increase in size over 
# the 2-year study period.


# (D)
# Probability can be estimated by calculating the area of a rectangle under the density curve containing
# the value of interest.
which(kde$x >=7)[1] # This returns the vector index (within the 10000-element vector of x) of the value 
                    # closest to, but just larger than 7.
# = 7063
upper <- kde$y[7063] # This returns the density estimate for the value closest to, but just larger than 7,
                     # using the vector index number obtained above.
lower <- kde$y[7062] # This returns the density estimate for the value just before the above value, by 
                     # using the preceding index number.
                     # These two values are the smallest interval encompassing 7. The density estimates 
                     # of these two values will be used to calculate the height of the rectangle under the
                     # density curve, of which the area will approximate the probability of the value 
                     # defining the rectangle (i.e. 7).
print(upper)
# = 0.003150547
print(lower)
# = 0.003147045

height <- (upper + lower)/2 # This calculates the midpoint of the two densities, and thus the height of 
                            # the rectangle
print(height)
# = 0.003148796

kde$x[7063] # This returns the actual value associated with the index number
# = 7.001039
kde$x[7062] 
# = 6.998791

width <- 7.001039 - 6.998791 # This gives the width of the rectangle, defined by the upper and lower 
                             # bounds surrounding the value of 7.
print(width)
# = 0.002248

height * width # This calculates the area of the rectangle, which approximates the probability of a duck 
               # population experiencing a 7% population increase across the 2-year study.
# = 7.078494e-06

# (E)
bootKDE <- numeric()                                       # an empty vector to hold all the probability 
                                                           # densities of observing 7%
for(i in 1:100000){                                        # a for loop, running 100,000 iterations of the
                                                           # processes contained in the curly brackets:
  boot <- sample(vducks, 100, replace=TRUE)                # a sampling of 100 draws from the vducks data 
                                                           # into a new vector "boot"
  bootKDens <- density(boot, n=10000)                      # calculating the kernel density estimates for 
                                                           # the new "boot" vector data. 
  bootKDE[i] <- bootKDens$y[which(bootKDens$x >= 7)[1]]    # drawing out the density associated with value
                                                           # 7 from the kernal density estimates and adding
                                                           #to the growing bootKDE vector
  }

hist(bootKDE) # This produces a histogram of all the probability densities, which is roughly similar to
              # the initial raw data frequency histogram we plotted.

quantile(bootKDE, probs = c(0.025, 0.975), na.rm = T) # This calculates sample quantiles for stipulated
                                                      # probabilities - in this case 0.025 and 0.975, which
                                                      # are used to frame a 95% confidence interval 
                                                      # surrounding the estimate of the probability density
                                                      # of observing a 7% population increase.
#         2.5%          97.5% 
# 4.738458e-06   9.179275e-03

# Bonus:
# Probability can be estimated by calculating the area of a rectangle under the density curve containing
# the value of interest. This process can be bootsrapped to produce 95% confidience intervals for this
# probability estimate.

bootProb <- numeric()                                      # an empty vector to hold all the probabilities 
                                                           # of observing 7%
for(i in 1:100000){                                        # a for loop, running 100,000 iterations of the
                                                           # processes contained in the curly brackets:
  boot <- sample(vducks, 100, replace=TRUE)                # a sampling of 100 draws from the vducks data 
                                                           # into a new vector "boot"
  bootKDens <- density(boot, n=10000)                      # calculating the kernel density estimates for 
                                                           # the new "boot" vector data. 
  upper <- bootKDens$y[which(bootKDens$x >=7)[1]]          # This returns the density estimate for the 
                                                           # value closest to, but just larger than 7,
                                                           # using the vector index number obtained from
                                                           # which function
  lower <- bootKDens$y[(which(bootKDens$x >=7)[1])-1]      # Same, but for index - 1
  height <- (upper + lower)/2                              # height of rectangle
  right <- bootKDens$x[which(bootKDens$x >=7)[1]]          # the value just larger than 7
  left <- bootKDens$x[(which(bootKDens$x >=7)[1])-1]       # the value preceding above value
  width <- right - left                                    # width of the rectangle 
  bootProb[i] <- height * width                            # area of rectangle, added to growing vector of
                                                           # probability estimates
}

hist(bootProb) # This produces a histogram of all the probabilities, which is roughly similar to
               # the initial raw data frequency histogram we plotted and to the histogram of probability
               # densities

quantile(bootProb, probs = c(0.025, 0.975), na.rm = T) # This calculates sample quantiles for stipulated
                                                       # probabilities - in this case 0.025 and 0.975, which
                                                       # are used to frame a 95% confidence interval 
                                                       # surrounding the estimate of the probability
                                                       # of observing a 7% population increase.
#          2.5%          97.5% 
#  1.164615e-08   2.115228e-05


