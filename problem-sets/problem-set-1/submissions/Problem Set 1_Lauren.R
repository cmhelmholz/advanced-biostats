## Problem Set 1
## Lauren Emerson


## 1
# Poisson 
# all natural numbers starting from 0
# lambda (all natural numbers starting from 0)
# discrete
# Data on the number of times a bird will forage per day for a subpopulation 

# Negative binomial 
# k <- all natural numbers starting at 0
# r or number of failures until experiment stopped (real numbers>0), p or probability of success (0 to 1)
# discrete
# Data on the number of attacks on a prey item before 3 prey items are successfully killed. You would need information
# on predator success rate for 1 attack

# t
# negative infiniti to positive infiniti 
# degrees of freedom (real numbers >0)
# continuous
# A subsample from a larger population looking at beak size in finches. This data has a sample size of 20 and variance is unknown. 


## 2
# k (observed number of occurrences) > lambda (expected number of occurrences)
# You could overestimate PMF values surrounding the mean/median, while underestimating PMF values in the tails. With overdispersion, the tails would be longer and therefore, more data would lie in the tails and less data would be near the mean.  
# Variance is unable to be altered separately of the mean with the Poisson distribution meaning that overdispersed data would not be accurately represented with a Poisson distribution. 
# The negative binomial has an additional parameter (r) which allows for more flexibility in the mean and variance. Mean and variance have no direct relationship and are therefore, altered separately, allowing for overdispersion to be modeled. 


## 3
# A
kurtosis<- function(x) {                                      #writing a function with argument x and the name kurtosis
  mu<- mean(x)                                                #creating a vector for mean of the data set x
  var<- var(x)                                                #creating a vector for variance of the data set x
  N<- length(x)                                               #creating a vector for the sample size of the data set by counting the numbers present in the vector
  numer<- (x-mu)^4                                            #creating a vector for the formula of the numerator of the kurtosis equation given by (x-mu)^4
  denom<- var^2                                               #creating a vector for the formula of the denominator of the kurtosis equation given by the variance squared
  kurt <- (1/N)*sum(numer/denom)                              #creating a vector that puts the formula all together, incorporating the numer and denom vectors 
  return(kurt) 
}
x<- rnorm(10000, 3, sqrt(1.5))                                #creating a vector, x, of 10,000 draws from a normal distribution with mu=3, var=1.5, sd is the square root of the variance
kurtosis(x)                                                   #calling on the kurtosis function to act on vector x and output a kurtosis value 
# Kurtosis= 2.996822

# B
a<- rnorm(10000, 12, sqrt(50))                                #creating a vector,"a", of 10,000 draws from a normal distribution with mean 12 and variance 50 
kurtosis(a)                                                   #calling for the kurtosis function to act on vector a
# Kurtosis= 3.030308

# C 
# Mean= 0 when v= 10 

# D
tvar<-function(v){                                            #creates a function with the name tvar that takes argument v
variance <- v/(v-2)                                           #formula for variance (v/(v-2))with v as a parameter
return(variance) 
}

# E
tvar(10)                                                      #calling the function tvar with argument v= 10
# variance= 1.25

# F
b <- rnorm(100000, 0, sqrt(1.25))                             #creates a vector, b, of 100,000 draws from a normal distribution with mean 0 and variance 1.25

# G
c <- rt(100000, 10)                                           #creates a vector, c, of 100,000 draws from a t distribution with v= 10 
d <- kurtosis(c)                                              #calls the function kurtosis with argument c and saves it as vector d
d-3                                                           #calculating excess kurtosis by subtracting kurtosis of c by a normal kurtosis of 3
# Excess kurtosis= .9284591

# H
sorted <- sort(c)                                             #sorts the data set c in ascending order which is required to plot a line graph
plot(sorted, dt(sorted, 10), type="l")                        #plotting a line graph with x axis being the sorted data from c and y-axis being the density of each data point based on a t distribution with V=10
sorted2 <- sort(b)                                            #sorts the data from 3F in ascending order and saves it to a new vector "sorted2"
lines(sorted2, dnorm(sorted2, 0, sqrt(1.25)), type= "l", col= "red")  #the function lines() inputs a line on a pre-existing plot. The line has x values of the sorted data and y values are the corresponding density values with each x value in a normal distribution. Type is specified as a line with the color red. 

plot(sorted, dt(sorted, 10), ylim= c(0, 0.001), type= "l")    #same plot as the plot function before but with the y-axis constrained between 0 and 0.001 using the ylim argument

# I 
# The t-distribution has longer tails, meaning it spans a larger range, taking into account potential outliers. In addition, this distribution is a result of a sample from a larger population, meaning there is a greater likelihood that the sample will incorporate outliers. 


## 4
# A
ducks<- read.csv("C:\\Users\\LEmer\\Documents\\ducks.csv")    #Imports the csv data file from the path specified
duckspop <- ducks[,2]                                         #isolates the data set to only the x values in column 2
duckspop                                                      #verification to make sure I only have column 2

# B
hist(duckspop)                                                #plots a histogram with argument "duckspop", the imported data
#Normal- The data is skewed to the left
#Gamma- Only has support above 0, this distribution goes below 0 on the x axis
#Beta- Support of this distribution is between 0 and 1 and this graph spans a greater range
#Poisson- This is a continuous distribution
#Negative binomial- This is a continuous distribution 
#t- This does not resemble a normal distribution and we don't know if the sample was taken from a normal distribution. In addition, the mean is not equal to 0.  

# C
kde <- density(duckspop, n=10000)                             #creates a vector "kde" for the Kernel Density Estimator function with argument duckspop and n=10000, output is a list with 7 objects
plot(kde$x, kde$y, type = 'l')                                #plots the result of the Kernel Density Estimator, which smooths out the data, as a line graph with the x vector on the x axis and the y vector on the y axis
kde$y[which(kde$x >= 7)[1]]                                   #indexing the y vector within "kde" for a value of x that is >= 7, indexes the x values for the first value that meets the requirements
#probability density= .00315 

# D
which(kde$x >= 7)[1]                                          #filtering the data from kde$x for the first value that corresponds to a number greater than or equal to 7
kde$x[7063]                                                   #navigates to the 7063rd value in the kde$x vector
kde$x[7062]                                                   #navigates to the 7062nd value in the kde$x vector
upper <- kde$y[7063]                                          #creates a vector "upper" for the 7063rd y value in the vector which corresponds to the x value
lower <- kde$y[7062]                                          #creates a vector "lower" for the 7062nd y value in the vector which corresponds to the x value
height <- (upper+lower)/2                                     #creating vector "height" for the average of the heights (or y-values) surrounding 7 
width <- 7.001039-6.998791                                    #creating vector "width" for the difference in x values given by the 7062nd and 7063rd values in the vector
area <- width*height                                          #calculates the area of the rectangle by multiplying width*height 
print(area)
#probability= 7.078494e-6

# E
boot.inc<- numeric()                                           #creates an empty numeric vector with name "boot.inc"
for(i in 1:100000){                                            #creates a for loop for i in 1:100000 which repeats the functions in an incrementing fashion starting with i=1
  boot <- sample(duckspop, 100, replace= TRUE)                 #takes a sample of 100 with replacement from the pre-existing duckspop data 
  boot.kde <- density(boot, n=10000)                           #for each sample of i in 1:100000, the function density() for the kernel density estimator is called                            
  boot.inc[i] <- boot.kde$y[which(boot.kde$x >= 7)[1]]         #from the x-values given by the kde, this sorts the x values for the first value closest to 7 which has a corresponding probability density in the y vector
}
hist(boot.inc)                                                 #creates a histogram for the values input into the boot.inc vector 
 
quantile(boot.inc, probs=c(0.025, 0.975), na.rm= TRUE)         #splits the distribution into a 95% central section and 5% tail sections, each having 2.5% of the data. This outputs the x values that correspond to the start of the 2.5% lower tail and 97.5% upper tail                       
#5.504e-6, 9.288e-3

