## Problem Set 1 
## Page Tofil 

##1 

#Poisson 
#All natural numbers between zero and infinity 
#only one parameter, lambda (??)
#discrete 
#number of touchdowns scored during the games in the first round of the NFL playoffs 

#Negative Binomial 
#Whole numbers representing the number of successes
#r (integers greater than 0), p (only 0 or 1 depending on success)

#t distribution 
#integers between negative infinity and infinity 
#degrees of freedom, all real numbers greater than 0 
#continuous 
#Scoring fluorescence intensity in cells with WT or mutant TR, comparing nuclear and cytosolic


##2

#a) when the proportion between variance and mean in the distribution is greater than 1
#b) having undetected overdispersion means that the PMF may be higher or lower than 
#the true probability. Therefore, calculating the PDF may be off as well. 
#c) b/c negative binomial distributions do not assume the variance equals the mean. 
# instead, the variance is dependent on the mean, but remains a separate parameter



##3

#a) Calculate Kurtosis from vector x

x <- rnorm(10000, 3, 1.5)                     #establishing vector 'x' with 10000 draws, mean 3, var 1.5

kurtosis <- function(x){                      #estabilshing object 'kurtosis' based on vector x 
  mu <- mean(x)                               # establishing object mu as mean of x 
  var <- var(x)                               # labeling variance of x as var
  N <- length(x)                              #establishing object of sample size of vector x 
  numer <- (x-mu)^4                           #object = data point minus mean - numerator of formula raised to power 4 
  denom <- var^2                              #establishing denominator for formula prior to dividing by N
  kurt <- (1/N)*sum(numer/denom, na.rm=FALSE)
  return(kurt)                                # when running object kurtosis, will return with formula for kurtosis based on
                                              #vector x's data 
}
                                              #kurt = 3.033577


#b) Calculate kurtosis of a 100000 draw, mean 12, var 50 

x.prime <- rnorm(100000, 12, 50)

kurtosis.prime <- function(x.prime){           #estabilshing object 'kurtosis.prime' based on vector x.prime 
  mu <- mean(x.prime)                          # establishing object mu as mean of x.prime
  var <- var(x.prime)                          # labeling variance of x.prime as var
  N <- length(x.prime)                         #establishing object of sample size of vector x.prime 
  numer <- (x.prime-mu)^4                      #object = data point minus mean - numerator of formula raised to power 4 
  denom <- var^2                               #establishing denominator for formula prior to dividing by N
  kurt.prime <- (1/N)*sum(numer/denom, na.rm=FALSE)
  return(kurt)                                 # when running object kurtosis, will return with formula for kurtosis based on
                                               #vector x.prime's data  
}
                                               #kurt.prime = 2.995967


#c - when ?? = 10, mean = 0


#d



tvar <- function(n){                           #establishing function tvar with ?? as argument
  var <- n/(n-2)                               #formula for variance of t-dist 
  return(var)
}
tvar(10)                                       # running formula with n = 10

#e : var = 1.25

#f
sampnorm <- rnorm (100000, 0, sqrt(1.25))      # establishing vector sampnorm with random normal, mean = 0, stdev = sqrt1.25



#g 
t <- rt(100000, 10)                            #vector t equal to rt(). did not think I needed an ncp - assuming central
                                               #distribution?

sorted <- sort(t)                              #sort data from vector t

plot(sorted, dt(sorted, 10), type = 'l')       # plot t distribution using data from vector t




#h 
dens<- density(sampnorm)                       #getting density estimate for dist. 
plot(dens$x, length(sampnorm)*dens$y, type = 'l', col = 'red')#making sure the data will plot

#putting it all together... 

plot(sorted, dt(sorted, 10), type = 'l', col = 'black') #t distribution curve
lines(dens$x, length(sampnorm)*dens$y, col = 'red') #normal distribution curve on top of other

 #i - t- distribution is more robust to outliers because it takes into account the sample size
#we use a t- distribution when working with smaller sample sizes, which tend to have more 
# outliers due to the size. 

#4

#a) 

#load in data set ducks (had to use url to make it work off of github), and assign to vector
#ducks for ease in use 
ducks <- read.csv("https://raw.githubusercontent.com/pageht/advanced-biostats/master/problem-sets/problem-set-1/ducks.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",)

# only showing column 2 (x) - data values, saving to vector x.column. called x.column because
#that is what it is called in the file 
x.column <- ducks[ ,2]

#b) 

# Normal - because if I'm not mistaken, that looks a bit bimodal right around 1. 
# Gamma - distribution appears bimodal, is outside support
# Beta - distribution appears bimodal, is outside support 
# Poisson - not binomial 
# Negative Binomial - not binomial data 
# t - mean is not 0 if this were a centralized dist., and data appears bimodal 


#c 

xdens<- density (x.column, 
                 n = 10000)                      #running density on x column of data, setting it to vector xdens
                                                 # with argument n= 10000
plot(xdens$x, xdens$y, type = 'l', col = 'blue') #just wanted to see it as a line to better
                                                 #see the distribution 



#working through your example... please bear with me. 

#what is the probability of drawing 7 from a normal distribution with mean and sd from data calculated below?

mean(xdens$x)                                   #mean = 2.3634
sd(xdens$x)                                     #sd = 6.491336


# select interval containing the number 1.25, for examble 1.249 - 1.251
upper <- dnorm(7.01, 2.3634, 6.491336, log = FALSE)        # dnorm gives density of the dist. 
lower <- dnorm(6.99, 2.3634, 6.491336, log = FALSE)
height <- (upper + lower)/2                     # height meaning height of rectangle (area under curve)
                                                # divide by two because midpoint between interval will give us height of rectangle
                                                # height = 0.048

                                                # find width - difference between upper and lower bounds of interval 
width <- 7.01 - 6.99

# approximate probability is the area of the rectangle 

area <- height * width                          #area = 0.000952

#check to see how accurate we were in our estimation - 

integrate(dnorm, mean = 2.3634, sd = 6.491336, lower = 6.99, upper = 7.01)$value 
                                                #returned with 0.00952


#d ) what is the prob. that the pop. experiences a 7% increase during the study
xdens$y[which(xdens$x >= 7)[1]]                 #xdens is the density vector from earlier 
                                                #what you in the worksheet called dens
                                                # = 0.00315


#e) bootstrapping - going through the steps outlined from the other exercise 

#1) draw from sample with replacement and save to new vector- using sample function 
bootx<- numeric()
bootx <- sample(xdens$x, 10000, replace = TRUE) #sample from xdens 10000 times, replace value
                                                #save to vector bootx 

#2) find test statistic: 

mean(bootx)                                     #mean = 2.354627

#3) repeat the above steps multiple times: 

bootxMEAN <- numeric()                          #set up vector to hold data 
for(i in 1:10000) {                             #do for loop to make process faster. do 10,000 iterations 
  bootx <- sample(xdens$x, 10000, replace = TRUE)
  bootxMEAN[i] <- mean(bootx)
}
#4) Plot your new distribution 
hist(bootxMEAN)

#5) Calculate the 95% confidence interval of your distribution 

CI95 <- function(x){
  z <- qnorm((1-0.95)/2, lower.tail = FALSE)
  mean <- mean(x)
  sd <- sd(x)
  n <- length(x)
  lwr <- mean-z*(sd/sqrt(n))
  upr <- mean+z*(sd/sqrt(n))
  return(paste(lwr, upr, sep=', ')) 
}
CI95(bootxMEAN)                                 # returned with "2.36182859674778, 2.36435294104152"

#Double check Ci with expected values 

quantile (bootxMEAN, probs = c(0.025, 0.975))   # returned with 2.234590, 2.487399 - bit off than expected. 

















