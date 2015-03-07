# http://genomicsclass.github.io/book/pages/clt_in_practice.html

library(devtools)
install_github('rafalib','ririzarr')

#library(downloader)
#url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
#if (!file.exists(filename)) download(url, destfile=filename)
dat <- read.csv(filename)
controlPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]
hfPopulation <- dat[dat$Sex=="F" & dat$Diet=="hf",3]

#
hist(hfPopulation)
hist(controlPopulation)

qqnorm(hfPopulation);qqline(hfPopulation)
qqnorm(controlPopulation);qqline(controlPopulation)

# 
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)

#Compute the population standard deviations as well. 
#Note that we do not use the R function sd because this is to compute the population based estimates that divide by the sample size - 1.
x<-controlPopulation
N<-length(x)
popvar <- mean((x-mean(x))^2)
identical(var(x),popvar)
identical(var(x)*(N-1)/N, popvar)

popvar <- function(x) mean( (x-mean(x))^2)
popsd <- function(x) sqrt(popvar(x)) 

sd_hf <- popsd(hfPopulation)
sd_control <- popsd(controlPopulation)

N <- 12
hf <- sample(hfPopulation,12)
control <- sample(controlPopulation,12)

# The CLT tells us that, for large N, each of these is approximately normal with average population mean 
# and standard error population variance divided by N. We mentioned that a rule of thumb is that N should be 30 or more. 
# But that is just a rule of thumb as the precisness of the approximation depends on the population distribution. 
# Here we can acually check the approximation and we do that for various values of N.
# Now we use sapply and replicate instead of for loops, which is recommended.
Ns <- c(3,12,25,50)
B <- 10000 #number of simulations
res <-  sapply(Ns,function(n){
  replicate(B,mean(sample(hfPopulation,n))-mean(sample(controlPopulation,n)))
})

library(rafalib)
mypar2(2,2)
for(i in seq(along=Ns)){
  title <- paste("N=",Ns[i],"Avg=",signif(mean(res[,i]),3),"SD=",signif(popsd(res[,i]),3)) ##popsd defined above
  qqnorm(res[,i],main=title)
  qqline(res[,i],col=2)
}

mypar2(2,2)
for(i in seq(along=Ns)){
  hist(res[,i],main=Ns[i])
}

#

Ns <- c(3,12,25,50)
B <- 10000 #number of simulations
##function to compute a t-stat
computetstat <- function(n){
  y<-sample(hfPopulation,n)
  x<-sample(controlPopulation,n)
  (mean(y)-mean(x))/sqrt(var(y)/n+var(x)/n)
}
res <-  sapply(Ns,function(n){
  replicate(B,computetstat(n))
})
mypar2(2,2)
for(i in seq(along=Ns)){
  qqnorm(res[,i],main=Ns[i])
  qqline(res[,i],col=2)
}


mypar2(2,2)
for(i in seq(along=Ns)){
  hist(res[,i],main=Ns[i])
}

