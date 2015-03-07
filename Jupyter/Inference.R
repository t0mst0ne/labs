# Week 3 inference II

# QUESTION 1.1  
babies = read.table("babies.txt", header=TRUE)

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

s1 = replicate(1000, t.test(sample(bwt.nonsmoke,30),sample(bwt.smoke,30))$conf.int[1])
s2 = replicate(1000, t.test(sample(bwt.nonsmoke,30),sample(bwt.smoke,30))$conf.int[2])

mean(s1-s2) # Ans : 18.39661

# Question 1.2
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
popdiff
mean(s1 < popdiff & s2 > popdiff)

# QUESTION 1.3

dat.ns = sample(bwt.nonsmoke, 30)
dat.s = sample(bwt.smoke, 30)
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/30 + sd.s^2/30)
tval = (X.ns - X.s)/sd.diff
ci.upper = (X.ns-X.s) + sd.diff*1.96
ci.lower = (X.ns-X.s) - sd.diff*1.96
(X.ns-X.s)
