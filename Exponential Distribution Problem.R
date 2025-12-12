# Ex-1: Exponential distribution with rate 0.00025
lambda<-0.00025

# (i) The probability of surviving 10,000 hours
S.10000<-exp(-lambda*10000)
S.10000

# (ii) Probability of surviving next 20,000 hours
S.20000_10000<-exp(-lambda*30000)/exp(-lambda*10000)
S.20000_10000
S.20000<-exp(-lambda*20000) #Memoryless Property
S.20000

# (iii) Mean time to failure
MTTF<-1/lambda
MTTF

# (iv) Median time to failure
t_0.5<-(-1/lambda)*(log(1-0.5))
t_0.5

# (v) Expected time when 30% product will fail
t_0.3<-(-1/lambda)*(log(1-0.3))
t_0.3

# (vi) Expected time when 63.21% product will fail
t<-(-log(1-0.6321))/lambda
t