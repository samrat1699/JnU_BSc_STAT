# Ex-2: Weibull distribution with scale 4000 and shape 1.50
eta<-4000
beta<-1.50

# (i) Mean and variance of time-to-failure
mean.T<-eta*gamma(1+1/beta)
var.T<-eta^2*(gamma(1+2/beta)-(gamma(1+1/beta))^2)
mean.T
var.T

# (ii) Probability that the product will operate 5000 hr
S.5000<-exp(-(5000/eta)^beta)
S.5000

# (iii) Probability that the product will operate next 2000 hr
S.7000<-exp(-(7000/eta)^beta)
S.2000_5000<-S.7000/S.5000
S.2000_5000