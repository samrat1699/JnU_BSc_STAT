

#setwd("C:/Users/User/Dropbox/Major Course/Biostatistics JnU/LAB Bio")
setwd("H:\\8 semester\\Biostatistics\\LabOverrall")

#Read data
# Option-1
usage_data <- read.csv(file.choose(),header = T)
# Option-2
usage_data <- read.csv("Automobile.csv",sep=",")
str(usage_data)


# Data show a part of the warranty claims data for an automobile component 
# (the variables age (in days) and usage (in km at failure) are considered here).


library(survival)
library(ggplot2)
library(survminer)

# Basic Descriptive Statistics
summary(usage_data)

mean(usage_data$Age, trim = 0.05)

sd(usage_data$Age)
sd(usage_data$Age)/mean(usage_data$Age)*100
IQR(usage_data$Age)

mean(usage_data$Usage, trim = 0.05)
sd(usage_data$Usage)
sd(usage_data$Usage)/mean(usage_data$Usage)*100
IQR(usage_data$Usage)

cor(usage_data$Age,usage_data$Usage)


# Plot of probability mass function and distribution function.
par(mfrow=c(1,2))
x <- 0:5
px <- c(0.05, 0.15, 0.25, 0.30, 0.20, 0.05)
plot(x, px, type = "h", lwd=2, xlab="X", ylab="P(x)", main="Probability mass function for X")
points(x, px, pch=19)
Fx <- cumsum(px)
plot(x, Fx, type="s", xlab="X", ylab="F(x)", main = "Probability distribution
     function for X")
par(mfrow=c(1,1))

# Example-1:Exponential distribution—survival probability, mean, quantile, etc.

# (i) The probability of surviving 10,000 hours can be obtained from the relationship S(t)=exp(-lambda*t)
lambda <- 0.00025
S.10000 <- exp(-lambda*10000)
S.10000
# (ii) Memoryless property
S.20000_10000 <- exp(-lambda*30000)/exp(-lambda*10000)
S.20000 <- exp(-lambda*20000)
S.20000
#(iii)
MTTF <- 1/lambda
MTTF
# 4000
#(iv)
t_0.5 <- -1/lambda*log(1-0.5)
t_0.5
#(v)
t_0.3 <- -1/lambda*log(1-0.3)
t_0.3
#(vi)
t <- -log(1-0.6321)/(lambda)
t

F.4000 <- 1-exp(-lambda*4000)
F.4000

# Example 2: Weibull distribution—survival probability, mean, quantile, etc.
eta <- 4000
beta <- 1.50
mean.T <- eta*gamma(1+1/beta)
var.T <- eta^2 * (gamma(1+2/beta) - (gamma(1+1/beta))^2)
S.5000 <- exp(-(5000/eta)^beta)
S.7000 <- exp(-(7000/eta)^beta)
S.2000_5000 <- S.7000/S.5000

# Let us consider λ = 1/η = 0.00025 and β = 1, then the Weibull distribution 
# reduces to an exponential distribution where the memoryless property 
# of the exponential distribution can be used.

S.2000.Exp <- exp(-1/eta*2000)


# Example: Plots of empirical cumulative distribution functions. In this example,
# Age and Usage are used to denote the variables age in days and usage in km at failure, respectively.
par(mfrow=c(1,2))
plot.ecdf(usage_data$Age, verticals=T, do.points=F, xlab="Age (in days)",
          ylab="ecdf", main = "Empirical cdf of Age")
plot.ecdf(usage_data$Usage, verticals=T, do.points=F, xlab="Usage (in km)", ylab="ecdf", main="Empirical cdf of Usage")
ecdf.Age <- ecdf(usage_data$Age)
ecdf.Age(90)
ecdf.Usage <- ecdf(usage_data$Usage)
ecdf.Usage(20000)
par(mfrow=c(1,1))


# Example 3: Schizophrenia data

#Read data
# Option-1
schizo <- read.csv(file.choose(),header = T)
# Option-2
schizo <- read.csv("Schizophrenia.csv",sep=",")
str(schizo)

# Example 1: Export to CSV without row names
write.csv(schizo,file='Schizophrenia.csv', row.names=FALSE)

schizo <- within(schizo, {
  sex <- factor(Gender, labels = c("Male", "Female"))
  censor <- factor(Censor, labels = c("Censor", "Death"))
  marital <- factor(Marital, labels = c("Single","Married","Alone again"))
  Time <- as.numeric(Time)
})
str(schizo)



table(schizo$censor)
table(schizo$censor)
table(schizo$censor, schizo$marital)
table(schizo$censor, schizo$sex)

# Censor  Death 
#  117    163 

schizo_curv <- survfit(Surv(Time, Censor)~1, data= schizo, type = "Kaplan-meier", conf.type = "log-log")
# Plot the Kaplan-Meier estimated survival curve for all patients, together with the confidence interval. 

schizo_surv <- survfit(Surv(Time,Censor)~1,data = schizo,type = "kaplan-meier", conf.type = "log-log")

# Median Survival time
print(schizo_surv)
summary(schizo_surv)
# plot(schizo_surv)

plot(schizo_surv, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)


# schizo_order <- schizo[order(schizo$Time),] 

ggsurvplot(schizo_surv,               # survfit object with calculated statistics.
           data = schizo,             # data used to fit survival curves.
           risk.table = TRUE,         # show risk table.
           pval = FALSE,              # show p-value of log-rank test.
           conf.int = T,              # show confidence intervals for # point estimates of survival curves.
           xlim = c(0,1800),          # present narrower X axis, but not affect # survival estimates.
           xlab = "Follow-up Time in days",     # customize X axis label.
           surv.median.line = "hv",
           legend = "none"
)


ggsurvplot(
  schizo_surv,               # survfit object with calculated statistics.
  data = schizo,             # data used to fit survival curves.
  risk.table = TRUE,         # show risk table.
  pval = FALSE,              # show p-value of log-rank test.
  conf.int = T,              # show confidence intervals for # point estimates of survival curves.
  xlim = c(0,1800),          # present narrower X axis, but not affect # survival estimates.
  xlab = "Follow-up Time in days",     # customize X axis label.
  surv.median.line = "hv",
  break.time.by = 500,       # break X axis in time intervals by 500.
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = T      # show bars instead of names in text annotations # in legend of risk table
)

#Specify the survival at 6 months, 18 months and 36 months (with 95% CIs).
# What can you say about the median survival time?
print(summary(schizo_surv, time=c(6*30, 18*30, 36*30)))
print(schizo_surv)
# If median is not computed: the median survival time cannot be estimated for this data
# because the proportion of patients who experience the event is less than 50% and hence
# the estimation is not possible. 

print(summary(schizo_surv, time=c(6*30, 18*30, 36*30)))
# Plot the Kaplan-Meier estimated survival curve for marital status. 

KM_schizo_mar <- survfit(Surv(Time,Censor) ~ marital,data=schizo,type="kaplan-meier", conf.type="log-log")
summary(KM_schizo_mar)
plot(KM_schizo_mar)

#ggsurvplot(KM_schizo, data = schizo)

ggsurvplot(
  KM_schizo_mar,             # survfit object with calculated statistics.
  data = schizo,             # data used to fit survival curves.
  risk.table = TRUE,         # show risk table.
  pval = TRUE,               # show p-value of log-rank test.
  conf.int = F,              # show confidence intervals for # point estimates of survival curves.
  xlim = c(0,1800),          # present narrower X axis, but not affect # survival estimates.
  xlab = "Time in days",     # customize X axis label.
  break.time.by = 500,       # break X axis in time intervals by 500.
  surv.median.line = "hv",
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = T      # show bars instead of names in text annotations # in legend of risk table
)

# Median Survival time for marital status group of patients.
print(KM_schizo_mar)



# Plot the Kaplan-Meier estimated survival curve for gender. 

KM_schizo_gender <- survfit(Surv(Time,Censor) ~ sex,data=schizo,type="kaplan-meier", conf.type="log-log")
summary(KM_schizo_gender)
plot(KM_schizo_gender)

#ggsurvplot(KM_schizo, data = schizo)

ggsurvplot(
  KM_schizo_gender,          # survfit object with calculated statistics.
  data = schizo,             # data used to fit survival curves.
  risk.table = TRUE,         # show risk table.
  pval = TRUE,               # show p-value of log-rank test.
  conf.int = F,              # show confidence intervals for # point estimates of survival curves.
  xlim = c(0,1800),          # present narrower X axis, but not affect # survival estimates.
  xlab = "Time in days",     # customize X axis label.
  break.time.by = 500,       # break X axis in time intervals by 500.
  surv.median.line = "hv",
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = T      # show bars instead of names in text annotations # in legend of risk table
)

# Median Survival time for gender of patients.
print(KM_schizo_gender)


