#install.packages("survminer")
#install.packages("survival")
#install.packages("ggplot2")
#install.packages("patchwork")


#---Problem 1. parametric questions and answers-------
lamda <- 0.00025

#i) 
s<- exp(-lamda*10000)
cat("S(10000):", round(s, 4))

#ii)
s_20000 <- exp(-lamda*20000)
cat("S(20000):", round(s_20000, 4))

#iii) 
mean.T <- 1/lamda
cat("MTTF:", round(mean.T, 4))

#iv)
median.T <- log(2)/lamda
cat("Median.T:", round(median.T, 4))

#v)
t_30_fail <- (-1/lamda)*log(1-0.3)
cat("30% failure of the product:", round(t_30_fail, 4))

#vi)
t_63.21_fail <- (-1/lamda) *log(0.3679)
cat("Failing time 63.21% is:", round(t_63.21_fail))


#--Problem2----------------solution---------------
eta <- 4000
beta <- 1.50

#1)
mean.T <- eta * gamma(1+1/beta)
cat("Mean time is:", round(mean.T, 4))

var.T <- eta^2 * (gamma(1+2/beta)-(gamma(1+1/beta))^2)
cat("Variance failure time is:", round(var.T, 4))

#2)
s.5000 <- exp(-(5000/eta)^beta)
cat("Survival 5000 is:", round(s.5000, 4))

#3)
s.5000 <- exp(-(5000/eta)^beta)
s.7000 <- exp(-(7000/eta)^beta)

s.5000_7000 <- s.7000/s.5000
cat("s.5000_7000 is:", round(s.5000_7000, 4))


#4)Compare with Exponential acse (Memoryless property)
s.2000.Exp <- exp(-1/eta*2000)
cat("s.2000.Exp:", round(s.2000.Exp, 4))




##-----Some distribution with grah------

library(ggplot2)
library(patchwork)

t <- seq(0, 10, length.out = 10000)
lamdas <- c(0.5, 1, 1.5, 0.05)

df <- do.call(rbind, lapply(lamdas, function(lamda){
    data.frame(
        time = t,
        lamda = lamda,
        pdf = lamda * exp(-lamda *t),
        cdf = 1 - exp(-lamda * t),
        survival  = exp(-lamda*t),
        hazard = rep(lamda, length(t)),
        group = paste(lamda)
    )
    
}))

p1 <- ggplot(df, aes(x = time, y = pdf, color = group)) + geom_line()
   + labs(title = "Exponetial pdf", y = "Density") + theme_minimal()


p2 <- ggplot(df, aes(x = time, y = survival, color = group)) + geom_line()
+ labs(title = "Exponetial survival", y = "Density") + theme_minimal()


p3 <- ggplot(df, aes(x = time, y = hazard, color = group)) + geom_line()
+ labs(title = "Exponetial hazard", y = "Density") + theme_minimal()


p4 <- ggplot(df, aes(x = time, y = cdf, color = group)) + geom_line()
+ labs(title = "Exponetial cdf", y = "Density") + theme_minimal()



p1 + p2 + p3 + p4






##----------Problem3------solution-----------
library(survival)
library(survminer)


setwd("E:\\Jagannath University Statistics\\statistics 4th year\\8 semester\\Biostatistics\\LabOverrall") #working directory

sc <- read.csv("Schizophrenia.csv", sep  = ",")
str(sc) 

sc <- within(sc, {
    censor <- factor(Censor, labels = c("censor", "death"))
    sex <- factor(Gender, labels = c("male", "female"))
    marital <- factor(Marital, labels = c("single", "married", "alone again"))
    Time <- as.numeric(Time)
})
str(sc)

#1)
table(sc$censor,sc$sex)
# Overall count of censored vs death
table_all <- table(sc$censor)
table_all
# Censoring status by marital status
table_marital <- table(sc$marital, sc$censor)
table_marital
# Censoring status by gender
table_gender <- table(sc$sex, sc$censor)
table_gender


#2)
schizo_curv <- survfit(Surv(Time, Censor)~ 1, data=sc, type = "kaplan-meier", conf.type = "log-log")
print(schizo_curv)
summary(schizo_curv)

plot(schizo_curv, 
     xlab = "Days of follow-up",
     ylab = "Survival Probability",
     main = "Overall Survival curve")

library(survminer)
schizo_curv <- survfit(Surv(Time, Censor)~ 1, data=sc, type = "kaplan-meier", conf.type = "log-log")
ggsurvplot(
    schizo_curv,
    risk.table = TRUE,
    pval = FALSE,
    conf.int = T,
    xlim = c(0, 1800),
    xlab = "Follow - up Times in day",
    surv.median.line = "hv",
    legend = "none"
)


# ggplot version with confidence intervals and risk table
library(survminer)
schizo_curv <- survfit(Surv(Time, Censor)~ 1, data=sc, type = "kaplan-meier", conf.type = "log-log")
ggsurvplot(
    schizo_curv,
    risk.table = TRUE,             
    pval = FALSE,                  
    conf.int = TRUE,               
    xlim = c(0, 1800),             
    xlab = "Follow-up Time (days)",
    ylab = "Survival Probability",
    surv.median.line = "hv",      
    legend = "none",               
    title = "Kaplan-Meier Survival Curve for Schizophrenia Patients"
)


#iii) 
print(summary(schizo_curv, times = c(6*30, 18*30, 36*30)))
print(schizo_curv)

# Median survival time with 95% CI
med_surv <- summary(schizo_curv)$table["median"]
med_ci <- summary(schizo_curv)$table[c("0.95LCL", "0.95UCL")]

cat("Median Survival Time (days):", med_surv, "\n")
cat("95% CI for Median Survival Time:", med_ci, "\n")

# Survival probability at 6, 18, and 36 months
# Convert months to days
times <- c(180, 540, 1080)

surv_at_times <- summary(schizo_curv, times = times)

# Print survival probability and confidence intervals
output <- data.frame(
    Time_in_Days = surv_at_times$time,
    Survival_Prob = round(surv_at_times$surv, 4),
    Lower_95_CI = round(surv_at_times$lower, 4),
    Upper_95_CI = round(surv_at_times$upper, 4)
)

print(output)


#iv) Plot the Kaplan-Meier estimated survival curve for marital status
km_schizo_mar <- survfit(Surv(Time, Censor) ~ marital, data= sc, type = "kaplan-meier", conf.type = "log-log")
print(km_schizo_mar)

ggsurvplot(km_schizo_mar,
           data = sc,
           risk.table = TRUE,
           pval = TRUE,
           conf.int = F,
           xlim = c(0, 1800),
           break.time.by = 500,
           surv.median.line = "hv",
           ggtheme = theme_light(),
           risk.table.y.text.col = T,
           risk.table.y.text = T,
           xlab = "Follow-up Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curves by Marital Status",
           legend.title = "Marital Status",
           
)

#eassy to use
plot(km_schizo_mar,
     xlab = "Follow up",
     ylab = "survival probability",
     main = "Marital status kaplan meier effect")



#v) 
km_schizo_sex <- survfit(Surv(Time, Censor) ~ sex, data= sc, type = "kaplan-meier", conf.type = "log-log")
print(km_schizo_sex)

#eassy to use
plot(km_schizo_sex,
     xlab = "Follow up",
     ylab = "survival probability",
     main = "Sex status kaplan meier effect")
#another way
km_schizo_sex <- survfit(Surv(Time, Censor) ~ sex, data= sc, type = "kaplan-meier", conf.type = "log-log")
ggsurvplot(km_schizo_sex,
           data = sc,
           risk.table = TRUE,
           pval = TRUE,
           conf.int = F,
           xlim = c(0, 1800),
           xlab = "Time in days",
           break.time.by = 500,
           surv.median.line = "hv",
           ggtheme = theme_light(),
           risk.table.y.text.col = T,
           risk.table.y.text = T,
           title = "Kaplan-Meier Survival Curves by Gender",
           legend.title = "Gender"
)


# Basic Kaplan-Meier plot using base R
plot(km_schizo_sex,
     col = c("red", "blue"),         # Different colors for groups
     lwd = 2,                        # Line width
     lty = 1,                        # Line type
     xlab = "Follow-up Time (days)", # Clearer x-axis label
     ylab = "Survival Probability",  # Clearer y-axis label
     main = "Kaplan-Meier Survival Curve by Sex")

legend("bottomleft", 
       legend = c("Male", "Female"),
       col = c("red", "blue"),
       lwd = 2, 
       lty = 1)




###----------AutomobileProblem----------
setwd("E:\\Jagannath University Statistics\\statistics 4th year\\8 semester\\Biostatistics\\LabOverrall")
usage_data <- read.csv("Automobile.csv", sep = ",")
str(usage_data)

# Basic Descriptive Statistics
summary(usage_data)

mean(usage_data$Age, trim = 0.05)

sd(usage_data$Age)
sd(usage_data$Age)/mean(usage_data$Age)*100
IQR(usage_data$Age)

cor(usage_data$Age,usage_data$Usage)




# Load data
usage_data <- read.csv("Automobile.csv", sep = ",")

# (i) Average age (in days) of failure
mean_age <- mean(usage_data$Age, trim = 0.05)
cat("(i) Average Age of Failure (Trimmed):", round(mean_age, 2), "days\n")

# (ii) Standard deviation of age at failure
sd_age <- sd(usage_data$Age)
cat("(ii) Standard Deviation of Age at Failure:", round(sd_age, 2), "\n")

# (iii) Average usage (in km) at failure
mean_usage <- mean(usage_data$Usage, trim = 0.05)
cat("(iii) Average Usage at Failure (Trimmed):", round(mean_usage, 2), "km\n")

# (iv) IQR for usage at failure
iqr_usage <- IQR(usage_data$Usage)
cat("(iv) IQR of Usage at Failure:", iqr_usage, "km\n")

# (v) Coefficient of variation for age
cv_age <- sd_age / mean(usage_data$Age) * 100
cat("(v) Coefficient of Variation (Age):", round(cv_age, 2), "%\n")

# (vi) Correlation between age and usage
correlation <- cor(usage_data$Age, usage_data$Usage)
cat("(vi) Correlation between Age and Usage:", round(correlation, 3), "\n")

# (vii) Scatter plot with trend line
library(ggplot2)
ggplot(usage_data, aes(x = Age, y = Usage)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Scatter Plot of Usage vs Age",
         x = "Age (days)",
         y = "Usage (km)")




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



#-------Probability mass function and Distribution Function---------
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

par(mfrow=c(1, 2))
x <- 0:5
px <- c(0.05, 0.15, 0.25, 0.30, 0.20, 0.05)
plot(x, px, type = "h", lwd = 2, xlab= "x", ylab="P(x)", main = "Probability mass function of x")
points(x, px, pch = 19)
Fx <- cumsum(px)
plot(x, Fx, type="s", xlab="X", ylab="F(x)", main = "Probability distribution
     function for X")
par(mfrow=c(1,2))






##-------someimportant---------

###weibull distribution ist pdf, cdf, haradr, survival
library(ggplot2)
library(patchwork)

lambda <- 1000
beta <- c(0.5, 1.0, 1.5, 2.0)
t <- seq(1, 2000, by = 1)

df <- do.call(rbind, lapply(beta, function(b) {
    lt <- lambda * t
    pdf_val <- (lambda * b) * (lt)^(b - 1) * exp(-(lt)^b)
    cdf_val <- 1 - exp(-(lt)^b)
    survival_val <- exp(-(lt)^b)
    hazard_val <- pdf_val / survival_val
    
    data.frame(
        time = t,
        shape = as.factor(b),
        pdf = pdf_val,
        cdf = cdf_val,
        survival = survival_val,
        hazard = hazard_val
    )
}))

# Plot all curves using patchwork
p1 <- ggplot(df, aes(time, pdf, color = shape)) + geom_line() + labs(title = "PDF", color = "Shape β") + theme_minimal()
p2 <- ggplot(df, aes(time, cdf, color = shape)) + geom_line() + labs(title = "CDF", color = "Shape β") + theme_minimal()
p3 <- ggplot(df, aes(time, survival, color = shape)) + geom_line() + labs(title = "Survival", color = "Shape β") + theme_minimal()
p4 <- ggplot(df, aes(time, hazard, color = shape)) + geom_line() + labs(title = "Hazard", color = "Shape β") + theme_minimal()

# Arrange plots using patchwork
(p1 | p2) / (p3 | p4)



lambda <- 1000
beta <- 1.50
t <- 2000

mean.T <- ((1/lambda)*(1/beta)) * gamma(1/beta)
cat("Mean of that:", mean.T)

var.T <- (1/((lambda^2)*beta)) * (2 * gamma(2/beta) - (1/beta) * (gamma(1/beta))^2)
cat("\nVariance of time:", var.T)


