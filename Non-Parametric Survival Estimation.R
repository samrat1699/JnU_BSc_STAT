# Ex-3: Schizophrenia data
schizo <- read.csv("Schizophrenia.csv",sep=",")
str(schizo)         #Structure of the data set

##Factor variables with corresponding names
schizo<-within(schizo,{
  sex<-factor(Gender,labels=c("Male","Female"))
  censor<-factor(Censor,labels=c("Censor","Death"))
  marital<-factor(Marital,labels=c("Single","Married","Alone Again"))
  Time<-as.numeric(Time)
})
str(schizo)

# (i) No. of censored observations and events 
table(schizo$censor)
# No. of censored observations and events by marital status
table(schizo$censor,schizo$marital)
# No. of censored observations and events by gender
table(schizo$censor,schizo$sex)


# (ii) Plot KM estimated survival curve for all patients,
# Together with the confidence interval.
library(survival)
schizo_surv<-survfit(Surv(Time,Censor)~1,data=schizo,
                     type="kaplan-meier",conf.type="log-log")

print(schizo_surv) # Median survival time
summary(schizo_surv)
plot(schizo_surv, 
     xlab="Days of follow-up",
     ylab="Survival Probability",   
     main="Overall Survival Curve"
)

# Better curve using ggplot
library(survminer)
ggsurvplot(schizo_surv,               # survfit object with calculated statistics.
           data=schizo,               # data used to fit survival curves.
           risk.table=TRUE,           # show risk table.
           pval=FALSE,                # show p-value of log-rank test.
           conf.int=T,                # show confidence intervals.
           xlim=c(0,1800),
           xlab="Follow-up Time in days",
           surv.median.line="hv",
           legend="none"
)

ggsurvplot(
  schizo_surv,               # survfit object with calculated statistics.
  data=schizo,             # data used to fit survival curves.
  risk.table=TRUE,         # show risk table.
  pval=FALSE,              # show p-value of log-rank test.
  conf.int=T,              # show confidence intervals for # point estimates of survival curves.
  xlim=c(0,1800),          # present narrower X axis, but not affect # survival estimates.
  xlab="Follow-up Time in days",     # customize X axis label.
  surv.median.line="hv",
  break.time.by=500,       # break X axis in time intervals by 500.
  ggtheme=theme_light(),   # customize plot and risk table with a theme.
  risk.table.y.text.col=T, # colour risk table text annotations.
  risk.table.y.text=T      # show bars instead of names in text annotations # in legend of risk table
)

# (iii) Specify survival at 6 months, 18 months and 36 months (with 95% CI).
print(summary(schizo_surv,time=c(6*30,18*30,36*30)))
print(schizo_surv)

# (iv) Plot the Kaplan-Meier estimated survival curve for marital status. 

KM_schizo_mar<-survfit(Surv(Time,Censor)~marital,data=schizo,
                       type="kaplan-meier",conf.type="log-log")
summary(KM_schizo_mar)

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

# (v) Plot the Kaplan-Meier estimated survival curve for gender.

KM_schizo_gender<-survfit(Surv(Time,Censor)~sex,data=schizo,
                          type="kaplan-meier",conf.type="log-log")
summary(KM_schizo_gender)

ggsurvplot(
  KM_schizo_gender,          # survfit object with calculated statistics.
  data=schizo,             # data used to fit survival curves.
  risk.table=TRUE,         # show risk table.
  pval=TRUE,               # show p-value of log-rank test.
  conf.int=F,              # show confidence intervals for # point estimates of survival curves.
  xlim=c(0,1800),          # present narrower X axis, but not affect # survival estimates.
  xlab="Time in days",     # customize X axis label.
  break.time.by=500,       # break X axis in time intervals by 500.
  surv.median.line="hv",
  ggtheme=theme_light(),   # customize plot and risk table with a theme.
  risk.table.y.text.col=T, # colour risk table text annotations.
  risk.table.y.text=T      # show bars instead of names in text annotations # in legend of risk table
)

# Median Survival time for gender of patients.
print(KM_schizo_gender)