library(survival)
library(survminer)
library(ggplot2)

setwd("H:\\8 semester\\Biostatistics\\LabOverrall")
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
table(schizo$censor, schizo$marital)

# Censor  Death 
#  117    163 



# Fit Kaplan-Meier survival estimate
schizo_surv <- survfit(Surv(Time, Censor) ~ 1, data = schizo, type = "kaplan-meier", conf.type = "log-log")

# Plot Kaplan-Meier Survival Curve using ggsurvplot
ggsurvplot(
    fit = schizo_surv,             # Kaplan-Meier estimate object
    data = schizo,                 # Dataset used
    risk.table = TRUE,             # Add risk table at the bottom
    pval = TRUE,                   # Show log-rank test p-value (for ~1, p=1)
    conf.int = TRUE,               # Show confidence interval (log-log CI)
    xlim = c(0, 1800),             # Limit x-axis (time) range
    break.time.by = 300,           # Break time every 300 days
    xlab = "Follow-up Time (Days)",# Label for x-axis
    ylab = "Survival Probability", # Label for y-axis
    title = "Overall Kaplan-Meier Survival Curve",
    surv.median.line = "hv",       # Add horizontal/vertical line for median survival
    legend = "none",               # No legend needed for overall KM
    ggtheme = theme_minimal(),     # Clean minimal theme
    risk.table.y.text.col = TRUE,  # Color risk table text
    risk.table.y.text = FALSE      # Don't use bar-style names
)









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


