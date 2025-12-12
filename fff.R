library(survival)
library(survminer)
library(ggplot2)


setwd("H:\\8 semester\\Biostatistics\\LabOverrall")

data <- read.csv(file.choose(),header = T)

data <- within(data, {
    sex <- factor(Gender, labels = c("Male", "Female"))
    censor <- factor(Censor, labels = c("Censor", "Death"))
    marital <- factor(Marital, labels = c("Single","Married","Alone again"))
    Time <- as.numeric(Time)
})


# (i) censoring and event counts
# Overall
table(data$censor)

# By marital status
table(data$censor, data$marital)

#By gender
table(data$censor, data$sex)



# (ii) kaplan -Meier Survival curve (Overall)

# Plot the Kaplan-Meier estimated survival curve for all patients, together with the confidence interval. 

schizo_surv <- survfit(Surv(Time,Censor)~1,data = data,type = "kaplan-meier", conf.type = "log-log")

# Median Survival time
print(schizo_surv)
summary(schizo_surv)

# schizo_order <- schizo[order(schizo$Time),] 

ggsurvplot(schizo_surv,               # survfit object with calculated statistics.
           data = data,             # data used to fit survival curves.
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
    data = data,             # data used to fit survival curves.
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

