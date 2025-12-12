library(survminer)
library(survival)
# Load the lung dataset
data("lung")  # NOT data(lung), not head(data)

# View the first few rows
head(lung)

lung$status <- lung$status - 1
surv_object <- Surv(time = lung$time, event = lung$status)
km_fit <- survfit(surv_object ~ 1)
summary(km_fit)
ggsurvplot(km_fit, conf.int = TRUE, data = lung
           xlab = "Time (days)", ylab = "Survival probability", 
           title = "Kaplan-Meier Curve (Overall)")
