#install.packages(c("survival", "survminer"))
library("survival")
library("survminer")


exists("lung")
head(lung)
# Create survival object
surv_obj <- Surv(time = lung$time, event = lung$status == 2)
surv_obj
# Fit Kaplan-Meier model
km_fit <- survfit(surv_obj ~ 1, data = lung)
km_fit
# Create survival curve with all titles centered
ggsurvplot(km_fit,
           title = "Overall Survival Curve",
           xlab = "Time (days)",
           ylab = "Survival Probability",
           )





# Create cumulative hazard plot by sex
# Fit Nelson-Aalen models for different sex groups
na_fit <- survfit(surv_obj ~ sex, data = lung, type = "fleming-harrington")
na_fit
# Create enhanced cumulative hazard plot
ggsurvplot(na_fit,
           fun = "cumhaz",
           title = "Cumulative Hazard by Sex",
           xlab = "Time (days)",
           ylab = "Cumulative Hazard",
           conf.int = TRUE,
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_minimal() + 
               theme(plot.title = element_text(hjust = 0.5),
                     axis.title.x = element_text(hjust = 0.5)),
           font.title = c(16, "bold"),
           font.x = c(14),
           font.y = c(14),
           font.tickslab = c(12),
           break.time.by = 250,
           legend.title = "Sex",
           legend.labs = c("Male", "Female"))






# Required packages

library(ggplot2)
library(gridExtra)
library(patchwork)
# Time points
t <- seq(0.01, 10, 0.1)
lambda <- 1

# Functions
pdf <- dexp(t, rate = lambda)
cdf <- pexp(t, rate = lambda)
surv <- 1 - cdf
haz <- pdf / surv

# Data frame
df <- data.frame(t, pdf, cdf, surv, haz)

# Plots
p1 <- ggplot(df, aes(t, pdf)) + geom_line(color="blue") + ggtitle("PDF")
p2 <- ggplot(df, aes(t, cdf)) + geom_line(color="purple") + ggtitle("CDF")
p3 <- ggplot(df, aes(t, surv)) + geom_line(color="green") + ggtitle("Survival Function")
p4 <- ggplot(df, aes(t, haz)) + geom_line(color="red") + ggtitle("Hazard Function")

# Combine plots side-by-side
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)
p1 + p2 + p3 + p4



#------------------------kkkkkkk----------------------------

library(ggplot2)
library(patchwork)

# Time points
t <- seq(0.01, 10, 0.1)
lambdas <- c(0.5, 1, 2)

# Create data for each lambda
df_all <- data.frame()

for (lambda in lambdas) {
    pdf <- dexp(t, rate = lambda)
    cdf <- pexp(t, rate = lambda)
    surv <- 1 - cdf
    haz <- pdf / surv
    df <- data.frame(t, pdf, cdf, surv, haz, lambda = as.factor(lambda))
    df_all <- rbind(df_all, df)
}

# Plotting
p1 <- ggplot(df_all, aes(x = t, y = pdf, color = lambda)) + geom_line() + ggtitle("PDF")
p2 <- ggplot(df_all, aes(x = t, y = cdf, color = lambda)) + geom_line() + ggtitle("CDF")
p3 <- ggplot(df_all, aes(x = t, y = surv, color = lambda)) + geom_line() + ggtitle("Survival Function")
p4 <- ggplot(df_all, aes(x = t, y = haz, color = lambda)) + geom_line() + ggtitle("Hazard Function")

# Combine plots in grid
(p1 | p2) / (p3 | p4)



