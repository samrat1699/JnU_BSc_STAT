setwd("E:\\download")
rocd <- read.csv("roket_propellant_data.csv", sep = ",")
str(rocd)

summary(rocd)

# rename the columns name
names(rocd) <- c("Observation", "Age_Weeks", "Shear_Strength")

plot(rocd$Age_Weeks, rocd$Shear_Strength, 
     main = "Scatterplot of Shear Strength vs age of propellant",
     xlab = "Age of Propellant",
     ylab = "Shear Strength",
     pch = 19, col = "blue")

library(ggplot2)
ggplot(rocd, aes(x = Age_Weeks, y=Shear_Strength)) + geom_point(color = "darkred", size = 3)+
    labs(title = "Shear Strength vs Age of Propellant",
       x = "Age of Propellant (weeks)",
       y = "Shear Strength") +
  theme_minimal()



## Applying the Linear model
model <- lm(Shear_Strength~Age_Weeks, data = rocd)

summary(model)
rocd$fitted <- fitted(model)
rocd$residuals <- resid(model)

head(rocd)


# Combine the observed, fitted, and residual values into a new data frame
result_df <- data.frame(
    Observed = rocd$Shear_Strength,
    Fitted = rocd$fitted,
    Residual = rocd$residuals
)

# View first few rows
head(result_df)

sum(result_df$Observed)
sum(result_df$Fitted)
sum(result_df$Residual)


#anova model
# ANOVA table
anova(model)


coef(model)
# Example output:
# (Intercept)   Age_Weeks 
#     2627.82      -37.154
# Residual standard error (S)
sigma(model)

# R-squared and Adjusted R-squared
summary(model)$r.squared
summary(model)$adj.r.squared

# F-statistic
summary(model)$fstatistic


#Fiited line
ggplot(rocd, aes(x = Age_Weeks, y = Shear_Strength)) +
    geom_point(color = "darkgreen", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(title = "Shear Strength vs Age with Regression Line",
         x = "Age of Propellant (weeks)",
         y = "Shear Strength") +
    theme_light()







# Load required library
library(ggplot2)

# Fit the linear regression model
model <- lm(Shear_Strength ~ Age_Weeks, data = rocd)

# Print model summary
summary(model)

# 1. 95% Confidence Intervals for beta_0 (intercept) and beta_1 (slope)
confint_beta <- confint(model, level = 0.95)
print("95% Confidence Intervals for coefficients:")
print(confint_beta)

# 2. Estimate of error variance (sigma^2) and 95% CI using chi-square distribution
RSS <- sum(residuals(model)^2)          # Residual Sum of Squares
df <- df.residual(model)                # Degrees of freedom
sigma2_hat <- RSS / df                  # Estimated variance

alpha <- 0.05
chi2_lower <- qchisq(1 - alpha/2, df)
chi2_upper <- qchisq(alpha/2, df)

# 95% CI for variance
CI_sigma2 <- c(
    lower = (df * sigma2_hat) / chi2_lower,
    upper = (df * sigma2_hat) / chi2_upper
)
print("95% Confidence Interval for sigma^2 (error variance):")
print(CI_sigma2)

# Optional: CI for sigma (standard deviation)
CI_sigma <- sqrt(CI_sigma2)
print("95% Confidence Interval for sigma (standard deviation):")
print(CI_sigma)

# 3. Plot with regression line and confidence interval
ggplot(rocd, aes(x = Age_Weeks, y = Shear_Strength)) +
    geom_point(color = "blue", size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red", formula = y ~ x) +
    labs(
        title = "Fitted Regression Line with 95% Confidence Band",
        x = "Age of Propellant (Weeks)",
        y = "Shear Strength"
    ) +
    theme_minimal()

# Assume your data frame is named 'rocd'
# Linear regression model
model <- lm(Shear_Strength ~ Age_Weeks, data = rocd)

# Get prediction with 95% confidence and prediction intervals
pred <- predict(model, interval = "prediction", level = 0.95, se.fit = TRUE)
conf <- predict(model, interval = "confidence", level = 0.95)

# Combine output into a data frame
results <- data.frame(
    Observation = rocd$Observation,
    Actual = rocd$Shear_Strength,
    Predicted = pred$fit,
    Std_Error = pred$se.fit,
    CI_Lower = conf[, "lwr"],
    CI_Upper = conf[, "upr"],
    PI_Lower = pred$fit[, "lwr"],
    PI_Upper = pred$fit[, "upr"],
    Residual = model$residuals
)

# View the first few rows
head(results)

# Optional: View summary statistics
cat("Sum of residuals:", sum(results$Residual), "\n")
cat("Sum of squared residuals (SSR):", sum(results$Residual^2), "\n")
results
write.csv(results, "regression_output.csv", row.names = FALSE)

