#Matrix Production

A <- matrix(c(1, 2, 3, 4), nrow = 2)
B <- matrix(c(5, 6, 7, 8), nrow = 2)

AB <- A %*% B
print(AB)


Z1 <- 21:50
Z2 <- seq(10.1, 13, length.out = 30)
Z3 <- seq(5, 63, length.out = 30)
Z <- cbind(Z1, Z2, Z3)
Z
Y <- cbind(Z, 1:30)
Y
# Step 3: Plot Z2 and Z3 against Z1
plot(Z1, Z2, type = "b", col = "blue", xlab = "Z1", ylab = "Value", main = "diagram")
lines(Z1, Z3, type = "o", col = "red")
legend("topleft", legend = c("Z2", "Z3"), col = c("blue", "red"), lty = 1, pch = c(1,1))

anova_model <- aov(gain ~ treatment)



A1 <- c(218, 225, 220, 245, 241)
A2 <- c(249, 240, 243, 241, 252)
A3 <- c(280, 285, 276, 278, 279)
A4 <- c(275, 270, 287, 284, 290)

gain <- c(A1, A2, A3, A4)

treatment <- factor(rep(c("A1", "A2", "A3", "A4"), each = 5))
treatment


anova_model <- aov(gain ~ treatment)
summary(anova_model)

TukeyHSD(anova_model)


# Load ggplot2
install.packages("ggplot2")  # Only once
library(ggplot2)

# Create a data frame from vectors
df <- data.frame(Z1, Z2, Z3)

# Plot Z2 and Z3 vs Z1 using ggplot
library(tidyr)

df_long <- pivot_longer(df, cols = c("Z2", "Z3"), names_to = "Variable", values_to = "Value")

ggplot(df_long, aes(x = Z1, y = Value, color = Variable, shape = Variable)) +
    geom_line() +
    geom_point() +
    labs(title = "diagram", x = "Z1", y = "Value") +
    theme_minimal()












#Polynomial Regression weight loss over time

t <- 0:9
y <- c(0.21, -1.46, -3.04, -3.21, -5.04, -5.37, -6.03, -7.21, -7.46, -7.96)

model <- lm(y ~ poly(t, 2))
summary(model)


# Base R plot
plot(t, y, main = "Weight Loss Over Time", xlab = "Time", ylab = "Weight Difference", pch = 19)
lines(t, predict(model), col = "blue", lwd = 2)
legend("bottomleft", legend = c("Observed", "Fitted"), col = c("black", "blue"), lty = c(NA,1), pch = c(19, NA))

residualss <- resid(model)
residualss

plot(t, residualss, type = "b", main = "Residual Plot", xlab = "Time", ylab = "Residuals", col = "red")
abline(h = 0, lty = 2)

qqnorm(residualss)
qqline(residualss)






data(trees)
head(trees)

#fit the model
model2 <- lm(Volume ~ Girth + Height, data = trees)
summary(model2)

#residuals up to 3 decimal places
residuals <- round(resid(model2), 3)
print(residuals)


shapiro.test(resid(model2))
qqnorm(resid(model2))
qqline(resid(model2), col = "red")

library(car)
vif(model2)
