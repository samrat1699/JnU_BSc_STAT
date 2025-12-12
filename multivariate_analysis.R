# Create the dataframe
real_estate_data <- data.frame(
    AssessedValue = c(15.31, 15.20, 16.25, 14.33, 14.57, 17.33, 14.48, 14.91, 15.25, 13.89,
                      15.18, 14.44, 14.87, 18.63, 15.20, 25.76, 19.05, 15.37, 18.06, 16.35),
    SellingPrice = c(57.3, 63.8, 65.4, 57.0, 63.8, 63.2, 60.2, 57.7, 56.4, 55.6,
                     62.6, 63.4, 60.2, 67.2, 57.1, 89.6, 68.6, 60.1, 66.3, 65.8),
    TotalDwellingSize = c(74.8, 74.0, 72.9, 70.0, 74.9, 76.0, 72.0, 73.5, 74.5, 73.5,
                          71.5, 71.0, 78.9, 86.5, 68.0, 102.0, 84.0, 69.0, 88.0, 76.0)
)

# View the dataframe
print(real_estate_data)

# Save to CSV file
#write.csv(real_estate_data, "real_estate_data.csv", row.names = FALSE)
# Fit the model
model <- lm(SellingPrice ~ AssessedValue + TotalDwellingSize, data = real_estate_data)

# Summary of the model
summary(model)
cor(real_estate_data)
# Calculate correlations
cor_matrix <- cor(real_estate_data)

# Print correlation matrix
print(cor_matrix)
# Plot SellingPrice vs AssessedValue
plot(real_estate_data$AssessedValue, real_estate_data$SellingPrice, 
     main = "Selling Price vs Assessed Value",
     xlab = "Assessed Value", ylab = "Selling Price",
     pch = 19, col = "blue")
abline(lm(SellingPrice ~ AssessedValue, data = real_estate_data), col = "red")

# Plot SellingPrice vs TotalDwellingSize
plot(real_estate_data$TotalDwellingSize, real_estate_data$SellingPrice, 
     main = "Selling Price vs Total Dwelling Size",
     xlab = "Total Dwelling Size", ylab = "Selling Price",
     pch = 19, col = "green")
abline(lm(SellingPrice ~ TotalDwellingSize, data = real_estate_data), col = "red")



# Fit your model (if not done already)
model <- lm(SellingPrice ~ AssessedValue + TotalDwellingSize, data = real_estate_data)

# Coefficients table
coefficients_table <- summary(model)$coefficients
print(coefficients_table)

# 95% Confidence Intervals for coefficients
conf_intervals <- confint(model, level = 0.95)
print(conf_intervals)

# Residual standard error
residual_se <- summary(model)$sigma
print(paste("Residual standard error:", residual_se))

# Multiple R-squared
r_squared <- summary(model)$r.squared
print(paste("Multiple R-squared:", r_squared))

# Adjusted R-squared
adj_r_squared <- summary(model)$adj.r.squared
print(paste("Adjusted R-squared:", adj_r_squared))

# F-statistic and p-value
f_statistic <- summary(model)$fstatistic
print(f_statistic)

# Calculate F-statistic p-value
f_p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
print(paste("F-statistic p-value:", f_p_value))








# MANOVA with two dependent variables
manova_model <- manova(cbind(AssessedValue, SellingPrice) ~ TotalDwellingSize, data = real_estate_data)

# Summary of MANOVA results
summary(manova_model)

# You can also use different test statistics:
summary(manova_model, test = "Wilks")       # Wilks' Lambda (default)
summary(manova_model, test = "Pillai")      # Pillai's Trace
summary(manova_model, test = "Hotelling")   # Hotelling-Lawley Trace
summary(manova_model, test = "Roy")  # Roy's Largest Root

























y1 <- c(9, 6, 9, 0, 2, 3, 1, 2)
y2 <- c(3, 2, 7, 4, 0, 8, 9, 7)
trt <- c(rep("trt1", 3), rep("trt2", 2), rep("trt3", 3))
dat <- data.frame(y1, y2, trt)
dat
library(car)
res.man <- lm(cbind(y1, y2)~trt)
man_fit <- car::Anova(res.man)
summary(man_fit)
