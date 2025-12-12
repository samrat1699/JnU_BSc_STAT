#install.packages("rio")
setwd("E:\\download")
library(rio)
dat <- import("demo.sav")
dim(dat)

setwd("E:\\download")
df <- read.csv("The Delivery Time Data .csv", sep=",")

#rename the columns
names(df) <- c("Observations", "Delivery_Time", "No_of_cases", "Distances")
summary(df)

#Check missing values
any(is.na(df))
#is.na(df)
sum(is.na(df))#counts the missing values
colSums(is.na(df)) #columns wise check missing values

#Check duplicated values
any(duplicated(df))
#duplicated(df)
#df[duplicated(df), ]
sum(duplicated(df))
#remove the duplicated
df_unique <- df[!duplicated(df), ]

#see columns name
colnames(df)
names(df)

#check uniques values
lapply(df, unique)


#Check each columns
unique(df$No_of_cases)
#Frequency of each unique value (value counts)
table(df$Distances)



library(dplyr)
#the average number of cases for each delivery time:
df %>%
    group_by(Delivery_Time) %>%
    summarise(Avg_Cases = mean(No_of_cases, na.rm = TRUE))

df %>%
    group_by(Delivery_Time, Distances) %>%
    summarise(Total_Cases = sum(No_of_cases))

df %>%
    group_by(Delivery_Time) %>%
    summarise(Count = n())

#scatterplot
library(ggplot2)

ggplot(df, aes(x = Delivery_Time, y = No_of_cases)) +
    geom_point(color = "blue") +
    labs(title = "Cases vs. Delivery Time")

#lineplot
ggplot(df, aes(x = Observations, y = Delivery_Time)) +
    geom_line(color = "darkgreen") +
    labs(title = "Delivery Time Trend")

#boxplot
ggplot(df, aes(x = factor(0), y = No_of_cases)) +
    geom_boxplot(fill = "orange") +
    labs(title = "Boxplot of Number of Cases")

ggplot(df, aes(x = factor(Delivery_Time), y = No_of_cases)) +
    geom_boxplot() +
    labs(title = "Cases by Delivery Time")

#Histogram
ggplot(df, aes(x = Distances)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Distances")

df %>%
    group_by(Delivery_Time) %>%
    summarise(Avg_Cases = mean(No_of_cases)) %>%
    ggplot(aes(x = factor(Delivery_Time), y = Avg_Cases)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Average Cases by Delivery Time")





library(corrplot)
corrplot(cor(df[sapply(df, is.numeric)], use = "complete.obs"), method = "circle")

# Pair only Delivery_Time with others
GGally::ggpairs(df[, c("Delivery_Time", "No_of_cases", "Distances")],
                title = "Delivery_Time vs Other Variables")

#  Pair plot df and contains numeric variables

# Simple pair plot with base R
pairs(df[, c("Delivery_Time", "No_of_cases", "Distances")],
      main = "Pair Plot: Delivery Time vs Other Variables",
      col = "darkblue", pch = 19)

#-------------------------3D----------------------------------
# Install if not already installed
#install.packages("scatterplot3d")

# Load the package
library(scatterplot3d)

# 3D scatter plot: Delivery_Time vs No_of_cases vs Distances
scatterplot3d(df$Delivery_Time, df$No_of_cases, df$Distances,
              xlab = "Delivery Time",
              ylab = "No of Cases",
              zlab = "Distances",
              main = "3D Scatterplot",
              color = "blue", pch = 19)

# Install if not already installed
#install.packages("plotly")

# Load the package
library(plotly)

# Create interactive 3D scatter plot
plot_ly(df, x = ~Delivery_Time, y = ~No_of_cases, z = ~Distances,
        type = "scatter3d", mode = "markers",
        marker = list(size = 4, color = 'blue')) %>%
    layout(title = "Interactive 3D Scatterplot",
           scene = list(xaxis = list(title = "Delivery Time"),
                        yaxis = list(title = "No of Cases"),
                        zaxis = list(title = "Distances")))


#-----------------Applying the Multiple model------------
model <- lm(Delivery_Time~No_of_cases + Distances, data = df)
summary(model)

anova(model)

# Add fitted values and residuals to the dataframe
df$fitted <- fitted(model)
df$residuals <- resid(model)

# View first few rows
head(df)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)



# Assuming you already have a linear model created:
# model <- lm(Delivery_Time ~ No_of_cases + Distances, data = df)

# Get confidence intervals
conf <- predict(model, interval = "confidence", level = 0.95)

# Get prediction intervals with standard errors
pred <- predict(model, interval = "prediction", level = 0.95, se.fit = TRUE)

# Combine everything into a data frame
results <- data.frame(
    Observation = 1:nrow(df),  # or df$Observations if available and matches
    Actual = df$Delivery_Time,
    Predicted = pred$fit[, "fit"],
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


#Coefficent
coef(model)

#Anova
anova(model)

