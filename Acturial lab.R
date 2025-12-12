library(flexsurv)
library(ggplot2)
library(cowplot)

# Define shape values
shape_vals <- c(0.1, 0.5, 1)
rate <- 1

# Generate random samples for x range
set.seed(123)
samples <- rgompertz(500, shape = max(shape_vals), rate = rate)
x_vals <- seq(0, max(samples), length.out = 500)

# Use lapply to generate list of data frames
df_list <- lapply(shape_vals, function(sh) {
    f_vals <- dgompertz(x_vals, shape = sh, rate = rate)
    F_vals <- pgompertz(x_vals, shape = sh, rate = rate)
    S_vals <- 1 - F_vals
    h_vals <- f_vals / S_vals
    
    data.frame(x = x_vals,
               PDF = f_vals,
               CDF = F_vals,
               Survival = S_vals,
               Hazard = h_vals,
               shape = factor(sh))
})

# Combine all data frames into one
df <- do.call(rbind, df_list)

# Plot PDF
p_pdf <- ggplot(df, aes(x, PDF, color = shape)) + geom_line(size = 1) + ggtitle("PDF")

# Plot CDF
p_cdf <- ggplot(df, aes(x, CDF, color = shape)) + geom_line(size = 1) + ggtitle("CDF")

# Plot Survival
p_surv <- ggplot(df, aes(x, Survival, color = shape)) + geom_line(size = 1) + ggtitle("Survival")

# Plot Hazard
p_hazard <- ggplot(df, aes(x, Hazard, color = shape)) + geom_line(size = 1) + ggtitle("Hazard")

# Combine plots
cowplot::plot_grid(p_pdf, p_cdf, p_surv, p_hazard, ncol = 2)





library(flexsurv)
library(ggplot2)
library(cowplot)

shape_values <- c(0.1, 0.5, 1)
rate <- 1
set.seed(123)
samples <- rgompertz(1000, shape = max(shape_values), rate = rate)
x_vals <- seq(0, max(samples), length.out = 1000)

df_list <- lapply(shape_values, function(sh) {
    f_vals <- dgompertz(x_vals, shape = sh, rate = rate)
    F_vals <- pgompertz(x_vals, shape = sh, rate = rate)
    S_vals <- 1 - F_vals
    h_vals <- f_vals / S_vals
    data.frame(x = x_vals,
               PDF = f_vals,
               CDF = F_vals,
               Survival = S_vals,
               Hazard = h_vals,
               shape = factor(sh))
})
df <- do.call(rbind, df_list)

p_pdf <- ggplot(df, aes(x, PDF, color = shape)) + geom_line(size = 0.5) + ggtitle("PDF")
p_cdf <- ggplot(df, aes(x, CDF, color = shape)) + geom_line(size = 0.5) + ggtitle("CDF")
p_surv <- ggplot(df, aes(x, Survival, color = shape)) + geom_line(size = 0.5) + ggtitle("Survival")
p_hazard <- ggplot(df, aes(x, Hazard, color = shape)) + geom_line(size = 0.5) + ggtitle("Hazard")
cowplot::plot_grid(p_pdf, p_cdf, p_surv, p_hazard, ncol = 2)
















library(flexsurv)
library(ggplot2)
library(tidyr)

# Define shape values
shape_vals <- c(0.1, 0.5, 1)
rate <- 1

# Generate random samples for x range
set.seed(123)
samples <- rgompertz(500, shape = max(shape_vals), rate = rate)
x_vals <- seq(0, max(samples), length.out = 500)

# Use lapply to generate data
df_list <- lapply(shape_vals, function(sh) {
    f_vals <- dgompertz(x_vals, shape = sh, rate = rate)
    F_vals <- pgompertz(x_vals, shape = sh, rate = rate)
    S_vals <- 1 - F_vals
    h_vals <- f_vals / S_vals
    
    data.frame(
        x = x_vals,
        Density = f_vals,
        Cumulative = F_vals,
        Survival = S_vals,
        Hazard = h_vals,
        Shape = factor(sh)
    )
})

# Combine into one dataframe
df <- do.call(rbind, df_list)

# Pivot to long format
df_long <- pivot_longer(df,
                        cols = c(Density, Cumulative, Survival, Hazard),
                        names_to = "Function",
                        values_to = "Value")

# Plot all curves in one graph using facet_wrap
ggplot(df_long, aes(x, Value, color = Shape)) +
    geom_line(size = 1) +
    facet_wrap(~ Function, scales = "free_y") +
    labs(title = "Gompertz Model Curves by Shape Parameter",
         x = "Time",
         y = "Value",
         color = "Shape") +
    theme_minimal()






##-Gompertz Distribution and and its survival function and hazard function





# Simulate data
u <- runif(1000)
x <- (1/log(1.03)) * (log(1 - (log(1.03)/0.0056) * log(1 - u)))
mu <- 0.0056 * (1.03)^x
sur <- exp(0.0056 / log(1.03) * (1 - (1.03)^x))

# Create dataframe
df <- data.frame(x = x, mu = mu, sur = sur)

# Load libraries
library(ggplot2)


library(patchwork)

# Plot 1: mu
p1 <- ggplot(df, aes(x = x, y = mu)) +
    geom_line(color = "red") +
    labs(title = "Hazard Function", x = "x", y = "h(x)") +
    theme_minimal()

# Plot 2: Survival
p2 <- ggplot(df, aes(x = x, y = sur)) +
    geom_line(color = "blue") +
    labs(title = "Survival Function", x = "x", y = "S(x)") +
    theme_minimal()


# Combine plots side by side
p1 + p2








