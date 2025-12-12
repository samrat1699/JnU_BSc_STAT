#--------------------------------------------------------------------------------
#generate data and create dataframe
set.seed(123)

beta0 <- 1
beta1 <- 2
beta2 <- 0.5
beta3 <- 1.5
X1 <- runif(30, min =30, max = 50)
Z <- sample(c("A", "B", "C"), size = 30, replace = TRUE)
Z1 <- ifelse(Z == "B", 1, 0)
Z2 <- ifelse(Z == "C", 1, 0)
epsilon <- rnorm(30, 0, 1)
Y <- beta0 + beta1 * X1 + beta2 * Z1 + beta3 * Z2 + epsilon

dat <- data.frame(Y, X1, Z1, Z2, Z)
head(dat)



#---------------------------------2-------------write the log function function
loglikFun <- function(params, data) {
    beta0 <- params[1]
    beta1 <- params[2]
    beta2 <- params[3]
    beta3 <- params[4]
    sigma <- abs(params[5])  # sigma must be positive
    
    mu <- beta0 + beta1 * data$X1 + beta2 * data$Z1 + beta3 * data$Z2
    loglik <- -sum(dnorm(data$Y, mean = mu, sd = sigma, log = TRUE))
    return(loglik)  # Negative log-likelihood
}

#----------------------------------3----------Estimate parameters using optim
start_vals <- c(0, 0, 0, 0, 1)
fit <- optim(start_vals, loglikFun, data = dat, method = "BFGS", hessian = TRUE)
estimates <- fit$par
names(estimates) <- c("beta0", "beta1", "beta2", "beta3", "sigma")
estimates



#-----------------------------------4-----------
library(numDeriv)
# Compute the observed Fisher Information (Hessian of log-likelihood)
fisher_info <- hessian(func = loglikFun, x = params_hat, data = dat)
# Invert Fisher Information to get the variance-covariance matrix
fisher_info_inv <- solve(fisher_info)
fisher_info_inv


#-----------------------------------5--------------
# Standard errors
se <- sqrt(diag(fisher_info_inv))
# 95% CI
CI_lower <- params_hat - 1.96 * se
CI_upper <- params_hat + 1.96 * se
# Combine into a table
conf_int <- cbind(Estimate = params_hat, SE = se, `CI Lower` = CI_lower, `CI Upper` = CI_upper)
round(conf_int, 4)


#----------------------------------6----------------------
# Function to compute medians
get_medians <- function(df) {
    list(
        column_medians = apply(df, 2, median),
        row_medians = apply(df, 1, median)
    )
}
# Apply only to numeric columns
median_results <- get_medians(dat[, c("Y", "X1", "Z1", "Z2")])
median_results




#--------------------------------7----------------------------
# Bootstrap standard error estimation

n <- nrow(dat)
B <- 1000
bootstrap_estimates <- replicate(B, {
    idx <- sample(1:n, replace = TRUE)
    d_boot <- dat[idx, ]
    optim(start_vals, loglikFun, data = d_boot, method = "BFGS")$par
})
# Standard errors from bootstrap
bootstrap_se <- apply(bootstrap_estimates, 1, sd)
names(bootstrap_se) <- c("beta0", "beta1", "beta2", "beta3", "sigma")
bootstrap_se

# Row-wise SE from bootstrap
bootstrap_row_se <- apply(t(bootstrap_estimates), 1, sd)
head(bootstrap_row_se, 29)






#---------------------------8----------------------------
# Jackknife standard error estimation
jackknife_estimates <- sapply(1:n, function(i) {
    d_jack <- dat[-i, ]
    optim(start_vals, loglikFun, data = d_jack, method = "BFGS")$par
})

# Convert to matrix
jack_est_mat <- t(jackknife_estimates)
jackknife_mean <- colMeans(jack_est_mat)

# Jackknife column-wise (SE)
jackknife_se <- sqrt((n - 1) / n * colSums((jack_est_mat - matrix(jackknife_mean, nrow = n, ncol = 5, byrow = TRUE))^2))
names(jackknife_se) <- c("beta0", "beta1", "beta2", "beta3", "sigma")
jackknife_se


# Row-wise SE from jackknife
jackknife_row_se <- apply(jack_est_mat, 1, sd)
jackknife_row_se

