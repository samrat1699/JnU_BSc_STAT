
# Generate 1000 samples from chi-square with 5 degrees of freedom
chi_samples <- rchisq(n = 1000, df = 5)
hist(chi_samples, main="Chi-square Distribution", col="lightblue")


# Generate 1000 samples from t-distribution with 10 degrees of freedom
t_samples <- rt(n = 1000, df = 10)
hist(t_samples, main="t Distribution", col="lightgreen")


# Generate 1000 samples from F-distribution with df1 = 5 and df2 = 10
f_samples <- rf(n = 1000, df1 = 5, df2 = 10)
hist(f_samples, main="F Distribution", col="orange")














































#write a r code to simulate a random sample of size 1000 from geometric(0.25) distribution
simulate_geom<- function(n, p){
    u <- runif(1000, 0,1)
    floor(log(1-u) / log(1-p))
}

set.seed(123)
samples <- simulate_geom(1000, 0.25)

hist(samples, breaks = 50, col = "lightblue",
     main = "Geometric simualted using inverion",
     xlab = "Failures Before First Success")
head(samples)


#1. Binomial(n, p) – Simulation via Bernoulli Trials
simulate_binomial <- function(n, size, p) {
    samples <- numeric(n)
    for (i in 1:n) {
        samples[i] <- sum(runif(size) < p)  # each trial: success if U < p
    }
    return(samples)
}

simulate_binomial(1000, size = 10, p = 0.5)

hist(samples)

#2. Negative Binomial
simulate_nbinom <- function(n, r, p) {
    replicate(n, sum(simulate_geometric(r, p)))
}
simulate_nbinom(1000, r = 5, p = 0.3)


#3. Poisson
simulate_poisson <- function(n, lambda) {
    samples <- numeric(n)
    for (i in 1:n) {
        L <- exp(-lambda)
        k <- 0
        p <- 1
        while (p > L) {
            p <- p * runif(1)
            k <- k + 1
        }
        samples[i] <- k - 1
    }
    return(samples)
}
simulate_poisson(1000, lambda = 4)

#4.Uniform 
simulate_uniform <- function(n, a, b) {
a + (b - a) * runif(n)
}
simulate_uniform(1000, 2, 5)

#5. exponential
simulate_exponential <- function(n, rate) {
    -log(runif(n)) / rate
}
simulate_exponential(1000, rate = 1)

