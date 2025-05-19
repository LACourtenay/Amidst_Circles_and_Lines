
library(sm)
library(sn)
library(pValueRobust)

# plot a normal distribution theoretically

# rnorm(n, mean = 0, sd = 1) - simulate n number of samples from this distribution
# dnorm(x, mean = 0, sd = 1) - the way to calculate the PDF of this distribution

plot(
  function(x) {dnorm(x, mean = 0, sd = 1)},
  from = -4, to = 4,
  xlab = "Density",
  ylab = "X",
  main = "Normal Distribution",
  col = "red", lwd = 2
)

# create a normal distribution

normal_distribution <- rnorm(100, mean = 0, sd = 1)

sm.density(normal_distribution)

# create a skewed normal distribution

plot(
  function(x) {dsn(x, xi = 0, omega = 1, alpha = 5)},
  from = -4, to = 4,
  xlab = "Density",
  ylab = "X",
  main = "Skewed Normal Distribution",
  col = "red", lwd = 2
)

mean(rsn(1000, xi = 0, omega = 1, alpha = 5))

# R simulations ---------------------------------------------------------

# run a simulation of a gaussian and non-gaussian distribution to test the power of
# parametric and non-parametric tests

# this is a simple version of what I ran in C++

set.seed(123)

# this is the sample size required to detect differences when mean 2 reaches approximately 0.4

number_of_samples <- 48
number_simulations <- 100

skewed_normal <- function(n, mu = 0, sigma = 1, outlier_prob = 0.2, outlier_strength = 10) {
  
  values <- rnorm(n, mean = mu, sd = sigma)
  is_outlier <- runif(n) < outlier_prob
  values[is_outlier] <- values[is_outlier] + outlier_strength * sigma * runif(sum(is_outlier))
  
  return(values)
  
}

# bootstrap for 100 iterations for computational speed.

bootstrap <- function(x, n_permutations = 100) {
  
  median_values <- c()
  
  for (iteration in 1:n_permutations) {
    
    x2 <- x[sample(length(x), length(x), replace = TRUE)]
    
    median_values <- c(median_values, median(x2))
    
  }
  
  return(
    list(
      lower_CI = quantile_CI(median_values, q = 0.025),
      median = quantile_CI(median_values, q = 0.5),
      upper_CI = quantile_CI(median_values, q = 0.975)
    )
  )
  
}

simulation_p_results_low <- c()
simulation_p_results_mid <- c()
simulation_p_results_high <- c()
simulation_F_results_low <- c()
simulation_F_results_mid <- c()
simulation_F_results_high <- c()

for (target_mu in seq(0, 1, 0.025)) {
  
  single_simulation_param_p <- c()
  single_simulation_param_F <- c()
  
  for (i in 1:number_simulations) {
    
    distribution_1 <- rnorm(number_of_samples, mean = 0, sd = 1)
    distribution_2 <- skewed_normal(number_of_samples, mu = target_mu)
    
    distributions <- c(distribution_1, distribution_2)
    
    labels <- c(rep("1", number_of_samples), rep("2", number_of_samples))
    
    simulated_data <- data.frame(
      x = distributions,
      y = as.factor(labels)
    )
    
    single_simulation_param_p <- c(
      single_simulation_param_p,
      anova(lm(x ~ y, data = simulated_data))$"Pr(>F)"[1]
    )
    
    single_simulation_param_F <- c(
      single_simulation_param_p,
      anova(lm(x ~ y, data = simulated_data))$F[1]
    )
    
  }
  
  boot_results_p <- bootstrap(single_simulation_param_p)
  boot_results_F <- bootstrap(single_simulation_param_F)
  
  simulation_p_results_low <- c(simulation_p_results_low, boot_results_p$lower_CI)
  simulation_p_results_mid <- c(simulation_p_results_mid, boot_results_p$median)
  simulation_p_results_high <- c(simulation_p_results_high, boot_results_p$upper_CI)
  simulation_F_results_low <- c(simulation_F_results_low, boot_results_F$lower_CI)
  simulation_F_results_mid <- c(simulation_F_results_mid, boot_results_F$median)
  simulation_F_results_high <- c(simulation_F_results_high, boot_results_F$upper_CI)
  
}

plot(seq(0, 1, 0.025), simulation_p_results_mid, type = "l")
lines(seq(0, 1, 0.025), simulation_p_results_low, col = "red")
lines(seq(0, 1, 0.025), simulation_p_results_high, col = "red")

# please note that the results are quite different from the C++ results because
# the number of iterations is very very small

#

# C++ Results ----------------------------------

# visualise the results obtained from the C++ simulations
# just take one of the examples, I did a number of simulations. The 0_20_10 one's 
# are the most basic

simulated_data <- read.table("..\\Output of Cplusplus Code\\simulation_results_ANOVA_skew_0_20_10.txt",
                             head = TRUE)

Median_CI <- simulated_data$Median_CI_p
Lower_CI <- simulated_data$Lower_CI_p
Upper_CI <- simulated_data$Upper_CI_p
Param <- simulated_data$Parameter


plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Mean of Distribution 2", ylab = "p-Value", main = "Anova Results for Skewed Distributions")
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)
lines(Param, Median_CI, lwd = 2, col = "black")
abline(v = 0.4, lwd = 2)
abline(h = 0.05, col = "blue", lwd = 2)
abline(h = 0.003, col = "red", lwd = 2)


simulated_data <- read.table("..\\Output of Cplusplus Code\\simulation_results_Kruskal-Wallis_skew_0_20_10.txt",
                             head = TRUE)

Median_CI <- simulated_data$Median_CI_p
Lower_CI <- simulated_data$Lower_CI_p
Upper_CI <- simulated_data$Upper_CI_p
Param <- simulated_data$Parameter


plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Mean of Distribution 2", ylab = "p-Value", main = "Kruskal-Wallis Results for Skewed Distributions")
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)
lines(Param, Median_CI, lwd = 2, col = "black")
abline(v = 0.4, lwd = 2)
abline(h = 0.05, col = "blue", lwd = 2)
abline(h = 0.003, col = "red", lwd = 2)


simulated_data <- read.table("..\\Output of Cplusplus Code\\simulation_results_ANOVA_Gauss.txt",
                             head = TRUE)

Median_CI <- simulated_data$Median_CI_p
Lower_CI <- simulated_data$Lower_CI_p
Upper_CI <- simulated_data$Upper_CI_p
Param <- simulated_data$Parameter


plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Mean of Distribution 2", ylab = "p-Value", main = "Anova Results for Gaussian Distributions")
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)
lines(Param, Median_CI, lwd = 2, col = "black")
abline(v = 0.4, lwd = 2)
abline(h = 0.05, col = "blue", lwd = 2)
abline(h = 0.003, col = "red", lwd = 2)


simulated_data <- read.table("..\\Output of Cplusplus Code\\simulation_results_Kruskal-Wallis_Gauss.txt",
                             head = TRUE)

Median_CI <- simulated_data$Median_CI_p
Lower_CI <- simulated_data$Lower_CI_p
Upper_CI <- simulated_data$Upper_CI_p
Param <- simulated_data$Parameter


plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Mean of Distribution 2", ylab = "p-Value", main = "Kruskal-Wallis Results for Gauss Distributions")
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)
lines(Param, Median_CI, lwd = 2, col = "black")
abline(v = 0.4, lwd = 2)
abline(h = 0.05, col = "blue", lwd = 2)
abline(h = 0.003, col = "red", lwd = 2)
