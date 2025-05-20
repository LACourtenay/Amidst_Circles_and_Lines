
library(circular)
library(sm)

quantile_CI <- function(x, q = c(0.025, 0.975)) {
  
  if (length(q) == 2) {
    
    if (!is.numeric(q[1]) | !is.numeric(q[2]))  {
      stop("Quantiles must be numeric")
    }
    if(q[1] < 0 | q[2] < 0 | q[1] > 1 | q[2] > 1) {
      stop("Quantile values must be between 0 and 1")
    }
    
    upper_CI <- sort(x)[ceiling(q[2] * length(x))]
    lower_CI <- sort(x)[ceiling(q[1] * length(x))]
    
    return(
      c(lower_CI, upper_CI)
    )
    
  } else if (length(q) == 1) {
    
    x_prima = sort(x)[ceiling(q * length(x))]
    
    return(x_prima)
    
  } else {
    stop("Invalid number of quantiles provided")
  }
  
}

mean_theta <- function(x) {
  
  n = length(x)
  numerator_x = 0
  numerator_y = 0
  
  for (i in 1:n) {
    
    numerator_x <- numerator_x + cos(x[i])
    numerator_y <- numerator_y + sin(x[i])
    
  }
  
  Y = numerator_y / n
  X = numerator_x / n
  
  r = sqrt(X^2 + Y^2)
  
  cosa = X / r
  sina = Y / r
  
  return(atan2(sina, cosa))
  
}

mean_x <- function(x) {
  
  n = length(x)
  sum_x = 0
  
  for (i in 1:n) {
    
    sum_x = sum_x + x[i]
    
  }
  
  return(sum_x / n)
  
}

convert_to_radians <- function(x) {
  
  return(x * (pi / 180))
  
}

convert_to_degrees <- function(x) {
  
  return(x * (180 / pi))
  
}

rmse <- function(y, y_pred) {
  
  mse <- sum((y - y_pred)^2) / length(y)
  return(sqrt(mse))
  
}

x <- rnorm(100, 0, 1)
xprima <- circular::circular(x, "angles", "radians")
mean.circular(xprima)
mean_theta(xprima)
mean(x)
mean_x(x)

#

# uniform distribution ------------------

rnorm(100, 0, 1)
runif(100, 0, 1)

upper_bound <- 90 * 4
lower_bound <- 0
number_of_samples <- 100
x <- runif(number_of_samples, lower_bound, upper_bound)
mean_x(x)
convert_to_degrees(mean_theta(convert_to_radians(x)))


n_simulations <- 10000
linear <- c()
circular <- c()

for (i in 1:n_simulations) {
  
  x <- runif(number_of_samples, lower_bound, upper_bound)
  linear <- c(linear, mean_x(x))
  circular <- c(circular, convert_to_degrees(mean_theta(convert_to_radians(x))))
  
}



rmse(linear, circular)

sm.density(x)
abline(v = mean_x(linear))
abline(v = mean_theta(circular), col = "red")

rose.diag(convert_to_radians(x), bin = 10, col = "grey")
arrows.circular(convert_to_radians(mean_x(linear)))
arrows.circular(mean_theta(convert_to_radians(circular)), col = "red")

bootstrap <- function(x, n = 10000) {
  
  results <- c()
  
  for (i in 1:n) {
    
    xprima <- sample(x, replace = TRUE)
    results <- c(results, median(xprima))
    
  }
  
  return(
    list(
      lower = pValueRobust::quantile_CI(results, q = 0.025),
      mid = pValueRobust::quantile_CI(results, q = 0.5),
      upper = pValueRobust::quantile_CI(results, q = 0.975)
    )
  )
  
}

lower_CI <- c()
upper_CI <- c()
median_CI <- c()

for (circ_value in 1:360) {
  
  upper_bound <- circ_value
  lower_bound <- 0
  number_of_samples <- 100
  n_simulations <- 100
  n_bootstrap <- 100
  linear <- c()
  circular <- c()
  
  for (i in 1:n_simulations) {
    
    x <- runif(number_of_samples, lower_bound, upper_bound)
    linear <- c(linear, mean_x(x))
    circular <- c(circular, convert_to_degrees(mean_theta(convert_to_radians(x))))
    
  }
  
  rmse_values <- c()
  
  for (i in 1:n_bootstrap) {
    
    linear_prima <- sample(linear, replace = TRUE)
    circular_prima <- sample(circular, replace = TRUE)
    rmse_values <- c(rmse_values, rmse(linear_prima, circular_prima))
    
  }
  
  lower_CI <- c(lower_CI, quantile_CI(rmse_values, q = 0.025))
  upper_CI <- c(upper_CI, quantile_CI(rmse_values, q = 0.975))
  median_CI <- c(median_CI, quantile_CI(rmse_values, q = 0.5))
  
}

plot(seq(1, 360), median_CI, type = "l")

#










