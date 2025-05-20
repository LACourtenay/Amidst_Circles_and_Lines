
# this code is a work in progress... in other words, i'm working on it!
# please don't judge its chaotic nature, it will be better soon

library(circular)
library(sm)

# the circular library produces quite a lot of annoying warning messages
# that are not actually relevant to most programming and statistical situtations...
# for this reason it is common throughout the code that I will put a suppressWarnings()
# function so that my screen doesn't fill up with red crap...

# visualising theoretical distributions

rose.diag(rvonmises(100, pi/2, 2.37), bins = 20, col = "darkgrey")

curve.circular(dvonmises(x, pi/2, 1), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, lty = 1, col = "darkgrey")
curve.circular(dvonmises(x, pi/2, 5), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 2, col = "darkgrey")
curve.circular(dvonmises(x, pi/2, 10), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 3, col = "darkgrey")
#curve.circular(dvonmises(x, pi/2, 15), join=TRUE, lwd=2, shrink = 2.5,
#               tcl.text = 0.3)
curve.circular(dvonmises(x, pi/2, 20), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 4, col = "darkgrey")

curve.circular(dwrappedcauchy(x, pi/2, 0.1), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, col = "darkgrey")
curve.circular(dwrappedcauchy(x, pi/2, 0.5), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 2, col = "darkgrey")
curve.circular(dwrappedcauchy(x, pi/2, 0.75), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 3, col = "darkgrey")
curve.circular(dwrappedcauchy(x, pi/2, 0.8), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 4, col = "darkgrey")

curve.circular(dcardioid(x, pi/2, 0), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, col = "darkgrey")
curve.circular(dcardioid(x, pi/2, 0.2), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 2, col = "darkgrey")
curve.circular(dcardioid(x, pi/2, 0.4), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 3, col = "darkgrey")
curve.circular(dcardioid(x, pi/2, 0.5), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 4, col = "darkgrey")

curve.circular(dwrappednormal(x, pi/2, rho = 0.1), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, col = "darkgrey")
curve.circular(dwrappednormal(x, pi/2, rho = 0.5), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 2, col = "darkgrey")
curve.circular(dwrappednormal(x, pi/2, rho = 0.75), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 3, col = "darkgrey")
curve.circular(dwrappednormal(x, pi/2, rho = 0.9), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, add = TRUE, lty = 4, col = "darkgrey")

rose.diag(runif(1000, 0, pi/2), bins = 20, col = "darkgrey", tcl.text = 0.2, shrink = 1.25)
lines(density.circular(runif(1000, 0, pi/2), bw = 2), lwd=2,
      lty = 1, col = "darkgrey")

rose.diag(runif(1000, 0, pi), bins = 20, col = "darkgrey", tcl.text = 0.2, shrink = 1.25)
lines(density.circular(runif(1000, 0, pi), bw = 2), lwd=2,
      lty = 1, col = "darkgrey")

rose.diag(runif(1000, 0, (3 * pi) / 2), bins = 20, col = "darkgrey", tcl.text = 0.2, shrink = 1.25)
lines(density.circular(runif(1000, 0, (3 * pi) / 2), bw = 2), lwd=2,
      lty = 1, col = "darkgrey")

rose.diag(runif(1000, 0, 360 * (pi / 180)), bins = 20, col = "darkgrey", tcl.text = 0.2, shrink = 1.25)
lines(density.circular(runif(1000, 0, 360 * (pi / 180)), bw = 2), lwd=2,
      lty = 1, col = "darkgrey")

hist(runif(1000, 0, pi/2), breaks = 20, col = "darkgrey", xlim = c(0, 360 * (pi / 180)),
     xlab = c("X"), freq = FALSE, main = "")
lines(density(runif(1000, 0, pi/2), bw = 0.25), lwd = 3)

hist(runif(1000, 0, pi), breaks = 20, col = "darkgrey", xlim = c(0, 360 * (pi / 180)),
     xlab = c("X"), freq = FALSE, main = "")
lines(density(runif(1000, 0, pi), bw = 0.5), lwd = 3)

hist(runif(1000, 0, (3 * pi) / 2), breaks = 20, col = "darkgrey", xlim = c(0, 360 * (pi / 180)),
     xlab = c("X"), freq = FALSE, main = "")
lines(density(runif(1000, 0, (3 * pi) / 2), bw = 0.5), lwd = 3)

hist(runif(1000, 0, 360 * (pi / 180)), breaks = 20, col = "darkgrey", xlim = c(0, 360 * (pi / 180)),
     xlab = c("X"), freq = FALSE, main = "")
lines(density(runif(1000, 0, 360 * (pi / 180)), bw = 0.5), lwd = 3)


curve.circular(dvonmises(x, pi/2, 1), join=TRUE, lwd=2, shrink = 2.5,
               tcl.text = 0.3, lty = 1, col = "darkgrey")


# plots to visualise histograms of circular distributions in a linear feature
# space with calculations for the Shapiro-Wilks test on each distribution

par(mfrow = c(4, 4))

for (i in c(0, 0.2, 0.4, 0.5)) {
  
  plot(density(as.numeric(suppressWarnings(rcardioid(1000, pi/2, i))), bw = 0.5), col = "darkgrey",
       xlim = c(0, 360 * (pi / 180)), 
       lwd = 3, xlab = c("X"), main = paste0("\U03C1 = ", i))
  print(shapiro.test(as.numeric(suppressWarnings(rcardioid(1000, pi/2, i)))))
  
}

for (i in c(0.1, 0.5, 0.75, 0.9)) {
  
  plot(density(as.numeric(suppressWarnings(rwrappednormal(1000, pi/2, i))), bw = 0.5), col = "darkgrey",
       xlim = c(0, 360 * (pi / 180)), 
       lwd = 3, xlab = c("X"), main = paste0("\U03C1 = ", i))
  print(shapiro.test(as.numeric(suppressWarnings(rwrappednormal(1000, pi/2, i)))))
  
}

for (i in c(0.1, 0.5, 0.75, 0.8)) {
  
  plot(density(as.numeric(suppressWarnings(rwrappedcauchy(1000, pi/2, i))), bw = 0.5), col = "darkgrey",
       xlim = c(0, 360 * (pi / 180)), 
       lwd = 3, xlab = c("X"), main = paste0("\U03C1 = ", i))
  print(shapiro.test(as.numeric(suppressWarnings(rwrappedcauchy(1000, pi/2, i)))))
  
}

for (i in c(1, 5, 10, 20)) {
  
  plot(density(as.numeric(suppressWarnings(rvonmises(1000, pi/2, i))), bw = 0.5), col = "darkgrey",
        xlim = c(0, 360 * (pi / 180)), 
        lwd = 3, xlab = c("X"), main = paste0("\U03BA = ", i))
  print(shapiro.test(as.numeric(suppressWarnings(rvonmises(1000, pi/2, i)))))
  
}

par(mfrow = c(1,1))

#

# Uniform --------------------------

uniform_results <- read.table("Output of Cplusplus Code\\simulation_results_Uniform.txt", sep = "\t", head = TRUE)
Median_CI <- uniform_results$Median_CI
Lower_CI <- uniform_results$Lower_CI
Upper_CI <- uniform_results$Upper_CI

# Base plot
plot(seq(1, 360), Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Degrees", ylab = "Error in Degrees", main = "Circular Uniform Distribution")

# Shading the confidence interval
polygon(c(seq(1, 360), rev(seq(1, 360))), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)

# Redrawing the median line on top
lines(seq(1, 360), Median_CI, lwd = 2, col = "black")

# visualise the first simulations up to 90 degrees

deg90 <- read.table("Output of Cplusplus Code\\Angles_90_degrees_Uniform.txt", sep = "\t", head = FALSE)
deg90 <- na.omit(as.numeric(deg90[sample(1:10000, 1),]))

rose.diag(circular::circular(deg90, "angles", "degrees"), bins = 20, col = "grey")
points.circular(circular::circular(deg90, "angles", "degrees"))

sm.density(deg90, col = "red", lwd = 2)

# visualise the first simulations up to 180 degrees

deg180 <- read.table("Output of Cplusplus Code\\Angles_180_degrees_Uniform.txt", sep = "\t", head = FALSE)
deg180 <- na.omit(as.numeric(deg180[sample(1:10000, 1),]))

rose.diag(circular::circular(deg180, "angles", "degrees"), bins = 20, col = "grey")
points.circular(circular::circular(deg180, "angles", "degrees"))

sm.density(deg180, col = "red", lwd = 2)

# visualise the first simulations up to 270 degrees

deg270 <- read.table("Output of Cplusplus Code\\Angles_270_degrees_Uniform.txt", sep = "\t", head = FALSE)
deg270 <- na.omit(as.numeric(deg270[sample(1:10000, 1),]))

rose.diag(circular::circular(deg270, "angles", "degrees"), bins = 20, col = "grey")
points.circular(circular::circular(deg270, "angles", "degrees"))

sm.density(deg270, col = "red", lwd = 2)

# visualise the first simulations up to 360 degrees

deg360 <- read.table("Output of Cplusplus Code\\Angles_360_degrees_Uniform.txt", sep = "\t", head = FALSE)
deg360 <- na.omit(as.numeric(deg360[sample(1:10000, 1),]))

rose.diag(circular::circular(deg360, "angles", "degrees"), bins = 20, col = "grey")
points.circular(circular::circular(deg360, "angles", "degrees"))

sm.density(deg360, col = "red", lwd = 2)

#

# Von Mises --------------------------

vm_results <- read.table("Output of Cplusplus Code\\simulation_results_Von Mises.txt", sep = "\t", head = TRUE)
Median_CI <- vm_results$Median_CI
Lower_CI <- vm_results$Lower_CI
Upper_CI <- vm_results$Upper_CI
Param <- vm_results$Parameter

# Base plot
plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Kappa", ylab = "Error in Degrees", main = "Von Mises Distribution")

# Shading the confidence interval
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)

# Redrawing the median line on top
lines(Param, Median_CI, lwd = 2, col = "black")

# simulate example data

example_data <- rvonmises(100, pi/2, (90 / 360) * 20)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dvonmises(x, pi/2, (90 / 360) * 20), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rvonmises(100, pi/2, (180 / 360) * 20)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dvonmises(x, pi/2, (180 / 360) * 20), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rvonmises(100, pi/2, (270 / 360) * 20)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dvonmises(x, pi/2, (270 / 360) * 20), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rvonmises(100, pi/2, (360 / 360) * 20)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dvonmises(x, pi/2, (360 / 360) * 20), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

#

# Wrapped Cauchy --------------------------

vm_results <- read.table("Output of Cplusplus Code\\simulation_results_Wrapped Cauchy.txt", sep = "\t", head = TRUE)
Median_CI <- vm_results$Median_CI
Lower_CI <- vm_results$Lower_CI
Upper_CI <- vm_results$Upper_CI
Param <- vm_results$Parameter

# Base plot
plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Rho", ylab = "Error in Degrees", main = "Wrapped Cauchy Distribution")

# Shading the confidence interval
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)

# Redrawing the median line on top
lines(Param, Median_CI, lwd = 2, col = "black")

# simulate example data

example_data <- rwrappedcauchy(100, pi/2, (90 / 360) * 0.8)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappedcauchy(x, pi/2, (90 / 360) * 0.8), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappedcauchy(100, pi/2, (180 / 360) * 0.8)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappedcauchy(x, pi/2, (180 / 360) * 0.8), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappedcauchy(100, pi/2, (270 / 360) * 0.8)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappedcauchy(x, pi/2, (270 / 360) * 0.8), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappedcauchy(100, pi/2, (360 / 360) * 0.8)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappedcauchy(x, pi/2, (360 / 360) * 0.8), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

#

#

# Cardioid --------------------------

vm_results <- read.table("Output of Cplusplus Code\\simulation_results_Cardioid.txt", sep = "\t", head = TRUE)
Median_CI <- vm_results$Median_CI
Lower_CI <- vm_results$Lower_CI
Upper_CI <- vm_results$Upper_CI
Param <- vm_results$Parameter

# Base plot
plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Rho", ylab = "Error in Degrees", main = "Cardioid Distribution")

# Shading the confidence interval
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)

# Redrawing the median line on top
lines(Param, Median_CI, lwd = 2, col = "black")

# simulate example data

example_data <- rcardioid(100, pi/2, (90 / 360) * 0.5)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dcardioid(x, pi/2, (90 / 360) * 0.5), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rcardioid(100, pi/2, (180 / 360) * 0.5)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dcardioid(x, pi/2, (180 / 360) * 0.5), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rcardioid(100, pi/2, (270 / 360) * 0.5)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dcardioid(x, pi/2, (270 / 360) * 0.5), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rcardioid(100, pi/2, (360 / 360) * 0.5)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dcardioid(x, pi/2, (360 / 360) * 0.5), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

#

# Normal --------------------------

vm_results <- read.table("Output of Cplusplus Code\\simulation_results_Wrapped Normal.txt", sep = "\t", head = TRUE)
Median_CI <- vm_results$Median_CI
Lower_CI <- vm_results$Lower_CI
Upper_CI <- vm_results$Upper_CI
Param <- vm_results$Parameter

# Base plot
plot(Param, Median_CI, type = "l", lwd = 2, col = "black", ylim = range(Lower_CI, Upper_CI), 
     xlab = "Rho", ylab = "Error in Degrees", main = "Cardioid Distribution")

# Shading the confidence interval
polygon(c(Param, rev(Param)), c(Upper_CI, rev(Lower_CI)), col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)

# Redrawing the median line on top
lines(Param, Median_CI, lwd = 2, col = "black")

# simulate example data

example_data <- rwrappednormal(100, pi/2, (90 / 360) * 0.95)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappednormal(x, pi/2, (90 / 360) * 0.95), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappednormal(100, pi/2, (180 / 360) * 0.95)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappednormal(x, pi/2, (180 / 360) * 0.95), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappednormal(100, pi/2, (270 / 360) * 0.95)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappednormal(x, pi/2, (270 / 360) * 0.95), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

example_data <- rwrappednormal(100, pi/2, (360 / 360) * 0.95)
rose.diag(example_data, bins = 20, col = "grey", shrink = 2.5,
          tcl.text = 0.25)
curve.circular(dwrappednormal(x, pi/2, (360 / 360) * 0.95), join=TRUE, lwd=2, add = TRUE,
               col = "red")
points.circular(example_data)

#

# example functions ----------------------

# watson u2

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

watson_u2 <- function(sample, cdf_func) {
  
  n <- length(sample)
  sample_sorted <- sort(sample) %% (2 * pi)
  u <- cdf_func(sample_sorted) %% 1
  u_bar <- mean_theta(u)
  u2 <- sum((u - ((2 * seq(1, n) - 1) / (2 * n)))^2) + (1 / (12 * n)) - n * (u_bar - 0.5)^2
  return(u2)
  
}

pwrappedcauchy <- function(x, mu, rho, n_steps = 1000) {
  x <- x %% (2 * pi)
  sapply(x, function(xi) {
    xs <- seq(0, xi, length.out = n_steps)
    dx <- xi / (n_steps - 1)
    sum(dwrappedcauchy(xs, mu, rho)) * dx
  }) / integrate(function(t) dwrappedcauchy(t, mu, rho), 0, 2*pi)$value
}

bootstrap <- function(sample, rdist_func, cdf_func, stat_func = watson_u2, B = 1000) {
  
  observed_stat <- stat_func(sample, cdf_func)
  n <- length(sample)
  sim_stats <- replicate(B, {
    sim_sample <- rdist_func(n)
    stat_func(sim_sample, cdf_func)
  })
  p_val <- mean(sim_stats >= observed_stat)
  list(
    stat = observed_stat,
    p_value = p_val
  )
  
}

CosSinUniScores <- function(target) {
  
  N = length(target)
  ranks = rank(target, ties.method= "random")
  CosUniScores = cos((ranks*2*pi)/N)
  SinUniScores = sin((ranks*2*pi)/N)
  
  return(list(
    CosUniScores,
    SinUniScores
  ))
  
}

WgVal <- function(CSUScores, ndat, g) {
  
  CosUScores = CSUScores[[1]]
  SinUScores = CSUScores[[2]]
  N = length(CosUScores)
  ndatcsum = cumsum(ndat)
  Wg = 0
  
  for (k in 1:g) {
    
    CosUScoresk = 0
    SinUScoresk = 0
    
    if (k==1) {
      
      low = 0
      
    } else if (k > 1) {
      
      low = ndatcsum[k - 1]
      
    }
    
    for (j in 1:ndat[k]) {
      
      CosUScoresk[j] = CosUScores[j+low] ; SinUScoresk[j] = SinUScores[j+low]
      
    }
    
    sumCkSq = (sum(CosUScoresk))**2
    sumSkSq = (sum(SinUScoresk))**2
    Wg = Wg + (sumCkSq + sumSkSq) / ndat[k]
    
  }
  
  Wg = 2 * Wg
  
  return(Wg)
  
}

# Fisher's non-parametric test for comparing the medians of two
# sets of angles.

MinusPiPi<-function(sample) {
  
  n = length(sample)
  
  for (j in 1:n) {
    
    if (sample[j] < -pi) {
      
      sample[j] = sample[j]+(2*pi)
      
    } else
      
      if (sample[j] > pi) {
        
        sample[j] = sample[j]-(2*pi)
        
      }
    
  }
  
  return(sample)
  
}

PgVal<-function(target, ndat, g) {
  
  N = length(target)
  sumterms = 0 ; M = 0
  ndatcsum = cumsum(ndat)
  gmedian = suppressWarnings(circular::median.circular(target)[[1]])
  
  for (k in 1:g) {
    
    if (k==1) {
      
      low = 0
      
    } else if (k > 1) {
      
      low = ndatcsum[k-1]
      
    }
    
    sample = suppressWarnings(circular::circular(0))
    
    for (j in 1:ndat[k]) {
      
      sample[j] = target[j+low]
      
    }
    
    shiftdat = MinusPiPi(sample - gmedian)
    m = length(shiftdat[shiftdat<0])
    M = M+m
    sumterms = sumterms + m*m/ndat[k]
    
  }
  
  term1 = ((N*N)/(M*(N-M)))
  term2 = (N*M)/(N-M)
  
  Pg = term1*sumterms - term2
  
  return(Pg)
  
}

