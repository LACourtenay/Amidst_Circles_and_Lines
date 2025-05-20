
library(rgl)
library(FactoMineR)
library(factoextra)
library(vegan)
library(corrplot)
library(dplyr)
library(ggplot2)

# for Geometric Morphometrics

library(geomorph)
library(shapes)
library(GraphGMM)

# step 1 - let's PCA a tree! ----------------------------------

# Basic First experiments with PCA
# using PCA to align point clouds...

tree <- read.table("An_Amazing_Tree.txt", head = FALSE, sep = " ")
colnames(tree) <- c("X", "Y", "Z")

open3d(); plot3d(tree[,1], tree[,2], tree[,3], aspect = "iso")

plot(tree[,1], tree[,2], asp = 1) # this is computationally intensive so might take a while
# (which is why i wouldnt work with 3D models in R!...)

tree_pca <- prcomp(tree) # simply calculate a PCA of the tree...

start_time <- Sys.time(); tree_pca <- prcomp(tree); end_time <- Sys.time()
end_time - start_time # see how fast it is!

plot(tree_pca$x[,1], tree_pca$x[,2], asp = 1)

# we could use biplot(tree_pca) to plot the principal axes, but this is too
# computationally intensive, so i'm just going to use
# factoextra to get the correlation plot

fviz_pca_var(tree_pca, arrowsize = 1.5, labelsize = 5)

# plot in 3D

open3d(); plot3d(tree_pca$x[,2], tree_pca$x[,1], tree_pca$x[,3], aspect = "iso")

# can we calculate the height of the tree now automatically?
# of course we can, thats the whole point

diff(range(tree_pca$x[,1]))

# 

# lets play with scale -----------------------------

data("airquality")
summary(airquality) # tells us what the data looks like
help(airquality) # gives us information about the data

airquality <- na.omit(airquality)
month <- as.factor(airquality[,c(5)])
airquality <- airquality[,-c(5, 6)]

pca_no_scale <- prcomp(airquality)
pca_scale <- prcomp(airquality, scale = TRUE) 

plot(pca_no_scale$x[,1], pca_no_scale$x[,2], pch = 19, asp = 1)
plot(pca_scale$x[,1], pca_scale$x[,2], pch = 19, asp = 1)

# export to test in other software

write.table(airquality, "airquality.txt", col.names = FALSE, row.names = FALSE, sep = ",")

#

# if you're interested in geometric morphometrics...

data(apes)

gpa <- gpagen(apes$x)
plot(gm.prcomp(gpa$coords), asp = 1, pch = 19)

# just read the gpagen documentaiton, you won't see a scale parameter anywhere

# in GraphGMM, because i made my library for many things, not just GMM,
# i added an option in my own pca function to scale if the user wishes, however
# by default scale is set to FALSE

pca_plot(vector_from_landmarks(gpa$coords))
pca_plot(vector_from_landmarks(gpa$coords), scale = TRUE)

# you'll notice that the feature spaces are different...

#

# knowing WHEN to scale

data(iris)

# quick visualisation

pca_plot(iris[,1:4], iris$Species, CI_ellipse = TRUE, scale_axes = FALSE,
         scale = TRUE)
pca_plot(iris[,1:4], iris$Species, CI_ellipse = TRUE, scale_axes = FALSE,
         scale = FALSE)

# let's study the variables themselves

pca_no_scale <- prcomp(iris[,1:4])
pca_scale <- prcomp(iris[,1:4], scale = TRUE)

fviz_eig(pca_scale)
fviz_eig(pca_no_scale)

fviz_pca_var(pca_scale)
fviz_pca_var(pca_no_scale)

fviz_contrib(pca_scale, choice = "var")
fviz_contrib(pca_no_scale, choice = "var")

#

# number of pc scores ------------------------

# inspect the eigenvalues and % of variance!

fviz_eig(pca_scale)
fviz_eig(pca_no_scale)

fviz_pca_var(pca_scale)
fviz_pca_var(pca_no_scale)

fviz_contrib(pca_scale, choice = "var")
fviz_contrib(pca_no_scale, choice = "var")

#

# biplots ------------------------


distribution <- array(numeric(), dim = c(100, 10))

for (dimension in 1:dim(distribution)[2]) {
  distribution[,dimension] <- rnorm(dim(distribution)[1])
}

pca <- prcomp(distribution)

biplot(pca, xlabs = rep("", nrow(distribution)))
points(pca$x[,1], pca$x[,2], pch = 19, cex = 0.1)

#

# highly correlated variables ------------------------

set.seed(666)

# create two sets of non-correlated variables and do a PCA

n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
df_uncor <- data.frame(x1, x2)

pca_uncor <- prcomp(df_uncor, scale = TRUE)
summary(pca_uncor)
biplot(pca_uncor, main = "Uncorrelated")
fviz_pca_biplot(pca_uncor, col.var = "red", geom = c("point"), labelsize = 7)

plot(pca_uncor$x, pch = 19, asp = 1)

# lets now force one dimension to be correlated with dimension 1

x3 <- x1 + rnorm(n, sd = 0.01)
df_cor <- data.frame(x1, x3)

pca_cor <- prcomp(df_cor, scale = TRUE)
biplot(pca_cor, main = "Correlated")
fviz_pca_var(pca_cor)
fviz_pca_biplot(pca_cor, col.var = "red", geom = c("point"), labelsize = 7)

plot(pca_cor$x, pch = 19)
plot(pca_cor$x, pch = 19, asp = 1)

# just compare the two...

barplot(summary(pca_uncor)$importance[2, ], main = "Uncorrelated: Variance Explained", ylim = c(0, 1))
barplot(summary(pca_cor)$importance[2, ], main = "Correlated: Variance Explained", ylim = c(0, 1))

# lets now add even more correlated variables

x4 <- x1 + rnorm(n, sd = 0.01)
df_multi_cor <- data.frame(x1, x3, x4)

pca_multi_cor <- prcomp(df_multi_cor, scale. = TRUE)
barplot(summary(pca_multi_cor)$importance[2, ], main = "Correlated: Variance Explained", ylim = c(0, 1))
plot(pca_multi_cor$x)
biplot(pca_multi_cor)

plot(pca_multi_cor$x, asp = 1, pch = 19)

# and yet another

x5 <- x1 + rnorm(n, sd = 0.3)
df_partial_cor <- data.frame(x1, x3, x5)
pca_partial <- prcomp(df_partial_cor, scale. = TRUE)
barplot(summary(pca_partial)$importance[2, ], main = "Correlated: Variance Explained", ylim = c(0, 1))
plot(pca_partial$x, asp = 1)
biplot(pca_partial)
plot(pca_partial$x, asp = 1, pch = 19)

# just test with a correlation plot!

corrplot(cor(df_partial_cor), method = "circle")
corrplot(cor(data.frame(rnorm(100), rnorm(100), rnorm(100))), method = "circle")

#

# let's explore scale and units of the PC score axes -------------------

# do a pca with the libraries of both

data(apes)

apes_gpa <- GPA(apes$x)

pca_plot(vector_from_landmarks(apes_gpa$coordinates))

gpa <- gpagen(apes$x)
plot(gm.prcomp(gpa$coords))

# let's change the asethetics though so at least they both look exactly the same

graph_gmm_pcs <- pca_plot(vector_from_landmarks(apes_gpa$coordinates))$pc_scores
geomorph_pcs <- gm.prcomp(gpa$coords)$x

plot(graph_gmm_pcs[,1], graph_gmm_pcs[,2], asp = 1, pch = 19)
plot(geomorph_pcs[,1], geomorph_pcs[,2], asp = 1, pch = 19)

# there are slight differences, and the scale is different, but the topology of the feature space is the same

# we can actually test this...

geomorph_topology <- dist(geomorph_pcs)
graph_gmm_topology <- dist(graph_gmm_pcs)

mantel(geomorph_topology, graph_gmm_topology, method = "pearson")

#

# convex-hulls vs confidence ellipses ------------------------

# lets create some fake data

set.seed(666)

sample1 <- cbind(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 0, sd = 1))
sample2 <- cbind(rnorm(100, mean = -2, sd = 1), rnorm(100, mean = 2, sd = 1))
sample3 <- cbind(rnorm(100, mean = 2, sd = 1), rnorm(100, mean = 2, sd = 1))

dataset <- data.frame(rbind(sample1, sample2, sample3), sample = c(rep("A", 100), rep("B", 100), rep("C", 100)))

distribution <- pca_plot(dataset[,1:2])$pc_scores
distribution <- data.frame(distribution, sample = c(rep("A", 100), rep("B", 100), rep("C", 100)))
colnames(distribution) <- c("PC1", "PC2", "Sample")
xlim <- range(distribution[,1])
ylim <- range(distribution[,2])

plot_ellipses <- function(dataset) {
  
  ggplot(data = dataset, aes(x = PC1, y = PC2, colour = Sample)) +
    geom_point(size = 4) +
    theme_bw() +
    stat_ellipse(linewidth = 1,
                 level = 0.95) +
    scale_color_manual(values = c(
      "black","orange","dodgerblue"
    )) +
    theme(
      legend.position = "none",
      plot.margin = unit(c(1,1,1,1), "cm"),
      axis.title.x = element_text(face = "bold", size = 18,
                                  margin = margin(t = 10, r = 0, b = 5, l = 0)),
      axis.title.y = element_text(face = "bold", size = 18,
                                  margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text.x = ggplot2::element_text(angle = 90, size = 15, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 15, face = "bold")
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim)
  
}

plot_hulls <- function(dataset) {
  
  hulls <- dataset %>%
    dplyr::group_by(Sample) %>%
    dplyr::slice(chull(PC1, PC2))
  conf_interval <- ggplot2::geom_polygon(
    data = hulls,
    alpha = 0,
    size = 1
  )
  
  ggplot(data = dataset, aes(x = PC1, y = PC2, colour = Sample)) +
    geom_point(size = 4) +
    theme_bw() +
    conf_interval +
    scale_color_manual(values = c(
      "black","orange","dodgerblue"
    )) +
    theme(
      legend.position = "none",
      plot.margin = unit(c(1,1,1,1), "cm"),
      axis.title.x = element_text(face = "bold", size = 18,
                                  margin = margin(t = 10, r = 0, b = 5, l = 0)),
      axis.title.y = element_text(face = "bold", size = 18,
                                  margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text.x = ggplot2::element_text(angle = 90, size = 15, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 15, face = "bold")
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim)
}


# Sample 5% from each group

subset_5 <- distribution %>%
  group_by(Sample) %>%
  sample_frac(0.05) %>%
  ungroup()

plot_ellipses(subset_5)
plot_hulls(subset_5)

# Sample 5% from each group

subset_25 <- distribution %>%
  group_by(Sample) %>%
  sample_frac(0.25) %>%
  ungroup()

plot_ellipses(subset_25)
plot_hulls(subset_25)

# Sample 100% from each group

subset_100 <- distribution

plot_ellipses(subset_100)
plot_hulls(subset_100)


# bounded variables ------------------------

# simple test creating a bound distribution

# in this test, i create a bound variable and see whether it has an impact on the
# PCA. I have not found enough to say its a huge issue, but i don't really feel
# comfortable with it... I think normalising this variable using some rule
# such as log(x / (1 - x)) for [0, 1] variables would be pretty useful
# but for other bounds, i'm not so sure...

set.seed(666)

trial <- rnorm(200, 0, 100)
sm::sm.density(trial)
shapiro.test(trial)
dist_limits <- c(mean(trial) - sd(trial),
                 mean(trial) + sd(trial))
trial2 <- trial[trial > dist_limits[1] & trial < dist_limits[2]]
sm::sm.density(trial2)
shapiro.test(trial2)

# simulation scenario 1

set.seed(666)

extreme_dist_negative <- rnorm(30, 0, 1)
extreme_dist_positive <- rnorm(30, 10, 1)
toy_variable <- c(extreme_dist_negative, extreme_dist_positive)

dist_limits <- c(mean(extreme_dist_negative) - sd(extreme_dist_negative),
                 mean(extreme_dist_positive) + sd(extreme_dist_positive))

random_noise <- runif(30, dist_limits[1], dist_limits[2])

toy_variable <- c(toy_variable, random_noise)

for (i in 1:length(toy_variable)) {
  
  if (!(toy_variable[i] > dist_limits[1] & toy_variable[i] < dist_limits[2])) {
    
    if (toy_variable[i] < dist_limits[1]) {
      print(toy_variable[i])
      
      toy_variable[i] <- dist_limits[1] + abs(rnorm(1, 0, 0.01))
      
    } else {
      
      toy_variable[i] <- dist_limits[2] - abs(rnorm(1, 0, 0.01))
      
    }
    
  }
  
}

toy_variable <- c(toy_variable, dist_limits)

toy_variable <- (toy_variable - min(toy_variable)) / (max(toy_variable) - min(toy_variable))
toy_variable <- toy_variable[1:(length(toy_variable) - 2)]

toy_data <- toy_variable; for (i in 1:5) {
  
  toy_data <- cbind(toy_data, rnorm(length(toy_variable), sd = 100))
  
}

summary(toy_data)

GraphGMM::pca_biplot(as.data.frame(toy_data), scale = TRUE)

toy_data2 <- toy_data
toy_data2[,1] <- log(toy_data[,1] / (1 - toy_data[,1]))

summary(toy_data2[,1])

GraphGMM::pca_biplot(as.data.frame(toy_data2), scale = TRUE)

sm::sm.density(toy_variable)
sm::sm.density(toy_data2[,1])

bounded_pca <- prcomp(as.data.frame(toy_data), scale = TRUE)
nonbounded_pca <- prcomp(as.data.frame(toy_data2), scale = TRUE)

fviz_eig(bounded_pca)
fviz_eig(nonbounded_pca)

fviz_pca_var(bounded_pca)
fviz_pca_var(nonbounded_pca)

fviz_contrib(bounded_pca, choice = "var")
fviz_contrib(nonbounded_pca, choice = "var")

#

# non-linear variables ------------------------

# let's create a non-linear variable and see what happens...

# simple sin wave

x <- seq(0, 10, length = 500)
y <- sin(2 * x)

plot(x, y, type = "l")

# add some noise to make it interesting

y2 <- y + rnorm(length(y), sd = 0.5)

plot(x, y2, pch = 19, col = "grey")
lines(x, y, col = "red", lwd = 4)

y3 <- (7 * sin(2 * x) + (5 * x)) + rnorm(length(x), sd = 4)
plot(x, y3, pch = 19, col = "grey")
lines(x, (7 * sin(2 * x) + (5 * x)), col = "red", lwd = 4)

sin2x <- prcomp(cbind(x, y2))
plot(sin2x$x[,1], sin2x$x[,2], pch = 19)

sin2x_inclined <- prcomp(cbind(x, y3))
plot(sin2x_inclined$x[,1], sin2x_inclined$x[,2], pch = 19)





