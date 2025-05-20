
# In the present code when landmark data is related we will remove the scaling
# of most applications for the caluclation of
# error/distances so that we have a metrically defined measurement of difference

library(GraphGMM)

# this is my own personal library available on GitHub
# to install this library you need rtools installed on your computer,
# and the devtools library to use install_github("https://github.com/LACourtenay/GraphGMM")

library(geomorph) # general GMM library
library(shapes) # general GMM library

#

# visualise extreme pits --------------

load_data <- function() {
  
  url1 <- "https://raw.githubusercontent.com/LACourtenay/Carnivore_Tooth_Pit_Classification/main/DataSets/Landmark_Carnivore_Data.txt"
  url2 <- "https://raw.githubusercontent.com/LACourtenay/VAE_MCMC_Pachycrocuta_Simulations/main/Courtenay_et_al_2021b_data.txt"
  
  if (!dir.exists(".\\data")) {
    dir.create(".\\data")
  }
  
  writeBin(httr::content(httr::GET(url1), "raw"), ".\\data\\Carnivore.txt")
  writeBin(httr::content(httr::GET(url2), "raw"), ".\\data\\Wolves.txt")
  
  first_dataset <- geomorph::read.morphologika(".\\data\\Carnivore.txt")
  second_dataset <- geomorph::read.morphologika(".\\data\\Wolves.txt")
  
  no_wolves_dataset <- first_dataset
  no_wolves_dataset$coords <- first_dataset$coords[,,first_dataset$labels != "Wolf"]
  no_wolves_dataset$labels <- first_dataset$labels[first_dataset$labels != "Wolf"]
  
  first_dataset <- no_wolves_dataset
  
  all_coordinates <- abind::abind(first_dataset$coords, second_dataset$coords, along = 3)
  all_labels <- c(first_dataset$labels, second_dataset$labels)
  
  graph_edges <- matrix(
    c(20,21,20,16,21,16,16,5,16,20,5,20,21,14,14,16,20,25,25,21,5,19,
      19,20,16,13,13,5,14,13,13,16,14,9,9,13,20,24,24,25,19,24,24,20,
      19,3,3,24,5,18,18,19,13,12,12,5,9,8,8,13,19,23,23,3,18,23,23,19,
      18,22,22,23,5,15,15,18,12,11,11,5,13,8,8,12,8,4,4,12,18,17,17,22,
      15,17,17,18,15,2,2,17,5,11,11,15,11,10,10,15,12,7,7,11,4,7,7,12,15,
      10,10,2,11,6,6,10,7,6,6,11,1,14,1,21),
    byrow = TRUE, ncol = 2
  )
  
  dataset <- list(
    coords = all_coordinates[c(1:7,9:15,17,19,21:27,29,30),,], # remove duplicate landmarks
    labels = as.factor(all_labels),
    wireframe = graph_edges
  )
  
  return(dataset)
  
}

tooth_pit_dataset <- load_data()

form_coordinates <- GPA(tooth_pit_dataset$coords, scale = FALSE)

pca <- pca_plot(vector_from_landmarks(form_coordinates$coordinates))

pca$pca_plot

negative_pc1 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,1], -5
)

negative_pc2 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,2], -8
)

positive_pc1 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,1], 20
)

positive_pc2 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,2], 8
)

plot_landmark_graph(
  positive_pc1, tooth_pit_dataset$wireframe,
  line_size = 2
)
plot_landmark_graph(
  negative_pc1, tooth_pit_dataset$wireframe,
  line_size = 2
)
plot_landmark_graph(
  positive_pc2, tooth_pit_dataset$wireframe,
  line_size = 2
)
plot_landmark_graph(
  negative_pc2, tooth_pit_dataset$wireframe,
  line_size = 2
)

#

# experimental linear regression -------------------------------

set.seed(666)

x <- seq(1, 100, length = 200)
y <- x + rnorm(length(x), sd = 10)

plot(x, y, pch = 19, xlim = c(0, 150), ylim = c(0, 150), col = "grey")

linear_model <- lm(y ~ x)
linear_coefficients <- linear_model$coefficients
linear_regression_line_x <- seq(1, 150)
linear_regression_line_y <- (linear_regression_line_x * linear_coefficients[2]) + linear_coefficients[1] 

lines(linear_regression_line_x, linear_regression_line_y, col = "red", lwd = 2)

new_point <- c(140, 100)
points(new_point[1], new_point[2], col = "black", pch = 4, cex = 2)

abline(h = predict(linear_model, newdata = data.frame(x = new_point[1], y = new_point[2])),
       lwd = 2)

#

# visualise extreme pits realistically --------------

pca$pca_plot

negative_pc2 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,2], -1
)

positive_pc2 <- morphological_predictor(
  form_coordinates$coordinates,
  pca$pc_scores[,2], 1
)

plot_landmark_graph(
  positive_pc2, tooth_pit_dataset$wireframe,
  line_size = 2
)
plot_landmark_graph(
  negative_pc2, tooth_pit_dataset$wireframe,
  line_size = 2
)

#

# o'higgins tests ------------------

data(apes)
procrustes_data <- GPA(apes$x, scale = FALSE)
procrustes_form_data <- GPA(apes$x, scale = FALSE)
apes$group <- gsub("^(gor|pan|pongo)[mf]$", "\\1", apes$group)

pca_plot(vector_from_landmarks(procrustes_data$coordinates), apes$group, CI_ellipse = TRUE)
pca_plot(vector_from_landmarks(procrustes_form_data$coordinates), apes$group, CI_ellipse = TRUE)

# please note that this is not the best simulation of the effects agnostophobia can have on
# TPS, in fact it is likely an infraestimation!
# Here we are using a linear regression to predict the morphology of certain individuals
# that are hidden during the definition of f(x), however, because both GPA and PCA have
# already been exposed to these individuals, there is still some signal contaminating
# the results. (for those interested, i suggest looking into what
# is known as contamination in training of models in data science, specifically 
# what happens when a "test set" contains information from the "training set").
# From this perspective, technically any results presented here
# would be much lower than what they really are (the errors should even be higher!)

# for those interested and motivated, this would be a pretty good moment to approach
# this question from a bayesian statistical perspective - predict the likely error
# given our encertainty that contamination is present!

pc_scores <- pca_plot(vector_from_landmarks(procrustes_form_data$coordinates))$pc_scores
error <- c()
sample <- c()

for (species in levels(as.factor(apes$group))) {
  
  leave_one_out_species <- procrustes_form_data$coordinates[,,apes$group == species]
  remaining <- procrustes_form_data$coordinates[,,apes$group != species]
  leave_one_out_species_pcs <- pc_scores[apes$group == species,]
  remaining_pcs <- pc_scores[apes$group != species,]
  
  for (individual in 1:dim(leave_one_out_species)[3]) {
    
    sample <- c(sample, species)
    
    target_individual <- morphological_predictor(
      remaining,
      remaining_pcs[,1:2],
      leave_one_out_species_pcs[individual,1:2]
    )
    
    original_prediction <- morphological_predictor(
      procrustes_form_data$coordinates,
      pc_scores[,1:2],
      leave_one_out_species_pcs[individual,1:2]
    )
    
    error <- c(error, sqrt(mean(target_individual - original_prediction)^2))
    
  }
  
}

boxplot(error ~ sample, horizontal = TRUE, xlab = "Error", ylab = "Species")


#
