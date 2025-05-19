
#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <sstream>
#include <string>
#include "target_experiment.h"
#include <boost/math/distributions/normal.hpp>

std::string model_comparison = "Uniform"; // select from 'Von Mises', 'Uniform', 'Wrapped Cauchy', 'Wrapped Normal' or 'Cardioid'
std::string test_comparison = "ANOVA"; // Choose either ANOVA or Kruskal-Wallis
std::string simulation_model = "AKW"; // select whether the simulation is for the angle experiment (Circular) or ANOVA vs Kruskal Wallis (AKW)
std::string distribution = "Skew"; // Gauss or Skew

int main() {

	const int n_simulations = 10000;
	const int n_bootstrap = 10000;

	if (simulation_model == "Circular") {

		std::cout << "Running Circular Simulations" << std::endl;

		circular_simulations(n_simulations, n_bootstrap, model_comparison, 0, 360);

	} else if (simulation_model == "AKW") {

		std::cout << "Running ANOVA vs Kruskal-Wallis Simulations" << std::endl;

		anova_kruskal(
			n_simulations, n_bootstrap, test_comparison, 0, 1, 0.025, distribution, 0.5, 10
		);
	
	} else {

		std::cout << "Incorrect Model Chosen" << std::endl;

	}

	std::cout << "Simulation Complete!" << std::endl;

	return 0;

}

