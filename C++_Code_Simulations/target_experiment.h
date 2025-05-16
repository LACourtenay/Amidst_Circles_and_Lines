#pragma once

#include <string>

void circular_simulations(int n_simulations, int n_bootstrap, std::string model_comparison, int lower_bound, int upper_bound);

void anova_kruskal(
	int n_simulations, int n_bootstrap, std::string test_comparison, int lower_bound, int upper_bound, double increment_ammount,
	std::string distribution_type, double outlier_prob, double outlier_strength
);
