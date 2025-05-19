#pragma once

#include <vector>
#include <random>

const double pi = 3.141592653589793;

double mean_x(const std::vector<double>& x);

double mean_theta(const std::vector<double>& x);

double to_radians(double degrees);

double to_degrees(double radians);

double rmse(const std::vector<double>& y, const std::vector<double>& y_pred);

double quantile(std::vector<double>& data, double q);

double circular_error(double x, double y);

double sample_von_mises(double mu, double kappa, std::mt19937& gen);

double sample_wrapped_cauchy(double mu, double rho, std::mt19937& gen);

double wrapped_normal(double mu, double rho, std::mt19937& gen);

double sample_cardioid(double rho, double mu, std::mt19937& gen);

double one_way_anova(const std::vector<std::vector<double>>& groups);

double anova_p(double F, int df1, int df2);

double kruskal_wallis(const std::vector<std::vector<double>>& groups);

double kruskal_p(double H, int df);

std::vector<double> skewed_normal(
	int n,
	double mu = 0.0,
	double sigma = 1.0,
	double outlier_prob = 0.05,
	double outlier_strength = 5.0
);

double shapiro_p(const std::vector<double> data, int num_simulations = 10000);

double shapiro_wilk(const std::vector<double>& data);