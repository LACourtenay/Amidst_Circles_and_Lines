
#include "simulation.h"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <vector>
#include <sstream>
#include <random>
#include <boost/math/distributions/fisher_f.hpp>
#include <boost/math/distributions/chi_squared.hpp>

double mean_x(const std::vector<double>& x) {

	return std::accumulate(
		x.begin(), x.end(), 0.0
	) / x.size();

}

double mean_theta(const std::vector<double>& x) {

	double sum_x = 0.0;
	double sum_y = 0.0;

	for (double angle : x) {
		sum_x += std::cos(angle);
		sum_y += std::sin(angle);
	}

	double r = std::sqrt(sum_x * sum_x + sum_y * sum_y);

	return std::atan2(sum_y / r, sum_x / r);

}

double to_radians(double degrees) {
	
	return degrees * (pi / 180.0);

}

double to_degrees(double radians) {

	return radians * (180.0 / pi);

}

double rmse(const std::vector<double>& y, const std::vector<double>& y_pred) {

	double sum_sq = 0.0;
	size_t n = y.size();

	for (size_t i = 0; i < n; i++) {

		double diff = y[i] - y_pred[i];
		sum_sq += diff * diff;

	}

	return std::sqrt(sum_sq / n);

}

double quantile(std::vector<double>& data, double q) {

	size_t index = static_cast<size_t>(q * data.size());
	std::nth_element(data.begin(), data.begin() + index, data.end());
	return data[index];

}

double circular_error(double x, double y) {

	//double diff = fabs(x - y);
	//return std::min(diff, 360 - diff);

	double delta_theta = std::fmod(std::abs(x - y), 360.0);
	if (delta_theta > 180.0) {
		delta_theta = 360 - delta_theta;
	}

	return delta_theta;

}

double sample_von_mises(double mu, double kappa, std::mt19937& gen) {

	if (kappa < 1e-6) {

		std::uniform_real_distribution<double> uniform_dist(-pi, pi);
		return uniform_dist(gen) + mu;

	}

	std::uniform_real_distribution<double> uniform_dist(0.0, 1.0);
	std::normal_distribution<double> normal_dist(0.0, 1.0);

	double a = 1.0 + std::sqrt(1.0 + 4.0 * kappa * kappa);
	double b = (a - std::sqrt(2.0 * a)) / (2.0 * kappa);
	double r = (1.0 + b * b) / (2.0 * b);

	double theta;

	while (true) {

		double u1 = uniform_dist(gen);
		double z = std::cos(pi * u1);
		double f = (1.0 + r * z) / (r + z);
		double c = kappa * (r - f);

		double u2 = uniform_dist(gen);

		if (u2 < c * (2.0 - c) || u2 <= std::exp(c - 1.0)) {

			theta = std::acos(f);
			break;

		}

	}

	if (uniform_dist(gen) > 0.5) {
		
		theta = -theta;

	}

	return mu + theta;

}

double sample_wrapped_cauchy(double mu, double rho, std::mt19937& gen) {

	if (rho < 0.0 || rho >= 1.0) {

		std::cerr << "Invald rho value.\n";
		return 0;

	}

	std::uniform_real_distribution<double> uniform_dist(0.0, 1.0);
	double U = uniform_dist(gen);

	double cauchy_sample = std::tan(pi * (U - 0.5));

	double wrapped_cauchy_sample = mu + 2 * std::atan(cauchy_sample / rho);
	wrapped_cauchy_sample = std::fmod(wrapped_cauchy_sample + pi, 2 * pi);

	return wrapped_cauchy_sample;

}

double wrapped_normal(double mu, double rho, std::mt19937& gen) {
	
	// \sigma = \sqrt{-2 \log \rho}

	double sigma = std::sqrt(-2 * std::log(rho));

	std::normal_distribution<double> normal_dist(mu, sigma);
	double theta = normal_dist(gen);
	theta = std::fmod(theta, 2 * pi);

	if (theta < 0) {
		theta += 2 * pi;
	}

	return theta;

}

double sample_cardioid(double rho, double mu, std::mt19937& gen) {

	std::uniform_real_distribution<double> uniform_dist(0.0, 1.0);
	std::uniform_real_distribution<double> angle_dist(0.0, 2 * pi);

	double theta;
	double u;
	double pdf_value;

	do {

		theta = angle_dist(gen);
		u = uniform_dist(gen) * (1 + 2 * rho);

		pdf_value = (1 + 2 * rho * cos(theta - mu)) / (2 * pi);

	} while (u > pdf_value);

	return theta;

}

double one_way_anova(const std::vector<std::vector<double>>& groups) {

	int k = groups.size();
	int N = 0;
	double grand_total = 0.0;

	for (const auto& group : groups) {

		for (double val : group) {

			grand_total += val;
			N++;

		}

	}
	
	double grand_mean = grand_total / N;

	double ssb = 0.0; // SSB = inter group variability

	for (const auto& group : groups) {

		double group_mean = mean_x(group);
		ssb += group.size() * std::pow(group_mean - grand_mean, 2);

	}

	double ssw = 0.0; // ssw = intra group variability

	for (const auto& group : groups) {

		double group_mean = mean_x(group);

		for (double val : group) {

			ssw += std::pow(val - group_mean, 2);

		}

	}

	double msb = ssb / (k - 1);
	double msw = ssw / (N - k);

	double F = msb / msw;

	return F;

}

double anova_p(double F, int df1, int df2) {

	boost::math::fisher_f dist(df1, df2);
	double p = 1 - cdf(dist, F);

	return p;

}

struct RankedValue {

	double value;
	int group;
	double rank;

};

double kruskal_wallis(const std::vector<std::vector<double>>& groups) {

	std::vector<RankedValue> all_values;

	int group_index = 0;

	for (const auto& group : groups) {

		for (double v : group) {

			all_values.push_back({ v, group_index, 0.0 });

		}

		++group_index;

	}

	std::sort(all_values.begin(), all_values.end(), [](auto& a, auto& b) {

		return a.value < b.value;

		});

	int n = all_values.size();

	for (int i = 0; i < n;) {

		int j = i + 1;
		while (j < n && all_values[j].value == all_values[i].value) ++j;

		double avg_rank = (i + 1 + j) / 2.0;

		for (int k = i; k < j; ++k) {
			all_values[k].rank = avg_rank;
		}

		i = j;

	}

	int k = groups.size();

	std::vector<double> rank_sums(k, 0.0);
	std::vector<int> group_sizes(k, 0);

	for (const auto& item : all_values) {

		rank_sums[item.group] += item.rank;
		group_sizes[item.group]++;

	}

	double N = n;
	double H = 0.0;

	for (int i = 0; i < k; i++) {

		H += (rank_sums[i] * rank_sums[i]) / group_sizes[i];

	}

	H = (12.0 / (N * (N + 1))) * H - 3 * (N + 1);

	return H;
		 
}

double kruskal_p(double H, int df) {

	boost::math::chi_squared dist(df);
	double p = 1 - cdf(dist, H);

	return p;

}

double shapiro_wilk(const std::vector<double>& data) {

	int n = data.size();

	std::vector<double> sorted_data = data;
	std::sort(sorted_data.begin(), sorted_data.end());

	double mean = mean_x(data);

	std::vector<double> a(n, 1.0);

	double sum_of_weighted_order_stats = 0.0;

	for (int i = 0; i < n; ++i) {

		sum_of_weighted_order_stats += a[i] * sorted_data[i];

	}

	double sum_of_squares = 0.0;

	for (double value : data) {

		sum_of_squares += (value - mean) * (value - mean);

	}

	double W = (sum_of_weighted_order_stats * sum_of_weighted_order_stats) / sum_of_squares;

	return W;

}

double shapiro_p(const std::vector<double> data, int num_simulations) {

	int n = data.size();

	double observed_w = shapiro_wilk(data);

	std::random_device rd;
	std::mt19937 gen(rd());
	std::normal_distribution<> d(0, 1);

	int count = 0;
	for (int i = 0; i < num_simulations; ++i) {

		std::vector<double> simulated_data(n);

		for (int j = 0; j < n; ++j) {

			simulated_data[j] = d(gen);

		}

		double simulated_w = shapiro_wilk(simulated_data);

		if (simulated_w <= observed_w) {

			++count;

		}

	}

	double p = static_cast<double>(count) / num_simulations;

	return p;

}

std::vector<double> skewed_normal(int n, double mu,	double sigma, double outlier_prob, double outlier_strength) {

	std::vector<double> result;
	std::mt19937 gen(std::random_device{}());
	std::normal_distribution<> normal_dist(mu, sigma);
	std::uniform_real_distribution<> uniform_dist(0.0, 1.0);

	for (int i = 0; i < n; ++i) {

		double val = normal_dist(gen);

		if (uniform_dist(gen) < outlier_prob) {

			val += outlier_strength * sigma * uniform_dist(gen);

		}

		result.push_back(val);

	}

	return result;

}