
#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <sstream>
#include <string>
#include <iomanip>
#include "simulation.h"

double round_two(double value) {
	return std::round(value * 100) / 100;
}

std::string to_string_fixed(double value, int decimals = 2) {

	std::ostringstream out;
	out << std::fixed << std::setprecision(decimals) << value;
	std::string s = out.str();
	std::replace(s.begin(), s.end(), '.', '_');
	return s;

}

void anova_kruskal(
	int n_simulations, int n_bootstrap, std::string test_comparison, int lower_bound, int upper_bound, double increment_ammount,
	std::string distribution_type, double outlier_prob, double outlier_strength
) {

	const int number_of_samples = 48; // sample size required to detect differences when mean2 reaches approximately 0.4

	if (distribution_type == "Skew") {

		std::cout << "Performing preliminary Shapiro-Wilks calculations" << std::endl;

		std::vector<double> w_values;
		std::vector<double> p_values;

		for (int i = 1; i < n_simulations; ++i) {

			std::vector<double> trial_distribution = skewed_normal(number_of_samples, 0.0, 1.0, outlier_prob, outlier_strength);
			
			w_values.push_back(shapiro_wilk(trial_distribution));
			p_values.push_back(shapiro_p(trial_distribution));

		}

		std::ofstream output_file(
			"Shapiro_Wilks_results_" + to_string_fixed(outlier_prob) + "_" + to_string_fixed(outlier_strength, 0) + ".txt"
		);

		if (output_file.is_open()) {

			output_file << "W\tp\n";
		
			for (size_t i = 0; i < w_values.size(); ++i) {

				output_file << w_values[i] << "\t" << p_values[i] << "\n";

			}

			output_file.close();

		}

		std::cout << "End of Shapiro-Wilks calculations" << std::endl;

	}


	std::vector<double> mean2_values;

	std::vector<double> lower_CI_p;
	std::vector<double> upper_CI_p;
	std::vector<double> median_CI_p;

	std::vector<double> lower_CI_stat;
	std::vector<double> upper_CI_stat;
	std::vector<double> median_CI_stat;

	std::random_device rd;
	std::mt19937 gen(rd());

	for (double mean2 = lower_bound; mean2 <= upper_bound; mean2 += increment_ammount) {

		std::normal_distribution<> dist1(0.0, 1.0);
		std::normal_distribution<> dist2(mean2, 1.0);

		mean2_values.push_back(mean2);

		std::vector<double> stat_vals;
		std::vector<double> p_vals;

		for (int i = 0; i < n_simulations; i++) {

			std::vector<double> group1(number_of_samples);
			std::vector<double> group2;

			for (int j = 0; j < number_of_samples; ++j) {

				group1[j] = dist1(gen);

			}

			if (distribution_type == "Skew") {

				group2 = skewed_normal(number_of_samples, mean2, 1.0, outlier_prob, outlier_strength);

			} else if (distribution_type == "Gauss") {

				group2.resize(number_of_samples);
				for (int j = 0; j < number_of_samples; ++j) {

					group2[j] = dist2(gen);

				}

			}

			std::vector<std::vector<double>> groups = { group1, group2 };

			double test_statistic;
			double p;

			if (test_comparison == "ANOVA") {

				test_statistic = one_way_anova(groups);
				p = anova_p(test_statistic, 1, 2 * number_of_samples - 2);

			}
			else if (test_comparison == "Kruskal-Wallis") {

				test_statistic = kruskal_wallis(groups);
				p = kruskal_p(test_statistic, 1);

			}
			else {

				std::cout << "The selected test is not an option" << std::endl;

			}

			stat_vals.push_back(test_statistic);
			p_vals.push_back(p);

		}

		std::vector<double> bootstrap_stat;
		std::vector<double> bootstrap_p;

		std::uniform_int_distribution<int> index_dist(0, n_simulations - 1);

		for (int i = 0; i < n_bootstrap; ++i) {

			std::vector<double> resampled_p(n_simulations);
			std::vector<double> resampled_stat(n_simulations);

			for (int j = 0; j < n_simulations; ++j) {

				int rand_index = index_dist(gen);
				resampled_p[j] = p_vals[rand_index];
				resampled_stat[j] = stat_vals[rand_index];

			}

			bootstrap_stat.push_back(quantile(resampled_stat, 0.5));
			bootstrap_p.push_back(quantile(resampled_p, 0.5));

		}

		lower_CI_p.push_back(quantile(bootstrap_p, 0.025));
		upper_CI_p.push_back(quantile(bootstrap_p, 0.975));
		median_CI_p.push_back(quantile(bootstrap_p, 0.5));

		lower_CI_stat.push_back(quantile(bootstrap_stat, 0.025));
		upper_CI_stat.push_back(quantile(bootstrap_stat, 0.975));
		median_CI_stat.push_back(quantile(bootstrap_stat, 0.5));

		std::cout << "Iteration complete for mean value " << mean2 << std::endl;

	}

	std::cout << "Saving File!" << std::endl;

	std::string output_name;

	if (distribution_type == "Gauss") {

		output_name = "simulation_results_" + test_comparison + "_" + distribution_type + ".txt";

	}
	else {

		output_name = "simulation_results_" + test_comparison + "_" + distribution_type + "_" +
			to_string_fixed(outlier_prob) + "_" + to_string_fixed(outlier_strength, 0) + ".txt";

	}	

	std::ofstream output_file(output_name);

	if (output_file.is_open()) {

		output_file << "Parameter\tLower_CI_stat\tMedian_CI_stat\tUpper_CI_stat\tLower_CI_p\tMedian_CI_p\tUpper_CI_p\n";

		for (size_t i = 0; i < mean2_values.size(); ++i) {

			output_file
				<< mean2_values[i] << "\t"
				<< lower_CI_stat[i] << "\t"
				<< median_CI_stat[i] << "\t"
				<< upper_CI_stat[i] << "\t"
				<< lower_CI_p[i] << "\t"
				<< median_CI_p[i] << "\t"
				<< upper_CI_p[i] << "\n";

		}

		output_file.close();


	}
	else {

		std::cerr << "Could not open file" << std::endl;

	}



}
