
#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <sstream>
#include <string>
#include "simulation.h"

void circular_simulations(int n_simulations, int n_bootstrap, std::string model_comparison, int lower_bound, int upper_bound) {

	bool zero_cross;

	if (lower_bound < 0) {
		
		zero_cross = true;

	} else {

		zero_cross = false;

	}

	const int number_of_samples = 100;

	// Von Mises Parameters

	const double max_kappa = 20.0;
	const double mu = 0;

	// Cardioid Parameters

	const double max_rho_cardioid = 0.5;

	// Wrapped Cauchy Parameters

	const double max_rho = 0.8;

	// wrapped normal parameters

	const double max_rho_normal = 0.95;

	std::vector<double> lower_CI;
	std::vector<double> upper_CI;
	std::vector<double> median_CI;

	std::random_device rd;
	std::mt19937 gen(rd());


	for (int circ_value = lower_bound; circ_value <= upper_bound; ++circ_value) {

		int wrapped_angle = (circ_value + 360) % 360;

		double upper_bound = wrapped_angle;
		double lower_bound = 0.0;
		double kappa = 0.0 + (wrapped_angle / 360.0) * max_kappa;
		double rho = 0.0 + (wrapped_angle / 360.0) * max_rho;
		double rho_cardioid = 0.0 + (wrapped_angle / 360.0) * max_rho_cardioid;
		double rho_norm = 0.0 + (wrapped_angle / 380.0) * max_rho_normal;

		std::vector<double> error_values;

		std::vector<std::vector<double>> saved_samples;

		std::uniform_real_distribution<double> uniform_dist(lower_bound, upper_bound);

		for (int i = 0; i < n_simulations; ++i) {

			std::vector<double> x(number_of_samples);

			for (int j = 0; j < number_of_samples; ++j) {

				if (model_comparison == "Uniform") {

					x[j] = uniform_dist(gen);

				}
				else if (model_comparison == "Von Mises") {

					x[j] = sample_von_mises(mu, kappa, gen);

				}
				else if (model_comparison == "Wrapped Cauchy") {

					x[j] = sample_wrapped_cauchy(mu, rho, gen);

				}
				else if (model_comparison == "Wrapped Normal") {

					x[j] = wrapped_normal(mu, rho_norm, gen);

				}
				else if (model_comparison == "Cardioid") {

					x[j] = sample_cardioid(rho_cardioid, mu, gen);

				}
				else {

					std::cerr << "Invalid model_comparison!\n";

				}

			}

			double mean_x_value = mean_x(x);
			double mean_theta_value = mean_theta(x);
			error_values.push_back(circular_error(mean_x_value, mean_theta_value));

			if (model_comparison == "Uniform") {

				if (wrapped_angle == 90 || wrapped_angle == 180 || wrapped_angle == 270 || wrapped_angle == 360) {

					saved_samples.push_back(x);

				}

			}

		}

		if (model_comparison == "Uniform") {

			if (wrapped_angle == 90 || wrapped_angle == 180 || wrapped_angle == 270 || wrapped_angle == 360) {

				std::ostringstream filename;

				filename << "Angles_" << wrapped_angle << "_degrees_" << model_comparison << ".txt";

				std::ofstream angle_file(filename.str());

				if (angle_file.is_open()) {

					for (const auto& sample_set : saved_samples) {

						for (double angle : sample_set) {

							angle_file << angle << "\t";

						}

						angle_file << "\n";

					}

					angle_file.close();
					std::cout << "Saved Sampled Angles" << std::endl;

				}
				else {

					std::cerr << "Error opening file " << filename.str() << "!\n";

				}

			}

		}

		std::vector<double> bootstrap_errors;
		std::uniform_int_distribution<int> index_dist(0, n_simulations - 1);

		for (int i = 0; i < n_bootstrap; ++i) {

			std::vector<double> resampled_errors(n_simulations);

			for (int j = 0; j < number_of_samples; ++j) {

				int rand_index = index_dist(gen);
				resampled_errors[j] = error_values[rand_index];

			}

			bootstrap_errors.push_back(mean_x(resampled_errors));

		}

		lower_CI.push_back(quantile(bootstrap_errors, 0.025));
		upper_CI.push_back(quantile(bootstrap_errors, 0.975));
		median_CI.push_back(quantile(bootstrap_errors, 0.5));

		std::cout << "Itetation complete for " << circ_value << std::endl;

	}

	std::cout << "Saving File!" << std::endl;

	std::string output_name;

	if (zero_cross) {

		output_name = "simulation_results_zero_cross_";

	}
	else {

		output_name = "simulation_results_";

	}

	std::ofstream output_file(output_name + model_comparison + ".txt");

	if (output_file.is_open()) {

		double target_param = 0;

		output_file << "Parameter\tLower_CI\tUpper_CI\tMedian_CI\n";

		for (size_t i = 0; i < lower_CI.size(); ++i) {

			if (model_comparison == "Wrapped Cauchy") {

				target_param = (i + 1) / 360.0 * max_rho;

			}
			else if (model_comparison == "Von Mises") {

				target_param = (i + 1) / 360.0 * max_kappa;

			}
			else if (model_comparison == "Cardioid") {

				target_param = (i + 1) / 360.0 * max_rho_cardioid;

			}
			else if (model_comparison == "Wrapped Normal") {

				target_param = (i + 1) / 360.0 * max_rho_normal;

			}

			double parameter = (model_comparison == "Uniform") ? static_cast<double>(i + 1) : target_param;

			output_file << parameter << "\t" << lower_CI[i] << "\t" << upper_CI[i] << "\t" << median_CI[i] << "\n";

		}

		output_file.close();

	}
	else {

		std::cerr << "Error Opening Output File!\n";

	}

}
