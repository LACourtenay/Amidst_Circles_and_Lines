# Amidst_Lines_and_Circles

This is where I will gradually provide the code and supporting materials for the seminar titled "Admist Lines and Circles"

For R, i used the following libraries;

* <b> sm </b> (v.2.2) - a really simple library to plot density plots quickly
* <b> sn </b> (v.2.1.1) - a library to sample from skewed-normal distributions
* <b> circular </b> (v.0.5) - for some of the circular statistics functions (although it can honestly be a bit annoying)
* <b> pValueRobust </b> (v.1) - my own library developed for a number of applications of my PhD, available from GitHub (see R manual to install)

I intend to include here code for circular statistics, but have not had time to clean it...

Notice in R code i bootstrap and only run some simulations 100 times, but in the real plots in C++ i ran both 10,000 times. R took too long though


Simulations included within this study were computed using the C++ programming language using the ISO C++14 Standard, and requires Boost.Math version 1.88.0 as a dependency. Compilation was done using the Microsoft Visual Studio 2022 (v.17.13.5) compiler.

