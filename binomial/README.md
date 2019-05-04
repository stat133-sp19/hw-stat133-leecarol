## Overview

"binomial" is an R package that provides functions to calculate values of a binomial random variable.

- bin_choose() computes the number of combinations in which k successes can occur in n trials
- bin_probability() computes the probability of getting a certain amount of successes in a number of trials with probability prob
- bin_distribution() creates a data frame with the probability distribution: successes in the first column, probability in the second column
- plot() method to create a barplot of a "binvar" object created by bin_distribution()
- bin_cumulative() creates a data frame with the probability distribution: successes in the first column, probability in the second column, cumulative probability in the third column
- plot() method to display a "bincum" object created by bin_cumulative()
- bin_variable() creates an object of class "binvar", which can be passed to other functions to calculate values
- bin_mean() to calculate mean
- bin_variance() to calculate variance
- bin_mode() to calculate mode
- bin_skewness() to calculate skewness
- bin_kurtosis() to calculate kurtosis

## Motivation

This package has been developed as part of a class project to learn and improve skill at the creation of an R project.
The binomial distribution is an important, commonly used distribution. This package allows the user to quickly and efficiently calculate values of a binomial random variable.

## Installation

Install the development version from GitHub via the package "devtools":

```{r}
# development version from GitHub:
#install.packages("devtools") 

# install "binomial" (without vignettes)
devtools::install_github("stat133-sp19/hw-stat133-leecarol/tree/master/binomial")

# install "binomial" (with vignettes)
devtools::install_github("stat133-sp19/hw-stat133-leecarol/tree/master/binomial", build_vignettes = TRUE)
```

## Usage


```{r}
library(binomial)

# Number of combinations of 2 out of 5
bin_choose(n = 5, k = 2)

# Calculate probability
bin_probability(success = 2, trials = 5, prob = 0.5)

# Calculate probability distribution
bin_distribution(trials = 5, prob = 0.5)

# Plot the distribution
plot(bin_distribution(trials = 5, prob = 0.5))

# Calculate cumulative probability distribution
bin_cumulative(trials = 5, prob = 0.5)

# Plot the cumulative distribution
plot(bin_cumulative(trials = 5, prob = 0.5))

# Create a binomial variable object
bin_variable(trials = 5, prob = 0.5)

# Get full summary description of a binomial variable
summary(bin_variable(trials = 5, prob = 0.5))

# Calculate values
bin_mean(5, 0.5)
bin_variance(5, 0.5)
bin_mode(5, 0.5)
bin_skewness(5, 0.5)
bin_kurtosis(5, 0.5)
```
