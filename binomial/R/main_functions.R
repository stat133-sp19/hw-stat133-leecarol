#' @title choose function
#' @description computes the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return the computed number of combinations
bin_choose <- function(n, k){
  if(k > n){
    stop("k cannot be greater than n")
  }
  else{
    return(factorial(n) / (factorial(k) * factorial(n - k)))
  }
}

#' @title probability function
#' @description computes the probability of getting a certain amount of successes in a number of trials with probability prob
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability
#' @return the computed probability
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success) * prob ^ success * (1 - prob) ^ (trials - success))
}

#' @title binomial distribution
#' @description creates a data frame with the probability distribution: successes in the first column, probability in the second column
#' @param trials number of trials
#' @param prob probability
#' @return data frame
bin_distribution <- function(trials, prob){
  success <- 0:trials
  probability <- bin_probability(0:trials, trials, prob)
  df <- data.frame(success = success, probability = probability)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

#' @export
plot.bindis <- function(bindis){
  plotted <- ggplot(bindis, aes(x = success, y = probability)) + geom_histogram(stat = 'identity') + xlab('Successes') + ylab('Probability') + ggtitle('Probability of Success in a Binomial Distribution')
  return(plotted)
}

#' @title cumulative binomial distribution
#' @description creates a data frame with the probability distribution: successes in the first column, probability in the second column, cumulative probability in the third column
#' @param trials number of trials
#' @param prob probability
#' @return data frame
bin_cumulative <- function(trials, prob){
  success <- 0:trials
  probability <- bin_probability(0:trials, trials, prob)
  cumulative <- cumsum(probability)
  df <- data.frame(success = 0:trials, probability = probability, cumulative = cumulative)
  class(df) <- c('bincum', 'data.frame')
  return(df)
}

#'@export
plot.bincum <- function(bincum){
  plotted <- ggplot(bincum, aes(x = success, y = cumulative)) + geom_point() + geom_line() + xlab('successes') + ylab('probability')
  return(plotted)
}

#' @title binomial random variable object
#' @description creates an object of class "binvar"
#' @param trials number of trials
#' @param prob probability
#' @return "binvar" object
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  varlist <- list(trials = trials, prob = prob)
  return(varlist)
}

#' @export
print.binvar <- function(binvar){
  cat('"Binomial variable"')
  cat('\n\n')
  cat('Parameters')
  cat('\n')
  cat('- number of trials : ', binvar$trials)
  cat('\n')
  cat('- prob of success : ', binvar$prob)
}

#' @export
summary.binvar <- function(binvar){
  trials <- binvar$trials
  prob <- binvar$prob
  mean <- aux_mean(trials, prob)
  variance <- aux_variance(trials, prob)
  mode <- aux_mode(trials, prob)
  skewness <- aux_skewness(trials, prob)
  kurtosis <- aux_kurtosis(trials, prob)
  return(list(trials = trials, prob = prob, mean = mean, variance = variance, mode = mode, skewness = skewness, kurtosis = kurtosis))
}

#' @export
print.summary.binvar <- function(binvar){
  summary <- summary.binvar(binvar)
  cat('"Summary Binomial"')
  cat('\n\n')
  cat('Parameters')
  cat('\n')
  cat('- number of trials: ', summary$trials)
  cat('\n')
  cat('- prob of success : ', summary$prob)
  cat('\n\n')
  cat('Measures')
  cat('\n')
  cat('- mean    :', summary$mean)
  cat('\n')
  cat('- variance:', summary$variance)
  cat('\n')
  cat('- mode    :', summary$mode)
  cat('\n')
  cat('- skewness:', summary$skewness)
  cat('\n')
  cat('- kurtosis:', summary$kurtosis)
}

#' @title mean of a binomial
#' @description calculates the mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return calculated mean
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mean <- aux_mean(trials, prob)
  return(mean)
}

#' @title variance of a binomial
#' @description calculates the variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return calculated variance
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  variance <- aux_variance(trials, prob)
  return(variance)
}

#' @title mode of a binomial
#' @description calculates the mode of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return calculated mode
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mode <- aux_mode(trials, prob)
  return(mode)
}

#' @title skewness of a binomial
#' @description calculates the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return calculated skewness
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  skewness <- aux_skewness(trials, prob)
  return(skewness)
}

#' @title kurtosis of a binomial
#' @description calculates the kurtosis of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return calculated kurtosis
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  kurtosis <- aux_kurtosis(trials, prob)
  return(mean)
}
