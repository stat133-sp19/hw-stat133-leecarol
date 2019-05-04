#return the computed mean
aux_mean <- function(trials, prob){
  return(trials * prob)
}
#return the computed variance
aux_variance <- function(trials, prob){
  return(trials * prob * (1 - prob))
}
#return the computed mode
aux_mode <- function(trials, prob){
  if((trials * prob + prob) %% 1 == 0){
    return(c(trials * prob + prob - 1, trials * prob + prob))
  }
  else{
    return((trials * prob + prob) - (trials * prob + prob) %% 1)
  }
}
#return the computed skewness
aux_skewness <- function(trials, prob){
  return((1 - (2 * prob)) / sqrt(aux_variance(trials, prob)))
}
#return the computed kurtosis
aux_kurtosis <- function(trials, prob){
  return((1 - (6 * prob * (1 - prob))) / aux_variance(trials, prob))
}
