#check if input prob is a valid probability value
check_prob <- function(prob){
  if(prob <= 0 | prob >= 1){
    stop('invalid prob value')
  }
  else{
    return(TRUE)
  }
}

#check if an input trials is a valid positive integer
check_trials <- function(trials){
  if(trials %% 1 == 0 & trials > 0){
    return(TRUE)
  }
  else{
    stop('invalid trials value')
  }
}

#check if an input success is a valid value for number of successes
check_success <- function(success, trials){
  if(success >= 0 & success <= trials){
    return(TRUE)
  }
  else{
    stop('invalid success value')
  }
}

