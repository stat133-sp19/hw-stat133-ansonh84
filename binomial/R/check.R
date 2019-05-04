#checks probability value
check_prob <- function(prob) {
  if (length(prob) > 1) {
    stop("\n 'prob' must be length 1")
  }
  if (prob < 0 | prob > 1) {
    stop("\n 'prob' must be between 0 and 1")
  }

  TRUE
}

#check trials value
check_trials <- function(trials) {
  if (length(trials) > 1) {
    stop("\n 'trials' must be length 1")
  }
  if (trials < 0) {
    stop("\n 'trials' must be a value > 0")
  }
  TRUE
}

#check success values
check_success <- function(success, trials) {
  for (i in 1:length(success)) {
    if (success[i] > trials) {
      stop("\n 'success' must be a value < 'trials'")
    }
    if (success[i] < 0) {
      stop("\n 'success' must be a vector of values >= 0")
    }
  }
  TRUE
}
