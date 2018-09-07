




#' Evaluator for continuous quantities
#'
#' @param observed1 vector with values of observed quantities for sample 1
#' @param estimates1 vector with values of estimated quantities for sample 1
#' @param observed2 vector with values of observed quantities for sample 2
#' @param estimates2 vector with values of estimated quantities for sample 2
#'
#' @return percent of correct guesses
#' @export
#'
#' @examples
#' obs1 <- c(100, 180)
#' obs2 <- c(150, 150)
#' est1 <- c(90, 160)
#' est2 <- c(120, 170)
#' continuous_evaluator(obs1, est1, obs2, est2)
continuous_evaluator <- function(observed1, estimates1, observed2, estimates2){
  correct_answer = ifelse(observed1 > observed2, 1, 2)

  guessed_answer = ifelse(estimates1 > estimates2, 1, 2)

  return ( sum(guessed_answer == correct_answer)/length(correct_answer) )
}


#' Evaluator for binomial tests
#'
#' @param trials1 vector with number of trials for sample 1 (with bookings)
#' @param estimates1 vector with estimated binomial probabilities for sample 1 (with bookings)
#' @param trials2 vector with number of trials for sample 2 (without bookings)
#' @param estimates2 vector with estimated binomial probabilities for sample 2 (without bookings)
#'
#' @return fraction of correct guesses
#' @export
#'
#' @examples
#' trials1 = c(10,10)
#' estimates1 = c(0.5, 0.5)
#' trials2 = c(10,10)
#' estimates2 = c(0.4, 0.6)
#' binomial_evaluator(trials1, estimates1, trials2, estimates2)
binomial_evaluator <- function(trials1, estimates1,
                               trials2, estimates2){

  if (any(estimates1 > 1) | any(estimates2 > 1) |
      any(estimates1 < 0) | any(estimates2 < 0)){
    stop('Conversion estimates must be between 0 and 1')
  }

  if (any(trials1 <= 0) | any(trials2 <= 0)){
    stop('Trials must be greater than 0')
  }

  correct_answer = 1

  nobookings1 = dbinom(0, trials1, estimates1)
  bookings1 = 1 - dbinom(0, trials1, estimates1)

  nobookings2 = dbinom(0, trials2, estimates2)
  bookings2 = 1 - dbinom(0, trials2, estimates2)

  nobookings1_and_bookings2 = nobookings1 * bookings2
  nobookings2_and_bookings1 = nobookings2 * bookings1

  guessed_answer = ifelse(nobookings2_and_bookings1 > nobookings1_and_bookings2, 1,
                   ifelse(nobookings2_and_bookings1 < nobookings1_and_bookings2, 2,
                   # final case: tie - randomly guess
                   ifelse(runif(length(trials1)) < 0.5, 1, 2)
                   ))

  return ( sum(guessed_answer == correct_answer)/length(guessed_answer) )
}
