#' Probabilistic Turing Error for conversion data
#'
#' @param d data frame with conversion data
#' @param trial_column column name with number of trials
#' @param success_column column name with number of successes
#' @param estimate_columns column names with conversion estimates
#' @param min_observations minimum number of observations per item
#' @param combine_low_volume_items flag indicating whether items that have less observations than minimum threshold should be combined
#' @param min_sample_size minimum number of random samples (and tests) generated
#'
#' @return probabilistic turing error of conversion data
#' @export
conversion_turing_error <- function(
  d,
  trial_column,
  success_column,
  estimate_columns,
  min_observations = 5,
  combine_low_volume_items = T,
  min_sample_size = 1000){

  if (! 'data.frame' %in% class(d)){
    stop('First argument must be data.frame')
  }
  if (! 'data.table' %in% class(d)){
    d = as.data.table(d)
  }

  sample = binomial_systematic_generator(
    d,
    trial_column,
    success_column,
    estimate_columns,
    min_observations,
    combine_low_volume_items,
    min_sample_size)


  sample_converted = sample[[1]]
  sample_not_converted = sample[[2]]

  r = NULL
  for (estimate in estimate_columns){
    guesses_correct = binomial_evaluator(
      as.numeric(t(sample_converted[,trial_column, with=F])),
      as.numeric(t(sample_converted[,estimate, with=F])),
      as.numeric(t(sample_not_converted[,trial_column, with=F])),
      as.numeric(t(sample_not_converted[,estimate, with=F]))
    )
    r = rbind(r, data.table(estimate = estimate, turingerror = 1 - guesses_correct))
  }
  return(r)
}

#' Probabilistic Turing Error for continuous data
#'
#' @param d data frame with data
#' @param weight_column name of the column with weights per item
#' @param obs_column name of column with observed (continuous) values
#' @param estimate_columns names of the columns that have predicted values
#' @param min_weight minimum weight per item
#' @param min_sample_size minimum number of random samples (and tests) generated
#'
#' @return turing error
#' @export
continuous_turing_error <- function(
  d,
  weight_column,
  obs_column,
  estimate_columns,
  min_weight = NULL,
  min_sample_size = 1000){
  if (! 'data.frame' %in% class(d)){
    stop('First argument must be data.frame')
  }
  if (! 'data.table' %in% class(d)){
    x = as.data.table(d)
  }

  sample1 = d[sample(nrow(d), min_sample_size, replace=T)]
  sample2 = d[sample(nrow(d), min_sample_size, replace=T)]

  r = NULL
  for (estimate in estimate_columns){
    guesses_correct = continuous_evaluator(
      sample1[,obs_column, with=F],
      sample1[,estimate, with=F],
      sample2[,obs_column, with=F],
      sample2[,estimate, with=F]
    )
    r = rbind(r, data.table(estimate = estimate, turing_error = 1-guesses_correct))
  }
  return(r)
}
