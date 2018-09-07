#' Generate samples of pairs to be tested
#'
#' @param d data frame with conversion data
#' @param trial_column column name with number of trials
#' @param success_column column name with number of successes
#' @param estimate_columns column names with conversion estimates
#' @param min_observations minimum number of observations per item
#' @param combine_low_volume_items flag indicating whether items that have less observations than minimum threshold should be combined
#' @param min_sample_size minimum number of random samples (and tests) generated
#'
#' @return list with two data frames, a sample of converted elements, and a sample (of the same size) with non converted elements
#' @export
binomial_systematic_generator <- function(
  d,
  trial_column,
  success_column,
  estimate_columns,
  min_observations = 5,
  combine_low_volume_items = T,
  min_sample_size = 1000){

  converted = d[as.numeric(t(d[,success_column, with=F])) > 0]
  not_converted = d[as.numeric(t(d[,success_column, with=F])) == 0]

  if (nrow(converted) == 0){
    warning('there are no converted items')
    return(NULL)
  }
  if (nrow(not_converted) == 0){
    warning('all items are converted')
    return(NULL)
  }

  number_loops = ceiling(min_sample_size/nrow(converted))

  sample_size_absolute_rounded  = number_loops * nrow(converted)


  if (combine_low_volume_items){
    sample_converted = NULL
    sample_not_converted = NULL
    for (i in 1:number_loops){
      converted_aux = get_combined_observations_binomial(converted, trial_column, success_column, estimate_columns, min_observations)

      not_converted_aux = get_combined_observations_binomial(not_converted, trial_column, success_column, estimate_columns, min_observations)
      not_converted_curr_sample = not_converted_aux[sample(nrow(not_converted_aux),
                                                   nrow(converted_aux),
                                                   replace=T),]

      sample_converted = rbind(sample_converted, converted_aux)
      sample_not_converted = rbind(sample_not_converted, not_converted_curr_sample)
    }
  } else {
    sample_converted = do.call("rbind", replicate(number_loops,converted,simplify = F))
    sample_not_converted = not_converted[sample(nrow(not_converted),
                                         sample_size_absolute_rounded,
                                         replace=T),]
  }


  return(list(
    sample_converted,
    sample_not_converted))
}
