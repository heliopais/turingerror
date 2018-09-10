## quiets concerns of R CMD check re: the .'s that appear in pipelines
## taken from https://github.com/STAT545-UBC/Discussion/issues/451
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "groups"))

#' Combine low observation items in binomial dataset
#'
#' @param d data.frame
#' @param trial_column column name with number of trials
#' @param success_column column name with number of successes
#' @param estimate_columns column names with conversion estimates
#' @param use_groups use 'groups' column to combine itemsg
#'
#' @return combined data.frame
#' @export
#'
#' @import data.table
combine_all_binomial <- function(
  d,
  trial_column,
  success_column,
  estimate_columns,
  use_groups = FALSE){


  if(use_groups){
    r_sum = d[, lapply(.SD, sum), .SDcols = c(trial_column, success_column), by = "groups"]


    weight_avg_part1 = function(w){ w * as.numeric(t(d[,trial_column,with=F]))}
    r_aux = cbind(d[,.(groups)], d[, lapply(.SD, weight_avg_part1), .SDcols = estimate_columns])
    r_aux

    r_bux = r_aux[, lapply(.SD, sum), .SDcols = estimate_columns, by = groups]

    weight_avg_part2 = function(w){ w / as.numeric(t(r_sum[,trial_column, with=F]))}
    r_avg = r_bux[, lapply(.SD, weight_avg_part2), .SDcols = estimate_columns]


  } else {
    obs_weighted_mean = function(w){stats::weighted.mean(w, as.numeric(t(d[,trial_column,with=F])))}
    r_sum = d[, lapply(.SD, sum), .SDcols = c(trial_column, success_column)]
    r_avg = d[, lapply(.SD, obs_weighted_mean), .SDcols = estimate_columns]
  }
  return(cbind(r_sum, r_avg))
}



find_item_groups <- function(d, trial_column, min_observations){
  r = cumsum(d[,trial_column, with=F])
  names(r) = 'groups'
  return(r[,.(groups = groups %/% min_observations)])
}




get_combined_observations_binomial <- function(
  d,
  trial_column,
  success_column,
  estimate_columns,
  min_observations,
  shuffle = TRUE){

  if (! 'data.frame' %in% class(d)){ stop('First argument should be data.frame') }
  if (! 'data.table' %in% class(d)){ d = data.table(d)}

  dlow = d[as.numeric(t(d[,trial_column, with=F])) < min_observations]
  dhigh = d[as.numeric(t(d[,trial_column, with=F])) >= min_observations]

  if (shuffle){
    #random shuffling of rows
    dlow_shuffled = dlow[sample(nrow(dlow), nrow(dlow), replace=F),]
  }else {
    dlow_shuffled = dlow
  }


  dlow_shuffled[, groups := find_item_groups(dlow_shuffled, trial_column, min_observations)]
  dlow_grouped = combine_all_binomial(dlow_shuffled, trial_column, success_column, estimate_columns, use_groups = TRUE)

  return (rbind(
    dhigh[,c(trial_column, success_column, estimate_columns),with=F],
    dlow_grouped[,c(trial_column, success_column, estimate_columns),with=F]
  ))
}



get_combined_observations_continuous <- function(
  d,
  weight_column,
  obs_column,
  estimate_columns,
  min_weight){

  dlow = d[d[,weight_column] < min_weight,]
  dhigh = d[d[,weight_column] >= min_weight,]

  #random shuffling of rows
  dlow_shuffled = dlow[sample(nrow(dlow), nrow(dlow), replace=F),]

  r = NULL
  curr_sum_weight = 0
  start_idx = 1
  for(i in 1:nrow(dlow)){

    curr_sum_weight = curr_sum_weight + dlow_shuffled[i, weight_column]

    if (curr_sum_weight > min_weight){
      complete_set = dlow_shuffled[start_idx:i,]

      agg = dlow[1,]
      # 'template' data.frame

      agg[,weight_column] = sum(complete_set[,weight_column])
      for(curr_column in c(obs_column, estimate_columns)){
        agg[, curr_column] = sum(complete_set[,weight_column] * complete_set[,curr_column])/sum(complete_set[,weight_column])
      }
      r = rbind(r, agg)

      start_idx = i + 1
      curr_sum_weight = 0
    }
    # note: it is possible that the last set of rows is not included in result
    #       (in the case where this set of rows is 'incomplete' - its sum is below min_weight)
  }
  return (rbind(dhigh, r))
}


