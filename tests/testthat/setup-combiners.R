setup({
  library(data.table)
  d = data.table(
    observations = c(1,2,3),
    conversions = c(0,0,1),
    estimate1 = c(0.1,0.1,0.1),
    estimate2 = c(0,0.6,0.1)
  )

  d2 = data.table(
    observations = c(1,4,3,2),
    conversions = c(0,0,1,1),
    estimate1 = c(0.1,0.1,0.1,0.1),
    estimate2 = c(0.2,0.2,0.3,0.3)
  )

  d3 = data.table(
    observations = c(9, 2, 2),
    conversions =  c( 1, 0, 0),
    estimate1 = c(0.1,0.1,0.1),
    estimate2 = c(0.2,0.2,0.4)
  )

  # testing grouping when last element doesn't belong to 'complete' group of items
  d4 = data.table(
    observations = c(10, 4, 1, 2),
    conversions =  c( 1, 0, 0, 0),
    estimate1 = c(0.1, 0.1, 0.1, 0.1),
    estimate2 = c(0.2, 0.2, 0.4, 0.1)
  )

  trial_column = 'observations'
  success_column = 'conversions'
  estimate_columns = c('estimate1', 'estimate2')
  min_observations = 5

  save(d, d2, d3, d4,
       trial_column, success_column, estimate_columns, min_observations,
       file = 'fixtures-combiners.RData')
})
