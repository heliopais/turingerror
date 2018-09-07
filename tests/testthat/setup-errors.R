setup({
  library(data.table)
  d = data.table(
    observations = c(100,100,100),
    conversions = c(0,10,70),
    estimate1 = c(0.001,0.1,0.7),
    estimate2 = c(0.7,0.001,0.001)
  )

  trial_column = 'observations'
  success_column = 'conversions'
  estimate_columns = c('estimate1', 'estimate2')
  min_observations = 5

  save(d,
       trial_column, success_column, estimate_columns, min_observations,
       file = 'fixtures-errors.RData')
})
