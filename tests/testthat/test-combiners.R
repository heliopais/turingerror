context('Combine items with low number of observations')


test_that('one group of items in binomial dataset is combined properly', {
  load('fixtures-combiners.RData')

  expected_comb = data.table(
    observations = 6,
    conversions = 1,
    estimate1 = 0.1,
    estimate2 = 0.25
  )
  computed_comb = combine_all_binomial(d, trial_column, success_column, estimate_columns)
  expect_equal(computed_comb, expected_comb)

})



test_that('combination of low observation items in binomial dataset works correctly', {
  load('fixtures-combiners.RData')

  expected_comb = data.table(
    observations = c(9,4),
    conversions = c(1,0),
    estimate1 = c(0.1,0.1),
    estimate2 = c(0.2,0.3)
  )
  computed_comb = get_combined_observations_binomial(
    d3,
    trial_column, success_column, estimate_columns,
    min_observations,
    shuffle = FALSE)
  expect_equal(computed_comb, expected_comb)

})



