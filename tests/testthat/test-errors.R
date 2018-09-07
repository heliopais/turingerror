context('Measure Turing Error')

test_that('binomial turing error is calculated correctly', {
  load('fixtures-errors.RData')

  expected_error = data.table(
    estimate = c('estimate1', 'estimate2'),
    turingerror = c(0, 1)
  )
  computed_error = conversion_turing_error(d, trial_column, success_column,
                                           estimate_columns,
                                           1, FALSE, 10)

  expect_equal(expected_error, computed_error)
})
