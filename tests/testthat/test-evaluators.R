context('Sample Evaluators')

test_that('Binomial guesses for valid data are calculated correctly', {
  load('fixtures-evaluators.RData')

  expected_guesses = 0
  computed_guesses = with(d, binomial_evaluator(trials1, estimates1, trials2, estimates2))
  expect_equal(expected_guesses, computed_guesses)

  expected_guesses = 1
  computed_guesses = with(d2, binomial_evaluator(trials1, estimates1, trials2, estimates2))
  expect_equal(expected_guesses, computed_guesses)
})


test_that('Binomial evaluator throws appropriate error for invalid data', {
  load('fixtures-evaluators.RData')

  expect_error(
    with(d3, binomial_evaluator(trials1, estimates1, trials2, estimates2)),
    'Conversion estimates must be between 0 and 1'
  )

  expect_error(
    with(d4, binomial_evaluator(trials1, estimates1, trials2, estimates2)),
    'Conversion estimates must be between 0 and 1'
  )

  expect_error(
    with(d5, binomial_evaluator(trials1, estimates1, trials2, estimates2)),
    'Trials must be greater than 0'
  )

})
