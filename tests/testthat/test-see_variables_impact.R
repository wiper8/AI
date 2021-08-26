test_that("see_variables_impact works", {
  expect_silent(see_variables_impact(nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = TRUE)))
  expect_silent(see_variables_impact(nn=neuralnetwork(x~a, hidden=1, linear.output = TRUE)))
  expect_silent(see_variables_impact(nn=neuralnetwork(x~a, hidden=c(2, 1), linear.output = TRUE)))
  expect_silent(see_variables_impact(nn=neuralnetwork(x~a+b, hidden=c(2, 1), linear.output = TRUE)))
  expect_silent(see_variables_impact(nn=neuralnetwork(x+y~a+b, hidden=c(2, 1), linear.output = TRUE)))

})
