test_that("number of layer works", {
  expect_equal(neuralnetwork(y~x, hidden=0)$n_hidden_layer, 0)
  expect_equal(neuralnetwork(y~x, hidden=0)$n_layer, 1)
  expect_equal(neuralnetwork(y~x, hidden=2)$n_hidden_layer, 1)
  expect_equal(neuralnetwork(y~x, hidden=2)$n_layer, 2)
  expect_equal(neuralnetwork(y+a~x, hidden=c(2, 3))$n_hidden_layer, 2)
  expect_equal(neuralnetwork(y+a~x, hidden=c(2, 3))$n_layer, 3)
})

test_that("weights structure works", {
  expect_identical(neuralnetwork(y~x, hidden=0, startweights = "zero")$weights, list(matrix(0, nrow=2)))
  expect_identical(neuralnetwork(y+a~x, hidden=c(2, 3), startweights = "zero")$weights, list(matrix(0, nrow=2, ncol=2), matrix(0, nrow=3, ncol=3), matrix(0, nrow=4, ncol=2)))
})

