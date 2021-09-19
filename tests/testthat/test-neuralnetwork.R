test_that("number of layer works", {
  expect_equal(neuralnetwork(y~x, hidden=0)$n_hidden_layer, 0)
  expect_equal(neuralnetwork(y~x, hidden=0)$n_layer, 1)
  expect_equal(neuralnetwork(y~x, hidden=2)$n_hidden_layer, 1)
  expect_equal(neuralnetwork(y~x, hidden=2)$n_layer, 2)
  expect_equal(neuralnetwork(y+a~x, hidden=c(2, 3))$n_hidden_layer, 2)
  expect_equal(neuralnetwork(y+a~x, hidden=c(2, 3))$n_layer, 3)
})

test_that("weights structure works", {
  expect_identical(neuralnetwork(y~x, hidden=0)$weights[[1]][1, 1], 0)
  expect_identical({
    weights=neuralnetwork(y+a~x, hidden=c(2, 3))$weights
    any(sapply(weights, function(x) any(x[1, ]!=0)))
  },
  FALSE)
  expect_identical({
    weights=neuralnetwork(y+a~x, hidden=c(50, 60))$weights
    sd(unlist(lapply(weights, function(x) x[-1, ])))
  },
  sqrt(2 / (1+2)),
  tolerance = 0.1)
  expect_identical(neuralnetwork(y~x, hidden=0, activation_fun = ReLU)$weights[[1]][1, 1], 0.1)
  expect_identical({
    weights=neuralnetwork(y+a~x, hidden=c(2, 3), activation_fun = ReLU)$weights
    any(sapply(weights, function(x) any(x[1, ]!=0.1)))
    }, FALSE)
  expect_identical({
    weights=neuralnetwork(y+a~x, hidden=c(50, 60), activation_fun = ReLU)$weights
    sd(unlist(lapply(weights, function(x) x[-1, ])))
  },
  sqrt(2 / (1)),
  tolerance = 0.1)
})

