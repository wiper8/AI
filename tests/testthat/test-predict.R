test_that("outputs are correct", {
  expect_equal(predict_nn(
    nn = neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(1, -2), ncol=1))),
    newdata = data.frame(a=c(0.2, 0.3))
  ),
  matrix(c(0.6, 0.4), dimnames=list(c("1", "2"), "x"))
  )

  expect_equal(
    predict_nn(
      neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(1, -2), ncol=1)), linear.output = FALSE),
      data.frame(a=c(0.2, 0.3))
    ),
    matrix(c(1/(1+exp(-c(0.6, 0.4)))), dimnames=list(c("1", "2"), "x"))
  )

  expect_equal({
    pred <- predict_nn(
      nn=neuralnetwork(x+y~a+b, hidden=1, startweights = list(matrix(c(1, 2, 3), ncol=1), matrix(c(-2, 1, 3, 2), ncol=2)), linear.output = TRUE),
      newdata=data.frame(a=c(1, 1.2), b=c(2, 2.5)))
    colnames(pred)
    },
               c("x", "y"))

  expect_equal(
    predict_nn(
      nn=neuralnetwork(x+y~a+b, hidden=1, startweights = list(matrix(c(1, 2, 3), ncol=1), matrix(c(-2, 1, 3, 2), ncol=2)), linear.output = TRUE),
      newdata=data.frame(a=c(1, -1), b=c(2, 1))),
               matrix(c(-1.00012339457599, -1.1192, 4.99975, 4.761594), ncol=2, dimnames=list(c("1", "2"), c("x", "y"))),
               tolerance=0.001)

  expect_equal(
    predict_nn(
      nn=neuralnetwork(x+y~a+b, hidden=1, startweights = list(matrix(c(1, 2, 3), ncol=1), matrix(c(-4, 1, 3, 2), ncol=2)), linear.output = TRUE, activation_fun = ReLU, dactivation_fun = dReLU),
      newdata=data.frame(a=c(1, 0.5), b=c(-2, 0.5))),
    matrix(c(-4, -0.5, 3, 10), ncol=2, dimnames =list(NULL, c("x", "y"))),
    tolerance=0.001)

})

