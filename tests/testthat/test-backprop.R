test_that("activation is ok", {
  expect_equal(unlist(activations(nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = TRUE),
                                  inputs=matrix(2:4))),
               c(2, 3, 4, 1.2, 1.7, 2.2))
  expect_equal(unlist(activations(nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = TRUE),
                                  inputs=matrix(2:4))),
               c(2, 3, 4, 0.76852478, 0.8455, 0.90025, -0.46295, -0.3089, -0.1995),
               tolerance = 0.001)

  expect_equal(unlist(activations(nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = FALSE),
                                  inputs=matrix(2:4))),
               c(2, 3, 4, 0.76852478, 0.8455, 0.90025),
               tolerance = 0.001)
  expect_equal(unlist(activations(nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = FALSE),
                                  inputs=matrix(2:4))),
               c(2, 3, 4, 0.76852478, 0.8455, 0.90025, 0.386286, 0.423383, 0.450289),
               tolerance = 0.001)

  expect_equal(unlist(activations(nn=neuralnetwork(x~a, hidden=c(1, 1), startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = TRUE),
                                  inputs=matrix(2:4))),
               c(2.0000000,  3.0000000,  4.0000000,  0.7685248,  0.8455347,  0.9002495,  0.3862861,  0.4233758,
                 0.4502895, -1.2274277, -1.1532484, -1.0994210),
               tolerance = 0.001)

})

test_that("errors is ok", {
  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = TRUE)
      unlist(errors(nn, acti=activations(nn, inputs=matrix(2:4)), target=matrix(rep(-1, 3))))
    },
    c(2.2, 2.7, 3.2, 4.4, 5.4, 6.4))
  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = TRUE)
      unlist(errors(nn, acti=activations(nn, inputs=matrix(2:4)), target=matrix(rep(-1, 3))))
    },
    c(0.191073, 0.18052, 0.14377, 0.382146, 0.361, 0.2875, 1.0741, 1.382, 1.601),
    tolerance = 0.001)

  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = FALSE)
      unlist(errors(nn, acti=activations(nn, inputs=matrix(2:4)), target=matrix(rep(-1, 3))))
    },
               c(0.3146107, 0.2410374, 0.1706430, 0.6292215, 0.4820749, 0.3412861),
               tolerance=0.001)

  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = FALSE)
      unlist(errors(nn, acti=activations(nn, inputs=matrix(2:4)), target=matrix(rep(-1, 3))))
    },
    c(0.11692848, 0.09076758, 0.06447458, 0.23385696, 0.18153516, 0.12894915, 0.65729137, 0.69497386,
      0.71797705),
    tolerance = 0.001)

  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = FALSE)
      unlist(errors(nn, acti=activations(nn, inputs=matrix(2:4)), target=matrix(rep(-1, 3)), policy_linear_output = FALSE))
      },
    c(-0.2338570, -0.5446055, -0.7736949,  0.2338570,  0.1815352,  0.1289492,  0.6572914,  0.6949739,
      0.7179770),
               tolerance = 0.001)
  ###########
  #expect_equal(unlist(errors(nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = TRUE),
  #                           acti=list(2, 1.2), inputs_indiv=data.frame(a=2), Loss_fun=FALSE)),
  #             c(0.5, 1))

  #expect_equal(unlist(errors(nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = TRUE),
  #                           acti=list(2, 0.7685248, -0.4629504), inputs_indiv=data.frame(a=2), Loss_fun=FALSE)),
  #             c(0.1778944, 0.3557889, 1),
  #             tolerance = 0.001)

  #expect_equal(unlist(errors(nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = FALSE),
   #                          acti=list(2, 0.7685248), inputs_indiv=data.frame(a=2), Loss_fun=FALSE)),
  #             c(0.088947, 0.1778944),
  #             tolerance=0.001)

  #expect_equal(unlist(errors(nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = FALSE),
   #                          acti=list(2, 0.7685248, 0.3862861), inputs_indiv=data.frame(a=2), Loss_fun=FALSE)),
  #             c(0.04217, 0.08434656, 0.2370691),
  #             tolerance = 0.001)

  #expect_equal(unlist(errors(nn=neuralnetwork(x~a, hidden=1, startweights = list(matrix(c(0.2, 0.5), ncol=1), matrix(c(-2, 2), ncol=1)), linear.output = FALSE),
  #                           acti=list(0.7, 0.7685248, 0.3862861), inputs_indiv=data.frame(a=0.7), Loss_fun=FALSE, policy_linear_output = FALSE)),
  #             c(0.0088564, 0.08434656, 0.2370691),
  #             tolerance = 0.001)
})

test_that("gradients is ok", {
  expect_equal(
    {
      nn=neuralnetwork(x~a, hidden=0, startweights = list(matrix(c(0.2, 0.5), ncol=1)), linear.output = TRUE)
      inputs=matrix(2:4)
      target=matrix(rep(-1, 3))
      acti=activations(nn, inputs)
      error=errors(nn, acti, input, target)
      gradients(nn, error, acti)
    },
    list(matrix(c(5.4, 16.8667), dimnames=list(c("b", ""), NULL))),
    tolerance = 0.001)

})

test_that("backprop converges to correct weights", {
  dat <- data.frame(a=rnorm(100, -1, 1.5), b=rnorm(100))
  dat2 <- as.data.frame(matrix(1/(1+exp(-(dat$a*2+dat$b*3+1))), ncol=2, nrow=100)*matrix(c(1, 2), byrow=TRUE, nrow=100, ncol=2)+matrix(c(-2, 3), ncol=2, nrow=100, byrow=TRUE))
  colnames(dat2) <- c("x", "y")
  expect_equal({
    test <- backprop(nn=neuralnetwork(x+y~a+b, hidden=1, startweights = "zero", linear.output = TRUE),
                               newdata=cbind(dat, dat2), step_size=0.5, stepmax = 200, threshold = 0)
    unlist(test$nn$weights)
    },
               unlist(list(matrix(1:3), matrix(c(-2, 1, 3, 2), ncol=2))),
               tolerance = 0.1)

  expect_equal({
    test2 <- backprop(nn=neuralnetwork(x+y~a+b, hidden=1, startweights = "zero", linear.output = TRUE),
                     newdata=cbind(dat, dat2), step_size=0.5, stepmax = 200, threshold = 0, algo="rprop+")
    unlist(test2$nn$weights)
  },
  unlist(list(matrix(1:3), matrix(c(-2, 1, 3, 2), ncol=2))),
  tolerance = 0.1)

})


test_that("backprop_policy is maximized", {
  expect_equal({
    test <- backprop_policy(policy_nn=neuralnetwork(x~a, c(1, 1), startweights="zero"),
                            critic_nn=neuralnetwork(reward~a+x, 1, startweights=list(matrix(c(0, 0.05, -1), ncol=1), matrix(c(-10, 2), ncol=1))),
                            newdata=data.frame(a=c(6, 12, 0, -5)), stepmax=100, step_size=0.5)
    tail(test$Reward, 1)
  },
  -8,
  tolerance = 0.01)

})
