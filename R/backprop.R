#' @include naming.R
#' @include neuralnetwork.R
NULL


#' calculate activations in neural network.
#'
#' @param nn neural network object.
#' @param inputs matrix of observations with every input.

#'
#' @return list of matrices of activations.
activations <- function(nn, inputs) {

  n <- nn$n_layer

  #initialiser les activations du neuralnetwork
  acti <- list()

  #first calculation is from inputs_indiv
  acti[[1]] <- inputs

  #next activations are from activations
  if (n > 1) {
    for (layer in 2:n) acti[[layer]] <- as.matrix(nn$activation_fun(cbind(`1`=rep(1, nrow(inputs)),
                                                                acti[[layer-1]])%*%nn$weights[[layer-1]]), nrow(inputs))
  }


  acti[[n+1]] <- as.matrix(cbind(`1`=rep(1, nrow(inputs)), acti[[n]])%*%nn$weights[[n]], nrow(inputs))
  if(!nn$linear.output) acti[[n+1]] <- nn$activation_fun(acti[[n+1]])

  acti
}

#' calculate errors in neural network for derivatives.
#' Errors are derivatives of the cost function with regards to the z (XW+b).
#'
#' @param nn neural network object.
#' @param acti list of matrices of activations (from activations function).
#' @param inputs matrix of observations with every input.
#' @param target matrix of observations with every real target.
#' @param Loss_fun logical : if the function to maximize/minimize is a Loss function.
#' if FALSE, it is considered to be a linear target.
#' @param policy_linear_output logical : if the policy output in DDPG is linear.
#'
#' @return list of matrices of errors for derivatives.
errors <- function(nn, acti, inputs, target, Loss_fun = TRUE,
                   policy_linear_output = TRUE) {

  n <- nn$n_layer+1

  #list of error vectors
  error <- list()

  #if minimizing a parabola
  if(Loss_fun) {
    #si c'est linear, la dernière dérivée est différente
    if(!nn$linear.output) {
      error[[n]] <- 2 * (target - acti[[n]]) * -1 * nn$dactivation_fun(acti[[n]])
    } else {
      error[[n]] <- 2 * (target - acti[[n]]) * -1
    }
  } else {
    #si c'est linear, la dernière dérivée est différente
    if(!nn$linear.output) {
      error[[n]] <- nn$dactivation_fun(acti[[n]])
    } else {
      error[[n]] <- matrix(rep(1L, length(acti[[n]])))
    }
  }

  #error in hidden layers
  if(n > 2) for (hidden in (n - 1):2) {
    error[[hidden]] <- error[[hidden+1]] %*% t(matrix(nn$weights[[hidden]][-1, ], ncol=ncol(nn$weights[[hidden]]))) * nn$dactivation_fun(acti[[hidden]])
  }
  #error in first layer
  if(policy_linear_output) {
    error[[1]] <- error[[2]]%*%t(matrix(nn$weights[[1]][-1, ], ncol=ncol(nn$weights[[1]])))
  } else {
      error[[1]] <- error[[2]]%*%t(matrix(nn$weights[[1]][-1, ], ncol=ncol(nn$weights[[1]]))) * nn$dactivation_fun(acti[[1]])

  }
  error
}

#' calculate gradients in neural network.
#'
#' @param nn neural network object.
#' @param error list of errors.
#' @param acti list of activations.
#'
#' @return list of matrices of errors for derivatives.
gradients <- function(nn, error, acti) {

  #remplir les gradients et updater
  gradient <- lapply(1:nn$n_layer, function(layer) {
    b <- apply(error[[layer + 1]], 2, mean)
    w <- t(acti[[layer]]) %*% error[[layer + 1]] / nrow(acti[[layer]])
    rbind(b, w)
  })

  gradient
}

trace <- function(bool, epoch) {
  if (bool) print(paste0("Epoch : ", epoch))
}

#' Backpropagate data on a neural network
#'
#' @param nn neural network
#' @param newdata data.frame of the inputs and the outputs
#' @param n_epoch integer : number of iterations of backpropagation going
#'   through every data once.
#' @param lr numeric or matrix : learning rate (multiplier of the graident).
#'   If a matrix : schedule of learning (row 1 is the epoch of change and row 2
#'   is the new lr. Col must be c(0, <lr_initial>))
#' @param minibatch_size integer : size of minibatch of backpropagation.
#' @param algo character : convergence algorithm either "backprop" or "rprop+".
#' @param trace logical : printing last ran epoch?
#'
#' @return list of neural network (class : nn) and the mean Loss
#' @export
# examples
#nn <- neuralnetwork(out~a+b+c, hidden=c(3, 2))
#newdata <- data.frame(
# a=rnorm(15, 10, 10),
# b=rnorm(15, 20, 10),
#  c=rnorm(15, 5, 20)
#)
#newdata <- cbind(out=newdata$a*4+newdata$b*-2+newdata$c*10, newdata)
#backprop(nn, newdata, 100, minibatch_size = 6, trace = TRUE, lr = 0.1)
#backprop(nn, newdata, 100, minibatch_size = 6, trace = TRUE,
#  lr = matrix(c(0, 0.1, 50, 0.01, 150, 0.001), nrow=2))
#
#nn <- neuralnetwork(out~a, hidden=c(1), startweights = "zero")
#newdata <- data.frame(
#  a=rnorm(150, 10, 10)
#)
#newdata <- cbind(out=ifelse(newdata$a<5, -10, 7), newdata)
#backprop(nn, newdata, 100, minibatch_size = 6, trace = TRUE)
#
#nn <- neuralnetwork(out~a, hidden=c(1), activation_fun = ReLU,
#  dactivation_fun = dReLU)
#newdata <- data.frame(
#  a=rnorm(150, 10, 10)
#)
#newdata <- cbind(out=ifelse(newdata$a<5, -10, 7), newdata)
#(model <- backprop(nn, newdata, 400, trace = TRUE))
#
#TODO ajouter des métriques de loss pour train, validation et test
#TODO ajouter l'option de splitter automatiquement le newdata en train validation et test
#  avec des options de id pour tjrs prendre (ou non) les memes données pour ces 3 datasets
# TODO permmettre la k-fold validation
backprop <- function(nn, newdata, validation = NULL, n_epoch = 100, lr = 1,
                     minibatch_size = ceiling(nrow(newdata) / 10), algo = "backprop",
                     trace = FALSE) {

  target <- as.matrix(stats::model.frame(as.formula(call("~", nn$formula[[2]])), newdata))

  #tracking the loss amount over improvements
  Loss <- mean(apply((target - predict_nn(nn, newdata)) ^ 2, 1, sum))

  if (!is.null(validation)) {
    target_validation <- as.matrix(stats::model.frame(as.formula(call("~", nn$formula[[2]])), validation))
    Loss_validation <- mean(apply((target_validation - predict_nn(nn, validation)) ^ 2, 1, sum))
  }

  n <- nn$n_layer

  inputs <- stats::model.frame(as.formula(call("~", nn$formula[[3]])), newdata)

  i <- 0
  if (!is.null(dim(lr))) {
    if (length(unique(lr[1, ])) != length(lr[1, ])) stop("Les n_epoch dans l'horaire des lr doit être unique.")
    effective_lr <- lr[2, 1]
  } else {
    effective_lr <- lr
  }
  if(algo == "backprop") {

    repeat {
      remainder_id <- 1:nrow(inputs)
      repeat{
        minibatch_id <- sample(remainder_id, min(minibatch_size, length(remainder_id)))
        remainder_id <- remainder_id[-match(minibatch_id, remainder_id)]

        acti <- activations(nn, as.matrix(inputs[minibatch_id, ]))

        error <- errors(nn, acti, as.matrix(inputs[minibatch_id, ]), as.matrix(target[minibatch_id, ]))

        gradient <- gradients(nn, error, acti)

        #update
        nn$weights <- mapply(function(x, y) x - y * effective_lr, nn$weights, gradient, SIMPLIFY = FALSE)

        new_mean_loss <- mean(apply((target - predict_nn(nn, newdata)) ^ 2, 1, sum))

        #if (tail(Loss, 1) <= new_mean_loss) {
        #  #update back
        #  nn$weights <- lapply(1:n, function(layer) nn$weights[[layer]] + gradient[[layer]] * effective_lr)
        #  effective_lr <- effective_lr / 2
        #} else {
        #  #double stepsize?
        #  nn$weights <- lapply(1:n, function(layer) nn$weights[[layer]] - gradient[[layer]] * lr)

        #if(new_mean_loss < mean(apply((target - predict_nn(nn, newdata)) ^ 2, 1, sum))) {
        #    #update back
        #    nn$weights <- lapply(1:n, function(layer) nn$weights[[layer]] + gradient[[layer]] * lr)
        #  } else {lr <- lr * 2}
        #}


        if (length(remainder_id) == 0) break
      }

      Loss <- c(Loss, mean(apply((target - predict_nn(nn, newdata)) ^ 2, 1, sum)))
      if (!is.null(validation)) Loss_validation <- c(Loss_validation, mean(apply((target_validation - predict_nn(nn, validation)) ^ 2, 1, sum)))

      i <- i+1
      trace(trace, i)
      if (!is.null(dim(lr))) if (i %in% lr[1, ]) effective_lr <- lr[2, which(lr[1, ] == i)]
      if (i == n_epoch) break
    }
  } else if(algo=="rprop+") {

    step_update <- lapply(1:n, function(layer) matrix(0.1, nrow=nrow(nn$weights[[layer]]), ncol=ncol(nn$weights[[layer]])))

    repeat {
      remainder_id <- 1:nrow(inputs)
      repeat{
        if (length(remainder_id) == 1) {
          minibatch_id <- remainder_id
        } else {
          minibatch_id <- sample(remainder_id, min(minibatch_size, length(remainder_id)))
        }
        remainder_id <- remainder_id[-match(minibatch_id, remainder_id)]

        acti <- activations(nn, as.matrix(inputs[minibatch_id, ]))

        error <- errors(nn, acti, as.matrix(inputs[minibatch_id, ]), as.matrix(target[minibatch_id, ]))

        gradient_new <- gradients(nn, error, acti)

        if (i == 0) gradient <- gradient_new

        #update
        change <- lapply(1:n, function(layer) (gradient_new[[layer]] >= 0) != (gradient[[layer]] >= 0))
        step_update <- lapply(1:n, function(layer) (change[[layer]] * step_update[[layer]] / 2 + (1 - change[[layer]])*step_update[[layer]]*1.2))
        step_update <- mapply(function(x) matrix(pmax(1e-10, pmin(0.5, x)), ncol=ncol(x)), step_update, SIMPLIFY = FALSE)
        nn$weights <- lapply(1:n, function(layer) nn$weights[[layer]] - effective_lr * step_update[[layer]] * sign(gradient_new[[layer]]))

        #new gradient is now old
        gradient <- gradient_new

        if (length(remainder_id) == 0) break
      }

      Loss <- c(Loss, mean(apply((target - predict_nn(nn, newdata)) ^ 2, 1, sum)))
      if (!is.null(validation)) Loss_validation <- c(Loss_validation, mean(apply((target_validation - predict_nn(nn, validation)) ^ 2, 1, sum)))

      i <- i+1
      trace(trace, i)
      if (!is.null(dim(lr))) if (i %in% lr[1, ]) effective_lr <- lr[2, which(lr[1, ] == i)]
      print(effective_lr)
      if (i == n_epoch) break
    }
  }

    #remove dimnames
    for(i in 1:n) dimnames(nn$weights[[i]]) <- NULL

    #change old infos on graph
    nn$mean_error_squ <- tail(Loss, 1)

    if(!is.null(validation)) {
      list(nn = nn, Loss = Loss, Loss_validation = Loss_validation)
    } else {
      list(nn = nn, Loss = Loss)
    }
}


#' Backpropagate data on a neural network
#'
#' @param policy_nn policy neural network.
#' @param critic_nn critic neural network.
#' @param newdata data.frame of the inputs from policy and the outputs wanted from critic.
#' @param n_epoch integer : number of iterations of backpropagation going
#'   through every data once.
#' @param lr numeric : multiplier of the gradient.
#' @param minibatch_size integer : size of minibatch of backpropagation.
#' @param trace logical : printing last ran epoch?
#'
#' @return list of neural network (class : nn) and the mean Loss
#' @export
#
# #examples
# backprop_policy(
# policy_nn=neuralnetwork(x~a, 1, startweights="zero"),
# critic_nn=neuralnetwork(reward~a+x, 1, startweights=list(matrix(c(0, 0.05, -1), ncol=1), matrix(c(-10, 2), ncol=1))),
# newdata=data.frame(a=c(6, 12, 0, -5)),
# n_epoch = 100,
# minibatch_size = 3
# )
backprop_policy <- function(policy_nn, critic_nn, newdata, n_epoch = 1,
                            lr = 0.1,
                            minibatch_size = ceiling(nrow(newdata) / 10),
                            trace = FALSE) {

  crit_inp <- as.data.frame(as.matrix(predict_nn(policy_nn, newdata)))
  colnames(crit_inp) <- naming(policy_nn, FALSE)

  inputs <- cbind(newdata, crit_inp)

  #reorder
  inputs <- stats::model.frame(as.formula(call("~", critic_nn$formula[[3]])), inputs)

  #tracking the reward amount over improvements
  rew <- mean(predict_nn(critic_nn, inputs))

  i <- 0
  repeat {
    remainder_id <- 1:nrow(inputs)
    repeat{

      if (length(remainder_id) == 1) {
        minibatch_id <- remainder_id
      } else {
        minibatch_id <- sample(remainder_id, min(minibatch_size, length(remainder_id)))
      }
      remainder_id <- remainder_id[-match(minibatch_id, remainder_id)]

      n_pol <- length(policy_nn$weights)

      acti <- activations(critic_nn, as.matrix(inputs[minibatch_id, ]))

      error <- errors(critic_nn, acti, as.matrix(inputs[minibatch_id, ]), Loss_fun=FALSE, policy_linear_output = policy_nn$linear.output)

      #activations neuralnetwork
      acti_policy <- activations(policy_nn, as.matrix(stats::model.frame(as.formula(call("~", policy_nn$formula[[3]])), as.data.frame(inputs[minibatch_id, ]))))

      #initialiser l'error du policy
      error_policy <- list()

      #error in last layer
      colnames(error[[1]]) <- naming(critic_nn)
      error_policy[[n_pol + 1]] <- as.matrix(stats::model.frame(as.formula(call("~", policy_nn$formula[[2]])), as.data.frame(error[[1]])))

      #error in hidden layers
      if(n_pol > 1) for(hidden in n_pol:2) error_policy[[hidden]] <- error_policy[[hidden+1]]%*%t(matrix(policy_nn$weights[[hidden]][-1, ], ncol=ncol(policy_nn$weights[[hidden]])))*acti_policy[[hidden]]*(1-acti_policy[[hidden]])

      #initialiser le gradient du policy
      gradient <- gradients(policy_nn, error_policy, acti_policy)

      #updating gradient
      policy_nn$weights <- lapply(1:n_pol, function(layer) policy_nn$weights[[layer]] + gradient[[layer]] * lr)

      if (length(remainder_id) == 0) break
    }

    #tracking the reward amount over improvements
    pred <- predict_nn(policy_nn, newdata)
    if(is.matrix(pred)) {
      crit_inp <- as.data.frame(pred)
      colnames(crit_inp) <- naming(policy_nn, FALSE)
    } else {
      crit_inp <- as.data.frame(t(pred))
      colnames(crit_inp) <- naming(policy_nn, FALSE)
    }


    new_inputs <- cbind(newdata, crit_inp)
    #reorder
    new_inputs <- stats::model.frame(as.formula(call("~", critic_nn$formula[[3]])), as.data.frame(new_inputs))

    new_mean_reward <- mean(predict_nn(critic_nn, new_inputs))

    #if descent instead of ascent
    if(tail(rew, 1) >= new_mean_reward) {
      #go back
      #updating gradient
      policy_nn$weights <- lapply(1:n_pol, function(layer) policy_nn$weights[[layer]]-gradient[[layer]]*lr)
      lr <- lr/2
    }


    rew <- c(rew, new_mean_reward)

    i <- i + 1
    trace(trace, i)
    if (i == n_epoch) break
  }


  list(nn=policy_nn, Reward=rew)
}

