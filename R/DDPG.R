#' @include backprop.R
#' @include neuralnetwork.R
#' @include predict.nn.R
#' @include state_action.R
NULL

#' Train a DDPG system.
#'
#' @param policy_nn policy nn to choose actions.
#' @param critic_nn critic nn to predict reward.
#' @param actualize function to move object.
#' @param reset function to reset object.
#' @param reward function to get reward.
#' @param done function to determine if episode if done.
#' @param episodes integer : number of scenarios to train to.
#' @param buffer_len integer : length of the replay buffer.
#' @param batch_size integer : length of the batches used to backpropagate networks.
#' @param max_iter integer : maximum number of iterations for every episode.
#' @param explor numeric : >0 standard deviation of a value added to all weights in policy.
#' @param gradient_step_n integer : number of backpropagation steps before moving on to updating targets.
#' @param discount numeric : [0, 1] actualisation rate.
#' @param polyak numeric : [0, 1] percentage of the new targets that we keep to update the actual targets.
#' @param object_inputs function : function to get together in a data.frame the inputs for policy.
#' @param see function : function to see the agent do his actions.
#' @param track_weights logical : if the evolution of weights will be used in a graph.
#' @param track_object logical : if the past states of object are to be seen.
#' @param ... (optional) other arguments passed to other functions.
#'
#' @return list of the policy and the critic's weights (and the tracking of the weights if track_weights is TRUE) and a plot of the last car position/line.
#' @export
#'
#' @examples DDPG(policy_nn=neuralnetwork(acc+steer~l+t_l+t+t_r+r+speed, c(3, 2)),
#' critic_nn=neuralnetwork(reward~l+t_l+t+t_r+r+speed+acc+steer, c(4, 3)),
#' actualize=car::actualize,
#' reset=function(...) car:::radar_distance(car::set(car::reset(x=0, y=0, orien=90)), walls=car::walls),
#' reward=car::reward,
#' done=car::done,
#' limit_FUN=car::limits_FUN,
#' episodes = 1,
#' buffer_len = 10,
#' batch_size = 4,
#' explor = 0.1,
#' gradient_step_n = 5,
#' discount = 0.999,
#' polyak = 0.9,
#' object_inputs=function(x) cbind(x$dist, speed=x$speed),
#' see=car::see_line,
#' track_weights=TRUE,
#' track_object=TRUE,
#' t=0.1,
#' walls=car::walls,
#' finish=car::finish
#' )
DDPG <- function(policy_nn, critic_nn, actualize, reset, reward, done, limit_FUN, episodes,
                 buffer_len, batch_size, explor, gradient_step_n, discount,
                 polyak, object_inputs, see, track_weights=FALSE, track_object=FALSE, ...) {

  #initialize buffer
  buffer <- list(s=NULL, a=NULL, s_prime=NULL, r=NULL, d=NULL)

  #initialize targets
  policy_nn_targ <- policy_nn
  critic_nn_targ <- critic_nn

  #if tracking weights
  if(track_weights) {
    wei <- unlist(unlist(policy_nn_targ$weights), unlist(critic_nn_targ$weights))
    tracking <- matrix(wei, ncol=length(wei))
  }

  #episodes loop
  for(e in 1:episodes) {
    #e <- 1

    Reward <- 0
    #initial state from the environnement
    object <- reset()

    track_obj <- NULL

    #initialize condition to stop when goal is achieved
    d <- FALSE
    v <- 0
    while(!d) {
      v <- v+1
      if(v==max_iter) break

      if(track_object) {
        track_obj[[v]] <- object
      }

      #exploration
      ################## diminuer l'exploration plus lentrainement est avancé
      dims <- lapply(policy_nn$weights, dim)
      for(i in 1:(policy_nn$n_hidden_layer+1)) {
        policy_nn$weights[[i]] <- policy_nn$weights[[i]] + rnorm(prod(dims[[i]]), 0, explor/e)
      }

      #prendre l'état
      s <- state(policy_nn$formula, object=object_inputs(object))

      #prendre décision de direction et d'acceleration
      #############le <object> devrait etre dans ... donc pour linstant cext pas automatisé
      a <- action(policy_nn, s, limit_FUN, object, ...)

      #actualiser le véhicule
      #object_prime <- actualize(object, a, t, walls)
      object_prime <- actualize(object, a, ...)

      ###temporaire
      print(paste0("e=", e, " v=", v, " x=", round(object_prime$x, 2), " y=", round(object_prime$y, 2), " speed=", round(object_prime$speed, 2), " ori=", round(object_prime$orien, 2),  " acc=", round(a$acc, 4), " steer=", round(a$steer, 4)))

      #prendre les mesures
      s_prime <- state(policy_nn$formula, object=object_inputs(object_prime))

      #reward and is done
      #rewa <- reward(object_prime, walls, finish, t)
      rewa <- reward(object_prime, ...)
      d <- done(rewa)

      Reward <- Reward+discount*rewa


      if(length(buffer$d)==buffer_len) {

        #removing old data
        buffer$s <- head(buffer$s, -1)
        buffer$a <- head(buffer$a, -1)
        buffer$s_prime <- head(buffer$s_prime, -1)
        buffer$rewa <- head(buffer$rewa, -1)
        buffer$done <- head(buffer$done, -1)
      }

      #inserting the new element
      buffer$s <- rbind(buffer$s, s)
      buffer$a <- rbind(buffer$a, a)
      buffer$s_prime <- rbind(buffer$s_prime, s_prime)
      buffer$rewa <- rbind(buffer$rewa, rewa)
      buffer$d <- rbind(buffer$d, d)

      #while(L>tol) {
      for(w in 1:gradient_step_n) {
        #w <- 1
        #if(L<tol) w <- gradient_step_n

        stepsize_critic <- 0.5
        stepsize_policy <- 0.5


        batch_id <- sample(1:min(length(buffer$d), buffer_len), min(length(buffer$d), batch_size))

        #target
        y <- numeric(length(batch_id))
        Q <- numeric(length(batch_id))

        L <- 0
        for(n in 1:length(batch_id)) {
          #n <- 1

          #pas clipper
          a_prime <- action(policy_nn_targ, s_prime)

          y[n] <- as.vector(buffer$rewa[n, 1]+
                              discount*(1-buffer$d[n, 1])*predict.nn(critic_nn_targ, cbind(buffer$s_prime[n, ], a_prime)))

          Q[n] <- predict.nn(critic_nn, cbind(buffer$s[n, ], buffer$a[n, ]))

          L <- L+(y[n]-Q[n])^2/length(batch_id)
        }

        #gradient descent
        descent <- backprop(critic_nn, cbind(reward=y, buffer$a[batch_id, ], buffer$s[batch_id, ]), threshold = 0, stepmax=1, step_size = stepsize_critic)

        #if it ascended, reduce step_size
        if(descent$Loss[1]<descent$Loss[2]) stepsize_critic <- stepsize_critic/2

        critic_nn <- descent$nn

        #gradient ascent
        ascent <- backprop_policy(policy_nn, critic_nn, buffer$s[batch_id, ], step_size = stepsize_policy)

        #if it descended, reduce step_size
        if(ascent$Reward[1]>ascent$Reward[2]) stepsize_policy <- stepsize_policy/2

        policy_nn <- ascent$nn

        #15
        #update les weights des targets

        #phi_targ
        for(layer in 1:length(critic_nn$weights)) {
          critic_nn_targ$weights[[layer]] <- critic_nn_targ$weights[[layer]]*polyak+
            critic_nn$weights[[layer]]*(1-polyak)
        }
        #theta_targ
        for(layer in 1:length(policy_nn$weights)) {
          policy_nn_targ$weights[[layer]] <- policy_nn_targ$weights[[layer]]*polyak+
            policy_nn$weights[[layer]]*(1-polyak)
        }
      }

      #if tracking weights
      if(track_weights) {
        wei <- unlist(unlist(policy_nn_targ$weights), unlist(critic_nn_targ$weights))
        tracking <- rbind(tracking, matrix(wei, ncol=length(wei)))
      }

      #update state
      object <- object_prime
    }
    print(e)
    #see(object_prime, walls, finish)

    if(track_object) {
      p <- see(object_prime, ...)
      for(i in r:length(track_obj)) p <- p+ggplot2::geom_line(ggplot2::aes_string())
    } else {
      see(object_prime, ...)
    }
  }

  if(track_weights) {
    return(list(theta=policy_nn_targ$weights, phi=critic_nn_targ$weights, tracking=track_weights))
  } else {
    return(list(theta=policy_nn_targ$weights, phi=critic_nn_targ$weights))
  }
}



#' See convergence of weights
#'
#' @param tracking matrix of every weights and biaises.
#'
#' @return ggplot.
#' @export
#'
#' @examples see_DDPG(DDPG(<...>)$tracking)
see_DDPG <- function(tracking) {
  p <- ggplot2::ggplot()
  n <- nrow(tracking)
  for(i in 1:ncol(tracking)) {
    p <- p+ggplot2::geom_line(ggplot2::aes_string(x=1:n, y=tracking[, i]), col=rgb(runif(1), runif(1), runif(1)))
  }
  p+ggplot2::xlab("iterations par object")
}

#' See effect of every inputs on actions or rewards
#'
#' @param policy_nn policy neural network.
#' @param critic_nn critic neural network.
#' @param policy logical : if policy is passed.
#'
#' @return ggplot2.
#' @export
see_variables_impact <- function(nn) {

  n <- length(nn$weights)
  if(n>1) {
    impact <- matrix(nn$weights[[n-1]][-1, ], ncol=ncol(nn$weights[[n-1]]))%*%matrix(nn$weights[[n]][-1, ], ncol=ncol(nn$weights[[n]]))
    if(n>2) for(i in (n-2):1) impact <- matrix(nn$weights[[i]][-1, ], ncol=ncol(nn$weights[[i]]))%*%impact

  } else {impact <- matrix(nn$weights[[1]][-1, ], ncol=ncol(nn$weights[[1]]))}

  var_names <- naming(nn)

  if(length(var_names)==1) {
    impact <- matrix(impact)
    rownames(impact) <- var_names
  } else {
    rownames(impact) <- var_names
  }

  var_names <- naming(nn, FALSE)

  if(length(var_names)==1) {
    colnames(impact) <- var_names
  } else {
    colnames(impact) <- var_names
  }

  p <- ggplot2::ggplot()+ggplot2::geom_hline(ggplot2::aes(yintercept=0))
  for(i in 1:nrow(impact)) {
    for(j in 1:ncol(impact)) {
      col <- ifelse(impact[i, j]>0, "green", "red")
      p <- p+ggplot2::geom_hline(ggplot2::aes_string(yintercept=impact[i, j]), col=col)
      p <- p+ggplot2::geom_text(ggplot2::aes_string(x=i, y=as.character(impact[i, j]), label=deparse(paste0(dimnames(impact)[[1]][i], "->", dimnames(impact)[[2]][j]))))
    }
  }
  p+ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()
                   )
}
