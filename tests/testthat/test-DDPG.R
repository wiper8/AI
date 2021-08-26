library(AI)
#load(file="theta.RData")
#load(file="phi.RData")
test <- data.frame(t=runif(100, 0, 100))
test <- cbind(test, r=ifelse(test$t<10, -100, -1))
test <- data.frame(t=runif(50, 0, 2))
test <- cbind(test, r=-1/(1+exp(-(3*test$t-5)))+0.1)
mod <- backprop(nn=neuralnetwork(r~t, 1, "zero"), newdata=test, threshold=0, stepmax=200)
mod <- backprop(nn=neuralnetwork(r~t, 1), newdata=test, threshold=0, stepmax=200)
plot(neuralnet::neuralnet(r~t, test, 1, algorithm = "rprop+"))
plot(mod$Loss, type="l", ylim=c(0, max(mod$Loss)))
plot.nn(mod$nn)
predict.nn(mod$nn, test)
policy_nn_targ=neuralnetwork(acc+steer~l+t_l+t+t_r+r+behind+speed, 3, startweights = "zero")
critic_nn_targ=neuralnetwork(reward~acc+steer+l+t_l+t+t_r+r+behind+speed, 3, startweights = "zero")
actualize=car::actualize
#########pas généralisé
reset=function(...) car:::radar_distance(car::set(car::reset(x=0, y=0, orien=90)), walls=car::walls5)
reward=car::reward
done=car::done
limit_FUN=car::limits_FUN
episodes = 1000
buffer_len = 1000
batch_size = 10
max_iter <- 1000
explor = 0.1
gradient_step_n = 10
discount = 0.99999
polyak = 0.9
object_inputs=function(x) cbind(x$dist, speed=x$speed)
see=car::see_line
track_weights=TRUE
track_object=TRUE
t=0.1
walls=car::walls5
finish=car::finish5

#initialize buffer
buffer <- list(s=NULL, a=NULL, s_prime=NULL, rewa=NULL, d=NULL)

#initialize targets
policy_nn <- policy_nn_targ
critic_nn <- critic_nn_targ

#if tracking weights
if(track_weights) {
  wei <- c(unlist(lapply(policy_nn_targ$weights, function(x) x[-1, ])), unlist(lapply(critic_nn_targ$weights, function(x) x[-1, ])))
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
    dims <- lapply(policy_nn$weights, dim)
    for(i in 1:(policy_nn$n_hidden_layer+1)) {
      policy_nn$weights[[i]] <- policy_nn$weights[[i]] + rnorm(prod(dims[[i]]), 0, explor/e)
    }

    #prendre l'état
    s <- state(policy_nn$formula, object=object_inputs(object))

    #prendre décision de direction et d'acceleration
    #############le <object> devrait etre dans ... donc pour linstant cext pas automatisé
    a <- action(policy_nn, s, limit_FUN, object, t)

    #actualiser le véhicule
    object_prime <- actualize(object, a, t, walls)

    ###temporaire
    print(paste0("e=", e, " v=", v, " x=", round(object_prime$x, 2), " y=", round(object_prime$y, 2), " speed=", round(object_prime$speed, 2), " ori=", round(object_prime$orien, 2),  " acc=", round(a$acc, 4), " steer=", round(a$steer, 4)))

    #prendre les mesures
    s_prime <- state(policy_nn$formula, object=object_inputs(object_prime))

    #reward and is done
    #rewa <- reward(object_prime, walls, finish)
    rewa <- reward(object_prime, walls, finish, t)
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
      if(descent$Loss[1]<=descent$Loss[2]) stepsize_critic <- stepsize_critic/2

      critic_nn <- descent$nn

      #gradient ascent
      ascent <- backprop_policy(policy_nn, critic_nn, buffer$s[batch_id, ], step_size = stepsize_policy)

      #if it descended, reduce step_size
      if(ascent$Reward[1]>=ascent$Reward[2]) stepsize_policy <- stepsize_policy/2

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
      wei <- c(unlist(lapply(policy_nn_targ$weights, function(x) x[-1, ])), unlist(lapply(critic_nn_targ$weights, function(x) x[-1, ])))
      tracking <- rbind(tracking, matrix(wei, ncol=length(wei)))
    }

    #update state
    object <- object_prime
  }
  print(e)

  print(see(object_prime, walls, finish, track_obj))
}
see_DDPG(tracking)
see_variables_impact(critic_nn_targ)
see_variables_impact(policy_nn_targ)
plot.nn(policy_nn_targ)
plot.nn(critic_nn_targ)
predict.nn(critic_nn_targ, cbind(state(policy_nn$formula, object=object_inputs(object_prime)), action(policy_nn_targ, state(policy_nn$formula, object=object_inputs(object_prime)))))


save(policy_nn_targ, file="theta.RData")
save(critic_nn_targ, file="phi.RData")


test_that("DDPG works", {
  expect_silent(DDPG(policy_nn=neuralnetwork(acc+steer~l+t_l+t+t_r+r+speed, c(3, 2)),
                     critic_nn=neuralnetwork(reward~l+t_l+t+t_r+r+speed+acc+steer, c(4, 3)),
                     actualize=car::actualize,
                     reset=function(...) car:::radar_distance(car::set(car::reset(x=0, y=0, orien=90)), walls=car::walls),
                     reward=car::reward,
                     done=car::done,
                     limit_FUN=car::limits_FUN,
                     episodes = 1,
                     buffer_len = 10,
                     batch_size = 4,
                     explor = 0.1,
                     gradient_step_n = 5,
                     discount = 0.999,
                     polyak = 0.9,
                     object_inputs=function(x) cbind(x$dist, speed=x$speed),
                     see=car::see_car_track,
                     track_weights=TRUE,
                     t=0.1,
                     walls=car::walls,
                     finish=car::finish
                     )
                )
})
