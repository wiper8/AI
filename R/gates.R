#' @include plot.nn.R

#Vérification des logic gates avec différentes fonctions d'activations
#on garde tjrs ici la dernière couche comme étant linéaire et les 0 et 1
#traditionnels des gates sont remplacés
#pour les gates avec 2 inputs, il faudrait un grah 3d ou 2D avec couleurs
library(ggplot2)

#Buffer

newdata <- data.frame(a=rnorm(500, 5, 30))
newdata <- cbind(out=ifelse(newdata$a<5, -10, 7), newdata)

test <- function(x) ifelse(x<5, -10, 7)
test2 <- function(x) predict.nn(model$nn, data.frame(a=x))
look <- function() {
  ggplot()+
    geom_line(aes(x=seq(min(newdata$a), max(newdata$a), length.out = 1000), y=test(seq(min(newdata$a), max(newdata$a), length.out = 1000))), col="green")+
    geom_line(aes(x=seq(min(newdata$a), max(newdata$a), length.out = 1000), y=test2(seq(min(newdata$a), max(newdata$a), length.out = 1000))), col="red")+
    geom_point(aes(x=a, y=out), newdata)
}


nn <- neuralnetwork(out~a, hidden=c(1))
(model <- backprop(nn, newdata, 20, algo = "rprop+", trace = TRUE))
look()


nn <- neuralnetwork(out~a, hidden=c(1), activation_fun = ReLU,
                    dactivation_fun = dReLU)
(model <- backprop(nn, newdata, 20, algo = "rprop+", trace = TRUE))
look()

#réseau large
nn <- neuralnetwork(out~a, hidden=c(5), activation_fun = ReLU,
                    dactivation_fun = dReLU)
(model <- backprop(nn, newdata, 20, algo = "rprop+", trace = TRUE))
look()

#réseau profond
nn <- neuralnetwork(out~a, hidden=c(1, 1, 1, 1, 1), activation_fun = ReLU,
                    dactivation_fun = dReLU)
(model <- backprop(nn, newdata, 20, algo = "rprop+", trace = TRUE))
look()

#réseau qui devrait le faire
nn <- neuralnetwork(out~a, hidden=c(1, 1), activation_fun = ReLU,
                    dactivation_fun = dReLU)
(model <- backprop(nn, newdata, 20, algo = "rprop+", trace = TRUE))
look()
plot.nn(model$nn)

#réseau large et profond
nn <- neuralnetwork(out~a, hidden=c(1, 2, 1), activation_fun = ReLU,
                    dactivation_fun = dReLU)
(model <- backprop(nn, newdata, 20, algo = "rprop+", lr = 0.5, trace = TRUE))
look()
plot.nn(model$nn)
