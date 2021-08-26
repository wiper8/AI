#' Create hexadecimal color between red and green for a value [0, 1]
#'
#' @param u numeric vector
#'
#' @return hexadecimal character color.
#'
#' @examples color_gradient.nn(c(0, 0.25, 0.5, 0.75, 1))
color_gradient.nn <- function(u) {
  grDevices::rgb(pmin(2-2*u, 1), pmin(2*u, 1), -2*abs(u-0.5)+1, 1)
}


#' Plot a neural network
#'
#' @param nn neural network
#' @param inputs_indiv optional : data.frame of one observations with every input to show activation in neurons.
#'
#' @return plot
#' @export
plot.nn <- function (nn, inputs_indiv=NULL) {

  if(!is.null(inputs_indiv)) acti <- activations(nn, inputs_indiv, nn$n_layer-1)

  circles <- lapply(nn$weights, function(x) dim(x)-c(1, 0))

  circles <- unlist(circles)[c(1, seq(2, length(unlist(circles)), by=2))]

  y <- mapply(seq, 1, unlist(circles), SIMPLIFY = FALSE)

  y_means <- unlist(lapply(y, mean))

  neur_y <- list()
  for(i in 1:length(y)) neur_y[[i]] <- (max(y_means)-y_means)[i]+y[[i]]

  #weights
  weights_coord <- list()

  lim_x <- c(Inf, -Inf)
  lim_y <- c(Inf, -Inf)
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]] <- data.frame("x1"=rep(i, circles[[i]]*circles[[i+1]]), "y1"=rep(rev(neur_y[[i]]), circles[[i+1]]), "x2"=rep(i+1, circles[[i]]*circles[[i+1]]), "y2"=rep(rev(neur_y[[i+1]]), each=circles[[i]]))
  for(i in 1:length(weights_coord)) lim_x[1] <- min(lim_x[1], min(weights_coord[[i]][, c("x1", "x2")]))
  for(i in 1:length(weights_coord)) lim_x[2] <- max(lim_x[2], max(weights_coord[[i]][, c("x1", "x2")]))
  for(i in 1:length(weights_coord)) lim_y[1] <- min(lim_y[1], min(weights_coord[[i]][, c("y1", "y2")]))
  for(i in 1:length(weights_coord)) lim_y[2] <- max(lim_y[2], max(weights_coord[[i]][, c("y1", "y2")]))
  lim_x <- lim_x+c(-1, 1)
  lim_y <- lim_y+c(-1, 1)
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]] <- cbind(weights_coord[[i]], "angle"=180/pi*atan((weights_coord[[i]]$y2-weights_coord[[i]]$y1)/(weights_coord[[i]]$x2-weights_coord[[i]]$x1)*(lim_x[2]-lim_x[1])/(lim_y[2]-lim_y[1])*2/3))
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]] <- cbind(weights_coord[[i]], "angle_stretched"=180/pi*atan((weights_coord[[i]]$y2-weights_coord[[i]]$y1)/(weights_coord[[i]]$x2-weights_coord[[i]]$x1)))
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$x1_lab <- weights_coord[[i]]$x1+0.3*cos(pi/180*weights_coord[[i]]$angle_stretched)
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$y1_lab <- weights_coord[[i]]$y1+0.3*sin(pi/180*weights_coord[[i]]$angle_stretched)
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$weight <- signif(as.vector(nn$weights[[i]][-1, ]), 2)
  max_weight <- max(abs(unlist(lapply(weights_coord, function(x) x$weight))))
  if(max_weight==0) {
    for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$col <- rep(0, length(weights_coord[[i]]$weight))+0.5
  } else {
    for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$col <- weights_coord[[i]]$weight/(2*max_weight)+0.5
  }
  for(i in 1:(length(neur_y)-1)) weights_coord[[i]]$col <- color_gradient.nn(weights_coord[[i]]$col)

  #biais
  biais_coord <- list()

  for(i in 1:(length(neur_y)-1)) biais_coord[[i]] <- data.frame("x"=rep(i+1, circles[[i+1]]), "y"=rev(neur_y[[i+1]])+0.2)
  for(i in 1:(length(neur_y)-1)) biais_coord[[i]]$biais <- signif(nn$weights[[i]][1, ], 2)

  #neurons
  #TODO corriger l'erreur
  #TODO supprimer l'affichage des poids si 5 neuronnes ou plus dans le layer concerné.
  #TODO ajuster la taille des cercles
  p <- ggplot2::ggplot()+
    if(!is.null(inputs_indiv)) {
      ggplot2::geom_point(ggplot2::aes(x=rep(1:nn$n_layer, unlist(circles)), y=unlist(neur_y), fill=unlist(acti)), shape=21, size=10)
    } else {
      ggplot2::geom_point(ggplot2::aes(x=rep(1:nn$n_layer, unlist(circles)), y=unlist(neur_y)), shape=21, size=10)
    }

  p <- p+ggplot2::scale_fill_gradient(low="#000000", high="#FFFFFF")

  #weights
  for(i in 1:length(weights_coord)) p <- p+ggplot2::geom_segment(data=weights_coord[[i]], mapping=ggplot2::aes(x=x1, xend=x2, y=y1, yend=y2), col=weights_coord[[i]]$col)
  for(i in 1:length(weights_coord)) p <- p+ggplot2::geom_text(data=weights_coord[[i]], mapping=ggplot2::aes(x=x1_lab, y=y1_lab, label=weight, angle=angle))

  #biais
  for(i in 1:length(biais_coord)) p <- p+ggplot2::geom_text(data=biais_coord[[i]], mapping=ggplot2::aes(x=x, y=y, label=biais))


  #theme
  p <- p+ggplot2::theme(axis.title = ggplot2::element_blank(),
                        axis.text = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank())+
    ggplot2::coord_fixed(ratio=(lim_x[2]-lim_x[1])/(lim_y[2]-lim_y[1])*2/3, xlim=lim_x, ylim=lim_y)

  p
}

