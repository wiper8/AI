naming <- function(nn, input=TRUE) {
  i <- ifelse(input, 3, 2)
  form <- nn$formula[[i]]
  run <- 1!=length(form)
  var_names <- c()
  if(!run) {
    var_names <- as.character(form)
  } else {
    while(run) {
      var_names <- c(var_names, as.character(form[[3]]))
      form <- form[[2]]
      run <- 1!=length(form)
    }
    var_names <- rev(c(var_names, as.character(form)))
  }
  var_names
}
