#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @param n (The number of components)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, newdata, n) {

  X <- as.matrix(scale(newdata))
  print(X)
  B <- model$reg.coefs[-nrow(model$reg.coefs),]
  eps <- matrix(rep(model$reg.coefs[nrow(model$reg.coefs),], each=nrow(X)), nrow(X), ncol(B))

  #Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X)<2) {
    stop("X must have more than 1 column each")
  }

  #Prediction
  Y = X %*% B + eps
  y = colnames(Y)[apply(Y,1,which.max)]

  return(list(Y = Y, y = y))
}
