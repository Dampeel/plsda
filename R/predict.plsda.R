#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, newdata) {

  # Setting data
  X <- as.matrix(newdata)
  B <- model$reg.coefs[-nrow(model$reg.coefs),]
  eps <- matrix(rep(model$reg.coefs[nrow(model$reg.coefs),], each=nrow(X)), nrow(X), ncol(B))

  #Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X)<2) {
    stop("X must have more than 1 column each")
  }

  # Applying scaling infos from training set
  X.means <- model$X.means
  X.sds <- model$X.sds
  X.old <- t(apply(X, 1, scale.plsda, X.means, X.sds))

  # Prediction
  Y.hat = X %*% B + eps # sous forme d'indicatrices
  y.hat = colnames(Y.hat)[apply(Y.hat, 1, which.max)] # sous forme de modalités

  # insérer le softmax ici pour obtenir dles probabilités d'appartenance

  return(list(Y.hat = Y.hat, y.hat = y.hat))
}
