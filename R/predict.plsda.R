#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, X) {

  # Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X) != (nrow(model$Coeffs)-1)) {
    stop("X must have the same number of columns than model")
  }

  # Setting data
  coeffs <- model$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))

  # Prediction
  Y.hat <- X %*% B + Cte

  # SoftMax
  Y.hat <- t(apply(Y.hat, 1, function(x) { exp(x) / sum(exp(x)) }))
  y.hat <- colnames(Y.hat)[apply(Y.hat, 1, which.max)]

  return(list(Y.hat = Y.hat, y.hat = y.hat))
}
