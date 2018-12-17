#' Fonction predict.plsda
#'
#' @description This function takes a plsda model and a set of predictors fitting to this model and will return a plsda-prev object containing the predicted values.
#' @param object A plsda object containing a trained model. The model can be generated with the fit function of this package.
#' @param X The set of predictor that will be used to compute prediction. It mus fit the predictors used to train the model.
#' @param ... Use to comply generic model
#' @return A plsda-pred object containing prediction. This object contains y.hat, a qualitative prediction and Y.hat, a binarized prediction.
#' @keywords fit, predict, pls, plsda
#' @export
#' @method predict plsda
#' @examples
#' model <- fit(Species ~ ., data = iris)
#' predict(model, iris[,1:4])
predict.plsda <- function(object, X, ...) {

  # Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X) != (nrow(object$Coeffs)-1)) {
    stop("X must have the same number of columns than model")
  }

  # Setting data
  coeffs <- object$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))

  # Prediction
  Y.hat <- X %*% B + Cte

  # SoftMax
  Y.hat <- t(apply(Y.hat, 1, function(x) { exp(x) / sum(exp(x)) }))
  y.hat <- colnames(Y.hat)[apply(Y.hat, 1, which.max)]

  result <- structure((list(
    Y.hat = Y.hat,
    y.hat = y.hat)))
  class(result) <- "plsda-pred"
  return(result)
}
