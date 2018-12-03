#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, X, Y.exp) {

  # Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X) != ncol(model$Coeffs)) {
    stop("X must have the same number of columns than model")
  }

  # Setting data
  coeffs <- model$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))

  #TODO: insérer le softmax ici pour obtenir les probabilités d'appartenance

  # Prediction
  Y.hat1 = X %*% B + Cte # sous forme d'indicatrices
  y.hat = colnames(Y.hat)[apply(Y.hat, 1, which.max)] # sous forme de modalités

  # Confusion Matrix
  total<-sum
  mc=addmargins(table(Y.exp,y.hat),FUN=total)
  mc=cbind(mc,pc.correct=diag(mc)/mc[,"total"]*100)

  return(list(Y.hat = Y.hat,
              y.hat = y.hat,
              Conf.Mat = mc))
}
