#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, X, y.exp = NULL) {

  # Controling data
  if (any(is.na(X))) {
    stop("X cannot contain null values")
  }

  if (ncol(X) != (nrow(model$Coeffs)-1)) {
    stop("X must have the same number of columns than model")
  }

  #TODO: tester vérifier que les colonnes du X correspondent bien au colonnes du modele

  # Setting data
  coeffs <- model$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))

  # Prediction
  Y.hat <- X %*% B + Cte # sous forme d'indicatrices
  #TODO: insérer le softmax ici pour obtenir les probabilités d'appartenance

  y.hat = colnames(Y.hat)[apply(Y.hat, 1, which.max)] # sous forme de modalités

  # In case we have an expected Y
  if (!is.null(y.exp)) {

    y.exp <- as.matrix(y.exp)

    # Controling data
    if (any(is.na(y.exp))) {
      stop("Y.exp cannot contain null values")
    }

    if (ncol(y.exp) != 1 && !is.factor(y.exp)) {
      stop("Y.exp must have only one column and must be a factor")
    }

    if (nrow(y.exp) != nrow(X)) {
      stop("Y.exp and X must have the same amount of rows")
    }

    Y.exp <- dummies::dummy(y.exp)

    # Residuals
    res <- Y.exp - Y.hat

    # Confusion Matrix
    total<-sum
    mc=addmargins(table(Y.quali,factor(Y.pred2,levels=colnames(Y))),FUN = total)
    mc=cbind(mc,pc.correct=diag(mc)/mc[,"total"]*100)

    return(list(Y.hat = Y.hat,
                y.hat = y.hat,
                Residuals = res,
                Conf.Mat = mc))
  }

  # Else we just return the Y hat
  else {
    return(list(Y.hat = Y.hat,
                y.hat = y.hat))
  }
}
