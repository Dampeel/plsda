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

  # Setting data
  coeffs <- model$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))

  # Prediction
  Y.hat <- X %*% B + Cte # sous forme d'indicatrices

  # SoftMax
  Y.hat <- t(apply(Y.hat, 1, function(x) { exp(x) / sum(exp(x)) }))
  y.hat <- colnames(Y.hat)[apply(Y.hat, 1, which.max)] # sous forme de modalités

  # In case we have an expected Y
  if (!is.null(y.exp)) {

    Y.exp <- as.matrix(dummies::dummy(y.exp))

    # Controling data
    if (any(is.na(Y.exp))) {
      stop("Y.exp cannot contain null values")
    }

    if (ncol(Y.exp) != 1 && !is.factor(y.exp)) {
      stop("Y.exp must have only one column and must be a factor")
    }

    if (nrow(Y.exp) != nrow(X)) {
      stop("Y.exp and X must have the same amount of rows")
    }

    # Residuals
    res <- Y.exp - Y.hat

    # Confusion Matrix
    total<-sum
    mc <- addmargins(table(y.exp, factor(y.hat, levels(y.exp))), FUN = total, quiet=TRUE)
    mc <- cbind(mc, pc.correct=diag(mc)/mc[,"total"]*100)

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
