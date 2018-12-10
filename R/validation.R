validation <- function(result, y.exp) {

  y.hat <- result$y.hat
  Y.hat <- result$Y.hat

  # Controling data y.exp
  if (is.null(y.exp)) {
    stop("y.exp must not be null")
  }

  if (is.matrix(y.exp)) {
    stop("y.exp must have only one column")
  }

  # Extracting binary values Y.exp from factor y.exp
  Y.exp <- as.matrix(dummies::dummy(y.exp))
  colnames(Y.exp) <- levels(y.exp)

  # Controling data Y.exp
  if (any(is.na(Y.exp))) {
    stop("Y.exp cannot contain null values")
  }

  if (nrow(Y.exp) != nrow(Y.hat)) {
    stop("Y.exp and X must have the same amount of rows")
  }

  # Residuals
  resid <- Y.exp - Y.hat

  # Confusion Matrix
  total <- sum
  mc <- addmargins(table(y.exp, factor(y.hat, colnames(Y.exp))), FUN = total, quiet = TRUE)
  mc <- cbind(mc, pc.correct=diag(mc)/mc[,"total"] * 100)

  # Error rate
  error <- round(1 - sum(diag(mc[,1:ncol(Y.exp)])) / sum(mc[ncol(Y.exp)+1, ncol(Y.exp)+1]), 2)

  return(list(Residuals = resid,
              Conf.Mat = mc,
              Error = error))
}
