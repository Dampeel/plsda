#' Fonction validation
#'
#' @description This function takes a plsda-pred, a prediction computed by the predict function of the plsda package and compare it to the y.exp vector provide.
#'     It will returns the residuals, the confusion matrix and the error rate.
#' @param result The result obtained with the predict.plsda of this package, must contains y.hat and Y.hat as a list.
#' @param y.exp The expected y, it will be confronted to y.hat found in the result parameter, so it must have the same number of items/
#' @return A plsda-valid object that contains residuals, confusion matrix and error rate.
#' @keywords confusion matrix, residuals, predict, pls, plsda
#' @export
#' @importFrom stats addmargins
#' @examples
#' model <- fit(Species ~ ., data = iris)
#' result <- predict(model, iris[,1:4])
#' validation(result, iris[,5])
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

  result <- structure(list(
    Residuals = resid,
    Conf.Mat = mc,
    Error = error))
  class(result) <- "plsda-valid"
  return(result)
}
