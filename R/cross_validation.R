#' Fonction cross_validation
#'
#' @param X (The X)
#' @param Y (The Y)
#' @param ncomp (The number of component to be used)
#' @keywords fit, pls, plsda
#' @export
cross_validation <- function(X, Y, ncomp) {

  R2_mean <- NULL
  for (n in 2:ncomp) {

    # Leave one out
    R2 <- NULL
    for (i in 1:nrow(X)) {
      Xapp <- as.matrix(X[-i,,drop=FALSE])
      Yapp <- as.matrix(Y[-i,,drop=FALSE])
      Xtest <- as.matrix(X[i,,drop=FALSE])
      Ytest <- as.matrix(Y[i,,drop=FALSE])

      model <- pls(Xapp, Yapp, n)
      prediction <- predict.plsda(model, Xtest, n)
      R2[i] <- sum((prediction$Y - Ytest)^2)
    }

    R2_mean[n] <- mean(R2)
  }

  print(which.min(R2_mean))
  return(which.min(R2_mean))
}
