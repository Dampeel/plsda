#' Fonction cross_validation
#'
#' @param X (The X)
#' @param Y (The Y)
#' @param ncomp (The number of component to be used)
#' @param nfold (The number of folds to be used for cross-validation. 0 is equivalent to Leave One Out)
#' @keywords fit, pls, plsda
#' @export
cross_validation <- function(X, Y, ncomp, nfold) {

  PRESS_mean <- NULL

  # TODO: vérifier si on peut faire avec moins de 2 composantes

  for (n in 2:ncomp) {
    PRESS <- NULL

    # Leave one out
    if (nfold == 0) {
      nfold <- nrow(X)
    }

    # Shuffling data
    shuffle <- sample(nrow(X))
    X <- X[shuffle,]
    Y <- Y[shuffle,]

    # Creating folds
    folds <- cut(seq(1, nrow(X)), breaks = nfold, labels=FALSE)

    for(i in 1:nfold) {

      # Segmenting data indexes
      indexes <- which(folds == i, arr.ind = TRUE)

      Xapp <- as.matrix(X[-indexes,, drop=FALSE])
      Yapp <- as.matrix(Y[-indexes,, drop=FALSE])
      Xtest <- as.matrix(X[indexes,, drop=FALSE])
      Ytest <- as.matrix(Y[indexes,, drop=FALSE])

      print(nrow(Xapp))
      print(nrow(Xtest))
      model <- pls(Xapp, Yapp, n)

      prediction <- predict.plsda(model, Xtest)
      PRESS[i] <- sum((prediction$Y - Ytest)^2)
    }

    PRESS_mean[n] <- mean(PRESS)
  }

  return(which.min(PRESS_mean))
}
