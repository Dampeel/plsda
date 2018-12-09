#' Fonction cross_validation
#'
#' @param X (The X)
#' @param Y (The Y)
#' @param ncomp (The number of component to be used)
#' @param nfold (The number of folds to be used for cross-validation. 0 is equivalent to Leave One Out)
#' @keywords fit, pls, plsda
#' @export
cross_validation <- function(X, Y, ncomp, nfold) {

  PRESS <- NULL

  for (n in 1:ncomp) {
    press <- NULL

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

      model <- pls(Xapp, Yapp, n, cv.int = FALSE, nfold = 0)
      prediction <- predict.plsda(model, Xtest)

      press[i] <- sum((prediction$Y.hat - Ytest)^2)
    }

    PRESS[n] <- sum(press)
  }

  ncomp <- which.min(PRESS)

  #TODO: plot des PRESS

  return(list(N.Comp = ncomp,
              PRESS = PRESS))
}
