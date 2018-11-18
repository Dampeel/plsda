#' Fonction cross_validation
#'
#' @param X (The X)
#' @param Y (The Y)
#' @param ncomp (The number of component to be used)
#' @keywords fit, pls, plsda
cross_validation <- function(X, Y, ncomp) {
  
  # Call to pls package
  library(pls)
  
  R2_mean <- NULL
  for (n in 1:ncomp) {
    
    # Leave one out
    R2 <- NULL
    for (i in 1:nrow(X)) {
      Xapp <- X[-i,,drop=FALSE]
      Yapp <- Y[-i,,drop=FALSE]
      Xtest <- X[i,,drop=FALSE]
      Ytest <- Y[i,,drop=FALSE]
      
      model <- pls::plsr(Yapp ~ Xapp, ncomp = n)
      prediction <- pls::predict(model, newdata=Xtest)[,,n] #on récupère le résultat de la dernière composante
      
      R2[i] <- sum((prediction-Ytest)^2)
    }
    
    R2_mean[n] <- mean(R2)
  }
  
  return(which.min(R2_mean))
}
