#' Fonction fit
#'
#' @param data (The dataset to analyse)
#' @param modality (The column to fit)
#' @param ncomp (The number of components)
#' @param cv (If the package should compute the ncomp with a cross validation)
#' @keywords fit, pls, plsda
#' @examples
#' fit("test")
fit <- function(data, modality=1, ncomp=2, cv=TRUE) {
  
  # Call to pls package
  library(pls)

  # Data formatting
  Y <- as.matrix(data.frame(model.matrix( ~ iris[,modality] - 1, data=iris)))
  X <- as.matrix(iris[, -modality, drop=FALSE])

  if (cv) {
    n <- cross_validation(Y, X, ncomp)
  }
  else {
    n <- ncomp
  }
  
  # Call to pls function  
  model <- pls::plsr(Y ~ X, ncomp = n)
  
  return(model)
}
