#' Fonction fit
#'
#' @param data (The dataset to analyse)
#' @param modality (The column to fit)
#' @param ncomp (The number of components)
#' @param cv (If the package should compute the ncomp with a cross validation)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' fit("test")
fit <- function(formula, data, ncomp=2, cv = FALSE, nfold = 0) {

  #Reading the formula and setting X and Y
  if (!plyr::is.formula(formula)) {
    stop("formula must be a valid R formula")
  }

  # Reading the X
  X <- model.matrix(update(formula, ~ . -1), data = data)
  # TODO: transformer les factor éventuels en indicatrices

  # Reading the Y
  y <- model.response(model.frame(formula, data))
  if(is.factor(y)) {
    Y <- as.matrix(data.frame(model.matrix( ~ y - 1, data=data)))
  } else {
    stop("y must be a factor")
  }

  print(Y)

  #Controling data
  if (any(is.na(X)) || any(is.na(Y))) {
    stop("X and Y cannot contain null values")
  }

  if ((ncol(X)<2) || (ncol(Y)<2)) {
    stop("X and Y must have more than 1 column each")
  }

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same amount of rows")
  }

  if ((ncomp > nrow(X)-1) || (ncomp <= 1))
  {
    stop("ncomp must be >1 and <n")
  }

  if (nfold > nrow(X)) {
    stop("nfold must not be > to number of rows in X")
  }

  # Launching PLS algorithm
  if (cv) {
    n <- cross_validation(X, Y, ncomp, nfold)
  }
  else {
    n <- ncomp
  }

  # Call to pls function
  model <- pls(X, Y, n)

  return(model)
}
