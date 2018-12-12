#' Fonction fit
#'
#' @param data (The dataset to analyse)
#' @param modality (The column to fit)
#' @param ncomp (The number of components)
#' @param cv (If TRUE the package will compute the ncomp with a cross validation)
#' @keywords fit, pls, pls-da
#' @export
#' @examples
#' fit(Species ~ ., data = iris)
#' fit(Species ~ ., data = iris, 2)
#' fit(Species ~ ., data = iris, 4, cv = "ext", nfold = 30)
#' fit(Species ~ ., data = iris, 4, cv = "int", nfold = 10)
fit <- function(formula, data, ncomp = 2, cv = FALSE, nfold = 0) {

  #Reading the formula and setting X and Y
  if (!plyr::is.formula(formula)) {
    stop("formula must be a valid R formula")
  }

  # Reading the X
  X <- model.matrix(formula, data = data)
  X <- X[,-1] # Removing intercept

  # Reading the Y
  y <- model.response(model.frame(formula, data))
  if(is.factor(y)) {
    Y <- dummies::dummy(y)
    colnames(Y) <- levels(y) #TODO: vérifier que levels est dans le mêm eordre que dummies
  } else {
    stop("y must be a factor")
  }

  # Controling data
  if (any(is.na(X)) || any(is.na(Y))) {
    stop("X and Y cannot contain null values")
  }

  if ((ncol(X)<2) || (ncol(Y)<2)) {
    stop("X and Y must have more than 1 column each")
  }

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same amount of rows")
  }

  if ((ncomp > nrow(X)-1) || (ncomp <= 0))
  {
    stop("ncomp must be >0 and <n")
  }

  if (nfold > nrow(X)) {
    stop("nfold must not be > to number of rows in X")
  }

  # Launching PLS algorithm
  if (cv == "ext") {
    cv <- cross_validation(X, Y, ncomp, nfold)
    model <- pls(X, Y, cv$N.Comp, cv.int = FALSE, nfold)
    model$CV <- cv$PRESS
  }
  else if (cv == "int") {
    model <- pls(X, Y, ncomp, cv.int = TRUE, nfold)
  }
  else {
    model <- pls(X, Y, ncomp, cv.int = FALSE)
  }

  return(model)
}
