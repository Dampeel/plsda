#' Fonction fit
#'
#' @param data (The dataset to analyse)
#' @param modality (The column to fit)
#' @param ncomp (The number of components)
#' @param cv (If TRUE the package will compute the ncomp with a cross validation)
#' @param plot (If TRUE the package swill display VIP and Quality plots)
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
    Y <- dummies::dummy(y)
    colnames(Y) <- levels(y) #TODO: vérifier que levels est dans le mêm eordre que dummies
  } else {
    stop("y must be a factor")
  }

  # TODO: controler que y a une seule colonne

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
    nc <- cv$N.Comp
    model <- pls(X, Y, nc, cv.int = FALSE, nfold)
  }
  else if (cv == "int") {
    model <- pls(X, Y, ncomp, cv.int = TRUE, nfold)
  }
  else {
    model <- pls(X, Y, ncomp, cv.int = FALSE, nfold)
  }

  return(model)
}
