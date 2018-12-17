#' Fonction fit
#'
#' @description Generate a plsda model according to formula and data provided. This model can be cross-validated.
#' @param formula A valid statistical R formula.
#' @param data The data to be used to rain the PLS-DA model. I must fit to the formula.
#'     The response must be a qualitative variable. The predictors must be a set of quantitative variables.
#' @param ncomp The number of component to compute. If cross-validation is activated it will be used as the maximum number of component to return.
#' @param cv If TRUE a cross-validation will be done. Default is FALSE.
#' @param nfold If cv parameter is TRUE, nfold will specify the kind of cross-validation. Default is 0.
#'     A value of 0 will be equivalent to a "Leave One Out" cross-validation. If > 0 it will do a k-fold cross-validation with k = nfold.
#' @param vs If TRUE an automatic variable selection will be done. Default is FALSE.
#' @param vs_crit Treshold value for the variables selection (vs=TRUE). Default value : 1 (fit fonction)
#' @return A plsda object that contains the component matrix, the trained model as coefficients, the explained variance on X and Y, VIP values, quality of the model and cross-validation results if asked
#' @keywords fit, pls, plsda
#' @export
#' @importFrom stats model.frame model.matrix model.response
#' @examples
#' fit(Species ~ ., data = iris)
#' fit(Species ~ ., data = iris, 2)
#' fit(Species ~ ., data = iris, vs = TRUE)
#' fit(Species ~ ., data = iris, 4, cv = TRUE, nfold = 10)
fit <- function(formula, data, ncomp = 2, cv = FALSE, nfold = 0, vs = FALSE, vs_crit = 1) {

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
    colnames(Y) <- levels(y) #TODO: vérifier que levels est dans le même ordre que dummies
  } else {
    stop("y must be a factor")
  }

  # Controling data
  if (any(is.na(X)) || any(is.na(Y))) {
    stop("X and Y cannot contain NA values")
  }

  if ((ncol(X)<2) || (ncol(Y)<2)) {
    stop("X and Y must have more than 1 column each")
  }

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same amount of rows")
  }

  if ((ncomp > ncol(X)) || (ncomp <= 0))
  {
    stop("ncomp must be >0 and < to the number of variables")
  }

  if ((nfold < 0) || (nfold > nrow(X))) {
    stop("nfold must not be > to number of rows in X and must be > 0")
  }

  # Launching PLS algorithm
  if (cv) {
    model <- pls(X, Y, ncomp, cv = TRUE, nfold)
  }
  else {
    model <- pls(X, Y, ncomp, cv = FALSE)
  }

  # Variables selection according to VIP criteria
  if (vs) {
    model <- var_select(model, vs_crit, X, Y, ncol(model$Coeffs))
  }

  return(model)
}
