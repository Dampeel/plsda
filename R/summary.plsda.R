#' Fonction summary.plsda
#'
#' @description Displays a plsda object attributes.
#' @param object A plsda object containing a trained model. The model can be generated with the fit function of this package.
#' @param ... Use to comply generic model
#' @return None.
#' @keywords summary, quality, VIP, predict, pls, plsda
#' @export
#' @method summary plsda
#' @examples
#' model <- fit(Species ~ ., data = iris)
#' summary(model)
summary.plsda <- function(object, ...) {
  cat("Package PLSDA - Universite Lyon 2, SISE, 2018\n")
  cat("\n")
  cat("Coefficients\n")
  print(object$Coeffs)
  cat("\n")
  cat("Model quality\n")
  print(object$Quality)
  cat("\n")
  cat("VIP\n")
  print(object$VIP)
  cat("\n")
  cat("Beauquel Camille, Leal Hugo, Peelman Damien\n")
}
