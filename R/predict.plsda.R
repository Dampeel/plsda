#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @param n (The number of components)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict.plsda <- function(model, newdata, n) {

  res <- predict(model, newdata)[,,n]
  
  return(res)
}
