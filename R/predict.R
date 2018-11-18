#' Fonction predict
#'
#' @param model (The model to use)
#' @param newdata (The test data)
#' @keywords fit, pls, plsda
#' @export
#' @examples
#' predict()
predict <- function(model, newdata) {
  
  # Call to pls package
  library(pls)
  res<- drop(pls::predict(model, ncomp=model.ncomp, newdata=newdata))
  
  return(res)
}
