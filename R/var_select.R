#' Fonction var_select
#'
#' @description This function takes a plsda model and selects variables from the X matrix using the VIP values returned.
#' @param model A plsda object containing a trained model. The model can be generated with the fit function of this package.
#' @param vs_crit Treshold value for the variables selection (vs=TRUE). Default value : 1 (fit fonction)
#' @param X The X matrix of predictors
#' @param Y The Y matrix of binarized response
#' @param ncomp The number of component to be used
#' @keywords fit, pls, plsda, VIP, variable, selection
#' @export
var_select <- function(model, vs_crit, X, Y, ncomp){

  # VIP matrix manipulation with the threshold
  vip <- model$VIP >= vs_crit
  m_vip <- vip[-which(vip == FALSE),]
  rm_var <- nrow(vip) - nrow(m_vip)

  # Variation depending on the VIP matrix and the variables selection
  if (rm_var == 0){
    print("No variable selection : Vs_crit < VIP")
  } else {
    print(paste("Variables removed :", rm_var))

    #Re-creating a new X matrix depending on the names
    name_x <- rownames(m_vip)
    new_x <- X[,name_x]

    #Creating a new model
    model <- pls(new_x, Y, ncomp, cv = FALSE)
  }

  return(model)
}
