summary.plsda <- function(model) {
  print(model$Coeffs)
  print(model$Quality)
  print(model$VIP)
}