summary.plsda <- function(model) {
  print("Package PLSDA - Université Lyon 2, SISE, 2018")
  print(model$Coeffs)
  print(model$Quality)
  print(model$VIP)
}
