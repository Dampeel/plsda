summary.plsda <- function(model) {
  cat("Package PLSDA - Université Lyon 2, SISE, 2018\n")
  cat("\n")
  cat("Coefficients\n")
  print(model$Coeffs)
  cat("\n")
  cat("Model quality\n")
  print(model$Quality)
  cat("\n")
  cat("VIP\n")
  print(model$VIP)
}
