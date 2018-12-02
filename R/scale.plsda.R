scale.plsda <- function(X, X.means, X.sds) {
  res <- (X - X.means) / X.sds
  return(res)
}

unscale.plsda <- function(X, X.means, X.sds) {
  res <- (X * X.sds) + X.means
  return(res)
}
