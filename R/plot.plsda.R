#' Fonction plot.plsda
#'
#' @description Plot the quality of the model and the VIP for each variable and component.
#' @param x A plsda object containing a trained model. The model can be generated with the fit function of this package.
#' @param ... Use to comply generic model
#' @return None.
#' @keywords VIP, quality, pls, plsda
#' @export
#' @method plot plsda
#' @importFrom graphics abline barplot
#' @examples
#' model <- fit(Species ~ ., data = iris)
#' plot(model)
plot.plsda <- function(x, ...) {

  # Graphic representation of the model quality
  quality <- x$Quality
  barplot(quality,
          beside=TRUE,
          main="Model quality by #Comp",
          xlab="Component",
          ylab="Quality",
          col=c("orange","green4"),
          space=c(0.05,0.2),
          legend=rownames(quality))

  #R VIP representation
  VIP <- x$VIP
  for (h in 1:ncol(VIP)) {
    barplot(VIP[,h],
            names.arg=rownames(VIP),
            main=c(paste("VIP for component",h),"Confidence interval at 0.95%"),
            xlab="Variables",
            ylab="VIP",
            col="blue")
    abline(a=0,b=0,h=0.8,v=0,lty=5)
    abline(a=0,b=0,h=1,v=0,lty=5)

  }
}
