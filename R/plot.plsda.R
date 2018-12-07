plot.plsda <- function(model) {

  #Représentation graphique de la qualité
  quality <- model$Quality
  barplot(quality,
          beside=TRUE,
          main="Model quality by #Comp",
          xlab="Component",
          ylab="Quality",
          col=c("orange","green4"),
          space=c(0.05,0.2),
          legend=rownames(quality))

  #Représentation graphique VIP
  VIP <- model$VIP
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
