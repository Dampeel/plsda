#' Fonction stat_desc
#'
#' @description Descriptive Statistics
#' @param X.init X.init
#' @param Y.quali Y.quali
#' @export
stats_desc <- function(X.init, Y.quali) {

  #Y
  Y.init = dummies::dummy(Y.quali)
  tabY=table(Y.quali)
  tabY=cbind(Effective=tabY, Percentage=round(tabY/sum(tabY)*100,2))
  print(tabY)

  #X
  tabX=t(apply(X.init,2,function(x){rbind(names(x),length(x),min(x),max(x),mean(x,2),round(sd(x),2))}))
  colnames(tabX)=list("Effective","Min","Max","Mean","Sd")
  print(tabX)

  #Correlation matrix
  cor.mat=cor(cbind(X.init,Y.init),cbind(X.init,Y.init))
  corrplot::corrplot(cor.mat)
}
