#Descriptive Statistics
#Y
tabY=table(Y.quali)
cbind(Modalily=tab, Effective=sum(tab), Percentage=round(tab/sum(tab)*100,2))
#X
tabX=t(apply(X.init,2,function(x){rbind(names(x),length(x),min(x),max(x),mean(x,2),round(sd(x),2))}))
colnames(tabX)=list("Effective","Min","Max","Mean","Sd")
#Correlation matrix
install.packages("corrplot")
library(corrplot)
cor.mat=cor(cbind(X.init,Y.init),cbind(X.init,Y.init))
corrplot(cor.mat)