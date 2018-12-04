pls <- function(X, Y, nc) {

  X.init=as.matrix(X)
  Y.init=as.matrix(Y)

  # Scaling Y
  X <- scale(X)
  Y <- scale(Y)

  #Initialisation des variables générales
  n=nrow(X)
  px=ncol(X)
  qy=ncol(Y)
  namesX<-colnames(X)
  namesY<-colnames(Y)

  #Initialisation des matrices
  W=matrix(0, px, nc)
  P=matrix(0, px, nc)
  Q=matrix(0, qy, nc)

  #Initialisation des premieres variables de l'algo
  A=t(X)%*%Y
  M=t(X)%*%X
  C=diag(nrow(A))

  #Boucle : Algo PLS
  for (h in 1:nc){

    #Construction de w
    AA=t(A)%*%A
    e=eigen(AA)
    q=e$vectors[,which.max(e$values)]
    w=C%*%A%*%q
    w=w/sqrt(sum(w^2))

    #Construction de p
    p=M%*%w
    c=as.numeric(t(w)%*%M%*%w)
    p=p/c

    #Construction de q
    q=t(A)%*%w/c

    #Attribution de w,p,q dans leur matrice
    W[,h]=w
    P[,h]=p
    Q[,h]=q

    #Ré initialisation
    A=A-(c*p%*%t(q))
    M=M-(c*p%*%t(p))
    C=C-(w%*%t(p))
  }

  #Calcul de T : matrice des composantes
  Th=X%*%W
  colnames(Th)=paste(rep("Comp",nc),1:nc,sep=" ")

  #Calcul des coefficients standards beta
  B=W%*%t(Q)
  #Calcul des coefficients de la régression PLS
  Br=diag(1/apply(X.init,2,sd)) %*% B %*% diag(apply(Y.init,2,sd))
  dimnames(Br)=list(namesX, namesY)
  #Calcul de la constante
  Cte=as.vector(apply(Y.init,2,mean)-apply(X.init,2,mean)%*%Br)
  #Matrice finale des coefficients
  coeffs=rbind(Constante=Cte,Br)

  #Détail du R² et redondance
  #Pour X (non cumulé et cumulé)
  R2x=cor(X.init,Th)^2
  colnames(R2x)=paste(rep("Comp",nc),1:nc,sep=" ")
  Var.Expliquee.X=rbind(R2x,Redondance=colMeans(R2x))
  R2x.cum=t(apply(R2x,1,cumsum))
  Var.Expliquee.X.Cum=rbind(R2x.cum,Redondance=colMeans(R2x.cum))

  #Pour Y (non cumulé et cumulé)
  R2y=cor(Y.init,Th)^2
  colnames(R2y)=paste(rep("Comp",nc),1:nc,sep=" ")
  Var.Expliquee.Y=rbind(R2y,Redondance=colMeans(R2y))
  R2y.cum=t(apply(R2y,1,cumsum))
  Var.Expliquee.Y.Cum=rbind(R2y.cum,Redondance=colMeans(R2y.cum))

  #Qualité du modèle globale
  qualite=rbind(Var.Expliquee.X.Cum[nrow(Var.Expliquee.X.Cum),], Var.Expliquee.Y.Cum[nrow(Var.Expliquee.Y.Cum),])
  rownames(qualite)=c("R²Xcum","R²Ycum")

  #Variable Importance in the Projection (VIP)
  Rd=matrix(0,nc,nc)
  Rdy=colMeans(R2y)
  for (h in 1:nc){
    Rd[1:h,h]=Rdy[1:h]
  }
  VIP=sqrt((W^2) %*% Rd %*% diag(px/cumsum(Rdy), nc, nc))
  dimnames(VIP)=list(namesX, paste(rep("VIP Comp",nc),1:nc,sep=" "))

  return(list(Matrice.Comp=Th,
              Coeffs=coeffs,
              Var.Expliquee.X=Var.Expliquee.X,
              Var.Expliquee.X.Cum=Var.Expliquee.X.Cum,
              Var.Expliquee.Y=Var.Expliquee.Y,
              Var.Expliquee.Y.Cum=Var.Expliquee.Y.Cum,
              Qualite=qualite,
              VIP = VIP))
}
