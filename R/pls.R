pls <- function(X, Y, nc) {
  
  X.init=as.matrix(X)
  Y.init=as.matrix(Y)
  
  # Scaling Y
  X <- scale(X)
  Y <- scale(Y)
  
  # Initialization of general variables
  n=nrow(X)
  px=ncol(X)
  qy=ncol(Y)
  namesX<-colnames(X)
  namesY<-colnames(Y)
  
  # Initialization of matrices
  W=matrix(0, px, nc)
  P=matrix(0, px, nc)
  Q=matrix(0, qy, nc)
  
  # Initialization of the first variables for the algo
  A=t(X)%*%Y
  M=t(X)%*%X
  C=diag(nrow(A))
  
  # Loop : Algo PLS
  for (h in 1:nc){
    
    # Construction of w
    AA=t(A)%*%A
    e=eigen(AA)
    q=e$vectors[,which.max(e$values)]
    w=C%*%A%*%q
    w=w/sqrt(sum(w^2))
    
    # Construction of p
    p=M%*%w
    c=as.numeric(t(w)%*%M%*%w)
    p=p/c
    
    # Construction of q
    q=t(A)%*%w/c
    
    # Assignment of w, p, q in their matrix
    W[,h]=w
    P[,h]=p
    Q[,h]=q
    
    # Update of the first variables
    A=A-(c*p%*%t(q))
    M=M-(c*p%*%t(p))
    C=C-(w%*%t(p))
  }
  
  # Calculation of matrix of components
  Th=X%*%W
  colnames(Th)=paste(rep("Comp",nc),1:nc,sep=" ")
  
  # Calculation of standards beta coefficients
  B=W%*%t(Q)
  # Calculation of PLS regression coefficients 
  Br=diag(1/apply(X.init,2,sd)) %*% B %*% diag(apply(Y.init,2,sd))
  dimnames(Br)=list(namesX, namesY)
  # Constant calculation
  Ct=as.vector(apply(Y.init,2,mean)-apply(X.init,2,mean)%*%Br)
  # Final matrix of the coefficients
  coeffs=rbind(Constant=Ct,Br)
  
  # Detail of R² and redundancy
  # For X (not cumulative and cumulative)
  Rx=cor(X.init,Th)^2
  colnames(Rx)=paste(rep("Comp",nc),1:nc,sep=" ")
  if (nc==1){
    Var.Explained.X=rbind(Rx,Redundancy=mean(Rx))
    Rx.cum=as.matrix(apply(Rx,1,cumsum))
    Var.Explained.X.Cum=rbind(Rx.cum,Redundancy=mean(Rx.cum))
  } else {
    Var.Explained.X=rbind(Rx,Redundancy=colMeans(Rx))
    Rx.cum=t(apply(Rx,1,cumsum))
    Var.Explained.X.Cum=rbind(Rx.cum,Redundancy=colMeans(Rx.cum))
  }
  
  # For Y (not cumulative and cumulated)
  Ry=cor(Y.init,Th)^2
  colnames(Ry)=paste(rep("Comp",nc),1:nc,sep=" ")
  if (nc==1){
    Var.Explained.Y=rbind(Ry,Redundancy=mean(Ry))
    Ry.cum=as.matrix(apply(Ry,1,cumsum))
    Var.Explained.Y.Cum=rbind(Ry.cum,Redundancy=mean(Ry.cum))
  } else {
    Var.Explained.Y=rbind(Ry,Redundancy=colMeans(Ry))
    Ry.cum=t(apply(Ry,1,cumsum))
    Var.Explained.Y.Cum=rbind(Ry.cum,Redundancy=colMeans(Ry.cum))
  }
  
  # Quality of the overall model
  quality=rbind(Var.Explained.X.Cum[nrow(Var.Explained.X.Cum),], Var.Explained.Y.Cum[nrow(Var.Explained.Y.Cum),])
  rownames(quality)=c("R²Xcum","R²Ycum")
  
  # Variable Importance in the Projection (VIP)
  R=matrix(0,nc,nc)
  Ry_means=colMeans(Ry)
  for (h in 1:nc){
    R[1:h,h]=Ry_means[1:h]
  }
  VIP=sqrt((W^2) %*% R %*% diag(px/cumsum(Ry_means)))
  dimnames(VIP)=list(namesX, paste(rep("VIP Comp",nc),1:nc,sep=" "))
  
  # Variables returned
  mylist=list(Comp.Matrix = Th,
              Coeffs = coeffs,
              Var.Explained.X = Var.Explained.X,
              Var.Explained.X.Cum = Var.Explained.X.Cum,
              Var.Explained.Y = Var.Explained.Y,
              Var.Explained.Y.Cum = Var.Explained.Y.Cum,
              Quality = quality,
              VIP = VIP)
  class(mylist)="PLSDA"
  return(mylist)
}