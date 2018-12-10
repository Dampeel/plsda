pls_low <- function(W, P, Q, A, M, C, h) {

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

  return(list(W=W, P=P, Q=Q, A=A, M=M, C=C))
}

pls <- function(X, Y, nc, cv.int, nfold) {

  X.init=as.matrix(X)
  Y.init=as.matrix(Y)

  # Scaling
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
  Th=matrix(0, px, nc)

  # Initialization of the first variables for the algo
  A=t(X)%*%Y
  M=t(X)%*%X
  C=diag(nrow(A))

  RESS <- rep(NA, nc)

  if (cv.int) {
    PRESS <- rep(NA, nc)
    Q2 <- rep(NA, nc)
  }

  # Loop : Algo PLS
  h <- 1
  repeat {

    # Cross validation
    if (cv.int) {

      # Leave one out if nfold = 0
      if (nfold == 0) {
        nfold <- nrow(X)
      }

      # Shuffling data
      shuffle <- sample(nrow(X))
      X.cv <- X.init[shuffle,]
      Y.cv <- Y.init[shuffle,]

      # Creating folds
      folds <- cut(seq(1, nrow(X)), breaks = nfold, labels=FALSE)

      press.cv <- NULL

      for (i in 1:nfold) {

        # Segmenting
        indexes <- which(folds == i, arr.ind = TRUE)
        Xapp.init <- as.matrix(X.cv[-indexes,, drop=FALSE])
        Yapp.init <- as.matrix(Y.cv[-indexes,, drop=FALSE])
        Xtest.init <- as.matrix(X.cv[indexes,, drop=FALSE])
        Ytest.init <- as.matrix(Y.cv[indexes,, drop=FALSE])

        # Scaling
        Xapp <- scale(Xapp.init)
        Yapp <- scale(Yapp.init)

        # PLS on train
        # Initialization of general variables
        n.cv <- nrow(Xapp)

        # Initialization of matrices
        W.cv <- matrix(0, px, h)
        P.cv <- matrix(0, px, h)
        Q.cv <- matrix(0, qy, h)

        # Initialization of the first variables for the algo
        A.cv <- t(Xapp) %*% Yapp
        M.cv <- t(Xapp) %*% Xapp
        C.cv <- diag(nrow(A.cv))

        for (h.cv in 1:h) {
          res.cv <- pls_low(W.cv, P.cv, Q.cv, A.cv, M.cv, C.cv, h.cv)
          W.cv <- res.cv$W
          P.cv <- res.cv$P
          Q.cv <- res.cv$Q
          A.cv <- res.cv$A
          M.cv <- res.cv$M
          C.cv <- res.cv$C
        }

        # Calculation of matrix of components
        Th.cv <- Xapp %*% W.cv

        # Calculation of standards beta coefficients
        B.cv <- W.cv %*% t(Q.cv)

        # Calculation of PLS regression coefficients
        Br.cv <- diag(1/apply(Xapp.init, 2, sd)) %*% B.cv %*% diag(apply(Yapp.init, 2, sd))

        # Constant calculation
        Ct.cv <- as.vector(apply(Yapp.init,2,mean) - apply(Xapp.init,2,mean) %*% Br.cv)

        # Evaluation
        Y.hat.cv <- Xtest.init %*% Br.cv + matrix(rep(Ct.cv, each=nrow(Xtest.init)), nrow(Xtest.init), ncol(Br.cv))
        press.cv[i] <- sum((Y.hat.cv - Ytest.init)^2)
      }

      PRESS[h] <- sum(press.cv)

      if (h > 1) {
        Q2[h] <- 1 - (PRESS[h]/RESS[h-1])
        if (Q2[h] < 0.1) {
          break
        }
      }
    }

    if (h > nc) {
      break
    }

    res <- pls_low(W, P, Q, A, M, C, h)
    W <- res$W
    P <- res$P
    Q <- res$Q
    A <- res$A
    M <- res$M
    C <- res$C

    # Calculation of matrix of components
    Th=X%*%W
    colnames(Th)=paste(rep("Comp",nc),1:h,sep=" ")

    # Calculation of standards beta coefficients
    B=W%*%t(Q)

    # Calculation of PLS regression coefficients
    Br=diag(1/apply(X.init,2,sd)) %*% B %*% diag(apply(Y.init,2,sd))
    dimnames(Br)=list(namesX, namesY)

    # Constant calculation
    Ct=as.vector(apply(Y.init,2,mean)-apply(X.init,2,mean)%*%Br)

    # Evaluation
    Y.hat <- X.init %*% Br + matrix(rep(Ct, each=nrow(X.init)), nrow(X.init), ncol(Br))
    RESS[h] <- sum((Y.hat - Y.init)^2)

    h <- h + 1
  }

  # Adaptation to the real number of components
  if (cv.int) {
    nc <- h - 1
    W <- W[, 1:nc, drop=FALSE]
    P <- P[, 1:nc, drop=FALSE]
    Q <- Q[, 1:nc, drop=FALSE]
    Th <- Th[, 1:nc, drop=FALSE]
  }

  # Final matrix of the coefficients
  coeffs=rbind(Constant=Ct, Br)

  # Cross validation results
  CV <- NULL
  if (cv.int) {
    q2cum <- NULL
    for (j in 1:nc) {
      q2cum[j] <- prod(PRESS[1:j]/RESS[1:j])
    }
    Q2cum <- 1 - q2cum
    CV <- cbind(PRESS[1:nc], RESS[1:nc], Q2[1:nc], rep(0.1, nc), Q2cum)
    dimnames(CV) = list(1:nc, c("PRESS","RESS","Q2","LimQ2","Q2cum"))
  }

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
  VIP=matrix(0,ncol(X.init), nc)
  for (i in 1:ncol(X.init)) {
    for(j in 1:nc){
      VIP[i,j]=sqrt(px/sum(Ry[,1:j])*sum(as.matrix(Ry[,1:j])%*%(W[i,1:j]^2)))
    }
  }
  dimnames(VIP)=list(namesX, paste(rep("VIP Comp",nc),1:nc,sep=" "))

  # Variables returned
  result=list(Comp.Matrix = Th,
              Coeffs = coeffs,
              Var.Explained.X = Var.Explained.X,
              Var.Explained.X.Cum = Var.Explained.X.Cum,
              Var.Explained.Y = Var.Explained.Y,
              Var.Explained.Y.Cum = Var.Explained.Y.Cum,
              Quality = quality,
              VIP = VIP,
              CV = CV)
  class(result)="plsda"
  return(result)
}
