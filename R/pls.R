pls_algo <- function(W, P, Q, A, M, C, h) {

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

pls_coeffs <- function(W, Q, X.init, Y.init) {

  # Calculation of standards beta coefficients
  B <- W %*% t(Q)

  # Calculation of PLS regression coefficients
  Br <- diag(1/apply(X.init, 2, sd)) %*% B %*% diag(apply(Y.init, 2, sd))

  # Constant calculation
  Ct <- as.vector(apply(Y.init,2,mean) - apply(X.init,2,mean) %*% Br)

  return(list(Br=Br, Ct=Ct))
}

pls_eval <- function(Br, Ct, X.init) {
  Y.hat <- X.init %*% Br + matrix(rep(Ct, each=nrow(X.init)), nrow(X.init), ncol(Br))
  return(Y.hat)
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

  # Initialization of the first variables for the algo
  A=t(X)%*%Y
  M=t(X)%*%X
  C=diag(nrow(A))

  RESS <- NULL

  # Initializing Cross Validation
  if (cv.int) {
    PRESS <- NULL
    Q2 <- NULL

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

    Xapp.init <- NULL
    Yapp.init <- NULL
    Xtest.init <- NULL
    Ytest.init <- NULL
    Xapp <- NULL
    Yapp <- NULL
    n.cv <- NULL
    W.cv <- NULL
    P.cv <- NULL
    Q.cv <- NULL
    A.cv <- NULL
    M.cv <- NULL
    C.cv <- NULL

    # Initializing variables for each fold
    for (i in 1:nfold) {

      # Segmenting
      indexes <- which(folds == i, arr.ind = TRUE)
      Xapp.init[[i]] <- as.matrix(X.cv[-indexes,, drop=FALSE])
      Yapp.init[[i]] <- as.matrix(Y.cv[-indexes,, drop=FALSE])
      Xtest.init[[i]] <- as.matrix(X.cv[indexes,, drop=FALSE])
      Ytest.init[[i]] <- as.matrix(Y.cv[indexes,, drop=FALSE])

      # Scaling
      Xapp[[i]] <- scale(Xapp.init[[i]])
      Yapp[[i]] <- scale(Yapp.init[[i]])

      # Initialization of general variables
      n.cv[[i]] <- nrow(Xapp[[i]])

      # Initialization of matrices
      W.cv[[i]] <- matrix(0, px, nc)
      P.cv[[i]] <- matrix(0, px, nc)
      Q.cv[[i]] <- matrix(0, qy, nc)

      # Initialization of the first variables for the algo
      A.cv[[i]] <- t(Xapp[[i]]) %*% Yapp[[i]]
      M.cv[[i]] <- t(Xapp[[i]]) %*% Xapp[[i]]
      C.cv[[i]] <- diag(nrow(A.cv[[i]]))
    }
  }

  # Loop : Algo PLS
  h <- 1
  while(h <= nc) {

    # Cross validation
    if (cv.int) {

      press.cv <- NULL

      for (i in 1:nfold) {

        # Applying PLS algo on train
        res <- pls_algo(W.cv[[i]], P.cv[[i]], Q.cv[[i]], A.cv[[i]], M.cv[[i]], C.cv[[i]], h)
        W.cv[[i]] <- res$W
        P.cv[[i]] <- res$P
        Q.cv[[i]] <- res$Q
        A.cv[[i]] <- res$A
        M.cv[[i]] <- res$M
        C.cv[[i]] <- res$C

        # Calculating coefficients ans constant
        r2 <- pls_coeffs(W.cv[[i]], Q.cv[[i]], Xapp.init[[i]], Yapp.init[[i]])
        Br.cv <- r2$Br
        Ct.cv <- r2$Ct

        # Evaluation
        Y.hat.cv <- pls_eval(Br.cv, Ct.cv, Xtest.init[[i]])
        press.cv[i] <- sum((Y.hat.cv - Ytest.init[[i]])^2)
      }

      PRESS[h] <- sum(press.cv)

      if (h > 1) {
        Q2[h-1] <- 1 - (PRESS[h]/RESS[h-1])
        if (Q2[h-1] < 0.05) {
          break
        }
      }
    }

    # Launching PLS algorithm
    a <- pls_algo(W, P, Q, A, M, C, h)
    W <- a$W
    P <- a$P
    Q <- a$Q
    A <- a$A
    M <- a$M
    C <- a$C

    # Calculating coefficients ans constant
    c <- pls_coeffs(W, Q, X.init, Y.init)
    Br <- c$Br
    Ct <- c$Ct

    # Evaluation
    Y.hat <- pls_eval(Br, Ct, X.init)
    RESS[h] <- sum((Y.hat - Y.init)^2)

    h <- h + 1
  }

  # If Cross Validation, adaptation to the real number of components
  if (cv.int) {
    nc <- h - 1
    W <- W[, 1:nc, drop=FALSE]
    P <- P[, 1:nc, drop=FALSE]
    Q <- Q[, 1:nc, drop=FALSE]
  }

  # Calculation of matrix of components
  Th <- X %*% W
  colnames(Th) <- paste(rep("Comp", nc), 1:nc ,sep=" ")

  # Final matrix of the coefficients
  dimnames(Br)=list(namesX, namesY)
  coeffs <- rbind(Constant = Ct, Br)

  # Cross validation results
  CV <- NULL
  if (cv.int) {
    q2cum <- NULL
    for (j in 1:nc) {
      q2cum[j] <- prod(PRESS[1:j]/RESS[1:j])
    }
    Q2cum <- 1 - q2cum
    CV <- cbind(PRESS[1:nc], RESS[1:nc], Q2[1:nc], Q2cum)
    dimnames(CV) <- list(1:nc, c("PRESS", "RESS", "Q2", "Q2cum"))
  } else {
    CV <- RESS[1:nc]
  }

  # Detail of R² and redundancy
  # For X (not cumulative and cumulative)
  Rx <- cor(X.init,Th)^2
  colnames(Rx) <- paste(rep("Comp",nc),1:nc,sep=" ")
  if (nc==1){
    Var.Explained.X <- rbind(Rx,Redundancy=mean(Rx))
    Rx.cum <- as.matrix(apply(Rx,1,cumsum))
    Var.Explained.X.Cum <- rbind(Rx.cum,Redundancy=mean(Rx.cum))
  } else {
    Var.Explained.X <- rbind(Rx,Redundancy=colMeans(Rx))
    Rx.cum <- t(apply(Rx,1,cumsum))
    Var.Explained.X.Cum <- rbind(Rx.cum,Redundancy=colMeans(Rx.cum))
  }

  # For Y (not cumulative and cumulated)
  Ry <- cor(Y.init,Th)^2
  colnames(Ry) <- paste(rep("Comp",nc),1:nc,sep=" ")
  if (nc==1){
    Var.Explained.Y <- rbind(Ry,Redundancy=mean(Ry))
    Ry.cum=as.matrix(apply(Ry,1,cumsum))
    Var.Explained.Y.Cum <- rbind(Ry.cum,Redundancy=mean(Ry.cum))
  } else {
    Var.Explained.Y <- rbind(Ry,Redundancy=colMeans(Ry))
    Ry.cum <- t(apply(Ry,1,cumsum))
    Var.Explained.Y.Cum <- rbind(Ry.cum,Redundancy=colMeans(Ry.cum))
  }

  # Quality of the overall model
  quality <- rbind(Var.Explained.X.Cum[nrow(Var.Explained.X.Cum),], Var.Explained.Y.Cum[nrow(Var.Explained.Y.Cum),])
  rownames(quality) <- c("R²Xcum","R²Ycum")

  # Variable Importance in the Projection (VIP)
  VIP <- matrix(0,ncol(X.init), nc)
  for (i in 1:ncol(X.init)) {
    for(j in 1:nc){
      VIP[i,j] <- sqrt(px/sum(Ry[,1:j])*sum(as.matrix(Ry[,1:j])%*%(W[i,1:j]^2)))
    }
  }
  dimnames(VIP) <- list(namesX, paste(rep("VIP Comp",nc),1:nc,sep=" "))

  # Variables returned
  result <- list(Comp.Matrix = Th,
              Coeffs = coeffs,
              Var.Explained.X = Var.Explained.X,
              Var.Explained.X.Cum = Var.Explained.X.Cum,
              Var.Explained.Y = Var.Explained.Y,
              Var.Explained.Y.Cum = Var.Explained.Y.Cum,
              Quality = quality,
              VIP = VIP,
              CV = CV)
  class(result) <- "plsda"
  return(result)
}
