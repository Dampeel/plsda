pls <- function(X, Y, ncomp = 2) {

  n = nrow(X)
  p = ncol(X)
  q = ncol(Y)

  # Scaling data
  # TODO: gérer le scaling en fonction de la PLS de Camille

  X.means <- apply(X, 2, mean)
  X.sds <- apply(X, 2, sd)
  X.old <- t(apply(X, 1, scale.plsda, X.means, X.sds))

  Y.means <- apply(Y, 2, mean)
  Y.sds <- apply(Y, 2, sd)
  Y.old <- t(apply(Y, 1, scale.plsda, Y.means, Y.sds))

  # Initializing
  Wh = matrix(0, p, ncomp)
  Uh = matrix(0, n, ncomp)
  Th = matrix(0, n, ncomp)
  Ch = matrix(0, q, ncomp)
  Ph = matrix(0, p, ncomp)
  bh = rep(0, ncomp)

  # =======================================================
  # PLSR1 algorithm
  # =======================================================
  h = 1
  repeat
  {
    u.new = Y.old[,1] # first column of Y.old
    w.old = rep(1, p)
    iter = 1
    repeat
    {
      w.new = t(X.old) %*% u.new / sum(u.new^2)
      w.new = w.new / sqrt(sum(w.new^2)) # normalize w.new
      t.new = X.old %*% w.new
      c.new = t(Y.old) %*% t.new / sum(t.new^2)
      u.new = Y.old %*% c.new / sum(c.new^2)
      w.dif = w.new - w.old
      w.old = w.new
      if (sum(w.dif^2)<1e-06 || iter==100) break
      iter = iter + 1
    }
    p.new = t(X.old) %*% t.new / sum(t.new^2)

    # deflate
    X.old = X.old - (t.new %*% t(p.new))
    Y.old = Y.old - (t.new %*% t(c.new))
    # populate
    Wh[,h] = w.new
    Uh[,h] = u.new
    Th[,h] = t.new
    Ch[,h] = c.new
    Ph[,h] = p.new
    bh[h] = t(u.new) %*% t.new
    # do we need to stop?
    if (is.null(ncomp) && crosval) {
      if (sum(Q2[h,]<0.0975) == q || h == ncomp) break
    } else {
      if (h == ncomp) break
    }
    h = h + 1
  } # end repeat

  # =======================================================
  # PLSR2 results
  # =======================================================
  Th = Th[,1:h]
  Ph = Ph[,1:h]
  Wh = Wh[,1:h]
  Uh = Uh[,1:h]
  Ch = Ch[,1:h]
  Ph = Ph[,1:h]
  # modified weights
  Ws = Wh %*% solve(t(Ph) %*% Wh)
  # std beta coeffs
  Bs = Ws %*% t(Ch)
  # regular coefficients
  Br = diag(1/apply(X,2,sd)) %*% Bs %*% diag(apply(Y,2,sd))
  cte = as.vector(apply(Y,2,mean) - apply(X,2,mean)%*%Br)
  # Y predicted
  Y.hat = X %*% Br + matrix(rep(cte,each=n),n,q)
  resids = Y - Y.hat  # residuals
  # correlations
  cor.xt = cor(X, Th)
  cor.yt = cor(Y, Th)
  cor.tu = cor(Th, Uh)
  cor.xu = cor(X, Uh)
  cor.yu = cor(Y, Uh)
  # explained variance
  R2x = cor(X, Th)^2  # R2 coefficients
  R2y = cor(Y, Th)^2  # R2 coefficients
  Rdx = colMeans(R2x) # redundancy
  Rdy = colMeans(R2y) # redundancy
  EV = cbind(Rdx, cumsum(Rdx), Rdy, cumsum(Rdy))
  Rd.mat = matrix(0, h, h)
  for (j in 1:h)
    Rd.mat[1:j,j] = Rdy[1:j]
  VIP = sqrt((Wh^2) %*% Rd.mat %*% diag(p/cumsum(Rdy), h, h))

  # add names
  dimnames(Wh) = list(colnames(X), paste(rep("w",h),1:h,sep=""))
  dimnames(Ws) = list(colnames(X), paste(rep("w*",h),1:h,sep=""))
  dimnames(Uh) = list(rownames(Y), paste(rep("u",h),1:h,sep=""))
  dimnames(Th) = list(rownames(X), paste(rep("t",h),1:h,sep=""))
  dimnames(Ch) = list(colnames(Y), paste(rep("c",h),1:h,sep=""))
  dimnames(Ph) = list(colnames(X), paste(rep("p",h),1:h,sep=""))
  dimnames(Bs) = list(colnames(X), colnames(Y))
  dimnames(Br) = list(colnames(X), colnames(Y))
  dimnames(cor.xt) = list(colnames(X), paste(rep("t",h),1:h,sep=""))
  dimnames(cor.yt) = list(colnames(Y), paste(rep("t",h),1:h,sep=""))
  dimnames(cor.xu) = list(colnames(X), paste(rep("u",h),1:h,sep=""))
  dimnames(cor.yu) = list(colnames(Y), paste(rep("u",h),1:h,sep=""))
  dimnames(cor.tu) = list(paste(rep("t",h),1:h,sep=""), paste(rep("u",h),1:h,sep=""))
  dimnames(EV) = list(paste(rep("t",h),1:h,sep=""), c("R2X","R2Xcum","R2Y","R2Ycum"))
  dimnames(Y.hat) = list(rownames(Y), colnames(Y))
  dimnames(resids) = list(rownames(Y), colnames(Y))
  dimnames(VIP) = list(colnames(X), paste(rep("t",h),1:h,sep=""))
  coeffs = rbind(Br, INTERCEPT=cte)

  # results
  list(x.scores = Th,
    x.loads = Ph,
    y.scores = Uh,
    y.loads = Ch,
    raw.wgs = Wh,
    mod.wgs = Ws,
    std.coefs = Bs,
    reg.coefs = coeffs,
    y.pred = Y.hat,
    resid = resids,
    expvar = EV,
    VIP = VIP,
    X.means = X.means,
    X.sds = X.sds,
    Y.means = Y.means,
    Y.sds = Y.sds)
}
