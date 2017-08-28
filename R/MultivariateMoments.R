###############################################################################
# Functions to perform multivariate matrix
# calculations on portfolios of assets.
#
# I've modified these to minimize the number of
# times the same calculation or statistic is run, and to minimize duplication
# of code from function to function.  This should make things more
# efficient when running against very large numbers of instruments or portfolios.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson for PerformanceAnalytics
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id$
###############################################################################


M3.MM.old = function(R,...){
  cAssets = ncol(R); T = nrow(R);
  if(!hasArg(mu)) mu = apply(R,2,'mean') else mu=mu=list(...)$mu
  M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
  for(t in c(1:T))
  {
    centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
    M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
  }
  return( 1/T*M3 );
}

#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname CoMoments
M3.MM <- function(R, unbiased = FALSE, as.mat = TRUE, ...) {
  if(!hasArg(mu)) mu = colMeans(R) else mu=list(...)$mu
  
  x <- coredata(R)
  
  NN <- NROW(x)
  PP <- NCOL(x)
  
  Xc <- x - matrix(mu, nrow = NN, ncol = PP, byrow = TRUE)
  
  if (unbiased) {
    if (NN < 3) stop("R should have at least 3 observations")
    CC <- NN / ((NN - 1) * (NN - 2))
  } else {
    CC <- 1 / NN
  }
  
  M3 <- .Call('M3sample', as.numeric(Xc), NN, PP, CC, PACKAGE="PerformanceAnalytics")
  if (as.mat) M3 <- M3.vec2mat(M3, PP)
  
  return( M3 )
}

M4.MM.old = function(R,...){
  cAssets = ncol(R); T = nrow(R);
  if(!hasArg(mu))   mu = apply(R,2,'mean')  else mu=list(...)$mu
  M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3);
  for(t in c(1:T))
  {
    centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
    M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
  }
  return( 1/T*M4 );
}

#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname CoMoments
M4.MM <- function(R, as.mat = TRUE, ...) {
  if(!hasArg(mu)) mu = colMeans(R) else mu=list(...)$mu
  
  x <- coredata(R)
  
  NN <- NROW(x)
  PP <- NCOL(x)
  
  Xc <- x - matrix(mu, nrow = NN, ncol = PP, byrow = TRUE)
  
  M4 <- .Call('M4sample', as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics")
  if (as.mat) M4 <- M4.vec2mat(M4, PP)
  
  return( M4 )
}

multivariate_mean <- function(w, mu) {
  return( t(w) %*% mu )
}

StdDev.MM <- function(w, sigma) {
  return( sqrt(t(w) %*% sigma %*% w) )
}

skewness.MM <- function(w, sigma, M3) {
  w <- as.numeric(w)
  if (NCOL(M3) != 1) M3 <- M3.mat2vec(M3)
  m3_univ <- .Call('M3port', w, M3, length(w), PACKAGE="PerformanceAnalytics")
  return( m3_univ / (StdDev.MM(w, sigma))^3 )
}

kurtosis.MM <- function(w, sigma, M4) {
  w <- as.numeric(w)
  if (NCOL(M4) != 1) M4 <- M4.mat2vec(M4)
  m4_univ <- .Call('M4port', w, M4, length(w), PACKAGE="PerformanceAnalytics")
  return( m4_univ / (StdDev.MM(w, sigma))^4 )
}

SR.StdDev.MM <- function(w, mu, sigma) {
  return( multivariate_mean(w, mu) / StdDev.MM(w, sigma)   )
}

GVaR.MM <- function(w, mu, sigma, p) {
  return( -multivariate_mean(w, mu) - qnorm(1 - p) * StdDev.MM(w, sigma) )
}

SR.GVaR.MM <- function(w, mu, sigma, p) {
  return( multivariate_mean(w, mu) / GVaR.MM(w, mu, sigma, p) )
}

mVaR.MM <- function(w, mu, sigma, M3, M4, p) {
  skew <- skewness.MM(w, sigma, M3)
  exkurt <- kurtosis.MM(w, sigma, M4) - 3
  z <- qnorm(1 - p)
  zc <- z + (1 / 6) * (z^2 - 1) * skew
  Zcf <- zc + (1 / 24) * (z^3 - 3 * z) * exkurt - (1 / 36) * (2 * z^3 - 5 * z) * skew^2
  return( -multivariate_mean(w, mu) - Zcf * StdDev.MM(w, sigma) )
}

SR.mVaR.MM <- function(w, mu, sigma, M3, M4, p) {
  return( multivariate_mean(w, mu) / mVaR.MM(w, mu, sigma, M3, M4, p) )
}

GES.MM <- function(w, mu, sigma, p) {
  return( -multivariate_mean(w, mu) + dnorm(qnorm(1 - p)) * StdDev.MM(w, sigma) / (1 - p) )
}

SR.GES.MM <- function(w, mu, sigma, p) {
  return( multivariate_mean(w, mu) / GES.MM(w, mu, sigma, p) )
}

Ipower = function(power,h){
  # probably redundant now
  fullprod = 1;
  if( (power%%2)==0 ) #even number: number mod is zero
  {
    pstar = power/2;
    for(j in c(1:pstar)){
      fullprod = fullprod*(2*j)  }
    I = fullprod*dnorm(h);
    
    for(i in c(1:pstar) )
    {
      prod = 1;
      for(j in c(1:i) ){
        prod = prod*(2*j)  }
      I = I + (fullprod/prod)*(h^(2*i))*dnorm(h)
    }
  }else{
    pstar = (power-1)/2
    for(j in c(0:pstar) ) {
      fullprod = fullprod*( (2*j)+1 ) }
    I = -fullprod*pnorm(h);
    for(i in c(0:pstar) ){
      prod = 1;
      for(j in c(0:i) ){
        prod = prod*( (2*j) + 1 )}
      I = I + (fullprod/prod)*(h^(  (2*i) + 1))*dnorm(h) }
  }
  return(I)
}

mES.MM <- function(w, mu, sigma, M3, M4, p) {
  skew <- skewness.MM(w, sigma, M3)
  exkurt <- kurtosis.MM(w, sigma, M4) - 3
  z <- qnorm(1 - p)
  h <- z + (1 / 6) * (z^2 - 1) * skew
  h <- h + (1 / 24) * (z^3 - 3 * z) * exkurt - (1 / 36) * (2 * z^3 - 5 * z) * skew^2
  
  # E = dnorm(h)
  # E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
  # E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
  # E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
  # E = E/(1-p)
  E <- dnorm(h) * (1 + h^3 * skew / 6.0 +
                     (h^6 - 9 * h^4 + 9 * h^2 + 3) * skew^2 / 72 +
                     (h^4 - 2 * h^2 - 1) * exkurt / 24) / (1 - p)
  
  return( -multivariate_mean(w, mu) - StdDev.MM(w, sigma) * min(-E, h) )
}

SR.mES.MM <- function(w, mu, sigma, M3, M4, p){
  return( multivariate_mean(w, mu) / mES.MM(w, mu, sigma, M3, M4, p) )
}

# Computes inner product between M3_1 and M3_2 and otherwise the Frobenius norm of M3_1
M3.innprod <- function(p, M3_1, M3_2 = NULL) {
  # p         : dimension of the data
  # M3_1      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) / 6)
  # M3_2      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) / 6)
  
  if (NCOL(M3_1) != 1) stop("M3_1 should only contain the unique coskewness elements (see e.g. output of M3.MM(R, as.mat = FALSE))")
  
  if (is.null(M3_2)) {
    M3_2 <- M3_1
  } else {
    if (length(M3_1) != length(M3_2)) stop("M3_2 should only contain the unique coskewness elements (see e.g. output of M3.MM(R, as.mat = FALSE))")
  }
  
  .Call('M3innprod', M3_1, M3_2, as.integer(p), PACKAGE="PerformanceAnalytics")
}

# Computes inner product between M4_1 and M4_2 and otherwise the Frobenius norm of M4_1
M4.innprod <- function(p, M4_1, M4_2 = NULL) {
  # p         : dimension of the data
  # M4_1      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) * (p + 3) / 24)
  # M4_2      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) * (p + 3) / 24)
  
  if (NCOL(M4_1) != 1) stop("M4_1 should only contain the unique coskewness elements (see e.g. output of M4.MM(R, as.mat = FALSE))")
  
  if (is.null(M4_2)) {
    M4_2 <- M4_1
  } else {
    if (length(M4_1) != length(M4_2)) stop("M4_2 should only contain the unique coskewness elements (see e.g. output of M4.MM(R, as.mat = FALSE))")
  }
  
  .Call('M4innprod', M4_1, M4_2, as.integer(p), PACKAGE="PerformanceAnalytics")
}

# Wrapper function for casting the vector with unique coskewness elements into the matrix format
M3.vec2mat <- function(M3, p) {
  # M3        : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) / 6)
  # p         : dimension of the data
  
  if (NCOL(M3) != 1) stop("M3 must be a vector")
  
  .Call('M3vec2mat', M3, as.integer(p), PACKAGE="PerformanceAnalytics")
}

# Wrapper function for casting the coskewness matrix into the vector of unique elements
M3.mat2vec <- function(M3) {
  # M3        : numeric matrix of dimension p x p^2
  
  if (is.null(dim(M3))) stop("M3 must be a matrix")
  
  .Call('M3mat2vec', as.numeric(M3), NROW(M3), PACKAGE="PerformanceAnalytics")
}

# Wrapper function for casting the vector with unique cokurtosis elements into the matrix format
M4.vec2mat <- function(M4, p) {
  # M4        : numeric vector with unique cokurtosis elements (p * (p + 1) * (p + 2) * (p + 3) / 24)
  # p         : dimension of the data
  
  if (NCOL(M4) != 1) stop("M4 must be a vector")
  
  .Call('M4vec2mat', M4, as.integer(p), PACKAGE="PerformanceAnalytics")
}

# Wrapper function for casting the cokurtosis matrix into the vector of unique elements
M4.mat2vec <- function(M4) {
  # M4        : numeric matrix of dimension p x p^3
  
  if (is.null(dim(M4))) stop("M4 must be a matrix")
  
  .Call('M4mat2vec', as.numeric(M4), NROW(M4), PACKAGE="PerformanceAnalytics")
}

#' Functions for calculating shrinkage-based comoments of financial time series
#' 
#' calculates covariance, coskewness and cokurtosis matrices using linear shrinkage
#' between the sample estimator and a structured estimator
#' 
#' The coskewness and cokurtosis matrices are defined as the matrices of dimension 
#' p x p^2 and p x p^3 containing the third and fourth order central moments. They
#' are useful for measuring nonlinear dependence between different assets of the 
#' portfolio and computing modified VaR and modified ES of a portfolio.
#' 
#' Shrinkage estimation for the covariance matrix was popularized by Ledoit and 
#' Wolf (2003, 2004). An extension to coskewness and cokurtosis matrices by
#' Martellini and Ziemann (2010) uses the 1-factor and constant-correlation structured
#' comoment matrices as targets. In Boudt, Cornilly and Verdonck (2017) the framework
#' of single-target shrinkage for the coskewness and cokurtosis matrices is 
#' extended to a multi-target setting, making it possible to include several target matrices
#' at once. Also, an option to enhance small sample performance for coskewness estimation
#' was proposed, resulting in the option 'unbiasedMSE' present in the 'M3.shrink' function.
#' 
#' The first four target matrices of the 'M2.shrink', 'M3.shrink' and 'M4.shrink' 
#' correspond to the models 'independent marginals', 'independent and identical marginals', 
#' 'observed 1-factor model' and 'constant correlation'. Coskewness shrinkage includes two
#' more options, target 5 corresponds to the latent 1-factor model proposed in Simaan (1993)
#' and target 6 is the coskewness matrix under central-symmetry, a matrix full of zeros.
#' For more details on the targets, we refer to Boudt, Cornilly and Verdonck (2017) and
#' the supplementary appendix.
#' 
#' If f is a matrix containing multiple factors, then the shrinkage estimator will
#' use each factor in a seperate single-factor model and use multi-target shrinkage
#' to all targets matrices at once.
#' @name ShrinkageMoments
#' @concept co-moments
#' @concept moments
#' @aliases ShrinkageMoments M2.shrink M3.shrink M4.shrink
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param targets vector of integers selecting the target matrices to shrink to. The first four
#' structures are, in order: 'independent marginals', 'independent and identical marginals', 
#' 'observed 1-factor model' and 'constant correlation'. See Details.
#' @param f vector or matrix with observations of the factor, to be used with target 3. See Details.
#' @param unbiasedMSE TRUE/FALSE whether to use a correction to have an unbiased
#' estimator for the marginal skewness values, in case of targets 1 and/or 2, default FALSE
#' @param as.mat TRUE/FALSE whether to return the full moment matrix or only
#' the vector with the unique elements (the latter is advised for speed), default
#' TRUE
#' @author Dries Cornilly
#' @seealso \code{\link{CoMoments}} \cr \code{\link{StructuredMoments}} \cr \code{\link{EWMAMoments}} \cr \code{\link{MCA}}
#' @references Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008.
#' Estimation and Decomposition of Downside Risk for Portfolios with Non-Normal
#' Returns. Journal of Risk. Winter.
#' 
#' Boudt, Kris, Cornilly, Dries and Verdonck, Tim. 2017. A Coskewness Shrinkage 
#' Approach for Estimating the Skewness of Linear Combinations of Random Variables. 
#' Submitted. Available at SSRN: https://ssrn.com/abstract=2839781
#' 
#' Ledoit, Olivier and Wolf, Michael. 2003. Improved estimation of the covariance matrix 
#' of stock returns with an application to portfolio selection. Journal of empirical 
#' finance, 10(5), 603-621.
#' 
#' Ledoit, Olivier and Wolf, Michael. 2004. A well-conditioned estimator for large-dimensional 
#' covariance matrices. Journal of multivariate analysis, 88(2), 365-411.
#' 
#' Martellini, Lionel and Ziemann, V\"olker. 2010. Improved estimates of higher-order 
#' comoments and implications for portfolio selection. Review of Financial 
#' Studies, 23(4), 1467-1502.
#' 
#' Simaan, Yusif. 1993. Portfolio selection and asset pricing: three-parameter framework. 
#' Management Science, 39(5), 68-577.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' 
#' # construct an underlying factor (market-factor, observed factor, PCA, ...)
#' f <- rowSums(edhec)
#' 
#' # multi-target shrinkage with targets 1, 3 and 4
#' # as.mat = F' would speed up calculations in higher dimensions
#' targets <- c(1, 3, 4)
#' sigma <- M2.shrink(edhec, targets, f)$M2sh
#' m3 <- M3.shrink(edhec, targets, f)$M3sh
#' m4 <- M4.shrink(edhec, targets, f)$M4sh
#' 
#' # compute equal-weighted portfolio modified ES
#' mu <- colMeans(edhec)
#' p <- length(mu)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' # compare to sample method
#' sigma <- cov(edhec)
#' m3 <- M3.MM(edhec)
#' m4 <- M4.MM(edhec)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' @export M2.shrink
M2.shrink <- function(R, targets = 1, f = NULL) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes the shrinkage estimator of the covariance matrix
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # targets   : vector of integers indicating which targets to use
  #           : T1 : independent, unequal marginals, Ledoit and Wolf (2003)
  #           : T2 : independent, equal marginals, Ledoit and Wolf (2003)
  #           : T3 : 1-factor model of Ledoit and Wolf (2003)
  #                : if multiple factors are provided, additionall 1-factor structured matrices are added the end
  #           : T4 : constant-correlation model of Ledoit and Wolf (2004)
  # f         : numeric vector with factor observations, needed for 1-factor coskewness matrix of Martellini and Ziemann
  #           : or a numeric matrix with columns as factors
  # as.mat    : output as a matrix or as the vector with only unique coskewness eleements
  #
  # Outputs:
  # M2sh      : the shrinkage estimator
  # lambda    : vector with shrinkage intensities
  # A         : A matrix in the QP
  # b         : b vector in QP
  
  X <- coredata(R)
  
  # input checking
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if (length(targets) == 0) stop("No targets selected")
  if (prod(targets %in% 1:4) == 0) stop("Select valid targets (out of 1, 2, 3, 4)")
  tt <- rep(FALSE, 4)
  tt[targets] <- TRUE
  targets <- tt
  if (targets[3] && is.null(f)) stop("Provide the factor observations")
  
  # prepare for additional factors if necessary
  if (targets[3] && (NCOL(f) != 1)) {
    nFactors <- NCOL(f)
    if (nFactors > 1) {
      f_other <- matrix(f[, 2:nFactors], ncol = nFactors - 1)
      f <- f[, 1]
      extraFactors <- TRUE
      targets <- c(targets, rep(TRUE, nFactors - 1))
    } else {
      f <- c(f)
      extraFactors <- FALSE
    }
  } else {
    extraFactors <- FALSE
  }
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  PP <- dim(X)[2]                                                   # number of assets
  nT <- sum(targets)                                                # number of targets
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  Xc2 <- Xc^2
  margvars <- colMeans(Xc2)
  m11 <- as.numeric(t(Xc) %*% Xc) / NN
  m22 <- as.numeric(t(Xc2) %*% Xc2) / NN
  
  ### coskewness estimators
  M2 <- cov(X) * (NN - 1) / NN
  T2 <- matrix(NA, nrow = PP * PP, ncol = nT)
  iter <- 1
  
  if (targets[1]) {
    # independent marginals
    T2[, iter] <- c(diag(margvars))
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    T2[, iter] <- c(mean(margvars) * diag(PP))
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Ledoit and Wolf (2003))
    beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
    fc <- f - mean(f)
    fvar <- mean(fc^2)
    T2_1f <- fvar * beta %*% t(beta)
    diag(T2_1f) <- margvars
    T2[, iter] <- T2_1f
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Ledoit and Wolf (2004))
    sd_vec <- sqrt(margvars)
    R2 <- diag(1 / sd_vec) %*% M2 %*% diag(1 / sd_vec)
    rcoef <- mean(R2[upper.tri(R2)])
    R2 <- matrix(rcoef, nrow = PP, ncol = PP)
    diag(R2) <- 1
    T2[, iter] <- diag(sd_vec) %*% R2 %*% diag(sd_vec)
    iter <- iter + 1
  }
  if (extraFactors) {
    # 1-factor model (Ledoit and Wolf (2003)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      beta_bis <- apply(Xc, 2, function(a) cov(a, f_bis) / var(f_bis))
      fc_bis <- f_bis - mean(f_bis)
      fvar_bis <- mean(fc_bis^2)
      T2_1f <- fvar_bis * beta_bis %*% t(beta_bis)
      diag(T2_1f) <- margvars
      T2[, iter] <- T2_1f
      iter <- iter + 1
    }
  }
  
  ### build A for the QP
  A <- matrix(NA, nrow = nT, ncol = nT)
  for (ii in 1:nT) {
    for (jj in ii:nT) {
      A[ii, jj] <- A[jj, ii] <- sum((T2[, ii] - M2) * (T2[, jj] - M2))
    }
  }
  
  ### build b for the QP
  VM2vec <- .Call('VM2', m11, m22, NN, PP, PACKAGE="PerformanceAnalytics")
  
  b <- rep(VM2vec[1], nT)
  iter <- 1
  if (targets[1]) {
    # independent marginals
    b[iter] <- b[iter] - VM2vec[3]
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    b[iter] <- b[iter] - VM2vec[2]
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Ledoit and Wolf (2003))
    b[iter] <- b[iter] - .Call('CM2_1F', as.numeric(Xc), fc, fvar, m11, m22, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Ledoit and Wolf (2004))
    b[iter] <- b[iter] - .Call('CM2_CC', as.numeric(Xc), rcoef, m11, m22, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (extraFactors) {
    # 1-factor model (Ledoit and Wolf (2003)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      fc_bis <- f_bis - mean(f_bis)
      fvar_bis <- mean(fc_bis^2)
      b[iter] <- b[iter] - .Call('CM2_1F', as.numeric(Xc), fc_bis, fvar_bis, m11, m22, NN, PP, PACKAGE="PerformanceAnalytics")
      iter <- iter + 1
    }
  }
  
  ### solve the QP
  if (nT == 1) {
    # single-target shrinkage
    lambda <- b / A                                                 # compute optimal shrinkage intensity
    lambda <- max(0, min(1, lambda))                                # must be between 0 and 1
    M2sh <- (1 - lambda) * M2 + lambda * T2[, 1]                    # compute shrinkage estimator
  } else {
    # multi-target shrinkage
    Aineq <- rbind(diag(nT), rep(-1, nT))                           # A matrix for inequalities quadratic program
    bineq <- matrix(c(rep(0, nT), -1), ncol = 1)                    # b vector for inequalities quadratic program
    lambda <- quadprog::solve.QP(A, b, t(Aineq), bineq, meq = 0)$solution # solve quadratic program
    M2sh <- (1 - sum(lambda)) * M2                                  # initialize estimator at percentage of sample estimator
    for (tt in 1:nT) {
      M2sh <- M2sh + lambda[tt] * T2[, tt]                          # add the target matrices
    }
  }
  
  return (list("M2sh" = M2sh, "lambda" = lambda, "A" = A, "b" = b))
}

#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname ShrinkageMoments
M3.shrink <- function(R, targets = 1, f = NULL, unbiasedMSE = FALSE, as.mat = TRUE) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes the shrinkage estimator of the coskewness matrix as in Boudt, Cornilly and Verdonck (2017)
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # targets   : vector of integers indicating which targets to use
  #           : T1 : independent, unequal marginals
  #           : T2 : independent, equal marginals
  #           : T3 : 1-factor model of Martellini and Ziemann (2010)
  #                : if multiple factors are provided, additionall 1-factor structured matrices are added the end
  #           : T4 : constant-correlation model of Martellini and Ziemann (2010), symmetrized
  #           : T5 : latent 1-factor model of Simaan (1993)
  #           : T6 : central-symmetric coskewness matrix (all zeros)
  # f         : numeric vector with factor observations, needed for 1-factor coskewness matrix of Martellini and Ziemann
  #           : or a numeric matrix with columns as factors
  # unbiasedMSE : boolean determining if bias is corrected when estimating the MSE loss function
  # as.mat    : output as a matrix or as the vector with only unique coskewness eleements
  #
  # Outputs:
  # M3sh      : the shrinkage estimator
  # lambda    : vector with shrinkage intensities
  # A         : A matrix in the QP
  # b         : b vector in QP
  
  X <- coredata(R)
  
  # input checking
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if (length(targets) == 0) stop("No targets selected")
  if (prod(targets %in% 1:6) == 0) stop("Select valid targets (out of 1, 2, 3, 4, 5, 6)")
  tt <- rep(FALSE, 6)
  tt[targets] <- TRUE
  targets <- tt
  if (targets[3] && is.null(f)) stop("Provide the factor observations")
  if (unbiasedMSE && (sum(targets[c(3, 4, 5)]) > 0)) stop("UnbiasedMSE can only be combined with T2, T3 and T6")
  
  # prepare for additional factors if necessary
  if (targets[3] && (NCOL(f) != 1)) {
    nFactors <- NCOL(f)
    if (nFactors > 1) {
      f_other <- matrix(f[, 2:nFactors], ncol = nFactors - 1)
      f <- f[, 1]
      extraFactors <- TRUE
      targets <- c(targets, rep(TRUE, nFactors - 1))
    } else {
      f <- c(f)
      extraFactors <- FALSE
    }
  } else {
    extraFactors <- FALSE
  }
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  if (unbiasedMSE) {
    if (NN < 6) stop("R should have at least 6 observations")
  }
  PP <- dim(X)[2]                                                   # number of assets
  nT <- sum(targets)                                                # number of targets
  ncosk <- PP * (PP + 1) * (PP + 2) / 6                             # number of unique coskewness elements
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  Xc2 <- Xc^2
  m11 <- as.numeric(t(Xc) %*% Xc) / NN
  m21 <- as.numeric(t(Xc2) %*% Xc) / NN
  m22 <- as.numeric(t(Xc2) %*% Xc2) / NN
  m31 <- as.numeric(t(Xc^3) %*% Xc) / NN
  m42 <- as.numeric(t(Xc^4) %*% Xc2) / NN
  m33 <- as.numeric(t(Xc^3) %*% Xc^3) / NN
  
  ### coskewness estimators
  M3 <- M3.MM(X, unbiased = unbiasedMSE, as.mat = FALSE)
  T3 <- matrix(NA, nrow = length(M3), ncol = nT)
  iter <- 1
  
  if (targets[1]) {
    # independent marginals
    margskews <- colMeans(Xc^3)
    if (unbiasedMSE) margskews <- margskews * NN^2 / ((NN - 1) * (NN - 2))
    T3[, iter] <- .Call('M3_T23', margskews, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    margskews <- colMeans(Xc^3)
    if (unbiasedMSE) margskews <- margskews * NN^2 / ((NN - 1) * (NN - 2))
    margskews <- rep(mean(margskews), PP)
    T3[, iter] <- .Call('M3_T23', margskews, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Martellini and Ziemann (2010))
    margskews <- colMeans(Xc^3)
    beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
    fc <- f - mean(f)
    fskew <- mean(fc^3)
    T3[, iter] <- .Call('M3_1F', margskews, beta, fskew, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Martellini and Ziemann (2010)) (symmetrized version)
    margvars <- colMeans(Xc^2)
    margskews <- colMeans(Xc^3)
    margkurts <- colMeans(Xc^4)
    r_generalized <- .Call('M3_CCoefficients', margvars, margkurts, m21,
                           m22, as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics")
    T3[, iter] <- .Call('M3_CC', margvars, margskews, margkurts,
                        r_generalized[1], r_generalized[2],
                        r_generalized[3], PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[5]) {
    # 1-factor model of Simaan (1993)
    margskews <- colMeans(Xc^3)
    margskewsroot <- sign(margskews) * abs(margskews)^(1 / 3)
    T3[, iter] <- .Call('M3_Simaan', margskewsroot, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[6]) {
    # central-symmetric
    T3[, iter] <- rep(0, ncosk)
    iter <- iter + 1
  }
  if (extraFactors) {
    # 1-factor model (Martellini and Ziemann (2010)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      margskews <- colMeans(Xc^3)
      beta_bis <- apply(Xc, 2, function(a) cov(a, f_bis) / var(f_bis))
      fc_bis <- f_bis - mean(f_bis)
      fskew_bis <- mean(fc_bis^3)
      T3[, iter] <- .Call('M3_1F', margskews, beta_bis, fskew_bis, PP, PACKAGE="PerformanceAnalytics")
      iter <- iter + 1
    }
  }
  
  ### build A for the QP
  A <- matrix(NA, nrow = nT, ncol = nT)
  for (ii in 1:nT) {
    for (jj in ii:nT) {
      A[ii, jj] <- A[jj, ii] <- .Call('M3innprod', T3[, ii] - M3, T3[, jj] - M3, PP, PACKAGE="PerformanceAnalytics")
    }
  }
  
  ### build b for the QP
  if (unbiasedMSE) {
    VM3vec <- .Call('VM3kstat', as.numeric(Xc), as.numeric(Xc2), m11 * NN, m21 * NN, m22 * NN, 
                    m31 * NN, m42 * NN, m33 * NN, NN, PP, PACKAGE="PerformanceAnalytics")
  } else {
    VM3vec <- .Call('VM3', as.numeric(Xc), as.numeric(Xc2), m11, m21, m22, m31, m42,
                    m33, NN, PP, PACKAGE="PerformanceAnalytics")
  }
  
  b <- rep(VM3vec[1], nT)
  iter <- 1
  if (targets[1]) {
    # independent marginals
    b[iter] <- b[iter] - VM3vec[3]
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    b[iter] <- b[iter] - VM3vec[2]
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Martellini and Ziemann (2010))
    fvar <- mean(fc^2)
    b[iter] <- b[iter] - .Call('CM3_1F', as.numeric(Xc), as.numeric(Xc2),
                               fc, fvar, fskew, m11, m21, m22, m42, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Martellini and Ziemann (2010)) (symmetrized version)
    marg5s <- colMeans(Xc^5)
    marg6s <- colMeans(Xc^6)
    m41 <- as.numeric(t(Xc^4) %*% Xc) / NN
    m61 <- as.numeric(t(Xc^6) %*% Xc) / NN
    m32 <- as.numeric(t(Xc^3) %*% Xc^2) / NN
    b[iter] <- b[iter] - .Call('CM3_CC', as.numeric(Xc), as.numeric(Xc2), margvars, margskews, margkurts, 
                               marg5s, marg6s, m11, m21, m31, m32, m41, m61, r_generalized[1], r_generalized[2],
                               r_generalized[3], NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[5]) {
    # 1-factor model of Simaan (1993)
    margskewsroot <- margskewsroot^(-2)
    m51 <- as.numeric(t(Xc^5) %*% Xc) / NN
    b[iter] <- b[iter] - .Call('CM3_Simaan', as.numeric(Xc), as.numeric(Xc2), margskewsroot, m11, m21, m22, 
                               m31, m42, m51, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[6]) iter <- iter + 1
  if (extraFactors) {
    # 1-factor model (Martellini and Ziemann (2010)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      fc_bis <- f_bis - mean(f_bis)
      fvar_bis <- mean(fc_bis^2)
      fskew_bis <- mean(fc_bis^3)
      b[iter] <- b[iter] - .Call('CM3_1F', as.numeric(Xc), as.numeric(Xc2), fc_bis, fvar_bis, fskew_bis, 
                                 m11, m21, m22, m42, NN, PP, PACKAGE="PerformanceAnalytics")
      iter <- iter + 1
    }
  }
  
  ### solve the QP
  if (nT == 1) {
    # single-target shrinkage
    lambda <- b / A                                                 # compute optimal shrinkage intensity
    lambda <- max(0, min(1, lambda))                                # must be between 0 and 1
    M3sh <- (1 - lambda) * M3 + lambda * T3                         # compute shrinkage estimator
  } else {
    # multi-target shrinkage
    Aineq <- rbind(diag(nT), rep(-1, nT))                           # A matrix for inequalities quadratic program
    bineq <- matrix(c(rep(0, nT), -1), ncol = 1)                    # b vector for inequalities quadratic program
    lambda <- quadprog::solve.QP(A, b, t(Aineq), bineq, meq = 0)$solution # solve quadratic program
    M3sh <- (1 - sum(lambda)) * M3                                  # initialize estimator at percentage of sample estimator
    for (tt in 1:nT) {
      M3sh <- M3sh + lambda[tt] * T3[, tt]                          # add the target matrices
    }
  }
  if (as.mat) M3sh <- M3.vec2mat(M3sh, PP)
  
  return (list("M3sh" = M3sh, "lambda" = lambda, "A" = A, "b" = b))
}


#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname ShrinkageMoments
M4.shrink <- function(R, targets = 1, f = NULL, as.mat = TRUE) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes the shrinkage estimator of the cokurtosis matrix as in Boudt, Cornilly and Verdonck (2017)
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # targets   : vector of integers indicating which targets to use
  #           : T1 : independent, unequal marginals
  #           : T2 : independent, equal marginals
  #           : T3 : 1-factor model of Martellini and Ziemann (2010)
  #                : if multiple factors are provided, additionall 1-factor structured matrices are added the end
  #           : T4 : constant-correlation model of Martellini and Ziemann (2010)
  # f         : numeric vector with factor observations, needed for 1-factor coskewness matrix of Martellini and Ziemann
  #           : or a numeric matrix with columns as factors
  # as.mat    : output as a matrix or as the vector with only unique coskewness eleements
  #
  # Outputs:
  # M4sh      : the shrinkage estimator
  # lambda    : vector with shrinkage intensities
  # A         : A matrix in the QP
  # b         : b vector in QP
  
  X <- coredata(R)
  
  # input checking
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if (length(targets) == 0) stop("No targets selected")
  if (prod(targets %in% 1:4) == 0) stop("Select valid targets (out of 1, 2, 3, 4)")
  tt <- rep(FALSE, 4)
  tt[targets] <- TRUE
  targets <- tt
  if (targets[3] && is.null(f)) stop("Provide the factor observations")
  
  # prepare for additional factors if necessary
  if (targets[3] && (NCOL(f) != 1)) {
    nFactors <- NCOL(f)
    if (nFactors > 1) {
      f_other <- matrix(f[, 2:nFactors], ncol = nFactors - 1)
      f <- f[, 1]
      extraFactors <- TRUE
      targets <- c(targets, rep(TRUE, nFactors - 1))
    } else {
      f <- c(f)
      extraFactors <- FALSE
    }
  } else {
    extraFactors <- FALSE
  }
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  PP <- dim(X)[2]                                                   # number of assets
  nT <- sum(targets)                                                # number of targets
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  Xc2 <- Xc^2
  margvars <- colMeans(Xc2)
  margkurts <- colMeans(Xc^4)
  m11 <- as.numeric(t(Xc) %*% Xc) / NN
  m21 <- as.numeric(t(Xc2) %*% Xc) / NN
  m22 <- as.numeric(t(Xc2) %*% Xc2) / NN
  m31 <- as.numeric(t(Xc^3) %*% Xc) / NN
  m32 <- as.numeric(t(Xc^3) %*% Xc2) / NN
  m41 <- as.numeric(t(Xc^4) %*% Xc) / NN
  m42 <- as.numeric(t(Xc^4) %*% Xc2) / NN
  
  ### coskewness estimators
  M4 <- M4.MM(X, as.mat = FALSE)
  T4 <- matrix(NA, nrow = length(M4), ncol = nT)
  iter <- 1
  
  if (targets[1]) {
    # independent marginals
    T4[, iter] <- .Call('M4_T12', margkurts, margvars, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    meanmargkurts <- mean(margkurts)
    meank_iikk <- sqrt(mean(margvars^2))
    T4[, iter] <- .Call('M4_T12', rep(meanmargkurts, PP), rep(meank_iikk, PP), PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Martellini and Ziemann (2010))
    beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
    fc <- f - mean(f)
    fvar <- mean(fc^2)
    fkurt <- mean(fc^4)
    epsvars <- margvars - beta^2 * fvar
    T4[, iter] <- .Call('M4_1f', margkurts, fvar, fkurt, epsvars, beta, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Martellini and Ziemann (2010)) 
    marg6s <- colMeans(Xc^6)
    r_generalized <- .Call('M4_CCoefficients', margvars, margkurts, marg6s,
                           m22, m31, as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics")
    T4[, iter] <- .Call('M4_CC', margvars, margkurts, marg6s, r_generalized[1], r_generalized[2], 
                        r_generalized[3], r_generalized[4], PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (extraFactors) {
    # 1-factor model (Martellini and Ziemann (2010)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      beta_bis <- apply(Xc, 2, function(a) cov(a, f_bis) / var(f_bis))
      fc_bis <- f_bis - mean(f_bis)
      fvar_bis <- mean(fc_bis^2)
      fkurt_bis <- mean(fc_bis^4)
      epsvars_bis <- margvars - beta_bis^2 * fvar_bis
      T4[, iter] <- .Call('M4_1f', margkurts, fvar_bis, fkurt_bis, epsvars_bis,
                          beta_bis, PP, PACKAGE="PerformanceAnalytics")
      iter <- iter + 1
    }
  }
  
  ### build A for the QP
  A <- matrix(NA, nrow = nT, ncol = nT)
  for (ii in 1:nT) {
    for (jj in ii:nT) {
      A[ii, jj] <- A[jj, ii] <- .Call('M4innprod', T4[, ii] - M4, T4[, jj] - M4, PP, PACKAGE="PerformanceAnalytics")
    }
  }
  
  ### build b for the QP
  VM4vec <- .Call('VM4', as.numeric(Xc), as.numeric(Xc2), m11, m21,
                  m22, m31, m32, m41, m42, NN, PP, PACKAGE="PerformanceAnalytics")
  
  b <- rep(VM4vec[1], nT)
  iter <- 1
  if (targets[1]) {
    # independent marginals
    b[iter] <- b[iter] - VM4vec[3]
    iter <- iter + 1
  }
  if (targets[2]) {
    # independent and equally distributed marginals
    b[iter] <- b[iter] - VM4vec[2]
    iter <- iter + 1
  }
  if (targets[3]) {
    # 1-factor model (Martellini and Ziemann (2010))
    fskew <- mean(fc^3)
    b[iter] <- b[iter] - .Call('CM4_1F', as.numeric(Xc), as.numeric(Xc2), fc, fvar, fskew, fkurt, 
                               m11, m21, m22, m31, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (targets[4]) {
    # constant-correlation (Martellini and Ziemann (2010))
    marg6s <- colMeans(Xc^6)
    marg7s <- colMeans(Xc^7)
    m33 <- as.numeric(t(Xc^3) %*% Xc^3) / NN
    b[iter] <- b[iter] - .Call('CM4_CC', as.numeric(Xc), as.numeric(Xc2), m11, m21, m22, m31, m32, m33, m41,
                               r_generalized[1], r_generalized[2], r_generalized[3], r_generalized[4],
                               marg6s, marg7s, NN, PP, PACKAGE="PerformanceAnalytics")
    iter <- iter + 1
  }
  if (extraFactors) {
    # 1-factor model (Martellini and Ziemann (2010)) - extra factors
    for (ii in 1:(nFactors - 1)) {
      f_bis <- f_other[, ii]
      fc_bis <- f_bis - mean(f_bis)
      fvar_bis <- mean(fc_bis^2)
      fskew_bis <- mean(fc_bis^3)
      fkurt_bis <- mean(fc_bis^4)
      b[iter] <- b[iter] - .Call('CM4_1F', as.numeric(Xc), as.numeric(Xc2), fc, fvar_bis, fskew_bis, fkurt_bis, 
                                 m11, m21, m22, m31, NN, PP, PACKAGE="PerformanceAnalytics")
      iter <- iter + 1
    }
  }
  
  ### solve the QP
  if (nT == 1) {
    # single-target shrinkage
    lambda <- b / A                                                 # compute optimal shrinkage intensity
    lambda <- max(0, min(1, lambda))                                # must be between 0 and 1
    M4sh <- (1 - lambda) * M4 + lambda * T4                         # compute shrinkage estimator
  } else {
    # multi-target shrinkage
    Aineq <- rbind(diag(nT), rep(-1, nT))                           # A matrix for inequalities quadratic program
    bineq <- matrix(c(rep(0, nT), -1), ncol = 1)                    # b vector for inequalities quadratic program
    lambda <- quadprog::solve.QP(A, b, t(Aineq), bineq, meq = 0)$solution # solve quadratic program
    M4sh <- (1 - sum(lambda)) * M4                                  # initialize estimator at percentage of sample estimator
    for (tt in 1:nT) {
      M4sh <- M4sh + lambda[tt] * T4[, tt]                          # add the target matrices
    }
  }
  if (as.mat) M4sh <- M4.vec2mat(M4sh, PP)
  
  return (list("M4sh" = M4sh, "lambda" = lambda, "A" = A, "b" = b))
}


#' Functions for calculating structured comoments of financial time series
#' 
#' calculates covariance, coskewness and cokurtosis matrices as structured estimators
#' 
#' The coskewness and cokurtosis matrices are defined as the matrices of dimension 
#' p x p^2 and p x p^3 containing the third and fourth order central moments. They
#' are useful for measuring nonlinear dependence between different assets of the 
#' portfolio and computing modified VaR and modified ES of a portfolio.
#' 
#' Structured estimation is based on the assumption that the underlying data-generating
#' process is known, or at least resembles the assumption. The first four structured estimators correspond to the models 'independent marginals', 
#' 'independent and identical marginals', 'observed multi-factor model' and 'constant correlation'. 
#' Coskewness estimation includes an additional model based on the latent 1-factor model
#' proposed in Simaan (1993).
#' 
#' The constant correlation and 1-factor coskewness and cokurtosis matrices can be found in 
#' Martellini and Ziemann (2010). If f is a matrix containing multiple factors, 
#' then the multi-factor model of Boudt, Lu and Peeters (2915) is used. For information
#' about the other structured matrices, we refer to Boudt, Cornilly and Verdonck (2017)
#' @name StructuredMoments
#' @concept co-moments
#' @concept moments
#' @aliases StructuredMoments M2.struct M3.struct M4.struct
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param struct string containing the preferred method. See Details.
#' @param f vector or matrix with observations of the factor, to be used with 'observedfactor'. See Details.
#' @param unbiasedMarg TRUE/FALSE whether to use a correction to have an unbiased
#' estimator for the marginal skewness values, in case of 'Indep' or 'IndepId', default FALSE
#' @param as.mat TRUE/FALSE whether to return the full moment matrix or only
#' the vector with the unique elements (the latter is advised for speed), default
#' TRUE
#' @author Dries Cornilly
#' @seealso \code{\link{CoMoments}} \cr \code{\link{ShrinkageMoments}} \cr \code{\link{EWMAMoments}} \cr \code{\link{MCA}}
#' @references Boudt, Kris, Lu, Wanbo and Peeters, Benedict. 2015. Higher order comoments of multifactor 
#' models and asset allocation. Finance Research Letters, 13, 225-233.
#' 
#' Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008.
#' Estimation and Decomposition of Downside Risk for Portfolios with Non-Normal
#' Returns. Journal of Risk. Winter.
#' 
#' Boudt, Kris, Cornilly, Dries and Verdonck, Tim. 2017. A Coskewness Shrinkage 
#' Approach for Estimating the Skewness of Linear Combinations of Random Variables. 
#' Submitted. Available at SSRN: https://ssrn.com/abstract=2839781
#' 
#' Ledoit, Olivier and Wolf, Michael. 2003. Improved estimation of the covariance matrix 
#' of stock returns with an application to portfolio selection. Journal of empirical 
#' finance, 10(5), 603-621.
#' 
#' Martellini, Lionel and Ziemann, V\"olker. 2010. Improved estimates of higher-order 
#' comoments and implications for portfolio selection. Review of Financial 
#' Studies, 23(4), 1467-1502.
#' 
#' Simaan, Yusif. 1993. Portfolio selection and asset pricing: three-parameter framework. 
#' Management Science, 39(5), 68-577.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' 
#' # structured estimation with constant correlation model
#' # 'as.mat = F' would speed up calculations in higher dimensions
#' sigma <- M2.struct(edhec, "CC")
#' m3 <- M3.struct(edhec, "CC")
#' m4 <- M4.struct(edhec, "CC")
#' 
#' # compute equal-weighted portfolio modified ES
#' mu <- colMeans(edhec)
#' p <- length(mu)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' # compare to sample method
#' sigma <- cov(edhec)
#' m3 <- M3.MM(edhec)
#' m4 <- M4.MM(edhec)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' @export M2.struct
M2.struct <- function(R, struct = c("Indep", "IndepId", "observedfactor", "CC"), f = NULL) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes different strutured covariance estimators
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # struct    : select the structured estimator
  #           : Indep   : independent, unequal marginals (Ledoit and Wolf (2004))
  #           : IndepId : independent, equal marginals (Ledoit and Wolf (2003))
  #           : observedfactor : 1-factor or multi-factor linear model
  #           : CC : constant-correlation model of (Ledoit and Wolf (2004))
  # f         : numeric vector or matrix with factor observations
  #
  # Outputs:
  # covariance matrix
  
  X <- coredata(R)
  
  # input checking
  struct <- match.arg(struct)
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if ((struct == "observedfactor") && is.null(f)) stop("Provide the factor observations")
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  PP <- dim(X)[2]                                                   # number of assets
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  margvars <- colMeans(Xc^2)
  
  # compute the coskewness matrix
  if (struct == "Indep") {
    # independent marginals
    T2 <- diag(margvars)
    return (T2)
    
  } else if (struct == "IndepId") {
    # independent and equally distributed marginals
    T2 <- mean(margvars) * diag(PP)
    return (T2)
    
  } else if (struct == "observedfactor") {
    # linear factor model
    if (NCOL(f) == 1) {
      beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
      fc <- f - mean(f)
      fvar <- mean(fc^2)
      T2 <- fvar * beta %*% t(beta)
      diag(T2) <- margvars
    } else {
      mod <- stats::lm(X ~ f)
      beta <- t(mod$coefficients[-1,])
      fcov <- cov(f) * (NN - 1) / NN
      T2 <- beta %*% fcov %*% t(beta)
      epsvars <- colMeans(mod$residuals^2)
      T2 <- T2 + diag(epsvars)
    }
    return (T2)
    
  } else if (struct == "CC") {
    # constant-correlation
    sd_vec <- sqrt(margvars)
    M2 <- t(Xc) %*% Xc / NN
    R2 <- diag(1 / sd_vec) %*% M2 %*% diag(1 / sd_vec)
    rcoef <- mean(R2[upper.tri(R2)])
    R2 <- matrix(rcoef, nrow = PP, ncol = PP)
    diag(R2) <- 1
    T2 <- diag(sd_vec) %*% R2 %*% diag(sd_vec)
    return (T2)
    
  }
}


#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname StructuredMoments
M3.struct <- function(R, struct = c("Indep", "IndepId", "observedfactor", "CC", "latent1factor", "CS"),
                      f = NULL, unbiasedMarg = FALSE, as.mat = TRUE) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes different strutured coskewness estimators
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # struct    : select the structured estimator
  #           : Indep   : independent, unequal marginals
  #           : IndepId : independent, equal marginals
  #           : observedfactor : observed linear factor model
  #           : CC : constant-correlation model of Martellini and Ziemann (2010), symmetrized
  #           : latent1factor : latent 1-factor model of Simaan (1993)
  #           : CS : central-symmetric coskewness matrix (all zeros)
  # f         : numeric vector with factor observations,
  # unbiasedMarg : boolean determining if bias is corrected when estimating the marginals with
  #              : methods "Indep" or "INdepID"
  #
  # Outputs:
  # coskewness matrix (as matrix or as vector, depending on as.mat)
  
  X <- coredata(R)
  
  # input checking
  struct <- match.arg(struct)
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if ((struct == "observedfactor") && is.null(f)) stop("Provide the factor observations")
  if (unbiasedMarg && !((struct == "Indep") || (struct == "IndepId"))) stop("unbiasedMarg can only be combined with T2, T3 and T6")
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  if (unbiasedMarg) {
    if (NN < 6) stop("R should have at least 6 observations")
  }
  PP <- dim(X)[2]                                                   # number of assets
  ncosk <- PP * (PP + 1) * (PP + 2) / 6                             # number of unique coskewness elements
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  margskews <- colMeans(Xc^3)
  
  # compute the coskewness matrix
  if (struct == "latent1factor") {
    # 1-factor model of Simaan (1993)
    margskewsroot <- sign(margskews) * abs(margskews)^(1 / 3)
    T3 <- .Call('M3_Simaan', margskewsroot, PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T3 <- M3.vec2mat(T3, PP)
    return (T3)
    
  } else if (struct == "Indep") {
    # independent marginals
    if (unbiasedMarg) margskews <- margskews * NN^2 / ((NN - 1) * (NN - 2))
    T3 <- .Call('M3_T23', margskews, PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T3 <- M3.vec2mat(T3, PP)
    return (T3)
    
  } else if (struct == "IndepId") {
    # independent and equally distributed marginals
    if (unbiasedMarg) margskews <- margskews * NN^2 / ((NN - 1) * (NN - 2))
    margskews <- rep(mean(margskews), PP)
    T3 <- .Call('M3_T23', margskews, PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T3 <- M3.vec2mat(T3, PP)
    return (T3)
    
  } else if (struct == "observedfactor") {
    # observed factor model
    if (NCOL(f) == 1) {
      beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
      fc <- f - mean(f)
      fskew <- mean(fc^3)
      T3 <- .Call('M3_1F', margskews, beta, fskew, PP, PACKAGE="PerformanceAnalytics")
    } else {
      mod <- stats::lm(X ~ f)
      beta <- t(mod$coefficients[-1,])
      M3_factor <- M3.MM(f, as.mat = FALSE)
      M3_factor <- .Call('M3timesFull', M3_factor, beta, NCOL(beta), PP, PACKAGE="PerformanceAnalytics")
      epsskews <- colMeans(mod$residuals^3)
      T3 <- M3_factor + .Call('M3_T23', epsskews, PP, PACKAGE="PerformanceAnalytics")
    }
    if (as.mat) T3 <- M3.vec2mat(T3, PP)
    return (T3)
    
  } else if (struct == "CC") {
    # constant-correlation (Martellini and Ziemann (2010)) (symmetrized version)
    margvars <- colMeans(Xc^2)
    margkurts <- colMeans(Xc^4)
    m21 <- as.numeric(t(Xc^2) %*% Xc) / NN
    m22 <- as.numeric(t(Xc^2) %*% Xc^2) / NN
    r_generalized <- .Call('M3_CCoefficients', margvars, margkurts, m21,
                           m22, as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics")
    T3 <- .Call('M3_CC', margvars, margskews, margkurts, r_generalized[1], r_generalized[2],
                r_generalized[3], PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T3 <- M3.vec2mat(T3, PP)
    return (T3)
    
  } else if (struct == "CS") {
    # coskewness matrix under central-symmetry
    return (rep(0, ncosk))
    
  }
}


#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname StructuredMoments
M4.struct <- function(R, struct = c("Indep", "IndepId", "observedfactor", "CC"), f = NULL, as.mat = TRUE) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes different strutured cokurtosis estimators
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # struct    : select the structured estimator
  #           : Indep   : independent, unequal marginals
  #           : IndepId : independent, equal marginals
  #           : observedfactor : linear factor model with observed factors
  #           : CC : constant-correlation model of Martellini and Ziemann (2010), symmetrized
  # f         : numeric vector with factor observations
  #
  # Outputs:
  # cokurtosis matrix (as matrix or as vector, depending on as.mat)
  
  X <- coredata(R)
  
  # input checking
  struct <- match.arg(struct)
  if (NCOL(X) < 2) stop("R must have at least 2 variables")
  if ((struct == "observedfactor") && is.null(f)) stop("Provide the factor observations")
  
  # compute useful variables
  NN <- dim(X)[1]                                                   # number of observations
  PP <- dim(X)[2]                                                   # number of assets
  
  Xc <- X - matrix(colMeans(X), nrow = NN, ncol = PP, byrow = TRUE) # center the observations
  margkurts <- colMeans(Xc^4)
  margvars <- colMeans(Xc^2)
  
  # compute the coskewness matrix
  if (struct == "Indep") {
    # independent marginals
    T4 <- .Call('M4_T12', margkurts, margvars, PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T4 <- M4.vec2mat(T4, PP)
    return (T4)
    
  } else if (struct == "IndepId") {
    # independent and equally distributed marginals
    meanmargkurts <- mean(margkurts)
    meank_iikk <- sqrt(mean(margvars^2))
    T4 <- .Call('M4_T12', rep(meanmargkurts, PP), rep(meank_iikk, PP), PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T4 <- M4.vec2mat(T4, PP)
    return (T4)
    
  } else if (struct == "observedfactor") {
    # linear factor model with observed factors
    if (NCOL(f) == 1) {
      beta <- apply(Xc, 2, function(a) cov(a, f) / var(f))
      fc <- f - mean(f)
      fvar <- mean(fc^2)
      fkurt <- mean(fc^4)
      epsvars <- margvars - beta^2 * fvar
      T4 <- .Call('M4_1f', margkurts, fvar, fkurt, epsvars, beta, PP, PACKAGE="PerformanceAnalytics")
    } else {
      mod <- stats::lm(X ~ f)
      beta <- t(mod$coefficients[-1,])
      M4_factor <- M4.MM(f, as.mat = FALSE)
      M4_factor <- .Call('M4timesFull', M4_factor, beta, NCOL(beta), PP, PACKAGE="PerformanceAnalytics")
      epskurts <- colMeans(mod$residuals^4)
      epsvars <- colMeans(mod$residuals^2)
      Stransf <- beta %*% cov(f) %*% t(beta) * (NN - 1) / NN
      M4_factor <- M4_factor + .Call('M4_MFresid', as.numeric(Stransf), epsvars, PP, PACKAGE="PerformanceAnalytics")
      T4 <- M4_factor + .Call('M4_T12', epskurts, epsvars, PP, PACKAGE="PerformanceAnalytics")
    }
    if (as.mat) T4 <- M4.vec2mat(T4, PP)
    return (T4)
    
  } else if (struct == "CC") {
    # constant-correlation (Martellini and Ziemann (2010))
    marg6s <- colMeans(Xc^6)
    m22 <- as.numeric(t(Xc^2) %*% Xc^2) / NN
    m31 <- as.numeric(t(Xc^3) %*% Xc) / NN
    r_generalized <- .Call('M4_CCoefficients', margvars, margkurts, marg6s,
                           m22, m31, as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics")
    T4 <- .Call('M4_CC', margvars, margkurts, marg6s, r_generalized[1], r_generalized[2], 
                r_generalized[3], r_generalized[4], PP, PACKAGE="PerformanceAnalytics")
    if (as.mat) T4 <- M4.vec2mat(T4, PP)
    return (T4)
    
  }
}


#' Functions for calculating EWMA comoments of financial time series
#' 
#' calculates exponentially weighted moving average covariance, coskewness and cokurtosis matrices
#' 
#' The coskewness and cokurtosis matrices are defined as the matrices of dimension 
#' p x p^2 and p x p^3 containing the third and fourth order central moments. They
#' are useful for measuring nonlinear dependence between different assets of the 
#' portfolio and computing modified VaR and modified ES of a portfolio.
#' 
#' EWMA estimation of the covariance matrix was popularized by the RiskMetrics report in 1996.
#' The M3.ewma and M4.ewma are straightforward extensions to the setting of third and fourth
#' order central moments
#' @name EWMAMoments
#' @concept co-moments
#' @concept moments
#' @aliases EWMAMoments M2.ewma M3.ewma M4.ewma
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns (with mean zero)
#' @param lambda decay coefficient
#' @param last.M2 last estimated covariance matrix before the observed returns R
#' @param last.M3 last estimated coskewness matrix before the observed returns R
#' @param last.M4 last estimated cokurtosis matrix before the observed returns R
#' @param as.mat TRUE/FALSE whether to return the full moment matrix or only
#' the vector with the unique elements (the latter is advised for speed), default
#' TRUE
#' @param \dots any other passthru parameters
#' @author Dries Cornilly
#' @seealso \code{\link{CoMoments}} \cr \code{\link{ShrinkageMoments}} \cr \code{\link{StructuredMoments}} \cr \code{\link{MCA}}
#' @references 
#' JP Morgan. Riskmetrics technical document. 1996.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' 
#' # EWMA estimation
#' # 'as.mat = F' would speed up calculations in higher dimensions
#' sigma <- M2.ewma(edhec, 0.94)
#' m3 <- M3.ewma(edhec, 0.94)
#' m4 <- M4.ewma(edhec, 0.94)
#' 
#' # compute equal-weighted portfolio modified ES 
#' mu <- colMeans(edhec)
#' p <- length(mu)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' # compare to sample method
#' sigma <- cov(edhec)
#' m3 <- M3.MM(edhec)
#' m4 <- M4.MM(edhec)
#' ES(p = 0.95, portfolio_method = "component", weights = rep(1 / p, p), mu = mu, 
#'     sigma = sigma, m3 = m3, m4 = m4)
#' 
#' @export M2.ewma
M2.ewma <- function(R, lambda = 0.97, last.M2 = NULL, ...) {
  # R         : numeric matrix of dimensions NN x PP; top of R are oldest observations, bottom are the newest
  #           : assumes a mean of zero
  # lambda    : decay parameter for the correlations
  if(hasArg(lambda_var)) lambda_var <- list(...)$lambda_var else lambda_var <- NULL
  
  x <- coredata(R)
  NN <- NROW(x)
  PP <- NCOL(x)
  
  if (is.null(last.M2)) {
    if (lambda > 1 - 1e-07) {
      lambda_vec <- rep(1 / NN, NN)
    } else {
      lambda_vec <- (1 - lambda) / (1 - lambda^NN) * lambda^((NN - 1):0)
    }
    Xc <- x * (matrix(lambda_vec, nrow = NN, ncol = PP, byrow = FALSE))^(1 / 2)
    M2 <- t(Xc) %*% Xc
    
    if (!is.null(lambda_var)) {
      sd_lambda <- sqrt(diag(M2))
      R2 <- diag(1 / sd_lambda) %*% M2 %*% diag(1 / sd_lambda)
      
      if (lambda_var > 1 - 1e-07) {
        lambda_var_vec <- rep(1 / NN, NN)
      } else {
        lambda_var_vec <- (1 - lambda_var) / (1 - lambda_var^NN) * lambda_var^((NN - 1):0)
      }
      sd_lambda_var <- sqrt(colSums(x^2 * matrix(lambda_var_vec, nrow = NN, ncol = PP, byrow = FALSE)))
      M2 <- diag(sd_lambda_var) %*% R2 %*% diag(sd_lambda_var)
    }
  } else {
    if (PP == 1) {
      x <- matrix(x, nrow = 1)
      NN <- 1
    }
    M2 <- last.M2
    for (tt in 1:NN) {
      xn <- matrix(x[tt,], nrow = 1)
      new.M2 <- t(xn) %*% xn
      M2 <- (1 - lambda) * new.M2 + lambda * M2
    }
  }
  
  return( M2 )
}

#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname EWMAMoments
M3.ewma <- function(R, lambda = 0.97, last.M3 = NULL, as.mat = TRUE, ...) {
  # R         : numeric matrix of dimensions NN x PP; top of R are oldest observations, bottom are the newest
  #           : assumes a mean of zero
  # lambda    : decay parameter for the standardized coskewnesses
  # lambda_var : if not NULL, use lambda_var for the variances
  # as.mat    : TRUE/FALSE whether or not the output is a matrix or not
  if(hasArg(lambda_var)) lambda_var <- list(...)$lambda_var else lambda_var <- NULL
  
  x <- coredata(R)
  NN <- NROW(x)
  PP <- NCOL(x)
  
  if (is.null(last.M3)) {
    if (lambda > 1 - 1e-07) {
      lambda_vec <- rep(1 / NN, NN)
    } else {
      lambda_vec <- (1 - lambda) / (1 - lambda^NN) * lambda^((NN - 1):0)
    }
    Xc <- x * (matrix(lambda_vec, nrow = NN, ncol = PP, byrow = FALSE))^(1 / 3)
    M3 <- .Call('M3sample', as.numeric(Xc), NN, PP, 1.0, PACKAGE="PerformanceAnalytics")
    
    if (!is.null(lambda_var)) {
      sd_lambda <- sqrt(colSums(x^2 * matrix(lambda_vec, nrow = NN, ncol = PP, byrow = FALSE)))
      R3 <- .Call('M3timesDiag', M3, 1 / sd_lambda, PP, PACKAGE="PerformanceAnalytics")
      
      if (lambda_var > 1 - 1e-07) {
        lambda_var_vec <- rep(1 / NN, NN)
      } else {
        lambda_var_vec <- (1 - lambda_var) / (1 - lambda_var^NN) * lambda_var^((NN - 1):0)
      }
      sd_lambda_var <- sqrt(colSums(x^2 * matrix(lambda_var_vec, nrow = NN, ncol = PP, byrow = FALSE)))
      
      M3 <- .Call('M3timesDiag', R3, sd_lambda_var, PP, PACKAGE="PerformanceAnalytics")
    }
  } else {
    if (PP == 1) {
      x <- matrix(x, nrow = 1)
      NN <- 1
      PP <- ncol(x)
    }
    if (NROW(last.M3) == PP) last.M3 <- M3.mat2vec(last.M3)
    M3 <- last.M3
    for (tt in 1:NN) {
      xn <- matrix(x[tt,], nrow = 1)
      new.M3 <- M3.MM(xn, as.mat = FALSE, mu = rep(0, PP))
      M3 <- (1 - lambda) * new.M3 + lambda * M3
    }
  }
  
  if (as.mat) M3 <- M3.vec2mat(M3, PP)
  
  return( M3 )
}

#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname EWMAMoments
M4.ewma <- function(R, lambda = 0.97, last.M4 = NULL, as.mat = TRUE, ...) {
  # R         : numeric matrix of dimensions NN x PP; top of R are oldest observations, bottom are the newest
  #           : assumes a mean of zero
  # lambda    : decay parameter for the standardized cokurtosisses
  # lambda_var : if not NULL, use lambda_var for the variances
  # as.mat    : TRUE/FALSE whether or not the output is a matrix or not
  if(hasArg(lambda_var)) lambda_var <- list(...)$lambda_var else lambda_var <- NULL
  
  x <- coredata(R)
  NN <- NROW(x)
  PP <- NCOL(x)
  
  if (is.null(last.M4)) {
    if (lambda > 1 - 1e-07) {
      lambda_vec <- rep(1 / NN, NN)
    } else {
      lambda_vec <- (1 - lambda) / (1 - lambda^NN) * lambda^((NN - 1):0)
    }
    Xc <- x * (matrix(lambda_vec, nrow = NN, ncol = PP, byrow = FALSE))^(1 / 4)
    M4 <- .Call('M4sample', as.numeric(Xc), NN, PP, PACKAGE="PerformanceAnalytics") * NN
    
    if (!is.null(lambda_var)) {
      sd_lambda <- sqrt(colSums(x^2 * matrix(lambda_vec, nrow = NN, ncol = PP, byrow = FALSE)))
      R4 <- .Call('M4timesDiag', M4, 1 / sd_lambda, PP, PACKAGE="PerformanceAnalytics")
      
      if (lambda_var > 1 - 1e-07) {
        lambda_var_vec <- rep(1 / NN, NN)
      } else {
        lambda_var_vec <- (1 - lambda_var) / (1 - lambda_var^NN) * lambda_var^((NN - 1):0)
      }
      sd_lambda_var <- sqrt(colSums(x^2 * matrix(lambda_var_vec, nrow = NN, ncol = PP, byrow = FALSE)))
      
      M4 <- .Call('M4timesDiag', R4, sd_lambda_var, PP, PACKAGE="PerformanceAnalytics")
    }
  } else {
    if (PP == 1) {
      x <- matrix(x, nrow = 1)
      NN <- 1
      PP <- ncol(x)
    }
    if (NROW(last.M4) == PP) last.M4 <- M4.mat2vec(last.M4)
    M4 <- last.M4
    for (tt in 1:NN) {
      xn <- matrix(x[tt,], nrow = 1)
      new.M4 <- M4.MM(xn, as.mat = FALSE, mu = rep(0, PP))
      M4 <- (1 - lambda) * new.M4 + lambda * M4
    }
  }
  
  if (as.mat) M4 <- M4.vec2mat(M4, PP)
  
  return( M4 )
}


#' Functions for doing Moment Component Analysis (MCA) of financial time series
#' 
#' calculates MCA coskewness and cokurtosis matrices
#' 
#' The coskewness and cokurtosis matrices are defined as the matrices of dimension 
#' p x p^2 and p x p^3 containing the third and fourth order central moments. They
#' are useful for measuring nonlinear dependence between different assets of the 
#' portfolio and computing modified VaR and modified ES of a portfolio.
#' 
#' MCA is a generalization of PCA to higher moments. The principal components in 
#' MCA are the ones that maximize the coskewness and cokurtosis present when projecting
#' onto these directions. It was introduced by Lim and Morton (2007) and applied to financial returns
#' data by Jondeau and Rockinger (2017)
#' 
#' If a coskewness matrix (argument M3) or cokurtosis matrix (argument M4) is passed in using ..., then 
#' MCA is performed on the given comoment matrix instead of the sample coskewness or cokurtosis matrix.
#' @name MCA
#' @concept co-moments
#' @concept moments
#' @aliases MCA M3.MCA M4.MCA
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns 
#' @param k the number of components to use
#' @param as.mat TRUE/FALSE whether to return the full moment matrix or only
#' the vector with the unique elements (the latter is advised for speed), default
#' TRUE
#' @param \dots any other passthru parameters
#' @author Dries Cornilly
#' @seealso \code{\link{CoMoments}} \cr \code{\link{ShrinkageMoments}} \cr \code{\link{StructuredMoments}} \cr \code{\link{EWMAMoments}}
#' @references 
#' Lim, Hek-Leng and Morton, Jason. 2007. Principal Cumulant Component Analysis. working paper
#' 
#' Jondeau, Eric and Jurczenko, Emmanuel. 2017. Moment Component Analysis: An Illustration 
#' With International Stock Markets. Journal of Business and Economic Statistics
#' @examples
#' data(edhec)
#' 
#' # coskewness matrix based on two components
#' M3mca <- M3.MCA(edhec, k = 2)$M3mca
#' 
#' # screeplot MCA 
#' M3dist <- M4dist <- rep(NA, ncol(edhec))
#' M3S <- M3.MM(edhec)  # sample coskewness estimator
#' M4S <- M4.MM(edhec)  # sample cokurtosis estimator
#' for (k in 1:ncol(edhec)) {
#'   M3MCA_list <- M3.MCA(edhec, k)
#'   M4MCA_list <- M4.MCA(edhec, k)
#' 
#'   M3dist[k] <- sqrt(sum((M3S - M3MCA_list$M3mca)^2))
#'   M4dist[k] <- sqrt(sum((M4S - M4MCA_list$M4mca)^2))
#' }
#' par(mfrow = c(2, 1))
#' plot(1:ncol(edhec), M3dist)
#' plot(1:ncol(edhec), M4dist)
#' par(mfrow = c(1, 1))
#' 
#' @export M3.MCA
M3.MCA <- function(R, k = 1, as.mat = TRUE, ...) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # MCA on coskewness matrix
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # k         : number of components to use
  # as.mat    : TRUE/FALSE whether or not the output is a matrix or not
  #
  # Outputs:
  # M3mca     : coskewness matrix based on k factors
  # converged : logical indicating convergence
  # iter      : number of iterations at convergence / at end 
  # U         : matrix with principal components
  
  x <- coredata(R)
  
  if (hasArg(maxit)) maxit <- list(...)$maxit else maxit <- 1000
  if (hasArg(abstol)) abstol <- list(...)$abstol else abstol <- 1e-05
  
  p <- NCOL(x)
  n <- NROW(x)
  
  # initialize projection matrix and sample coskewness matrix
  if (hasArg(M3)) {
    M3 <- list(...)$M3
    if (NROW(M3) == p) M3 <- M3.mat2vec(M3)
  } else {
    M3 <- M3.MM(x, as.mat = FALSE)
  }
  if (hasArg(U0)) U0 <- list(...)$U0 else U0 <- svd(M3.vec2mat(M3, p), nu = k, nv = 0)$u
  
  # iterate until convergence or maximum number of iterations is reached
  iter <- 0
  converged <- FALSE
  while ((iter < maxit) & !converged) {
    # project using the last projection matrix U0 and build new projection matrix U1
    Z3 <- .Call('M3HOOIiterator', M3, as.numeric(U0), p, k, PACKAGE="PerformanceAnalytics")
    U1 <- svd(Z3, nu = k, nv = 0)$u
    
    # check for convergence - absolute values are because the direction of the eigenvectors might alternate
    if (sqrt(sum((abs(U1) - abs(U0))^2)) < abstol) converged <- TRUE
    
    # update U and the iterator
    U0 <- U1
    iter <- iter + 1
  }
  
  # build estimated coskewness matrix from the projection matrix U0
  C3 <- .Call('M3timesFull', M3, as.numeric(t(U0)), p, k, PACKAGE="PerformanceAnalytics")
  M3mca <- .Call('M3timesFull', C3, as.numeric(U0), k, p, PACKAGE="PerformanceAnalytics")
  
  if (as.mat) M3mca <- M3.vec2mat(M3mca, p)
  
  return ( list("M3mca" = M3mca, "converged" = converged, "iter" = iter, "U" = U0) )
}


#'@useDynLib PerformanceAnalytics
#'@export
#'@rdname MCA
M4.MCA <- function(R, k = 1, as.mat = TRUE, ...) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # MCA on cokurtosis matrix
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # k         : number of components to use
  # as.mat    : TRUE/FALSE whether or not the output is a matrix or not
  #
  # Outputs:
  # M4mca     : cokurtosis matrix based on k factors
  # converged : logical indicating convergence
  # iter      : number of iterations at convergence / at end 
  # U         : matrix with principal components
  
  x <- coredata(R)
  
  if (hasArg(maxit)) maxit <- list(...)$maxit else maxit <- 1000
  if (hasArg(abstol)) abstol <- list(...)$abstol else abstol <- 1e-05
  
  p <- NCOL(x)
  n <- NROW(x)
  
  # initialize projection matrix and sample cokurtosis matrix
  if (hasArg(M4)) {
    M4 <- list(...)$M4
    if (NROW(M4) == p) M4 <- M4.mat2vec(M4)
  } else {
    M4 <- M4.MM(x, as.mat = FALSE)
  }
  if (hasArg(U0)) U0 <- list(...)$U0 else U0 <- svd(M4.vec2mat(M4, p), nu = k, nv = 0)$u
  
  # iterate until convergence or maximum number of iterations is reached
  iter <- 0
  converged <- FALSE
  while ((iter < maxit) & !converged) {
    # project using the last projection matrix U0 and build new projection matrix U1
    Z4 <- .Call('M4HOOIiterator', M4, as.numeric(U0), p, k, PACKAGE="PerformanceAnalytics")
    U1 <- svd(Z4, nu = k, nv = 0)$u
    
    # check for convergence - absolute values are because the direction of the eigenvectors might alternate
    if (sqrt(sum((abs(U1) - abs(U0))^2)) < abstol) converged <- TRUE
    
    # update U and the iterator
    U0 <- U1
    iter <- iter + 1
  }
  
  # build estimated coskewness matrix from the projection matrix U0
  C4 <- .Call('M4timesFull', M4, as.numeric(t(U0)), p, k, PACKAGE="PerformanceAnalytics")
  M4mca <- .Call('M4timesFull', C4, as.numeric(U0), k, p, PACKAGE="PerformanceAnalytics")
  
  if (as.mat) M4mca <- M4.vec2mat(M4mca, p)
  
  return ( list("M4mca" = M4mca, "converged" = converged, "iter" = iter, "U" = U0) )
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson and Kris Boudt
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################