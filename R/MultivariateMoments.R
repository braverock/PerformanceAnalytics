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
    if (NN < 3) stop("X should have at least 3 observations")
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
  
  if (NCOL(M3_1) != 1) stop("M3_1 should only contain the unique coskewness elements (see M3mat2vec)")
  
  if (is.null(M3_2)) {
    M3_2 <- M3_1
  } else {
    if (length(M3_1) != length(M3_2)) stop("M3_2 should only contain the unique coskewness elements (see (M3mat2vec)")
  }
  
  .Call('M3innprod', M3_1, M3_2, as.integer(p), PACKAGE="PerformanceAnalytics")
}

# Computes inner product between M4_1 and M4_2 and otherwise the Frobenius norm of M4_1
M4.innprod <- function(p, M4_1, M4_2 = NULL) {
  # p         : dimension of the data
  # M4_1      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) * (p + 3) / 24)
  # M4_2      : numeric vector with unique coskewness elements (p * (p + 1) * (p + 2) * (p + 3) / 24)
  
  if (NCOL(M4_1) != 1) stop("M4_1 should only contain the unique coskewness elements (see M4mat2vec)")
  
  if (is.null(M4_2)) {
    M4_2 <- M4_1
  } else {
    if (length(M4_1) != length(M4_2)) stop("M4_2 should only contain the unique coskewness elements (see (M4mat2vec)")
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