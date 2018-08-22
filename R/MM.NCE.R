###############################################################################
# Functions to compute the Nearest Comoment Estimator.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson for PerformanceAnalytics
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id$
###############################################################################

### build objective function with gradient
NCE_obj <- function(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt,
                    W2 = NULL, W3 = NULL, W4 = NULL) {
  
  # model parameters
  nd <- 0
  if (is.null(B)) {
    Binclude <- 1
    B <- matrix(theta[(nd + 1):(nd + p * k)], ncol = k)
    nd <- nd + p * k
  } else {
    Binclude <- 0
  }
  if (is.null(epsvar) & (include.mom[1] | include.mom[3])) {
    epsvarinclude <- 1
    epsvar <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  } else {
    epsvarinclude <- 0
  }
  if (is.null(fskew) & include.mom[2]) {
    fskewinclude <- 1
    fskew <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  } else {
    fskewinclude <- 0
  }
  if (is.null(epsskew) & include.mom[2]) {
    epsskewinclude <- 1
    epsskew <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  } else {
    epsskewinclude <- 0
  }
  if (is.null(fkurt) & include.mom[3]) {
    fkurtinclude <- 1
    fkurt <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  } else {
    fkurtinclude <- 0
  }
  if (is.null(epskurt) & include.mom[3]) {
    epskurtinclude <- 1
    epskurt <- theta[(nd + 1):(nd + p)]
  } else {
    epskurtinclude <- 0
  }
  
  # model moments
  if (include.mom[1]) {
    mod2 <- B %*% t(B) + diag(epsvar)
    mod2 <- mod2[lower.tri(diag(p), diag = TRUE)]
  } else {
    mod2 <- NULL
  }
  if (include.mom[2]) {
    mod3 <- .Call('M3_T23', fskew, k, PACKAGE="PerformanceAnalytics")
    mod3 <- .Call('M3timesFull', mod3, as.numeric(B), k, p, PACKAGE="PerformanceAnalytics")
    mod3 <- mod3 + .Call('M3_T23', epsskew, p, PACKAGE="PerformanceAnalytics")
  } else {
    mod3 <- NULL
  }
  if (include.mom[3]) {
    mod4 <- .Call('M4_T12', fkurt, rep(1, k), k, PACKAGE="PerformanceAnalytics")
    mod4 <- .Call('M4timesFull', mod4, as.numeric(B), k, p, PACKAGE="PerformanceAnalytics")
    Stransf <- B %*% t(B)
    mod4 <- mod4 + .Call('M4_MFresid', as.numeric(Stransf), epsvar, p, PACKAGE="PerformanceAnalytics")
    mod4 <- mod4 + .Call('M4_T12', epskurt, epsvar, p, PACKAGE="PerformanceAnalytics")
  } else {
    mod4 <- NULL
  }
  
  nelem_mom <- c(p * (p + 1) / 2, p * (p + 1) * (p + 2) / 6, p * (p + 1) * (p + 2) * (p + 3) / 24)
  ### calculate objective value
  if (!is.null(W)) { # single W matrix for all moments - either diagonal or full
    if (NCOL(W) == 1) {
      # case of diagonal W matrix
      nd <- 0
      objval <- 0
      if (include.mom[1]) {
        mdiff2w <- (m2 - mod2) * W[(nd + 1):(nd + nelem_mom[1])]
        objval <- objval + sum(mdiff2w * (m2 - mod2))
        nd <- nd + nelem_mom[1]
      }
      if (include.mom[2]) {
        mdiff3w <- (m3 - mod3) * W[(nd + 1):(nd + nelem_mom[2])]
        objval <- objval + sum(mdiff3w * (m3 - mod3))
        nd <- nd + nelem_mom[2]
      }
      if (include.mom[3]) {
        mdiff4w <- (m4 - mod4) * W[(nd + 1):(nd + nelem_mom[3])]
        objval <- objval + sum(mdiff4w * (m4 - mod4))
      }
    } else {
      # case of full W matrix
      momdiff <- c(m2 - mod2, m3 - mod3, m4 - mod4)
      mdiffw <- W %*% momdiff
      objval <- sum(momdiff * mdiffw)
      nd <- 0
      if (include.mom[1]) {
        mdiff2w <- mdiffw[(nd + 1):(nd + nelem_mom[1])]
        nd <- nd + nelem_mom[1]
      }
      if (include.mom[2]) {
        mdiff3w <- mdiffw[(nd + 1):(nd + nelem_mom[2])]
        nd <- nd + nelem_mom[2]
      }
      if (include.mom[3]) {
        mdiff4w <- mdiffw[(nd + 1):(nd + nelem_mom[3])]
      }
    }
  } else { # seperate W matrices for covariance, coskewness or cokurtosis elements
    objval <- 0
    if (include.mom[1]) {
      if (NCOL(W2) == 1) {
        mdiff2w <- (m2 - mod2) * W2
      } else {
        mdiff2w <- W2 %*% (m2 - mod2)
      }
      objval <- objval + sum(mdiff2w * (m2 - mod2))
    }
    if (include.mom[2]) {
      if (NCOL(W3) == 1) {
        mdiff3w <- (m3 - mod3) * W3
      } else {
        mdiff3w <- W3 %*% (m3 - mod3)
      }
      objval <- objval + sum(mdiff3w * (m3 - mod3))
    }
    if (include.mom[3]) {
      if (NCOL(W4) == 1) {
        mdiff4w <- (m4 - mod4) * W4
      } else {
        mdiff4w <- W4 %*% (m4 - mod4)
      }
      objval <- objval + sum(mdiff4w * (m4 - mod4))
    }
  }
  
  ### calculate gradient
  if (include.mom[1]) {
    grad2 <- .Call('mod2grad', p, k, as.numeric(B), Binclude, epsvarinclude, PACKAGE="PerformanceAnalytics") %*% mdiff2w
    grad2 <- c(grad2, rep(0, length(theta) - length(grad2)))
  } else {
    grad2 <- rep(0, length(theta))
  }
  if (include.mom[2]) {
    grad3 <- .Call('mod3grad', p, k, as.numeric(B), Binclude, epsvarinclude,
                   fskew, fskewinclude, epsskewinclude, PACKAGE="PerformanceAnalytics") %*% mdiff3w
    grad3 <- c(grad3, rep(0, length(theta) - length(grad3)))
  } else {
    grad3 <- rep(0, length(theta))
  }
  if (include.mom[3]) {
    grad4 <- .Call('mod4grad', p, k, as.numeric(B), Binclude, epsvarinclude, epsvar,
                   fskewinclude, epsskewinclude, fkurt, fkurtinclude,
                   epskurt, epskurtinclude, PACKAGE="PerformanceAnalytics") %*% mdiff4w
  } else {
    grad4 <- rep(0, length(theta))
  }
  
  grad <- -2 * (grad2 + grad3 + grad4)
  
  return ( list("objective" = objval, "gradient" = grad) )
}


### build objective function without gradient
NCE_objDE <- function(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt, include.ineq) {
  
  objval <- NCE_obj(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt)$objective
  
  if (include.ineq) {
    ineq <- NCE_obj_ineq(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt)$constraints
    if (max(ineq) > 1e-10) objval <- objval + 1e10
  }
  
  return ( objval )
}

### build objective function gradient
NCE_objDE_grad <- function(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt, include.ineq) {
  
  grad <- NCE_obj(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt)$gradient
  
  return ( grad )
}


### moment inequalities
NCE_obj_ineq <- function(theta, p, k, W, m2, m3, m4, include.mom, B, epsvar, fskew, epsskew, fkurt, epskurt) {
  
  # model parameters
  nd <- 0
  if (is.null(B)) {
    Binclude <- 1
    B <- matrix(theta[(nd + 1):(nd + p * k)], ncol = k)
    nd <- nd + p * k
  } else {
    Binclude <- 0
  }
  if (is.null(epsvar) & (include.mom[1] | include.mom[3])) {
    epsvarinclude <- 1
    epsvar <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  } else {
    epsvarinclude <- 0
  }
  if (is.null(fskew) & include.mom[2]) {
    fskewinclude <- 1
    fskew <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  } else {
    fskewinclude <- 0
  }
  if (is.null(epsskew) & include.mom[2]) {
    epsskewinclude <- 1
    epsskew <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  } else {
    epsskewinclude <- 0
  }
  if (is.null(fkurt) & include.mom[3]) {
    fkurtinclude <- 1
    fkurt <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  } else {
    fkurtinclude <- 0
  }
  if (is.null(epskurt) & include.mom[3]) {
    epskurtinclude <- 1
    epskurt <- theta[(nd + 1):(nd + p)]
  } else {
    epskurtinclude <- 0
  }
  
  ### compute inequalities
  fineq <- fskew^2 + 1 - fkurt
  epsineq <- epsskew^2 / epsvar^3 + 1 - epskurt / epsvar^2
  objineq <- c(fineq, epsineq)
  
  ### compute gradient
  grad <- matrix(0, nrow = p + k, ncol = length(theta))
  nd <- 0
  if (Binclude) nd <- p * k
  if (include.mom[1] & epsvarinclude) {
    for (ii in 1:p) grad[k + ii, nd + ii] <- -3 * epsskew[ii]^2 / epsvar[ii]^4 + 2 * epskurt[ii] / epsvar[ii]^3
    nd <- nd + p
  }
  if (include.mom[2]) {
    if (fskewinclude) {
      for (ii in 1:k) grad[ii, nd + ii] <- 2 * fskew[ii]
      nd <- nd + k
    }
    if (epsskewinclude) {
      for (ii in 1:p) grad[k + ii, nd + ii] <- 2 * epsskew[ii] / epsvar[ii]^3
      nd <- nd + p
    }
  }
  if (include.mom[3]) {
    if (fkurtinclude) {
      for (ii in 1:k) grad[ii, nd + ii] <- -1
      nd <- nd + k
    }
    if (epskurtinclude) {
      for (ii in 1:p) grad[k + ii, nd + ii] <- -1 / epsvar[ii]^2
    }
  }
  
  return ( list("constraints" = objineq, "jacobian" = grad) )
}


#' Functions for calculating the nearest comoment estimator for financial time series
#' 
#' calculates NCE covariance, coskewness and cokurtosis matrices
#' 
#' The coskewness and cokurtosis matrices are defined as the matrices of dimension 
#' p x p^2 and p x p^3 containing the third and fourth order central moments. They
#' are useful for measuring nonlinear dependence between different assets of the 
#' portfolio and computing modified VaR and modified ES of a portfolio.
#' 
#' The nearest comoment estimator is a way to estimate the covariance, coskewness and
#' cokurtosis matrix by means of a latent multi-factor model. The method is proposed in 
#' CITE TODO.
#' @name NCE
#' @concept co-moments
#' @concept moments
#' @aliases NCE MM.NCE
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns (with mean zero)
#' @param include.mom select to compute covariance, coskewness or cokurtosis matrix
#' @param as.mat TRUE/FALSE whether to return the full moment matrix or only
#' the vector with the unique elements (the latter is advised for speed), default
#' TRUE
#' @param \dots any other passthru parameters
#' @author Dries Cornilly
#' @seealso \code{\link{CoMoments}} \cr \code{\link{ShrinkageMoments}} \cr \code{\link{StructuredMoments}}  
#' \cr \code{\link{EWMAMoments}} \cr \code{\link{MCA}}
#' @references 
#' TODO
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' 
#' # NCE estimation
#' TODO
#' 
#' @export MM.NCE
MM.NCE <- function(R, include.mom = c(TRUE, TRUE, TRUE), as.mat = TRUE, ...) {
  # @author Dries Cornilly
  #
  # DESCRIPTION:
  # computes the nearest comoment estimators as in Boudt, Cornilly and Verdonck (2017)
  #
  # Inputs:
  # R         : numeric matrix of dimensions NN x PP
  # include.mom : select to compute covariance, coskewness or cokurtosis matrix
  # as.mat    : output as a matrix or as the vector with only unique coskewness eleements
  #
  # Outputs:
  # M2nce     : NCE covariance matrix
  # M3nce     : NCE coskewness matrix
  # M4nce     : NCE cokurtosis matrix
  # optim.sol ; output of the optimizer
  
  x <- zoo::coredata(R)
  n <- nrow(x)
  p <- ncol(x)
  
  ### set number of factors to use
  if (hasArg(k)) k <- list(...)$k else k <- 1
  
  ### compute initial estimators, if necessary
  if (hasArg(m2)) {
    m2 <- list(...)$m2
  } else {
    if (include.mom[1]) m2 <- cov(x)[lower.tri(diag(p), diag = TRUE)] * (n - 1) / n else m2 <- NULL
  }
  if (hasArg(m3)) {
    m3 <- list(...)$m3
  } else {
    if (include.mom[2]) m3 <- M3.MM(x, as.mat = FALSE) else m3 <- NULL
  }
  if (hasArg(m4)) {
    m4 <- list(...)$m4
  } else {
    if (include.mom[3]) m4 <- M4.MM(x, as.mat = FALSE) else m4 <- NULL
  }
  
  ### buil W matrix, if necessary
  nelem_mom <- c(p * (p + 1) / 2, p * (p + 1) * (p + 2) / 6, p * (p + 1) * (p + 2) * (p + 3) / 24)
  nelem <- sum(include.mom * nelem_mom)
  if (hasArg(W)) {
    W <- list(...)$W 
    if (is.list(W)) {
      W2 <- W$W2
      W3 <- W$W3
      W4 <- W$W4
      W <- NULL
    } else {
      W2 <- W3 <- W4 <- NULL
    }
  } else {
    W <- rep(1, nelem)
    W2 <- W3 <- W4 <- NULL
  }
  
  ### prepare model parameters
  if (hasArg(B)) B <- list(...)$B else B <- NULL
  if (hasArg(epsvar)) epsvar <- list(...)$epsvar else epsvar <- NULL
  if (hasArg(fskew)) fskew <- list(...)$fskew else fskew <- NULL
  if (hasArg(epsskew)) epsskew <- list(...)$epsskew else epsskew <- NULL
  if (hasArg(fkurt)) fkurt <- list(...)$fkurt else fkurt <- NULL
  if (hasArg(epskurt)) epskurt <- list(...)$epskurt else epskurt <- NULL
  
  if (k == 0) {
    B <- matrix(0, nrow = p, ncol = 1)
    fskew <- 0
    fkurt <- 3
  }
  
  ### initial estimate
  if (hasArg(x0)) {
    x0 <- list(...)$x0
  } else {
    x0list <- NCEinitialPCA(x, k)
    x0 <- NULL
    if (is.null(B)) x0 <- c(x0, x0list$x0list$B)
    if (is.null(epsvar) & (include.mom[1] | include.mom[3])) x0 <- c(x0, x0list$x0list$Deltadiag)
    if (is.null(fskew) & include.mom[2]) x0 <- c(x0, x0list$x0list$Gmarg)
    if (is.null(epsskew) & include.mom[2]) x0 <- c(x0, x0list$x0list$Omarg)
    if (is.null(fkurt) & include.mom[3]) x0 <- c(x0, x0list$x0list$Pmarg)
    if (is.null(epskurt) & include.mom[3]) x0 <- c(x0, x0list$x0list$GGmarg)
  }
  
  ### fit the model
  if (k == 0) k <- 1 # since B, fskew and fkurt are already initialised at zeros, these are not optimized;
  # k=1 here tricks the objective function which requires at least one factor.
  if (hasArg(optimize_method)) optimize_method <- list(...)$optimize_method else optimize_method <- "nloptr"
  if (hasArg(include.ineq)) include.ineq <- list(...)$include.ineq else include.ineq <- FALSE
  if (hasArg(optscontrol)) {
    optscontrol <- list(...)$optscontrol
  } else {
    optscontrol <- list(algorithm = "NLOPT_LD_MMA", xtol_rel = 1e-05, 
                        ftol_rel = 1e-05, ftol_abs = 1e-05, maxeval = 1000, print_level = 0,
                        check_derivatives = FALSE)
  }
  lowerbound <- NULL
  if (is.null(B)) lowerbound <- c(lowerbound, rep(-Inf, p * k))
  if (is.null(epsvar) & (include.mom[1] | include.mom[3])) lowerbound <- c(lowerbound, rep(1e-10, p))
  if (is.null(fskew) & include.mom[2]) lowerbound <- c(lowerbound, rep(-Inf, k))
  if (is.null(epsskew) & include.mom[2]) lowerbound <- c(lowerbound, rep(-Inf, p))
  if (is.null(fkurt) & include.mom[3]) lowerbound <- c(lowerbound, rep(1e-10, k))
  if (is.null(epskurt) & include.mom[3]) lowerbound <- c(lowerbound, rep(3e-20, p))
  upperbound <- rep(Inf, length(lowerbound))
  
  if (optimize_method == "nloptr") {
    stopifnot("package:nloptr" %in% search() || requireNamespace("nloptr", quietly = TRUE))
    
    if (include.ineq) {
      sol <- nloptr::nloptr(x0 = x0, eval_f = NCE_obj, eval_g_ineq = NCE_obj_ineq,
                            lb = lowerbound, ub = upperbound, opts = optscontrol,
                            p = p, k = k, W = W, m2 = m2, m3 = m3, m4 = m4,
                            include.mom = include.mom, B = B, epsvar = epsvar, fskew = fskew,
                            epsskew = epsskew, fkurt = fkurt, epskurt = epskurt,
                            W2 = W2, W3 = W3, W4 = W4)
    } else {
      sol <- nloptr::nloptr(x0 = x0, eval_f = PerformanceAnalytics:::NCE_obj, lb = lowerbound, ub = upperbound,
                            opts = optscontrol, p = p, k = k, W = W, m2 = m2, m3 = m3, m4 = m4,
                            include.mom = include.mom, B = B, epsvar = epsvar, fskew = fskew,
                            epsskew = epsskew, fkurt = fkurt, epskurt = epskurt,
                            W2 = W2, W3 = W3, W4 = W4)
    }
    theta <- sol$solution
  } else if (optimize_method == "genoud") {
    ubM2 <- max(colMeans((x - matrix(colMeans(x), nrow = n, ncol = p, byrow = TRUE))^2)) # max of marginal variances of observations
    ubM3 <- max(abs(colMeans((x - matrix(colMeans(x), nrow = n, ncol = p, byrow = TRUE))^3))) # max of marginal variances of observations
    ubM4 <- max(abs(colMeans((x - matrix(colMeans(x), nrow = n, ncol = p, byrow = TRUE))^4))) # max of marginal variances of observations
    lowerbound2 <- NULL
    upperbound2 <- NULL
    if (is.null(B)) {
      lowerbound2 <- c(lowerbound2, rep(-sqrt(1.1 * ubM2), p * k))
      upperbound2 <- c(upperbound2, rep(sqrt(1.1 * ubM2), p * k))
    }
    if (is.null(epsvar) & (include.mom[1] | include.mom[3])) {
      lowerbound2 <- c(lowerbound2, rep(2e-10, p))
      upperbound2 <- c(upperbound2, rep(1.1 * ubM2, p))
    }
    if (is.null(fskew) & include.mom[2]) {
      lowerbound2 <- c(lowerbound2, rep(-3, k))
      upperbound2 <- c(upperbound2, rep(3, k))
    }
    if (is.null(epsskew) & include.mom[2]) {
      lowerbound2 <- c(lowerbound2, rep(-1.1 * ubM3 - 3 * k * (sqrt(1.1 * ubM2))^3 , p))
      upperbound2 <- c(upperbound2, rep(1.1 * ubM3 + 3 * k * (sqrt(1.1 * ubM2))^3 , p))
    }
    if (is.null(fkurt) & include.mom[3]) {
      lowerbound2 <- c(lowerbound2, rep(2e-10, k))
      upperbound2 <- c(upperbound2, rep(25, k))
    }
    if (is.null(epskurt) & include.mom[3]) {
      lowerbound2 <- c(lowerbound2, rep(6e-20, p))
      upperbound2 <- c(upperbound2, rep(1.1 * ubM4, p))
    }
    nvars <- length(lowerbound)
    stopifnot("package:rgenoud" %in% search() || requireNamespace("rgenoud", quietly = TRUE))
    
    if (hasArg(optscontrol_genoud)) {
      optscontrol_genoud <- list(...)$optscontrol_genoud
      pop.size <- optscontrol_genoud$pop.size
      max.generations <- optscontrol_genoud$max.generations
      print.level <- optscontrol_genoud$print.level
    } else {
      pop.size <- 500
      max.generations <- 100
      print.level <- 0
    }
    
    options(warn = -1)
    sol <- rgenoud::genoud(fn = NCE_objDE, nvars = nvars, max = FALSE, pop.size = pop.size,
                           max.generations = max.generations, wait.generations = 10, hard.generation.limit = TRUE,
                           starting.values = x0, MemoryMatrix = TRUE, Domains = cbind(lowerbound2, upperbound2),
                           solution.tolerance = 0.001, gr = NCE_objDE_grad,
                           boundary.enforcement = 2, lexical = FALSE, gradient.check = FALSE,
                           BFGS = TRUE, data.type.int = FALSE, hessian = FALSE,
                           unif.seed = 812821, int.seed = 53058, print.level = print.level, share.type = 0,
                           instance.number = 0, output.path = "stdout", output.append = FALSE, project.path = NULL,
                           P1 = 50, P2 = 50, P3 = 50, P4 = 50, P5 = 50, P6 = 50, P7 = 50, P8 = 50, P9 = 0,
                           P9mix = NULL, BFGSburnin = 0, BFGSfn = NULL, BFGShelp = NULL,
                           control = list(),
                           optim.method = "L-BFGS-B", transform = FALSE, debug = FALSE, cluster = FALSE, balance = FALSE,
                           p = p, k = k, W = W, m2 = m2, m3 = m3, m4 = m4, include.mom = include.mom, B = B,
                           epsvar = epsvar, fskew = fskew, epsskew = epsskew, fkurt = fkurt, epskurt = epskurt,
                           include.ineq = include.ineq)
    options(warn = 0)
    if (include.ineq) {
      sol <- nloptr::nloptr(x0 = sol$par, eval_f = NCE_obj, eval_g_ineq = NCE_obj_ineq,
                            lb = lowerbound, ub = upperbound, opts = optscontrol,
                            p = p, k = k, W = W, m2 = m2, m3 = m3, m4 = m4,
                            include.mom = include.mom, B = B, epsvar = epsvar, fskew = fskew,
                            epsskew = epsskew, fkurt = fkurt, epskurt = epskurt,
                            W2 = W2, W3 = W3, W4 = W4)
    } else {
      sol <- nloptr::nloptr(x0 = sol$par, eval_f = NCE_obj, lb = lowerbound, ub = upperbound,
                            opts = optscontrol, p = p, k = k, W = W, m2 = m2, m3 = m3, m4 = m4,
                            include.mom = include.mom, B = B, epsvar = epsvar, fskew = fskew,
                            epsskew = epsskew, fkurt = fkurt, epskurt = epskurt,
                            W2 = W2, W3 = W3, W4 = W4)
    }
    theta <- sol$solution
  }
  
  ### construct the moment estimates
  nd <- 0
  if (is.null(B)) {
    B <- matrix(theta[(nd + 1):(nd + p * k)], ncol = k)
    nd <- nd + p * k
  }
  if (is.null(epsvar) & (include.mom[1] | include.mom[3])) {
    epsvar <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  }
  if (is.null(fskew) & include.mom[2]) {
    fskew <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  }
  if (is.null(epsskew) & include.mom[2]) {
    epsskew <- theta[(nd + 1):(nd + p)]
    nd <- nd + p
  }
  if (is.null(fkurt) & include.mom[3]) {
    fkurt <- theta[(nd + 1):(nd + k)]
    nd <- nd + k
  }
  if (is.null(epskurt) & include.mom[3]) {
    epskurt <- theta[(nd + 1):(nd + p)]
  }
  
  if (include.mom[1]) {
    mod2 <- B %*% t(B) + diag(epsvar)
  } else {
    mod2 <- NULL
  }
  if (include.mom[2]) {
    mod3 <- .Call('M3_T23', fskew, k, PACKAGE="PerformanceAnalytics")
    mod3 <- .Call('M3timesFull', mod3, as.numeric(B), k, p, PACKAGE="PerformanceAnalytics")
    mod3 <- mod3 + .Call('M3_T23', epsskew, p, PACKAGE="PerformanceAnalytics")
    if (as.mat) mod3 <- M3.vec2mat(mod3, p)
  } else {
    mod3 <- NULL
  }
  if (include.mom[3]) {
    mod4 <- .Call('M4_T12', fkurt, rep(1, k), k, PACKAGE="PerformanceAnalytics")
    mod4 <- .Call('M4timesFull', mod4, as.numeric(B), k, p, PACKAGE="PerformanceAnalytics")
    Stransf <- B %*% t(B)
    mod4 <- mod4 + .Call('M4_MFresid', as.numeric(Stransf), epsvar, p, PACKAGE="PerformanceAnalytics")
    mod4 <- mod4 + .Call('M4_T12', epskurt, epsvar, p, PACKAGE="PerformanceAnalytics")
    if (as.mat) mod4 <- M4.vec2mat(mod4, p)
  } else {
    mod4 <- NULL
  }
  
  ### return object
  result <- list("M2nce" = mod2, "M3nce" = mod3, "M4nce" = mod4, "optim.sol" = sol)
  
  ### return model parameters if necessary
  if (hasArg(model.par)) model.par <- list(...)$model.par else model.par <- FALSE
  if (model.par) {
    result$model.par <- list("B" = B, "epsvar" = epsvar, "fskew" = fskew, 
                             "epsskew" = epsskew, "fkurt" = fkurt, "epskurt" = epskurt)
  }
  
  return ( result )
}


NCEinitialPCA <- function (x, k) {
  ### Computes initial starting values for the NCE algorithm based on PCA
  # x: data matrix, columns are dimension, each row is a new observation
  # k: number of factors to use
  
  n <- dim(x)[1]                                                      # number of observations
  p <- dim(x)[2]                                                      # number of dimensions
  
  x <- x - matrix(colMeans(x), nrow = n, ncol = p, byrow = TRUE)      # center the data
  
  if (k == 0) {
    x0list <- list()
    x0list$B <- NULL
    x0list$Deltadiag <- colMeans(x^2)
    x0list$Gmarg <- NULL
    x0list$Omarg <- colMeans(x^3)
    x0list$Pmarg <- NULL
    x0list$GGmarg <- colMeans(x^4)
    f <- NULL
    
  } else {
    pca <- princomp(x, cor = FALSE, scores = k)                         # compute PCA
    
    x0list <- list()                                                    # initialize list of starting values
    f <- pca$scores[, 1:k]                                              # compute factor estimates
    if (k == 1) f <- matrix(f, ncol = 1)
    sdf <- sqrt(apply(f, 2, var))                                       # to normalize f and B
    f <- f / matrix(sdf, nrow = n, ncol = k, byrow = TRUE)              # normalized f
    x0list$B <- as.matrix(pca$loadings[, 1:k]) * matrix(sdf, nrow = p, ncol = k, byrow = TRUE) # list the B matrix
    eps <- x - f %*% t(x0list$B)                                        # compute residual term
    
    x0list$Deltadiag <- apply(eps, 2, var)                              # list marginal variances of eps
    x0list$Gmarg <- apply(f, 2, function(a) sum((a - mean(a))^3) * n / ((n - 1) * (n - 2))) # list marginal skewness of factors
    x0list$Omarg <- apply(eps, 2, function(a) sum((a - mean(a))^3) * n / ((n - 1) * (n - 2))) # list marginal skewness of eps
    
    x0list$Pmarg <- apply(f, 2, function(a) sum((a - mean(a))^4) / n)   # list marginal kurtosis of factors
    x0list$GGmarg <- apply(eps, 2, function(a) sum((a - mean(a))^4) / n) # list marginal kurtosis of eps
  }
  
  x0 <- c(c(x0list$B), x0list$Deltadiag, x0list$Gmarg, x0list$Omarg, x0list$Pmarg, x0list$GGmarg)
  
  return ( list("x0list" = x0list, "x0" = x0, "f" = f) )
}


NCEconstructW <- function (X, Wid = "RidgeD", alpha = 0.1, include.mom = c(TRUE, TRUE, TRUE)) {
  
  n <- nrow(X)
  p <- ncol(X)
  Xc <- X - matrix(colMeans(X), nrow = n, ncol = p, byrow = TRUE)
  m11 <- t(Xc) %*% Xc / n
  W <- NULL
  nelem_mom <- c(p * (p + 1) / 2, p * (p + 1) * (p + 2) / 6, p * (p + 1) * (p + 2) * (p + 3) / 24)
  nelem <- sum(include.mom * nelem_mom)
  
  if (length(Wid) == 1 && Wid == "Id") {
    # equal-weighted weight matrix
    W <- rep(1, nelem)
    Xi <- NULL
  } else {
    # pseudo moment observations
    Xi_obs <- .Call("NCEAcov", as.numeric(m11), p, n, as.numeric(Xc),
                    include.mom[1], include.mom[2], include.mom[3], PACKAGE="PerformanceAnalytics")
    if (length(Wid) == 1) {
      if (Wid == "D") {
        # variance weighted diagonal W
        W <- apply(Xi_obs, 2, var) * (n - 1) / n
        W <- 1 / W
        Xi <- NULL
      }
      if ((Wid == "Opt") || ((Wid == "RidgeD") || (Wid == "RidgeI"))) {
        Xi <- cov(Xi_obs) * (n - 1) / n
        if (Wid == "Opt") {
          if (n > nelem) {
            W <- solve(Xi)
          } else {
            Wid <- "RidgeD"
          }
        }
        if (Wid == "RidgeD") {
          toInvert <- (1 - alpha) * Xi + alpha * diag(diag(Xi))
          W <- tryCatch(base::chol2inv(base::chol(toInvert)), 
                        error = function(x) base::chol2inv(base::chol(0.9 * Xi + 0.1 * diag(diag(Xi)))))
        }
        if (Wid == "RidgeI") {
          toInvert <- (1 - alpha) * Xi + alpha * diag(ncol(Xi))
          W <- tryCatch(base::chol2inv(base::chol(toInvert)), 
                        error = function(x) base::chol2inv(base::chol(0.9 * Xi + 0.1 * diag(ncol(Xi)))))
        }
        W <- 0.5 * (W + t(W))
      }
    } else {
      W <- vector('list', 3)
      nd <- 0
      for (ii in 1:3) {
        if (include.mom[ii]) {
          if (Wid[ii] == "D") {
            W[[ii]] <- n / ((n - 1) * apply(Xi_obs[, (nd + 1):(nd + nelem_mom[ii])], 2, var))
          } else if (Wid[ii] == "RidgeD") {
            Xi <- cov(Xi_obs[, (nd + 1):(nd + nelem_mom[ii])]) * (n - 1) / n
            toInvert <- (1 - alpha) * Xi + alpha * diag(diag(Xi))
            W[[ii]] <- tryCatch(base::chol2inv(base::chol(toInvert)), 
                                error = function(x) base::chol2inv(base::chol(0.9 * Xi + 0.1 * diag(diag(Xi)))))
          } else if (Wid[ii] == "RidgeI") {
            Xi <- cov(Xi_obs[, (nd + 1):(nd + nelem_mom[ii])]) * (n - 1) / n
            toInvert <- (1 - alpha) * Xi + alpha * diag(ncol(Xi))
            W[[ii]] <- tryCatch(base::chol2inv(base::chol(toInvert)), 
                                error = function(x) base::chol2inv(base::chol(0.9 * Xi + 0.1 * diag(ncol(Xi)))))
          } else {
            W[[ii]] <- rep(1, nelem_mom[ii])
          }
        }
        nd <- nd + include.mom[ii]
      }
      names(W) <- c("W2", "W3", "W4")
      Xi <- NULL
    }
  }
  
  return (list("W" = W, "Xi" = Xi))
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