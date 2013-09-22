#'@title Fitting Generalized Linear Models with HC and HAC Covariance Matrix Estimators
#'@description
#' glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution.
#' @details
#' see  \code{\link{glm}}.
#' @param formula  
#'an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#'@param family  
#' a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See family for details of family functions.)
#'@param data  
#'an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
#'
#'@param vcov HC-HAC covariance estimation
#'@param weights an optional vector of weights to be used in the fitting process. 
#'@param subset	
#'an optional vector specifying a subset of observations to be used in the fitting process.
#'
#'
#'@param na.action	
#'a function which indicates what should happen when the data contain NAs. Another possible value is NULL, no action. Value na.exclude can be useful.
#'
#'@param start  
#'starting values for the parameters in the linear predictor.
#'
#'@param etastart	
#'starting values for the linear predictor.
#'
#'@param mustart	
#'starting values for the vector of means.
#'
#'@param offset	
#'this can be used to specify an a priori known component to be included in the linear predictor during fitting. This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included in the formula instead or as well, and if more than one is specified their sum is used. See model.offset.
#'
#'@param control	
#'a list of parameters for controlling the fitting process. For glm.fit this is passed to glm.control.
#'
#'@param model	
#' a logical value indicating whether model frame should be included as a component of the returned value.
#'@param method	
#'the method to be used; for fitting, currently only method = "qr" is supported; method = "model.frame" returns the model frame (the same as with model = TRUE, see below).
#'
#'@param x logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#'@param y logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#'
#'@param contrasts	
#'an optional list. See the contrasts.arg of model.matrix.default.
#'
#'@param \dots	
#'additional arguments to be passed to the low level regression fitting functions (see below).
#' @author The original R implementation of glm was written by Simon Davies working for Ross Ihaka at the University of Auckland, but has since been extensively re-written by members of the R Core team.
#' The design was inspired by the S function of the same name described in Hastie & Pregibon (1992).
#' @examples
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' print(d.AD <- data.frame(treatment, outcome, counts))
#'glm.D93 <- glmi(counts ~ outcome + treatment, family = poisson())
#'summary(glm.D93)
#' @keywords HC HAC covariance estimation regression fitting model
#' @rdname glmi
#' @export
glmi <- function (formula, family = gaussian, data,vcov = NULL, weights, subset, 
          na.action, start = NULL, etastart, mustart, offset, control = list(...), 
          model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, 
          ...) 
{
  call <- match.call()
  if (is.character(family)) 
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "etastart", "mustart", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  if (identical(method, "model.frame")) 
    return(mf)
  if (!is.character(method) && !is.function(method)) 
    stop("invalid 'method' argument")
  if (identical(method, "glm.fit")) 
    control <- do.call("glm.control", control)
  mt <- attr(mf, "terms")
  Y <- model.response(mf, "any")
  if (length(dim(Y)) == 1L) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) 
      names(Y) <- nm
  }
  X <- if (!is.empty.model(mt)) 
    model.matrix(mt, mf, contrasts)
  else matrix(, NROW(Y), 0L)
  weights <- as.vector(model.weights(mf))
  if (!is.null(weights) && !is.numeric(weights)) 
    stop("'weights' must be a numeric vector")
  if (!is.null(weights) && any(weights < 0)) 
    stop("negative weights not allowed")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(Y)) 
      stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
                    length(offset), NROW(Y)), domain = NA)
  }
  mustart <- model.extract(mf, "mustart")
  etastart <- model.extract(mf, "etastart")
  fit <- eval(call(if (is.function(method)) "method" else method, 
                   x = X, y = Y, weights = weights, start = start, etastart = etastart, 
                   mustart = mustart, offset = offset, family = family, 
                   control = control, intercept = attr(mt, "intercept") > 
                     0L))
  if (length(offset) && attr(mt, "intercept") > 0L) {
    fit2 <- eval(call(if (is.function(method)) "method" else method, 
                      x = X[, "(Intercept)", drop = FALSE], y = Y, weights = weights, 
                      offset = offset, family = family, control = control, 
                      intercept = TRUE))
    if (!fit2$converged) 
      warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
    fit$null.deviance <- fit2$deviance
  }
  if (model) 
    fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  if (x) 
    fit$x <- X
  if (!y) 
    fit$y <- NULL
  fit <- c(fit, list(call = call, formula = formula, terms = mt, 
                     data = data, offset = offset, control = control, method = method, 
                     contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, 
                                                                             mf)))
  class(fit) <- c(fit$class, c("glm", "lm"))
  fit
  if(is.null(vcov)) {
    se <- vcov(fit)
  } else {
    if (is.function(vcov))
      se <- vcov(fit)
    else
      se <- vcov
  }
  fit = list(fit,vHaC = se) 
  fit
  
}