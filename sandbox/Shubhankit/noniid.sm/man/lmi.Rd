\name{lmi}
\alias{lmi}
\title{Fitting Generalized Linear Models with HC and HAC Covariance Matrix Estimators}
\usage{
  lmi(formula, data, vcov = NULL, subset, weights,
    na.action, method = "qr", model = TRUE, x = FALSE,
    y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, ...)
}
\arguments{
  \item{formula}{an object of class "formula" (or one that
  can be coerced to that class): a symbolic description of
  the model to be fitted. The details of model
  specification are given under ‘Details’.}

  \item{data}{an optional data frame, list or environment
  (or object coercible by as.data.frame to a data frame)
  containing the variables in the model. If not found in
  data, the variables are taken from environment(formula),
  typically the environment from which lm is called.}

  \item{vcov}{HC-HAC covariance estimation}

  \item{weights}{an optional vector of weights to be used
  in the fitting process. Should be NULL or a numeric
  vector. If non-NULL, weighted least squares is used with
  weights weights (that is, minimizing sum(w*e^2));
  otherwise ordinary least squares is used. See also
  ‘Details’,}

  \item{subset}{an optional vector specifying a subset of
  observations to be used in the fitting process.}

  \item{na.action}{a function which indicates what should
  happen when the data contain NAs. The default is set by
  the na.action setting of options, and is na.fail if that
  is unset. The ‘factory-fresh’ default is na.omit.
  Another possible value is NULL, no action. Value
  na.exclude can be useful.}

  \item{method}{the method to be used; for fitting,
  currently only method = "qr" is supported; method =
  "model.frame" returns the model frame (the same as with
  model = TRUE, see below).}

  \item{model}{logicals. If TRUE the corresponding
  components of the fit (the model frame, the model matrix,
  the response, the QR decomposition) are returned.}

  \item{x}{logicals. If TRUE the corresponding components
  of the fit (the model frame, the model matrix, the
  response, the QR decomposition) are returned.}

  \item{y}{logicals. If TRUE the corresponding components
  of the fit (the model frame, the model matrix, the
  response, the QR decomposition) are returned.}

  \item{qr}{logicals. If TRUE the corresponding components
  of the fit (the model frame, the model matrix, the
  response, the QR decomposition) are returned.}

  \item{singular.ok}{logical. If FALSE (the default in S
  but not in R) a singular fit is an error.}

  \item{contrasts}{an optional list. See the contrasts.arg
  of model.matrix.default.}

  \item{offset}{this can be used to specify an a priori
  known component to be included in the linear predictor
  during fitting. This should be NULL or a numeric vector
  of length equal to the number of cases. One or more
  offset terms can be included in the formula instead or as
  well, and if more than one are specified their sum is
  used. See model.offset.}

  \item{\dots}{additional arguments to be passed to the low
  level regression fitting functions (see below).}
}
\description{
  lm is used to fit generalized linear models, specified by
  giving a symbolic description of the linear predictor and
  a description of the error distribution.
}
\details{
  see \code{\link{lm}}.
}
\author{
  The original R implementation of glm was written by Simon
  Davies working for Ross Ihaka at the University of
  Auckland, but has since been extensively re-written by
  members of the R Core team. The design was inspired by
  the S function of the same name described in Hastie &
  Pregibon (1992).
}
\keyword{HAC}
\keyword{HC}
\keyword{covariance}
\keyword{estimation}
\keyword{fitting}
\keyword{model}
\keyword{regression}

