lmi <- function (formula, data,vcov = NULL, subset, weights, na.action, method = "qr", 
          model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
          contrasts = NULL, offset, ...) 
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  if (method == "model.frame") 
    return(mf)
  else if (method != "qr") 
    warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) 
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(y)) 
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                    length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
                                                      3) else numeric(), residuals = y, fitted.values = 0 * 
                y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                                                                                0) else if (is.matrix(y)) nrow(y) else length(y))
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <- if (is.null(w)) 
      lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
             ...)
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                 ...)
  }
  class(z) <- c(if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  if (!qr) 
    z$qr <- NULL
  #z
  if(is.null(vcov)) {
    se <- vcov(z)
  } else {
    if (is.function(vcov))
      se <- vcov(z)
    else
      se <- vcov
  }
  z = list(z,vHaC = se) 
  z
}
