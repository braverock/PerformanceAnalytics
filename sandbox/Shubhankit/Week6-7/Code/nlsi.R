nlsi <- function (formula, data = parent.frame(),vcov = NULL, start, control = nls.control(), 
          algorithm = c("default", "plinear", "port"), trace = FALSE, 
          subset, weights, na.action, model = FALSE, lower = -Inf, 
          upper = Inf, ...) 
{
  formula <- as.formula(formula)
  algorithm <- match.arg(algorithm)
  if (!is.list(data) && !is.environment(data)) 
    stop("'data' must be a list or an environment")
  mf <- match.call()
  varNames <- all.vars(formula)
  if (length(formula) == 2L) {
    formula[[3L]] <- formula[[2L]]
    formula[[2L]] <- 0
  }
  form2 <- formula
  form2[[2L]] <- 0
  varNamesRHS <- all.vars(form2)
  mWeights <- missing(weights)
  pnames <- if (missing(start)) {
    if (!is.null(attr(data, "parameters"))) {
      names(attr(data, "parameters"))
    }
    else {
      cll <- formula[[length(formula)]]
      func <- get(as.character(cll[[1L]]))
      if (!is.null(pn <- attr(func, "pnames"))) 
        as.character(as.list(match.call(func, call = cll))[-1L][pn])
    }
  }
  else names(start)
  env <- environment(formula)
  if (is.null(env)) 
    env <- parent.frame()
  if (length(pnames)) 
    varNames <- varNames[is.na(match(varNames, pnames))]
  lenVar <- function(var) tryCatch(length(eval(as.name(var), 
                                               data, env)), error = function(e) -1)
  if (length(varNames)) {
    n <- sapply(varNames, lenVar)
    if (any(not.there <- n == -1)) {
      nnn <- names(n[not.there])
      if (missing(start)) {
        if (algorithm == "plinear") 
          stop("no starting values specified")
        warning("No starting values specified for some parameters.\n", 
                "Initializing ", paste(sQuote(nnn), collapse = ", "), 
                " to '1.'.\n", "Consider specifying 'start' or using a selfStart model", 
                domain = NA)
        start <- setNames(as.list(rep(1, length(nnn))), 
                          nnn)
        varNames <- varNames[i <- is.na(match(varNames, 
                                              nnn))]
        n <- n[i]
      }
      else stop(gettextf("parameters without starting value in 'data': %s", 
                         paste(nnn, collapse = ", ")), domain = NA)
    }
  }
  else {
    if (length(pnames) && any((np <- sapply(pnames, lenVar)) == 
                                -1)) {
      message(sprintf(ngettext(sum(np == -1), "fitting parameter %s without any variables", 
                               "fitting parameters %s without any variables"), 
                      paste(sQuote(pnames[np == -1]), collapse = ", ")), 
              domain = NA)
      n <- integer()
    }
    else stop("no parameters to fit")
  }
  respLength <- length(eval(formula[[2L]], data, env))
  if (length(n) > 0L) {
    varIndex <- n%%respLength == 0
    if (is.list(data) && diff(range(n[names(n) %in% names(data)])) > 
          0) {
      mf <- data
      if (!missing(subset)) 
        warning("argument 'subset' will be ignored")
      if (!missing(na.action)) 
        warning("argument 'na.action' will be ignored")
      if (missing(start)) 
        start <- getInitial(formula, mf)
      startEnv <- new.env(hash = FALSE, parent = environment(formula))
      for (i in names(start)) assign(i, start[[i]], envir = startEnv)
      rhs <- eval(formula[[3L]], data, startEnv)
      n <- NROW(rhs)
      wts <- if (mWeights) 
        rep(1, n)
      else eval(substitute(weights), data, environment(formula))
    }
    else {
      mf$formula <- as.formula(paste("~", paste(varNames[varIndex], 
                                                collapse = "+")), env = environment(formula))
      mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
      mf$lower <- mf$upper <- NULL
      mf[[1L]] <- as.name("model.frame")
      mf <- eval.parent(mf)
      n <- nrow(mf)
      mf <- as.list(mf)
      wts <- if (!mWeights) 
        model.weights(mf)
      else rep(1, n)
    }
    if (any(wts < 0 | is.na(wts))) 
      stop("missing or negative weights not allowed")
  }
  else {
    varIndex <- logical()
    mf <- list(0)
    wts <- numeric()
  }
  if (missing(start)) 
    start <- getInitial(formula, mf)
  for (var in varNames[!varIndex]) mf[[var]] <- eval(as.name(var), 
                                                     data, env)
  varNamesRHS <- varNamesRHS[varNamesRHS %in% varNames[varIndex]]
  m <- switch(algorithm, plinear = nlsModel.plinear(formula, 
                                                    mf, start, wts), port = nlsModel(formula, mf, start, 
                                                                                     wts, upper), nlsModel(formula, mf, start, wts))
  ctrl <- nls.control()
  if (!missing(control)) {
    control <- as.list(control)
    ctrl[names(control)] <- control
  }
  if (algorithm != "port") {
    if (!missing(lower) || !missing(upper)) 
      warning("upper and lower bounds ignored unless algorithm = \"port\"")
    convInfo <- .Call(C_nls_iter, m, ctrl, trace)
    nls.out <- list(m = m, convInfo = convInfo, data = substitute(data), 
                    call = match.call())
  }
  else {
    pfit <- nls_port_fit(m, start, lower, upper, control, 
                         trace, give.v = TRUE)
    iv <- pfit[["iv"]]
    msg.nls <- port_msg(iv[1L])
    conv <- (iv[1L] %in% 3:6)
    if (!conv) {
      msg <- paste("Convergence failure:", msg.nls)
      if (ctrl$warnOnly) 
        warning(msg)
      else stop(msg)
    }
    v. <- port_get_named_v(pfit[["v"]])
    cInfo <- list(isConv = conv, finIter = iv[31L], finTol = v.[["NREDUC"]], 
                  nEval = c(`function` = iv[6L], gradient = iv[30L]), 
                  stopCode = iv[1L], stopMessage = msg.nls)
    cl <- match.call()
    cl$lower <- lower
    cl$upper <- upper
    nls.out <- list(m = m, data = substitute(data), call = cl, 
                    convInfo = cInfo, convergence = as.integer(!conv), 
                    message = msg.nls)
  }
  nls.out$call$algorithm <- algorithm
  nls.out$call$control <- ctrl
  nls.out$call$trace <- trace
  nls.out$na.action <- attr(mf, "na.action")
  nls.out$dataClasses <- attr(attr(mf, "terms"), "dataClasses")[varNamesRHS]
  if (model) 
    nls.out$model <- mf
  if (!mWeights) 
    nls.out$weights <- wts
  nls.out$control <- control
  class(nls.out) <- "nls"
  nls.out
  if(is.null(vcov)) {
    se <- vcov(nls.out)
  } else {
    if (is.function(vcov))
      se <- vcov(nls.out)
    else
      se <- vcov
  }
  nls.out = list(nls.out,vHaC = se) 
  nls.out
  
}