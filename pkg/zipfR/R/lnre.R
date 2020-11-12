lnre <- function (type=c("zm", "fzm", "gigp"),
                  spc=NULL, debug=FALSE,
                  cost=c("gof", "chisq", "linear", "smooth.linear", "mse", "exact"),
                  m.max=15, runs=5,
                  method=c("Nelder-Mead", "NLM", "BFGS", "SANN", "Custom"),
                  exact=TRUE, sampling=c("Poisson", "multinomial"),
                  bootstrap=0, verbose=TRUE, parallel=1L,
                  ...)
{
  type <- match.arg(type)
  sampling <- match.arg(sampling)
  user.param <- list(...)               # model parameters passed by user
  user.pnames <- names(user.param)
  method <- match.arg(method)
  
  if (sampling == "multinomial") {
    warning("multinomial sampling not yet implemented, falling back to Poisson sampling")
    sampling <- "Poisson"
  }

  if (!is.function(cost)) {
    cost <- match.arg(cost)
    cost.function <- switch(cost,         # implementation of chosen cost function
                            gof=lnre.cost.gof,
                            chisq=lnre.cost.chisq,
                            linear=lnre.cost.linear,
                            smooth.linear=lnre.cost.smooth.linear,
                            mse=lnre.cost.mse,
                            exact=lnre.cost.mse, # use MSE cost with adjusted value for m.max
                            stop("internal error - can't find suitable cost function"))
  } else {
    cost.function <- cost
    cost <- "User"
  }

  constructor <- switch(type,           # select appropriate constructor function
                        zm = lnre.zm,
                        fzm = lnre.fzm,
                        gigp = lnre.gigp,
                        stop("internal error - can't find suitable LNRE model constructor"))

  model <- constructor(param=user.param) # initialize model with user-specified parameter values
  model$exact <- exact
  model$multinomial <- sampling == "multinomial"
  
  pnames <- names(model$param)          # get list of model parameters

  given.param <- pnames[pnames %in% user.pnames]
  missing.param <- pnames[! (pnames %in% user.pnames)]
  unknown.param <- user.pnames[! (user.pnames %in% pnames)]
  if (length(unknown.param) > 0) warning("unknown parameter(s) ignored: ", unknown.param)

  if (length(missing.param) > 0) {
    ## incomplete model -> estimate parameters from observed frequency spectrum
    if (missing(spc)) stop("parameter(s) ", paste(missing.param, collapse=", ")," not specified")
    if (debug) {
      cat("Estimating parameter(s) <", missing.param, "> from observed spectrum.\n")
      cat("Default values for other parameters:\n")
      print(as.data.frame(model$param))
    }

    if (cost == "exact") {
      ## adjust m.max for "exact" parameter estimation (to match V and first V_m exactly)
      m.max <- max(length(missing.param) - 1, 1)
    }
    else if (missing(m.max) && !is.null(spc)) {
      ## otherwise auto-adjust unspecified m.max to avoid low-frequency spectrum elements with poor normal approximation
      keep <- Vm(spc, 1:m.max) >= 5
      if (!all(keep)) {
        new.max <- min(which(!keep)) - 1
        new.max <- max(new.max, length(missing.param) + 2) # need at least as many spectrum elements as parameters to be estimated (better a few more)
        m.max <- min(m.max, new.max)
      }
    }
      
    if (method == "Custom") { # custom estimation uses method call to fall back on default automatically
      if (is.null(spc)) stop("method='Custom' requires a non-NULL observed frequency spectrum")
      model <- estimate.model(model, spc=spc, param.names=missing.param, debug=debug,
                              method=method, cost.function=cost.function, m.max=m.max, runs=runs)
    }
    else {
      model <- estimate.model.lnre(model, spc=spc, param.names=missing.param, debug=debug,
                                   method=method, cost.function=cost.function, m.max=m.max, runs=runs)
    }
    
    model$spc <- spc
    
    if (bootstrap > 0) {
      model$bootstrap <- lnre.bootstrap(
        model, N(spc), ESTIMATOR=lnre, STATISTIC=identity, replicates=bootstrap, simplify=FALSE, verbose=verbose, parallel=parallel,
        type=type, cost=cost, m.max=m.max, method=method, exact=exact, sampling=sampling, debug=FALSE, ...)
    }
  }
  else {
    ## all parameters specified -> no estimation necessary
    if (!is.null(spc)) warning("no use for observed frequency spectrum 'spc' (ignored)")
    if (bootstrap > 0) warning("can't bootstrap fully specified model (skipped)")
    ## parameter values have already been set in constructor call above
  }

  model
}
  
