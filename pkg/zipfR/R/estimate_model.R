## internal: estimate parameters of LNRE model of specified class
estimate.model <- function (model, spc, param.names,
                            method, cost.function, m.max=15,
                            runs=3, debug=FALSE, ...)
{
  UseMethod("estimate.model")
}

## generic estimation procedure for class 'lnre'
estimate.model.lnre <- function (model, spc, param.names,
                                 method, cost.function, m.max=15,
                                 runs=3, debug=FALSE, ...)
{
  ## "Custom" not implemented for this LNRE model -> fall back to standard optimization method
  if (method == "Custom") {
    method <- if (length(param.names) > 1) "Nelder-Mead" else "NLM" # probably the best choices
  }
  
  compute.cost <- function (P.vector, param.names, model, spc, m.max=15, debug=FALSE)
    {
      P.trans <- as.list(P.vector)                     # translate parameter vector into list
      names(P.trans) <- param.names
      P <- model$util$transform(P.trans, inverse=TRUE) # convert parameters to normal scale
      model <- model$util$update(model, P)             # update model parameters (checks ranges)
      cost <- cost.function(model, spc, m.max)

      if (debug) {
        report <- as.data.frame(model$param)
        report$cost <- round(cost, digits=2)
        rownames(report) <- ""
        print(report)
      }
      cost
    }

  res.list <- list()
  cost.list <- numeric(0)
  err.list <- character(0)
  for (R. in 1:runs) {
    ## vector of init values for parameters in transformed scale (same order as in param.names)
    param.values <- if (R. == 1) rep(0, length(param.names)) else rnorm(length(param.names), sd=2)
    
    if (method == "NLM") {                # NLM = standard nonlinear minimization
      result <- try(
        nlm(compute.cost, param.values, print.level=debug, stepmax=10, steptol=1e-12,
            param.names=param.names, model=model, spc=spc, m.max=m.max, debug=debug),
        silent=TRUE)

      if (inherits(result, "try-error")) {
        err.list <- append(err.list, result)
      } else {
        res.code <- result$code
        if (res.code > 3) {
          err.list <- append(err.list, sprintf("parameter estimation failed (code %d)", res.code))
        }
        else if (res.code == 3) {
          err.list <- append(err.list, "estimated parameter values may be incorrect (code 3)")
        }
        else {
          P.estimate <- as.list(result$estimate)
          names(P.estimate) <- param.names
          res.list <- append(res.list, list(P.estimate))
          cost.list <- append(cost.list, result$minimum)
        }
      }
    }
    else {                                # Nelder-Mead, SANN, BFGS = selected optim() algorithm
      result <- try(
        optim(param.values, compute.cost, method=method,
              control=list(trace=debug, reltol=1e-12),
              param.names=param.names, model=model, spc=spc, m.max=m.max, debug=debug),
        silent=TRUE)

      if (inherits(result, "try-error")) {
        err.list <- append(err.list, result)
      } else {
        res.conv <- result$convergence
        if (res.conv > 1) {
          err.list <- append(err.list, sprintf("parameter estimation failed (code %d)", res.conv))
        }
        else if (res.conv > 0) {
          err.list <- append(err.list, "iteration limit exceeded, estimated parameter values may be incorrect (code 1)")
        }
        else {
          P.estimate <- as.list(result$par)
          names(P.estimate) <- param.names
          res.list <- append(res.list, list(P.estimate))
          cost.list <- append(cost.list, result$value)
        }
      }
    }
  }

  if (length(res.list) == 0) {
    stop("parameter estimation failed (errors: ", paste(err.list, collapse="; "), ")")
  }
  idx <- which.min(cost.list) # best run
  P.estimate <- res.list[[idx]]
  
  model <- model$util$update(model, P.estimate, transformed=TRUE)
  model$gof <- lnre.goodness.of.fit(model, spc, n.estimated=length(param.names))
    
  model
}

