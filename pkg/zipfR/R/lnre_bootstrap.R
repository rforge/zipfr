##
##  Parametric bootstrapping can be used to obtain approximate confidence intervals for parameters, predictions, etc.
##

lnre.bootstrap <- function (model, N, ESTIMATOR, STATISTIC, replicates=100, sample=c("spc", "tfl", "tokens"), 
                            simplify=TRUE, verbose=TRUE, parallel=1L, seed=NULL, ...) {
  if (!inherits(model, "lnre")) stop("first argument must belong to a subclass of 'lnre'")
  if (!is.function(sample)) sample <- match.arg(sample)
  stopifnot(replicates >= 1)
  stopifnot(is.function(ESTIMATOR))
  stopifnot(is.function(STATISTIC))

  par.method <- "single"
  if (inherits(parallel, "cluster")) {
    n.proc <- length(clusterCall(parallel, function () library(zipfR)))
    par.method <- "cluster"
  } else {
    n.proc <- parallel
    stopifnot(is.numeric(n.proc), length(n.proc) == 1, n.proc >= 1)
    n.proc <- as.integer(n.proc)
    if (n.proc > 1) par.method <- "fork"
  }

  ## select batch size so that
  ##  - each batch computes at most 10 replicate pre process
  ##  - there are at least 10 batches (assuming that small value of replicates indicates high computational cost)
  ##  - batch size is a multiple of n.proc so that work is divided evenly
  n.batch <- n.proc * max(1, min(10, floor(replicates / (10 * n.proc))))
  if (par.method == "single") n.batch <- 1
  
  ## worker function that generates and processes a single sample
  ## (all parameters should be bound by the closure, so no arguments need to be passed)
  SAMPLER <- if (is.function(sample)) sample else
    switch(sample,
           tokens = function (model, n) rlnre(model, n, what="tokens"),
           tfl = function (model, n) rlnre(model, n, what="tfl"),
           spc = function (model, n) tfl2spc(rlnre(model, n, what="tfl")))

  .worker <- function (n) {
    .sample <- SAMPLER(model, n)
    .estimated.model <- try(suppressWarnings(ESTIMATOR(.sample, ...)), silent=TRUE)
    if (is(.estimated.model, "try-error")) return(NULL)
    .stats <- try(suppressWarnings(STATISTIC(.estimated.model)), silent=TRUE)
    if (is(.stats, "try-error")) return(NULL)
    return(.stats)
  }
  
  if (verbose) {
    if (n.proc > 1) {
      cat(sprintf("Bootstrapping from %s object, using %d processes ...\n", class(model)[1], n.proc))
    } else {
      cat(sprintf("Bootstrapping from %s object ...\n", class(model)[1]))
    }
    .progress <- txtProgressBar(min=0, max=replicates, initial=0, style=3)
  }

  .result <- list()
  .errors <- 0
  if (!is.null(seed)) set.seed(seed)
  .got <- 0
  while (.got < replicates) {
    if (.errors > replicates) stop("failure rate > 50%, procedure aborted")
    n <- min(n.batch, replicates - .got)
    n <- n.proc * ceiling(n / n.proc) # round up to multiple of workers
    batch <- switch(
      par.method,
      single = list(.worker(N)),
      fork = mclapply(rep(N, n), .worker, mc.cores=n.proc),
      cluster = clusterApply(parallel, rep(N, n), .worker),
      stop("internal error (invalid par.method)"))
    stopifnot(n == length(batch)) # sanity check
    for (res in batch) {
      if (is.null(res)) {
        .errors <- .errors + 1
      } else {
        .got <- .got + 1
        .result[[.got]] <- res
      }
    }
    if (verbose) setTxtProgressBar(.progress, min(.got, replicates))
  }

  if (verbose) {
    close(.progress)
    if (.errors > 0) cat("[bootstrap failed for", .errors, "samples]\n")
  }
  if (.got > replicates) .result <- .result[1:replicates]
  
  if (simplify) {
    do.call(rbind, .result)
  }
  else {
    attr(.result, "N") <- N
    attr(.result, "errors") <- .errors
    attr(.result, "model") <- model
    .result
  }
}

## ***TODO*** -- implement and document prediction intervals as predict.lnre method (? or in EV, EVm, ...)
## bootstrap.extrap <- function (model, replicates=100, ...) {
##   lnre.bootstrap(
##     model,
##     function (spc, ...) lnre(model$type, spc, ...),
##     function (m, ...) c(EV(m, 2*N(m)), EVm(m, 1, 2*N(m))),
##     col.names=c("EV.2N", "EV1.2N"), verbose=TRUE, replicates=replicates,
##     ...
##   )
## }
