lnre.productivity.measures <- function (model, N=NULL, measures, data.frame=TRUE, 
                                        bootstrap=FALSE, method="normal", conf.level=.95,
                                        replicates=1000, parallel=1L, verbose=TRUE, seed=NULL)
{
  if (! inherits(model, "lnre")) stop("first argument must belong to a subclass of 'lnre'")
  if (is.null(N)) N <- N(model)
  if (! (is.numeric(N) && all(N > 0))) stop("'N' must be a positive integer vector")

  supported <- qw("V TTR R C k U W P Hapax H S alpha2 K D")
  if (bootstrap) supported <- c(supported, qw("Entropy eta"))
  if (missing(measures) || is.null(measures)) measures <- supported
  measures <- sapply(measures, match.arg, choices=supported)
  
  ## empirical distribution from parametric bootstrapping
  if (bootstrap) {
    if (length(N) > 1L) stop("only a single 'N' value is allowed with bootstrap=TRUE")
    res <- lnre.bootstrap(model, N, ESTIMATOR=productivity.measures, measures=measures,
                          STATISTIC=identity, sample="spc", simplify=TRUE,
                          replicates=replicates, parallel=parallel, seed=seed, verbose=verbose)
    return(bootstrap.confint(res, level=conf.level, method=method, data.frame=data.frame))
  }
  
  ## delta = probability of drawing same type twice = second moment of tdf
  delta <- function (model) {
    if (inherits(model, "lnre.fzm")) {
      alpha <- model$param$alpha
      A <- model$param$A
      B <- model$param$B
      C <- model$param2$C
      C / (2 - alpha) * (B^(2 - alpha) - A^(2 - alpha))
    }
    else if (inherits(model, "lnre.zm")) {
      alpha <- model$param$alpha
      B <- model$param$B
      C <- model$param2$C
      C / (2 - alpha) * B^(2 - alpha)
    }
    else stop("cannot compute delta for LNRE model of class ", class(model)[1])
  }
  
  .EV <- EV(model, N)
  .EV1 <- EVm(model, 1, N)
  .EV2 <- EVm(model, 2, N)
  res <- sapply(measures, function (M.) {
    switch(M.,
           ## measures based on V and N
           V = .EV,
           TTR = .EV / N,
           R = .EV / sqrt(N),
           C = log(.EV) / log(N),
           k = log(.EV) / log(log(N)),
           U = log(N)^2 / (log(N) - log(.EV)),
           W = N ^ (.EV ^ -0.172),
           ## measures based on hapax count (V1)
           P = .EV1 / N,
           Hapax = .EV1 / .EV,
           H = 100 * log(N) / (1 - .EV1 / .EV),
           ## measures based on the first two spectrum elements (V1 and V2)
           S = .EV2 / .EV,
           alpha2 = 1 - 2 * .EV2 / .EV1,
           ## Yule K and Simpson D can only be computed from full spectrum
           K = 10e4 * (N - 1) / N * delta(model),
           D = rep(delta(model), length(N)),
           stop("internal error -- measure '", M., "' not implemented yet"))
  })
  if (!is.matrix(res)) res <- t(res)
  rownames(res) <- N
  if (data.frame) as.data.frame(res, optional=TRUE) else res
}

## if (!is.null(conf.sd)) {
##   sds <- c(-conf.sd, +conf.sd)
##   conf.int <- switch(what,
##                      R = (V(obj) + sds * sqrt(VV(obj))) / sqrt(N(obj)),
##                      C = log( V(obj) + sds * sqrt(VV(obj)) ) / log( N(obj) ),
##                      P = (Vm(obj, 1) + sds * sqrt(VVm(obj, 1))) / N(obj),
##                      TTR = (V(obj) + sds * sqrt(VV(obj))) / N(obj),
##                      V = V(obj) + sds * sqrt(VV(obj)))
##   res$lower <- conf.int[1]
##   res$upper <- conf.int[2]
## }

## if (!is.null(conf.sd)) {
##   ## Evert (2004b, Lemma A.8) gives the following approximation for the variance:
##   ##  - Var[Vm / V] = Var[V] / E[V]^2 * (s(1 - s) + (r - s)^2)
##   ##  - with r = E[Vm] / E[V] and s = Var[Vm] / Var[V]
##   sd.VmV <- function (obj, m) {
##     r <- Vm(obj, m) / V(obj)
##     s <- VVm(obj, m) / VV(obj)
##     sqrt(VV(obj)) / V(obj) * sqrt( s * (1-s) + (r - s)^2 )
##   }
##   sds <- c(-conf.sd, +conf.sd)
##   conf.int <- switch(what,
##                      S = (res$estimate + sds * sd.VmV(obj, 2)),
##                      H = 100 * log( N(obj) ) / (1 - (Vm(obj, 1) / V(obj) + sds * sd.VmV(obj, 1))),
##                      Hapax = (res$estimate + sds * sd.VmV(obj, 1)))
##   res$lower <- conf.int[1]
##   res$upper <- conf.int[2]
## }

## conf.sd <- if (isTRUE(attr(obj, "hasVariances"))) -qnorm((1 - conf.level) / 2) else NULL
