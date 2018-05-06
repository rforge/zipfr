productivity.measures.spc <- function (obj, measures, conf.level=.95, ...)
{
  measures <- sapply(measures, match.arg, choices=qw("K D R S H C P TTR Hapax V"))
  names(measures) <- NULL # for do.call at end of function
  
  ## K and D
  comp.Yule <- function (obj, what=qw("K D"), m.max, conf.sd, ...) {
    match.arg(what)
    stop("not implemented yet")
  }
  
  ## measures based on single spectrum element
  comp.V <- function (obj, what=qw("R C P TTR V"), m.max, conf.sd, ...) {
    match.arg(what)
    if (m.max < 1) stop("need spectrum at least up to V1 for '", what, "' measure")
    res <- data.frame(
      estimate = switch(what,
                        R = V(obj) / sqrt(N(obj)),
                        C = log( V(obj) ) / log( N(obj) ),
                        P = Vm(obj, 1) / N(obj),
                        TTR = V(obj) / N(obj),
                        V = V(obj)),
      row.names = what)
    if (!is.null(conf.sd)) {
      sds <- c(-conf.sd, +conf.sd)
      conf.int <- switch(what,
                         R = (V(obj) + sds * sqrt(VV(obj))) / sqrt(N(obj)),
                         C = log( V(obj) + sds * sqrt(VV(obj)) ) / log( N(obj) ),
                         P = (Vm(obj, 1) + sds * sqrt(VVm(obj, 1))) / N(obj),
                         TTR = (V(obj) + sds * sqrt(VV(obj))) / N(obj),
                         V = V(obj) + sds * sqrt(VV(obj)))
      res$lower <- conf.int[1]
      res$upper <- conf.int[2]
    }
    res
  }
  
  ## measures based on two spectrum elements
  comp.H <- function (obj, what=qw("S H Hapax"), m.max, conf.sd, ...) {
    match.arg(what)
    if (m.max < 2) stop("need spectrum at least up to V2 for '", what, "' measure")
    ## for expected spectrum, this computation is based on a rough normal approximation
    ## of the distribution of Vm / V:
    ##  - assumption: Vm and (V - Vm) are independent (not actually true)
    ##  - then Vm / (Vm + (V - Vm)) is roughly normal (Evert 2004b, Lemma A.8, p. 179)
    ##  - with E[Vm / V] = E[Vm] / E[V]
    ##  - H also assumes that the transformation 1 / (1 - Vm / V) doesn't skew the distribution
    res <- data.frame(
      estimate = switch(what,
                        S = Vm(obj, 2) / V(obj),
                        H = 100 * log( N(obj) ) / (1 - Vm(obj, 1) / V(obj)),
                        Hapax = Vm(obj, 1) / V(obj)),
      row.names = what)
    if (!is.null(conf.sd)) {
      ## Evert (2004b, Lemma A.8) gives the following approximation for the variance:
      ##  - Var[Vm / V] = Var[V] / E[V]^2 * (s(1 - s) + (r - s)^2)
      ##  - with r = E[Vm] / E[V] and s = Var[Vm] / Var[V]
      sd.VmV <- function (obj, m) {
        r <- Vm(obj, m) / V(obj)
        s <- VVm(obj, m) / VV(obj)
        sqrt(VV(obj)) / V(obj) * sqrt( s * (1-s) + (r - s)^2 )
      }
      sds <- c(-conf.sd, +conf.sd)
      conf.int <- switch(what,
                         S = (res$estimate + sds * sd.VmV(obj, 2)),
                         H = 100 * log( N(obj) ) / (1 - (Vm(obj, 1) / V(obj) + sds * sd.VmV(obj, 1))),
                         Hapax = (res$estimate + sds * sd.VmV(obj, 1)))
      res$lower <- conf.int[1]
      res$upper <- conf.int[2]
    }
    res
  }

  m.max <- attr(obj, "m.max") # if m.max > 0, spectrum is incomplete
  if (m.max == 0) m.max <- Inf
  conf.sd <- if (isTRUE(attr(obj, "hasVariances"))) -qnorm((1 - conf.level) / 2) else NULL
  do.call(rbind, lapply(measures, function (M.) {
    switch(M.,
           K = comp.Yule(obj, "K", m.max, conf.sd),
           D = comp.Yule(obj, "D", m.max, conf.sd),
           R = comp.V(obj, "R", m.max, conf.sd),
           C = comp.V(obj, "C", m.max, conf.sd),
           P = comp.V(obj, "P", m.max, conf.sd),
           TTR = comp.V(obj, "TTR", m.max, conf.sd),
           C = comp.V(obj, "V", m.max, conf.sd),
           S = comp.H(obj, "S", m.max, conf.sd),
           H = comp.H(obj, "H", m.max, conf.sd),
           Hapax = comp.H(obj, "Hapax", m.max, conf.sd))
  }))
}
