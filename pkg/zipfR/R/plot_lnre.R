plot.lnre <- function (x, y, ...,
                      type=c("types", "probability", "cumulative"),
                      xlim=c(1e-9, 1), ylim=NULL, steps=200,
                      xlab=NULL, ylab=NULL, legend=NULL, grid=FALSE,
                      main="LNRE Population Distribution",
                      lty=NULL, lwd=NULL, col=NULL, bw=zipfR.par("bw"))
{
  ## collect all specified LNRE models in single list
  if (is.list(x) && inherits(x[[1]], "lnre")) {
    if (!missing(y)) stop("only a single list of LNRE models may be specified")
    Models <- x
  } else {
    Models <- list(x)   # this is a bit complicated because of the plot() prototype
    if (! missing(y)) Models <- c(Models, list(y), list(...))
  }
  n.mod <- length(Models)
  
  ## check other arguments
  type <- match.arg(type)
  if (isTRUE(legend)) legend <- sapply(Models, function (.M) .M$util$label(.M))
  if (!is.null(legend) && length(legend) != n.mod) stop("'legend' argument must be character or expression vector of same length as number of LNRE models")
  if (is.null(xlab)) xlab <- expression(pi)
  if (is.null(ylab)) ylab <- switch(type, types="type density", probability="probability density", cumulative="cumulative probability")

  ## evaluate density or cumulative probability distribution
  X <- 10 ^ seq(log10(xlim[1]), log10(xlim[2]), length.out=steps) # logarithmically equidistant steps
  if (type == "cumulative") {
    Ys <- lapply(Models, function (.M) plnre(.M, X, lower.tail=TRUE))
    if (is.null(ylim)) ylim <- c(0, 1)
  } else {
    log.density <- if (type == "types") ltdlnre else ldlnre # will also be used further below
    Ys <- lapply(Models, function (.M) log.density(.M, X))
    if (is.null(ylim)) ylim <- c(0, 1.05 * do.call(max, Ys))
  }

  ## get default styles unless manually overridden
  if (is.null(lty)) lty <- zipfR.par("lty", bw.mode=bw)
  if (is.null(lwd)) lwd <- zipfR.par("lwd", bw.mode=bw)
  if (is.null(col)) col <- zipfR.par("col", bw.mode=bw)
  
  ## set up plotting region and labels
  plot(1, 1, type="n", xlim=xlim, ylim=ylim, log="x", xaxs="i", yaxs="i",
       xlab=xlab, ylab=ylab, main=main)
  
  if (grid) {
    rng <- c(round(log10(xlim[1])), floor(round(xlim[2])))
    if (diff(rng) >= 0) abline(v=10^seq(rng[1], rng[2], 1), lwd=.5)
    if (type != "types") abline(h=seq(0, ylim[2], .1), lwd=.5)
  }
  
  for (i in seq_len(n.mod)) {                 # plot all specified models
    x <- X
    y <- Ys[[i]]
    M <- Models[[i]]
    ## special case for better display of (f)ZM type densities at cutoff
    if (type != "cumulative" && inherits(M, c("lnre.zm", "lnre.fzm"))) {
      B <- M$param$B # apply upper cutoff
      idx <- x > B
      if (any(idx)) {
        y <- c(y[!idx], log.density(M, B), 0)
        x <- c(x[!idx], B, B)
      }
      if (inherits(M, "lnre.fzm")) {
        A <- M$param$A # apply lower cutoff
        idx <- x < A
        if (any(idx)) {
          y <- c(0, log.density(M, A), y[!idx])
          x <- c(A, A, x[!idx])
        }
      }
    }
    lines(x, y, col=col[i], lwd=lwd[i], lty=lty[i])
  }
  
  if (!is.null(legend)) {             # add legend if specified by user
    legend.args <- list(if (type == "types") "topright" else "topleft",
                        inset=.02, bg="white", legend=legend, col=col, lwd=lwd + 1, lty=lty)
    do.call("legend", legend.args)
  }
}
