plot.tfl <- function (x, y, ...,
                      min.rank=1, max.rank=NULL, log=c("", "x", "y", "xy"), 
                      type=c("p", "l", "b", "o", "s"),
                      xlim=NULL, ylim=NULL, freq=TRUE,
                      xlab="rank", ylab="frequency", legend=NULL, grid=FALSE,
                      main="Type-Frequency List (Zipf ranking)",
                      bw=zipfR.par("bw"), cex=1, steps=200,
                      pch=NULL, lty=NULL, lwd=NULL, col=NULL)
{
  ## collect all specified TFL in single list
  if (is.list(x) && inherits(x[[1]], c("tfl", "lnre"))) {
    if (!missing(y)) stop("only a single list of TFLs may be specified")
    TFLs <- x
  } else {
    TFLs <- list(x)   # this is a bit complicated because of the plot() prototype
    if (! missing(y)) TFLs <- c(TFLs, list(y), list(...))
  }
  n.tfl <- length(TFLs)
  if (!all(sapply(TFLs, inherits, c("tfl", "lnre")))) stop("all unnamed arguments must be type-frequency lists or LNRE models")
  idx.lnre <- sapply(TFLs, inherits, "lnre") # which arguments are LNRE models
  
  ## check other arguments
  if (any(idx.lnre) && freq) stop("LNRE models can only be plotted with freq=FALSE")
  if (length(log) > 1 || log != "") log <- match.arg(log)
  type <- match.arg(type)
  if (!missing(legend) && length(legend) != n.tfl) stop("'legend' argument must be character or expression vector of same length as number of TFLs")
  if (any(sapply(TFLs[!idx.lnre], function (.TFL) isTRUE(attr(.TFL, "incomplete"))))) stop("plotting of incomplete type-frequency lists is not supported")
  x.log <- log == "x" || log == "xy"
  y.log <- log == "y" || log == "xy"
  if (missing(ylab) && !freq) ylab <- "relative frequency (per million words)"
  
  ## determine range of ranks available & check requested min/max
  if (any(!idx.lnre)) {
    max.rank.available <- max(sapply(TFLs[!idx.lnre], function (.TFL) length(.TFL$f)))
    if (is.null(max.rank)) {
      max.rank <- max.rank.available
    } else {
      if (min.rank >= max.rank.available) stop(sprintf("no data available for ranks > %d (min.rank)", min.rank))
    }
  }
  else {
    if (is.null(max.rank)) stop("max.rank must be specified if only LNRE models are plotted")
  }
  if (min.rank < 1) stop(sprintf("min.rank=%d invalid, must be integer >= 1", min.rank))
  if (min.rank >= max.rank) stop(sprintf("min.rank=%d must be smaller than max.rank=%d", min.rank, max.rank))

  ## collect x/y-coordinates for each TFL or LNRE model to be plotted
  coord <- lapply(seq_len(n.tfl), function (i) {
    .TFL <- TFLs[[i]]
    x <- NULL
    y <- NULL
    if (idx.lnre[i]) {
      ## LNRE model: plot type probabilities for equidistant steps from min.rank to max.rank
      .max <- min(max.rank, .TFL$S)
      if (.max >= min.rank) {
        if (!x.log) {
          x <- seq(min.rank, .max, length.out=steps) # ranks (not rounded to integer)
        } else {
          x <- exp( seq(log(min.rank), log(.max), length.out=steps) ) # logarithmic steps
        }
        y <- 1e6 * tqlnre(.TFL, x - .5) # approximate probability pmw of type i = G^{-1}(i - 0.5)
        ## correct probability mass is F(G^{-1}(i - 1)) - F(G^{-1}(i)), but this would have
        ## cancellation issues for small probabilities, so the simple approximation is preferred
      }
    } else {
      ## TFL: collect selected ranks and corresponding (relative) frequencies; adjust for type="s"
      f <- sort(.TFL$f, decreasing=TRUE)
      .max <- min(max.rank, length(f))
      x <- seq(min.rank, .max)
      y <- f[x]
      if (type == "s") {
        idx <- c(diff(y) != 0, TRUE) # for every horizontal bar, keep only last point as right upper edge
        idx[1] <- TRUE # but make sure the first point isn't discarded
        x <- x[idx]
        y <- y[idx]
      }
      if (!freq) y <- 1e6 * y / N(.TFL) # rescale to relative frequency pmw
    }
    list(x=x, y=y)
  })
  
  ## determine range of y-coordinates
  tmp <- unlist(lapply(coord, function (.XY) .XY$y))
  f.max <- max(tmp)
  f.min <- if (freq) 1 else min(tmp)
    
  ## get default styles unless manually overridden
  if (is.null(pch)) pch <- zipfR.par("pch", bw.mode=bw)
  if (is.null(lty)) lty <- zipfR.par("lty", bw.mode=bw)
  if (is.null(lwd)) lwd <- zipfR.par("lwd", bw.mode=bw)
  if (is.null(col)) col <- zipfR.par("col", bw.mode=bw)
  
  ## choose suitable ranges on the axes, unless specified by user
  if (is.null(xlim)) xlim <- c(min.rank, max.rank)
  if (is.null(ylim)) ylim <- if (y.log) c(2/3*f.min, 1.5*f.max) else c(0, 1.05 * f.max)
      
  ## set up plotting region and labels
  plot(1, 1, type="n", xlim=xlim, ylim=ylim, log=log, xaxs="r", yaxs="i",
       xlab=xlab, ylab=ylab, main=main)

  ## display grid (orders of magnitude) on any logarithmic axis
  if (grid) {
    log.steps <- function (x) 10 ^ seq(floor(log10(min(x))), ceiling(log10(max(x))))
    if (x.log) abline(v=log.steps(xlim), lwd=.5)
    if (y.log) abline(h=log.steps(ylim), lwd=.5)
  }
  
  ## go through all specified TFLs / LNRE models and plot the pre-computed coordinates
  for (i in seq_len(n.tfl)) {
    .XY <- coord[[i]]
    if (length(.XY$x) > 0) {
      .type <- if (idx.lnre[i]) "l" else if (type == "s") "S" else type
      points(.XY$x, .XY$y, type=.type, cex=cex, pch=pch[i], lty=lty[i], lwd=lwd[i], col=col[i])
    }
  }

  ## add legend if specified by user
  if (!is.null(legend)) {
    legend.args <- list("topright", inset=.02, bg="white", legend=legend, col=col)
    if (type %in% c("p","b","o")) {
      .pch <- ifelse(idx.lnre, NA, pch)
      legend.args <- append(legend.args, list(pch=.pch, pt.cex=1.4*cex, pt.lwd=lwd))
    }
    if (type != "p" || any(idx.lnre)) legend.args <- append(legend.args, list(lwd=lwd, lty=lty))
    do.call("legend", legend.args)
  }  
}
