confint.lnre <- function (object, parm, level=0.95, method=c("mad", "normal", "empirical"), plot=FALSE, breaks="Sturges", ...)
{ ## object should belong to class "lnre" if this method is called
  if (!("bootstrap" %in% names(object))) stop("no bootstrapping data available for LNRE model")
  replicates <- length(object$bootstrap)
  method <- match.arg(method)
  
  .type <- object$type
  if (missing(parm) || is.null(parm)) {
    parm <- switch(.type,
      zm=c("alpha", "B"),
      fzm=c("alpha", "B", "A"),
      gigp=c("gamma", "B", "C"))
    parm <- c(parm, "S", "X2")
  }
  
  n.parm <- length(parm)
  idx.parm <- parm %in% names(object$param)
  idx.gof <- parm %in% names(object$gof)
  idx.S <- parm == "S"
  idx.logP <- parm == "logP"
  idx.fail <- !(idx.parm | idx.gof | idx.S | idx.logP)
  if (any(idx.fail)) stop("unknown parameter(s): ", paste(parm[idx.fail], collapse=", "))
  
  .res.list <- lapply(object$bootstrap, function (.M) {
     .res <- numeric(length=n.parm)
     if (any(idx.parm)) .res[idx.parm] <- sapply(parm[idx.parm], function (.name) .M$param[[.name]])
     if (any(idx.gof)) .res[idx.gof] <- sapply(parm[idx.gof], function (.name) .M$gof[[.name]])
     if (any(idx.S)) .res[idx.S] <- .M$S
     if (any(idx.logP)) .res[idx.logP] <- -log10(.M$gof$p)
     names(.res) <- parm
     .res
   })
   
  .res.table <- bootstrap.confint(do.call(rbind, .res.list), level=level, method=method)
  colnames(.res.table) <- parm

  if (plot) {
    for (.p in parm) {
      x <- sapply(.res.list, function (.r) .r[.p])
      .stats <- .res.table[, .p]
      .min <- min(x)
      .max <- max(x)
      if (.min > -Inf && .max < Inf) {
        hist(x, freq=FALSE, main=sprintf("Bootstrap of %s over %d replicates", .p, replicates),
             breaks=breaks, xlab=.p, xlim=c(min(.min, .stats[1]), max(.max, .stats[2])))
        abline(v=.stats[3], lwd=2, col="red")   # central value
        abline(v=.stats[1:2], lwd=1, col="red") # boundaries of confidence interval
        lines(density(x), col="blue", lwd=2)
      }
    }
  }

  as.data.frame(.res.table)
}

