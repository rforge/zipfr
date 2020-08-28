bootstrap.confint <- function (x, level=0.95, method=c("normal", "mad", "empirical"), 
                               data.frame=FALSE) {
  method <- match.arg(method)
  .sig.level <- (1 - level) / 2  # one-sided significance level corresponding to selected confidence level
  if (inherits(x, "data.frame")) {
    if (!all(sapply(x, is.numeric))) stop("all columns of data.frame x must be numeric")
    x <- as.matrix(x)
  }
  if (!(is.matrix(x) && is.numeric(x))) stop("x must be a numeric matrix")
  replicates <- nrow(x)
  
  if (method == "empirical") {
    n.outliers <- round(replicates * .sig.level) # number of "outliers" to drop from each tail
    if (n.outliers < 3) stop("insufficient bootstrap data for confidence level=", level, " (need at least ", ceiling(3 / .sig.level)," replicates)")
    .sig.level <- n.outliers / replicates # "true" significance level of the empirical interval
  }
  confint.labels <- c(sprintf("%g%%", 100 * c(.sig.level, 1 - .sig.level)), "center", "spread")
  
  ## confint.fnc takes a vector x and returns the four estimates (lower, upper, center, spread)
  confint.fnc <- switch(
    method,
    ## estimates based on Gaussian distribution (mean + s.d.)
    normal = function (x) {
      .mean <- mean(x)
      .sd <- sd(x)
      .C <- -qnorm(.sig.level) # extension of confidence interval in s.d. (z-score)
      c(.mean - .C * .sd, .mean + .C * .sd, .mean, .sd)
    },
    ## estimates based on median absolute deviation (median, MAD adusted to s.d.)
    mad = function (x) {
      .mid <- median(x)
      y <- x - .mid
      .right <- y >= 0  # data points above median
      .left <- y < 0    # data points below median
      .madR <- median(abs(y[.right])) # one-sided median absolute deviation
      .madL <- median(abs(y[.left]))
      .sdR <- .madR / qnorm(3/4) # robust MAD estimator for s.d. of Gaussian distribution
      .sdL <- .madL / qnorm(3/4) # (cf. Wikipedia article "Median absolute deviation")
      .C <- -qnorm(.sig.level) # extension of confidence interval in s.d. (z-score)
      c(.mid - .C * .sdL, .mid + .C * .sdR, .mid, mean(.sdL, .sdR))
    },
    ## empirical confidence interval (median, IQR/2 adjusted to s.d.)
    empirical = function (x) {
      y <- sort(x)
      c(y[n.outliers + 1], y[replicates - n.outliers], median(x), IQR(x) / (2 * qnorm(3/4)))
    }
  )
  confint.wrap <- function (x) {
    ## catch some special cases
    if (!any(is.na(x)) && min(x) == Inf && max(x) == Inf) {
      c(Inf, Inf, Inf, Inf) # special case for infinite population diversity
    }
    else if (any(!is.finite(x))) {
      c(NA, NA, NA, NA) # if there are missing or infinite values
    }
    else confint.fnc(x)
  }
  
  .res.table <- apply(x, 2, confint.wrap)
  rownames(.res.table) <- confint.labels

  if (data.frame) as.data.frame(.res.table, optional=TRUE) else .res.table
}