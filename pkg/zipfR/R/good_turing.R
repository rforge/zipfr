gtlnre <- function (model, m, N) {
  if (! inherits(model, "lnre")) stop("first argument must be object of class 'lnre'")
  if (!(is.numeric(N) && all(N >= 0))) stop("argument 'N' must be non-negative integer")
  if (!(is.numeric(m) && all(m >= 1))) stop("argument 'm' must be positive integer")

  (m+1) * EVm(model, m+1, N) / EVm(model, m, N)
}