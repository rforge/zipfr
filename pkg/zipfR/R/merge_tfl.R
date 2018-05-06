merge.tfl <- function (x, y, ...)
{
  tfl.list <- c(list(x, y), list(...))
  ok <- sapply(tfl.list, function (x) inherits(x, "tfl"))
  if (!all(ok)) stop("all arguments must be type frequency lists (of class tfl)")
  ok <- sapply(tfl.list, function (x) "type" %in% colnames(x))
  if (!all(ok)) stop("all type frequency lists to be merged must contain type labels")
  ok <- sapply(tfl.list, function (x) !isTRUE(attr(x, "incomplete")))
  if (!all(ok)) stop("only complete type frequency lists can be merged")
  
  vocab <- Reduce(union, lapply(tfl.list, function (x) x$type))
  f.total <- numeric(length(vocab))
  for (x in tfl.list) {
    idx <- match(x$type, vocab)
    stopifnot(!any(is.na(idx)))
    f.total[idx] <- f.total[idx] + x$f
  }

  tfl(f.total, type=vocab)
}
