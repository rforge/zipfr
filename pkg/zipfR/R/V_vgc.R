V.vgc <- function (obj, ...)
{
  if (! inherits(obj, "vgc")) stop("argument must be object of class 'vgc'")

  structure(obj$V, names=obj$N)
}
