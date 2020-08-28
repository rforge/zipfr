productivity.measures.default <- function (obj, measures, data.frame=TRUE, ...) {
  if (!(is.factor(obj) || is.character(obj) || is.numeric(obj))) stop("'obj' must be a factor, character or integer vector")
  obj <- vec2spc(factor(obj))
  productivity.measures(obj, measures, data.frame=data.frame, ...)
}
