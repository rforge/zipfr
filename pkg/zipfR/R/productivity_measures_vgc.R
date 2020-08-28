productivity.measures.vgc <- function (obj, measures, data.frame=TRUE, ...)
{
  supported <- qw("V TTR R C k U W P Hapax H S alpha2")
  if (missing(measures)) measures <- supported
  measures <- sapply(measures, match.arg, choices=supported)
  res <- numeric(length(measures))

  m.max <- attr(obj, "m.max") 

  ## check which measures can be computed from the available spectrum elements
  idx.ok <- (
    (measures %in% qw("V TTR R C k U W")) | 
      (measures %in% qw("P Hapax H") & m.max >= 1) | 
      (measures %in% qw("S alpha2") & m.max >= 2))
  res <- matrix(0, nrow=length(obj$N), ncol=length(measures))
  res[, !idx.ok] <- NA

  ## now compute all selected productivity measures
  res[, idx.ok] <- sapply(measures[idx.ok], function (M.) {
    ## same code as in productivity.measures.spc (because of accessor methods),
    ## but encapsulating this in an internal function would only make the code less transparent
    switch(M.,
           ## measures based on V and N
           V = V(obj),
           TTR = V(obj) / N(obj),
           R = V(obj) / sqrt(N(obj)),
           C = log( V(obj) ) / log( N(obj) ),
           k = log( V(obj) ) / log(log( N(obj) )),
           U = log( N(obj) )^2 / ( log(N(obj)) - log(V(obj)) ),
           W = N(obj) ^ (V(obj) ^ -0.172),
           ## measures based on hapax count (V1)
           P = Vm(obj, 1) / N(obj),
           Hapax = Vm(obj, 1) / V(obj),
           H = 100 * log( N(obj) ) / (1 - Vm(obj, 1) / V(obj)),
           ## measures based on the first two spectrum elements (V1 and V2)
           S = Vm(obj, 2) / V(obj),
           alpha2 = 1 - 2 * Vm(obj, 2) / Vm(obj, 1)
           ## Yule K and Simpson D cannot be computed for vocabulary growth curves
           )
  })

  colnames(res) <- measures
  rownames(res) <- N(obj)
  if (data.frame) as.data.frame(res, optional=TRUE) else res
}
