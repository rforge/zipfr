productivity.measures.spc <- function (obj, measures, data.frame=TRUE, ...)
{
  supported <- qw("V TTR R C k U W P Hapax H S alpha2 K D Entropy eta")
  if (missing(measures) || is.null(measures)) measures <- supported
  measures <- sapply(measures, match.arg, choices=supported)
  res <- numeric(length(measures))

  m.max <- attr(obj, "m.max") # if m.max > 0, spectrum is incomplete
  if (m.max == 0) m.max <- Inf

  ## check which measures can be computed from the available spectrum
  idx.ok <- (
    (measures %in% qw("V TTR R C k U W")) | 
    (measures %in% qw("P Hapax H") & m.max >= 1) | 
    (measures %in% qw("S alpha2") & m.max >= 2) | 
    (m.max == Inf))
  res <- numeric(length(measures))
  res[!idx.ok] <- NA

  ## now compute all selected productivity measures
  res[idx.ok] <- sapply(measures[idx.ok], function (M.) {
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
           alpha2 = 1 - 2 * Vm(obj, 2) / Vm(obj, 1),
           ## Yule K and Simpson D can only be computed from full spectrum
           K = {
             m <- as.numeric(obj$m)
             Vm <- as.numeric(obj$Vm)
             10e4 * (sum(m * m * Vm) - N(obj)) / (N(obj) ^ 2)
           },
           D = {
             m <- as.numeric(obj$m)
             Vm <- as.numeric(obj$Vm)
             sum(Vm * (m / N(obj)) * ((m - 1) / (N(obj) - 1)))
           },
           ## same for Entropy and eta (only included for evaluation purposes)
           Entropy = {
             m <- as.numeric(obj$m)
             Vm <- as.numeric(obj$Vm)
             - sum(Vm * (m / N(obj) * log2(m / N(obj))))
           },
           eta = {
             m <- as.numeric(obj$m)
             Vm <- as.numeric(obj$Vm)
             - sum(Vm * (m / N(obj) * log2(m / N(obj)))) / log2(V(obj))
           },
           stop("internal error -- measure '", M., "' not implemented yet"))
  })

  names(res) <- measures
  if (data.frame) as.data.frame(t(res), optional=TRUE) else res
}
