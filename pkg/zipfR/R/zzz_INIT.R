## package initialization

.onLoad <- function (libname, pkgname)
{
  ## use quartz() device by default on the native Mac OS X version of R ("AQUA")
  if (.Platform$GUI == "AQUA") zipfR.par(device="quartz")
}
