\name{spc.interp}
\alias{spc.interp}
\title{Expected Frequency Spectrum by Binomial Interpolation (zipfR)}

\description{

  \code{spc.interp} computes the expected frequency spectrum for a
  random sample of specified size \eqn{N}, taken from a data set
  described by the frequency spectrum object \code{obj}.
  
}

\usage{

  spc.interp(obj, N, m.max=max(obj$m), allow.extrapolation=FALSE)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing the frequency
    spectrum of the data set from which samples are taken}

  \item{N}{a single non-negative integer specifying the sample size for
    which the expected frequency spectrum is calculated}

  \item{m.max}{number of spectrum elements listed in the expected
    frequency spectrum.  By default, as many spectrum elements are
    included as the spectrum \code{obj} contains, since the expectations
    of higher spectrum elements will always be 0 in the binomial
    interpolation.  See note in section "Details" below.} 

  \item{allow.extrapolation}{if \code{TRUE}, the requested sample size
    \eqn{N} may be larger than the sample size of the frequency spectrum
    \code{obj}, for binomial \emph{extrapolation}.  This obtion should
    be used with great caution (see \code{\link{EVm.spc}} for details).}
    
}

\value{

  An object of class \code{spc}, representing the expected frequency
  spectrum for a random sample of size \code{N} taken from the data set
  that is described by \code{obj}.
  
}

\details{

  See the \code{\link{EVm.spc}} manpage for more information, especially
  concerning binomial \emph{extrapolation}.

  For large frequency spectra, the default value of \code{m.max} may
  lead to very long computation times.  It is therefore recommended to
  specify \code{m.max} explicitly and calculate only as many spectrum
  elements as are actually required.
  
}

\seealso{

  \code{\link{spc}} for more information about frequency spectra and
  links to relevant functions

  The implementation of \code{spc.interp} is based on the functions
  \code{\link{EV.spc}} and \code{\link{EVm.spc}}.  See the respective
  manpages for technical details.

  \code{\link{vgc.interp}} computes expected vocabulary growth curves by
  binomial interpolation from a frequency spectrum

  \code{\link{sample.spc}} takes a single \emph{concrete} random
  subsample from a spectrum and returns the spectrum of the subsample,
  unlike \code{spc.interp}, that computes the \emph{expected}
  frequency spectrum for random subsamples of size \code{N} by
  binomial interpolation.
  
}

\keyword{ manip }
\keyword{ distribution }
  
\examples{

## load the Tiger NP expansion spectrum
## (sample size: about 109k tokens) 
data(TigerNP.spc)

## interpolated expected frequency subspectrum of 50k tokens
TigerNP.sub.spc <- spc.interp(TigerNP.spc,5e+4)
summary(TigerNP.sub.spc)

## previous is slow since it calculates all expected  spectrum
## elements; suppose we only need the first 10 expected
## spectrum element frequencies; then we can do:
TigerNP.sub.spc <- spc.interp(TigerNP.spc,5e+4,m.max=10) # much faster!
summary(TigerNP.sub.spc)

}
