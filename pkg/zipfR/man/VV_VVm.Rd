\name{VV-Vm}
\alias{VV}
\alias{VVm}
\title{Variances of the Expected Frequency Spectrum (zipfR)}

\description{

  \code{VV} and \code{VVm} are generic methods that can (and should) be
  used to compute the variance of the vocabulary size and the variances
  of spectrum elements according to an LNRE model (i.e. an object of
  class \code{lnre}).  These methods are also used to access variance
  information stored in some objects of class \code{spc} and \code{vgc}.

}

\usage{

  VV(obj, N=NA, ...)
  VVm(obj, m, N=NA, ...)

}

\arguments{

  \item{obj}{an object of class \code{lnre} (LNRE model), \code{spc}
    (frequency spectrum) or \code{vgc} (vocabulary growth curve).}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} for which variances are returned (or a vector of such values).}

  \item{N}{sample size \eqn{N} for which variances are calculated
    (\code{lnre} objects only)}
  
  \item{...}{additional arguments passed on to the method implementation
    (see respective manpages for details)}
  
}

\details{

  \code{spc} and \code{vgc} objects must represent an expected or
  interpolated frequency spectrum or VGC, and must include variance
  data.
  
  For \code{vgc} objects, the \code{VVm} method allows only a single
  value \code{m} to be specified.

  The argument \code{N} is only allowed for LNRE models and will trigger
  an error message otherwise.

}

\value{

  For a LNRE model (class \code{lnre}), \code{VV} computes the variance
  of the random variable \eqn{V(N)} (vocabulary size),  and \code{VVm}
  computes the variance of the random variables \eqn{V_m(N)} (spectrum
  elements), for a sample of specified size \eqn{N}.

  For an observed or interpolated frequency spectrum (class \code{spc}),
  \code{VV} returns the variance of the expected vocabulary size, and
  \code{VVm} returns variances of the spectrum elements.  These methods
  are only applicable if the \code{spc} object includes variance
  information.

  For an expected or interpolated vocabulary growth curve (class
  \code{vgc}), \code{VV} returns the variance vector of the expected
  vocabulary sizes \eqn{V}, and \code{VVm} the corresponding vector for
  \eqn{V_m}.  These methods are only applicable if the \code{vgc} object
  includes variance information.
  
}

\seealso{

  For details on the implementations of these methods, see \code{\link{VV.spc}}, \code{\link{VV.vgc}}, etc.

  Expected vocabulary size and frequency spectrum for a sample of size
  \eqn{N} according to a LNRE model can be computed with the analogous
  methods \code{\link{EV}} and \code{\link{EVm}}.  For \code{spc} and
  \code{vgc} objects, \eqn{V} and \eqn{V_m} are always accessed with the
  methods \code{\link{V}} and \code{\link{Vm}}, even if they represent
  expected or interpolated values.
  
}

\keyword{ methods }
\keyword{ manip }
\keyword{ distribution }
\keyword{ models }

\examples{

## see lnre documentation for examples

}
