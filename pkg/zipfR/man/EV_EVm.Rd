\name{EV-EVm}
\alias{EV}
\alias{EVm}
\title{Expected Frequency Spectrum (zipfR)}

\description{

  \code{EV} and \code{EVm} are generic methods for computing the
  expected vocabulary size \eqn{E[V]} and frequency spectrum
  \eqn{E[V_m]} according to a LNRE model (i.e. an object belonging to a
  subclass of \code{lnre}).

  When applied to a frequency spectrum (i.e. an object of class
  \code{spc}), these methods perform binomial interpolation (see
  \code{\link{EV.spc}} for details), although \code{\link{spc.interp}}
  and \code{\link{vgc.interp}} might be more convenient binomial
  interpolation functions for most purposes.

}

\usage{

  EV(obj, N, ...)
  EVm(obj, m, N, ...)

}

\arguments{

  \item{obj}{an LNRE model (i.e. an object belonging to a subclass of
    \code{lnre}) or frequency spectrum (i.e. an object of class
    \code{spc})}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} to be returned (or a vector of such values)}

  \item{N}{sample size \eqn{N} for which the expected vocabulary size
    and frequency spectrum are calculated (or a vector of sample sizes)}
  
  \item{...}{additional arguments passed on to the method implementation
    (see respective manpages for details)}
  
}

\value{

  \code{EV} returns the expected vocabulary size \eqn{E[V(N)]} in a
  sample of \eqn{N} tokens, and \code{EVm} returns the expected spectrum
  elements \eqn{E[V_m(N)]}, according to the LNRE model given by
  \code{obj} (or according to binomial interpolation).
  
}

\seealso{

%  Implementation details of these methods for various LNRE models are
%  described in the respective manpages, e.g. \code{\link{EV.lnre.zm}}
%  and \code{\link{EVm.lnre.zm}}.

  See \code{\link{lnre}} for more information on LNRE models, a listing
  of available models, and methods for parameter estimation.

  The variances of the random variables \eqn{V(N)} and \eqn{V_m(N)} can
  be computed with the methods \code{\link{VV}} and \code{\link{VVm}}.
%  See also \code{\link{VV.lnre}} and \code{\link{VVm.lnre}}, which
%  describe their generic implementation for all types of LNRE models.

  See \code{\link{EV.spc}} and \code{\link{EVm.spc}} for more
  information about the usage of these methods to perform binomial
  interpolation (but consider using \code{\link{spc.interp}} and
  \code{\link{vgc.interp}} instead).
  
}

\keyword{ methods }
\keyword{ distribution }
\keyword{ models }

\examples{

## see lnre() documentation for examples

}
