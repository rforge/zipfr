\name{N-V-Vm.spc}
\alias{N.spc}
\alias{V.spc}
\alias{Vm.spc}
\alias{VV.spc}
\alias{VVm.spc}
\title{Access Methods for Frequency Spectra (zipfR)}

\description{

  Return the sample size (\code{N.spc}), vocabulary size (\code{V.spc})
  and class sizes (\code{Vm.spc}) of the frequency spectrum represented
  by a \code{spc} object.  For an expected spectrum with variance
  information, \code{VV.spc} returns the variance of the expected
  spectrum size and \code{VVm.spc} the variances of individual spectrum
  elements.

  Note that these functions are not user-visible.  They can be called
  implicitly through the generic methods \code{N}, \code{V}, \code{Vm},
  \code{VV} and \code{VVm}, applied to an object of type \code{spc}.

}

\usage{

  \method{N}{spc}(obj, ...)

  \method{V}{spc}(obj, ...)

  \method{Vm}{spc}(obj, m, ...)

  \method{VV}{spc}(obj, N=NA, ...)

  \method{VVm}{spc}(obj, m, N=NA, ...)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing an observed or
    expected frequency spectrum}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} to be returned (or a vector of such values).}

  \item{N}{not applicable (this argument of the generic method is not
    used by the implementation for \code{spc} objects and must not be
    specified)}
  
  \item{...}{additional arguments passed on from generic method will be ignored}

}

\details{

  \code{VV.spc} a \code{VVm.spc} will fail if the object \code{obj} is
  not an expected frequency spectrum with variance data.

  For an incomplete frequency spectrum, \code{Vm.spc} (and
  \code{VVm.spc}) will return \code{NA} for all spectrum elements that
  are not listed in the object (i.e. for \code{m > m.max}).
  
}

\value{

  \code{N.spc} returns the sample size \eqn{N}, \code{V.spc} returns the
  vocabulary size \eqn{V} (or expected vocabulary size \eqn{E[V]}), and
  \code{Vm.spc} returns a vector of class sizes \eqn{V_m} (ot the
  expected spectrum elements \eqn{E[V_m]}).

  For an expected spectrum with variances, \code{VV.spc} returns the
  variance \eqn{\mathop{Var}[V]}{Var[V]} of the expected vocabulary
  size, and \code{VVm.spc} returns variances
  \eqn{\mathop{Var}[V_m]}{Var[V_m]} of the spectrum elements.
  
}

\seealso{
  
  \code{\link{N}}, \code{\link{V}}, \code{\link{Vm}}, \code{\link{VV}},
  \code{\link{VVm}} for the generic methods and links to other
  implementations

  \code{\link{spc}} for details on frequency spectrum objects and links
  to other relevant functions
  
}

\keyword{ manip }

%% ~~ no examples for internal function! ~~
%%\examples{

%%## ~~ TODO ~~

%%}
