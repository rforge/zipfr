\name{N-V-Vm.vgc}
\alias{N.vgc}
\alias{V.vgc}
\alias{Vm.vgc}
\alias{VV.vgc}
\alias{VVm.vgc}
\title{Access Methods for Vocabulary Growth Curves (zipfR)}

\description{

  Return the vector of sample sizes (\code{N.vgc}), vocabulary sizes
  (\code{V.vgc}) or class sizes (\code{Vm.vgc}) from the vocabulary
  growth curve (VGC) represented by a \code{vgc} object.  For an
  expected or interpolated VGC with variance information, \code{VV.vgc}
  returns the vector of variances of the vocabulary size and
  \code{VVm.vgc} the variance vectors for individual spectrum elements.

  Note that these functions are not user-visible.  They can be called
  implicitly through the generic methods \code{N}, \code{V}, \code{Vm},
  \code{VV} and \code{VVm}, applied to an object of type \code{vgc}.

}

\usage{

  \method{N}{vgc}(obj, ...)

  \method{V}{vgc}(obj, ...)

  \method{Vm}{vgc}(obj, m, ...)

  \method{VV}{vgc}(obj, N=NA, ...)

  \method{VVm}{vgc}(obj, m, N=NA, ...)

}

\arguments{

  \item{obj}{an object of class \code{vgc}, representing an observed,
    interpolated or expected VGC}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} for which the vector of class sizes is returned}

  \item{N}{not applicable (this argument of the generic method is not
    used by the implementation for \code{vgc} objects and must not be
    specified)}
  
  \item{...}{additional arguments passed on from generic method will be ignored}

}

\details{

  \code{VV.vgc} a \code{VVm.vgc} will fail if the object \code{obj} does
  not include variance data.  \code{Vm.vgc} and \code{VVm.vgc} will fail
  if the selected frequency class is not included in the VGC data.
}

\value{

  \code{N.vgc} returns the vector of sample sizes \eqn{N}, \code{V.vgc}
  returns the corresponding vocabulary sizes \eqn{V(N)} (or expected
  vocabulary sizes \eqn{E[V(N)]}), and \code{Vm.vgc} returns the vector of
  class sizes \eqn{V_m(N)} (or the expected spectrum elements
  \eqn{E[V_m(N)]}) for the selected frequency class \eqn{m}.

  For an expected or interpolated VGC with variance information,
  \code{VV.vgc} returns the vector of variances
  \eqn{\mathop{Var}[V(N)]}{Var[V(N)]} of the expected vocabulary size,
  and \code{VVm.vgc} returns vector of variances
  \eqn{\mathop{Var}[V_m(N)]}{Var[V_m(N)]} for the selected frequency
  class \eqn{m}.
  
  Except for \code{N.vgc}, the vector returned will be labelled with
  corresponding sample sizes.
}

\seealso{
  
  \code{\link{N}}, \code{\link{V}}, \code{\link{Vm}}, \code{\link{VV}},
  \code{\link{VVm}} for the generic methods and links to other
  implementations

  \code{\link{vgc}} for details on vocabulary growth curve objects and
  links to other relevant functions
  
}

\keyword{ manip }


%% ~~ no examples for internal function! ~~
%%\examples{

%%## ~~ TODO ~~

%%}

