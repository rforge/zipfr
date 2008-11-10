\name{N-V-Vm.tfl}
\alias{N.tfl}
\alias{V.tfl}
\alias{Vm.tfl}
\title{Access Methods for Type Frequency Lists (zipfR)}

\description{

  Return the sample size (\code{N.tfl}) and vocabulary size
  (\code{V.tfl}) of the type frequency list represented by a \code{tfl}
  object, as well as class sizes (\code{Vm.tfl}) of the corresponding
  frequency spectrum.

  Note that these functions are not user-visible.  They can be called
  implicitly through the generic methods \code{N}, \code{V} and
  \code{Vm}, applied to an object of type \code{tfl}.

}

\usage{

  \method{N}{tfl}(obj, ...)

  \method{V}{tfl}(obj, ...)

  \method{Vm}{tfl}(obj, m, ...)

}

\arguments{

  \item{obj}{an object of class \code{tfl}, representing an observed
  type frequency list}

  \item{m}{non-negative integer value determining the frequency class
    \eqn{m} to be returned}

  \item{...}{additional arguments passed on from generic method will be
    ignored}
  
}

\details{

  Only a single value is allowed for \eqn{m}, which may also be 0.
  In order to obtain multiple class sizes \eqn{V_m}, convert the type
  frequency list to a frequency spectrum with \code{tfl2spc} first.
  
  For an incomplete type frequency list, \code{Vm.tfl} will return
  \code{NA} if \code{m} is outside the range of listed frequencies
  (i.e. for \code{m < f.min} or \code{m > f.max}).

}

\value{

  \code{N.tfl} returns the sample size \eqn{N}, \code{V.tfl} returns the
  vocabulary size \eqn{V} (or expected vocabulary size \eqn{E[V]}), and
  \code{Vm.tfl} returns the number of types that occur exactly \eqn{m}
  times in the sample, i.e. the class size \eqn{V_m}.
  
}

\seealso{
  
  \code{\link{N}}, \code{\link{V}}, \code{\link{Vm}} for the generic
  methods and links to other implementations

  \code{\link{tfl}} for details on type frequency list objects and links
  to other relevant functions
  
}

\keyword{ manip }

%% ~~ no examples for internal function! ~~
%%\examples{

%%## ~~ TODO ~~

%%}

