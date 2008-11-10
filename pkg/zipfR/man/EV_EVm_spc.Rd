\name{EV-EVm.spc}
\alias{EV.spc}
\alias{EVm.spc}
\title{Binomial Interpolation (zipfR)}

\description{

  Compute the expected vocabulary size \eqn{E[V(N)]} (with function
  \code{EV.spc}) or expected frequency spectrum \eqn{E[V_m(N)]} (with
  function \code{EVm.spc}) for a random sample of size \eqn{N} from a
  given frequency spectrum (i.e., an object of class \code{spc}).  The
  expectations are calculated by binomial interpolation (following
  Baayen 2001, pp. 64-69).
  

  Note that these functions are not user-visible.  They can be called
  implicitly through the generic methods \code{EV} and \code{EVm},
  applied to an object of type \code{spc}.

}

\usage{

  \method{EV}{spc}(obj, N, allow.extrapolation=FALSE, ...)

  \method{EVm}{spc}(obj, m, N, allow.extrapolation=FALSE, ...)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing a frequency
    spectrum}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} for which \eqn{E[V_m(N)]} be returned (or a vector of such
    values)}

  \item{N}{sample size \eqn{N} for which the expected vocabulary size
    or frequency spectrum are calculated (or a vector of sample sizes)}

  \item{allow.extrapolation}{if \code{TRUE}, the requested sample size
    \eqn{N} may be larger than the sample size of the frequency spectrum
    \code{obj}, for binomial \emph{extrapolation}.  This obtion should
    be used with great caution (see "Details" below).}
    
  \item{...}{additional arguments passed on from generic methods will be
    ignored}
  
}

\value{

  \code{EV} returns the expected vocabulary size \eqn{E[V(N)]} for a
  random sample of \eqn{N} tokens from the frequency spectrum
  \code{obj}, and \code{EVm} returns the expected spectrum elements
  \eqn{E[V_m(N)]} for a random sample of \eqn{N} tokens from \code{obj},
  calculated by binomial interpolation.
  
}

\details{

  These functions are naive implementations of binomial interpolation,
  using Equations (2.41) and (2.43) from Baayen (2001).  No guarantees
  are made concerning their numerical accuracy, especially for extreme
  values of \eqn{m} and \eqn{N}.

  According to Baayen (2001), pp. 69-73., the same equations can also be
  used for binomial \emph{extrapolation} of a given frequency spectrum
  to larger sample sizes.  However, they become numerically unstable in
  this case and will typically break down when extrapolating to more
  than twice the size of the observed sample (Baayen 2001, p. 75).
  Therefore, extrapolation has to be enabled explicitly with the option
  \code{allow.extrapolation=TRUE} and should be used with great caution.

}

\references{

  Baayen, R. Harald (2001). \emph{Word Frequency Distributions.}
  Kluwer, Dordrecht.

}
  
\seealso{

  \code{\link{EV}} and \code{\link{EVm}} for the generic methods and
  links to other implementations
  
  \code{\link{spc.interp}} and \code{\link{vgc.interp}} are convenience
  functions that compute an expected frequency spectrum or vocabulary
  growth curve by binomial interpolation
  
}

\keyword{ methods }
\keyword{ distribution }
\keyword{ manip }

%%\examples{
%%
%%## ~~ TODO ~~
%%
%%}
