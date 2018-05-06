\name{productivity.measures.spc}
\alias{productivity.measures.spc}
\title{Compute Measures of Productivity from Frequency Spectrum (zipfR)}
\encoding{UTF-8}

\description{

  Compute various measures of productivity and lexical richness
  from an observed or expected frequency spectrum.
  See \code{\link{productivity.measures}} for more information and
  a list of available measures.
  
}

\usage{

\method{productivity.measures}{spc}(obj, measures, conf.level=.95, ...)

}

\arguments{

  \item{obj}{a frequency spectrum, i.e. an object of class \code{spc}}

  \item{measures}{character vector naming the productivity measures to
    be computed.}

  \item{conf.level}{confidence level for the optional confidence intervals}
  
  \item{...}{any further arguments are ignored}
  
}

\value{

  A data frame with one or three columns, and rows labelled with the full names of the productivity measures:

  \item{estimate}{the observed or expected value of the productivity measure}
  \item{lower}{lower bound of the confidence interval (if available)}
  \item{upper}{upper bound of the confidence interval (if available)}
    
}

\details{

  For an observed frequency spectrum, only sample estimates of the productivity measures are computed.
  Some measures may not be available for an incomplete spectrum.

  For an expected frequency spectrum, productivity measures can only be computed if a sufficient number
  of spectrum elements are included (depending on the selected measure). Yule's \eqn{K} and Simpson's \eqn{D}
  can only be approximated, and a warning will be printed if the approximation is likely to be inaccurate.
  For Sichel's \eqn{S}, Honoré's \eqn{H} and the proportion \eqn{V_1 / V} of hapaxes, the expectation is
  based on a normal approximation of the ratio \eqn{V_m / V} derived from Evert (2004b, Lemma A.8) using
  an (incorrect) independence assumption for \eqn{V_m} and \eqn{V - V_m}.
  
  If variances are included in the expected frequency spectrum, approximate confidence intervals are
  determined for all measures except \eqn{K} and \eqn{D}.  They are based on normal approximations for
  normal approximations for \eqn{V} and \eqn{V_m}, and on the normal approximation for \eqn{V_m / V} in
  the case of \eqn{S}, \eqn{H} and the proportion of hapaxes.

}

\seealso{

  \code{\link{productivity.measures}} for general usage, return value and a list of supported measures.
  
}

\keyword{ methods }
\keyword{ univar }

\examples{

## TODO

}
