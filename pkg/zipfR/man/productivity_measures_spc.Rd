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

\method{productivity.measures}{spc}(obj, measures, ...)

}

\arguments{

  \item{obj}{a frequency spectrum, i.e. an object of class \code{spc}}

  \item{measures}{character vector naming the productivity measures to be computed.}

  \item{...}{any further arguments are ignored}
  
}

\value{

  A numeric vector of the same length as \code{measures} with the corresponding observed or expected values of the productivity measures.

}

\details{

  For an observed frequency spectrum, sample estimates of the productivity measures are computed.
  Some measures may not be available for an incomplete spectrum.

  For an expected frequency spectrum, productivity measures can only be computed if a sufficient number
  of spectrum elements are included (depending on the selected measure). Yule's \eqn{K} and Simpson's \eqn{D}
  can only be approximated, and a warning will be printed if the approximation is likely to be inaccurate.
  For Sichel's \eqn{S}, Honoré's \eqn{H} and the proportion \eqn{V_1 / V} of hapaxes, the expectation is
  based on a normal approximation of the ratio \eqn{V_m / V} derived from Evert (2004b, Lemma A.8) using
  an (incorrect) independence assumption for \eqn{V_m} and \eqn{V - V_m}.
  
}

\seealso{

  \code{\link{productivity.measures}} for general usage, return value and a list of supported measures.
  
}

\keyword{ methods }
\keyword{ univar }

\examples{

## TODO

}
