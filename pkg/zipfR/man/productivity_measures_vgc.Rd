\name{productivity.measures.vgc}
\alias{productivity.measures.vgc}
\title{Compute Measures of Productivity from Vocabulary Growth Curve (zipfR)}
\encoding{UTF-8}

\description{

  Compute various measures of productivity and lexical richness
  from an observed or expected vocabulary growth curve.
  See \code{\link{productivity.measures}} for more information and
  a list of available measures.
  
}

\usage{

\method{productivity.measures}{vgc}(obj, measures, ...)

}

\arguments{

  \item{obj}{a vocabulary growth curve, i.e. an object of class \code{vgc}}

  \item{measures}{character vector naming the productivity measures to be computed.}

  \item{...}{any further arguments are ignored}
  
}

\value{

  A numeric matrix with columns corresponding to the selected
  productivity measures and rows corresponding to the sample sizes of
  the vocabulary growth curve.
  
}

\details{

  Some measures are only available if a sufficient number of spectrum elements have been included in the \code{vgc} object.
  Yule's \eqn{K} and Simpson's \eqn{D} cannot be computed at al from a vocabulary growth curve.
  
  For an observed vocabulary growth curve, sample estimates of the productivity measures are computed.
  
  For an expected vocabulary growth curve, expectations of the productivity scores are computed or approximated.
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
