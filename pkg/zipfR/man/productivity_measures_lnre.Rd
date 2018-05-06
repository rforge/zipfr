\name{productivity.measures.lnre}
\alias{productivity.measures.lnre}
\title{Compute Measures of Productivity for Samples from LNRE Model (zipfR)}
\encoding{UTF-8}

\description{

  Compute expectations and confidence intervals for various measures of
  productivity and lexical richness based on an LNRE model.
  See \code{\link{productivity.measures}} for more information and a list of available measures.
  
}

\usage{

\method{productivity.measures}{lnre}(obj, measures, conf.level=.95, ...)

}

\arguments{

  \item{obj}{a frequency spectrum, i.e. an object of class \code{spc}}

  \item{measures}{character vector naming the productivity measures to
    be computed.}

  \item{conf.level}{confidence level for the optional confidence intervals}
  
  \item{...}{any further arguments are ignored}
  
}

\value{

  A data frame with three columns, and rows labelled with the full names of the productivity measures:

  \item{estimate}{the observed or expected value of the productivity measure}
  \item{lower}{lower bound of the confidence interval}
  \item{upper}{upper bound of the confidence interval}
    
}

\details{

  \bold{TODO: not implemented yet}

}

\seealso{

  \code{\link{productivity.measures}} for general usage, return value and a list of supported measures.
  
}

\keyword{ methods }
\keyword{ univar }

\examples{

## TODO

}
