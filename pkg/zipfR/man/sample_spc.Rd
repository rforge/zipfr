\name{sample.spc}
\alias{sample.spc}
\title{Incremental Samples from a Frequency Spectrum (zipfR)}

\description{

  Compute incremental random samples from a frequency spectrum (an object
  of class \code{spc}).
  
}

\usage{

  sample.spc(obj, N, force.list=FALSE)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing a frequency
    spectrum}

  \item{N}{a vector of non-negative integers in increasing order, the
    sample sizes for which incremental samples will be generated}

  \item{force.list}{if \code{TRUE}, the return value will always be a
    list of \code{spc} objects, even if \code{N} is just a single
    integer}

}

\value{
  
  If \code{N} is a single integer (and the \code{force.list} flag is not
  set), a \code{spc} object representing the frequency spectrum of a
  random sample of size \eqn{N} from \code{obj}.

  If \code{N} is a vector of length greater one, or if
  \code{force.list=TRUE}, a list of \code{spc} objects representing the
  frequency spectra of incremental random samples of the specified sizes
  \eqn{N}.  \emph{Incremental} means that each sample is a superset of
  the preceding sample.
  
}

\details{

  This function is currently implemented as a wrapper around
  \code{sample.tfl}, using \code{spc2tfl} and \code{tfl2spc} to convert
  between frequency spectra and type frequency lists.  A direct
  implementation might be slightly more efficient, but would very likely
  not make a substantial difference.
  
}

\seealso{

  \code{\link{spc}} for more information about frequency spectra

  \code{\link{sample.tfl}} is an analogous function for type frequency
  lists (objects of class \code{tfl})

  \code{\link{sample.spc}} takes a single \emph{concrete} random
  subsample from a spectrum and returns the spectrum of the subsample,
  unlike \code{spc.interp}, that computes the \emph{expected}
  frequency spectrum for random subsamples of size \code{N} by
  binomial interpolation.

}

\keyword{ manip }
\keyword{ distribution }
  
\examples{

## read Brown spectrum
data(Brown.spc)
summary(Brown.spc)

## sample a spectrum of 100k tokens
MiniBrown.spc <- sample.spc(Brown.spc,1e+5)
summary(MiniBrown.spc)

## if we repat, we get a different sample
MiniBrown.spc <- sample.spc(Brown.spc,1e+5)
summary(MiniBrown.spc)

}
