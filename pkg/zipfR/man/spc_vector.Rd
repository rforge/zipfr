\name{spc.vector}
\alias{spc.vector}
\title{Create Vector of Spectrum Elements (zipfR)}

\description{

  \code{spc.vector} returns a selected range of elements from a
  frequency spectrum as a plain numeric vector (which may contain
  entries with \eqn{V_m = 0}, unlike the \code{spc} object
  itself).

}

\usage{

  spc.vector(obj, m.min=1, m.max=15, all=FALSE)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing an observed or
    expected frequency spectrum}

  \item{m.min, m.max}{determine the range of frequency classes to be returned
    (defaulting to 1 \ldots 15)}

  \item{all}{if \code{TRUE}, a vector containing the entire frequency
    spectrum is returned, i.e. \code{m.max} is set to \code{max(obj$m)}.
    Note that the value of \code{m.min} can still be overridden manually
    to return only part of the spectrum.}
    
}

\value{
  
  A numeric vector with the selected elements of the frequency spectrum.
  In this vector, empty frequency classes (\eqn{V_m = 0}) are
  represented by 0 entries (unlike the \code{spc} object, which omits
  all empty classes).
  
}

\details{

  \code{spc.vector(obj, a, b)} is fully equivalent to \code{Vm(obj,
  a:b)} (and is implemented in this way).

}

\seealso{

  \code{\link{spc}} for more information about \code{spc} objects and
  links to relevant functions

  \code{\link{Vm.spc}} for an alternative way of extracting spectrum
  vectors from a \code{.spc} object, and \code{\link{N.spc}},
  \code{\link{V.spc}}, \code{\link{VV.spc}}, \code{\link{VVm.spc}} for
  extracting related information
  
}

\keyword{ manip }
  
\examples{

## Brown Noun spectrum
data(BrownNoun.spc)

## by default, extract first 15 elements
spc.vector(BrownNoun.spc)

## first five elements
spc.vector(BrownNoun.spc,1,5)

## just frequencies of spc elements 4 and 5 
spc.vector(BrownNoun.spc,4,5)
## same as
Vm(BrownNoun.spc,4:5)

}

