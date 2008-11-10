\name{spc2tfl}
\alias{spc2tfl}
\alias{tfl2spc}
\title{Convert Between Frequency Spectra and Type Frequency Lists (zipfR)}

\description{

  \code{tfl2spc} computes an observed frequency spectrum from a type
  frequency list, while \code{spc2tfl} reconstructs the type frequency
  list underlying a frequency spectrum (but without type
  representations).
  
}

\usage{

  tfl2spc(tfl)

  spc2tfl(spc)

}

\arguments{

  \item{tfl}{an object of class \code{tfl}, representing a type
    frequency list}

  \item{spc}{an object of class \code{spc}, representing a frequency
  spectrum}

}

\value{
  
  For \code{tfl2spc}, an object of class \code{spc} representing the
  frequency spectrum corresponding to the type frequency list \code{tfl}.
  
  For \code{spc2tfl}, an object of class \code{tfl} representing type
  frequency list underlying the observed frequency spectrum \code{tfl}.
  
}

\details{

  The current implementation of these functions does not support
  incomplete type frequency lists and frequency spectra.

  \code{spc2tfl} can only convert frequency spectra where all class
  sizes are integers.  For this reason, expected frequency spectra
  (including all spectra with variance data) are not supported.

}

\seealso{

  \code{\link{spc}} for more information about \code{spc} objects and
  links to relevant functions; \code{\link{tfl}} for more information
  about \code{tfl} objects and links to relevant functions
  
}

\keyword{ manip }
  
\examples{

## Brown tfl and spc
data(Brown.tfl)
data(Brown.spc)


## a spectrum from a tfl
Brown.spc2 <- tfl2spc(Brown.tfl)

## identical to Brown.spc:
summary(Brown.spc)
summary(Brown.spc2)

tail(Brown.spc)
tail(Brown.spc2)


## a tfl from a spectrum
Brown.tfl2 <- spc2tfl(Brown.spc)

## same frequency information as Brown.tfl
## but with different ids and no type labels
summary(Brown.tfl)
summary(Brown.tfl2)

print(Brown.tfl2)
print(Brown.tfl)

}
