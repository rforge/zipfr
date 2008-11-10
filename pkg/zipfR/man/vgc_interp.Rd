\name{vgc.interp}
\alias{vgc.interp}
\title{Expected Vocabulary Growth by Binomial Interpolation (zipfR)}

\description{

  \code{vgc.interp} computes the expected vocabulary growth curve for
  random sample taken from a data set described by the frequency
  spectrum object \code{obj}.
  
}

\usage{

  vgc.interp(obj, N, m.max=0, allow.extrapolation=FALSE)

}

\arguments{

  \item{obj}{an object of class \code{spc}, representing the frequency
    spectrum of the data set from which samples are taken}

  \item{N}{a vector of increasing non-negative integers specifying the
    sample sizes for the expected vocabulary size is calculated (as well
    as expected spectrum elements if requested)}

  \item{m.max}{an integer in the range \eqn{1 \ldots 9}, specifying the
    number of spectrum elements to be included in the vocabulary growth
    curve (default: none)}

  \item{allow.extrapolation}{if \code{TRUE}, the requested sample sizes
    \eqn{N} may be larger than the sample size of the frequency spectrum
    \code{obj}, so that binomial \emph{extrapolation} is performed.
    This obtion should be used with great caution (see
    \code{\link{EV.spc}} for details).}
    
}

\value{

  An object of class \code{vgc}, representing the expected vocabulary
  growth curves for random samples taken from the data set described by
  \code{obj}.  Data points will be generated for the specified sample
  sizes \code{N}.
  
}

\details{

  See the \code{\link{EV.spc}} manpage for more information, especially
  concerning binomial \emph{extrapolation}.

  Note that the \emph{result} of \code{vgc.interp} is an object of class
  \code{vgc} (a vocabulary growth curve), but its \emph{input} is an
  object of class \code{spc} (a frequency spectrum).
  
}

\seealso{

  \code{\link{vgc}} for more information about vocabulary growth curves
  and links to relevant functions; \code{\link{spc}} for more
  information about frequency spectra

  The implementation of \code{vgc.interp} is based on the functions
  \code{\link{EV.spc}} and \code{\link{EVm.spc}}.  See the respective
  manpages for technical details.

  \code{\link{spc.interp}} computes the expected frequency spectrum for
  a random sample by binomial interpolation.
  
}

\keyword{ manip }
\keyword{ distribution }
  
\examples{

## load the Tiger PP expansion spectrum
## (sample size: about 91k tokens) 
data(TigerPP.spc)

## binomially interpolated curve
TigerPP.bin.vgc <- vgc.interp(TigerPP.spc,(1:100)*910)
summary(TigerPP.bin.vgc)

## let's also add growth of V_1 to V_5 and plot
TigerPP.bin.vgc <- vgc.interp(TigerPP.spc,(1:100)*910,m.max=5)
plot(TigerPP.bin.vgc,add.m=c(1:5))


}

