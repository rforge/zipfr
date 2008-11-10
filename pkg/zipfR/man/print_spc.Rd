\name{print.spc}
\alias{print.spc}
\alias{summary.spc}
\title{Printing Frequency Spectra (zipfR)}

\description{

  Implementations of the \code{\link{print}} and \code{\link{summary}}
  methods for frequency spectrum objects (of class \code{spc}).
  
}

\usage{

  \method{print}{spc}(x, all=FALSE, ...)

  \method{summary}{spc}(object, ...)

}

\arguments{

  \item{x, object}{an object of class \code{spc}, representing a
    frequency spectrum}

  \item{all}{if \code{FALSE}, only the first ten non-empty frequency
    classes will be shown (default)}

  \item{...}{other arguments passed on from generic method will be ignored}

}

\value{

  \code{NULL}

  Unlike other implementations of the \code{summary} method,
  \code{summary.spc} only prints a summary on screen and does not return
  a special "summary" object.
  
}

\details{

  \bold{NB:} implementation details and format of the summary are subject to
  change in future releases

  \code{print.spc} works similar to the standard \code{print} method for
  data frames, but provides additional information about \eqn{N} and
  \eqn{V}.  Unless \code{all} is set to \code{TRUE}, only the first ten
  non-zero spectrum elements will be shown.

  \code{summary.spc} gives a concise summary of the most important
  information about the frequency spectrum.  In addition to \eqn{N}
  \eqn{V}, the first spectrum elements are shown.  The summary will also
  indicate whether the spectrum is incomplete, an expected spectrum, or
  has variances (but does not show the variances).
  
}

\seealso{

  See the \code{\link{spc}} manpage for details on \code{spc} objects.

}

\keyword{ print }

\examples{

## load Brown verbs dataset
data(BrownVer.spc)

## look at summary and print BrownVer.spc
summary(BrownVer.spc)
print(BrownVer.spc)

## print all non-zero spectrum elements
print(BrownVer.spc,all=TRUE)

## estimate zm model and construct expected spectrum with
## variances
zm <- lnre("zm",BrownVer.spc)
zm.spc <- lnre.spc(zm,N(zm),variances=TRUE)

## summary and print for the expected spectrum
summary(zm.spc)
print(zm.spc)

}

