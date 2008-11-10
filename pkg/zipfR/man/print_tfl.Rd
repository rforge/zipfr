\name{print.tfl}
\alias{print.tfl}
\alias{summary.tfl}
\title{Printing Type Frequency Lists (zipfR)}

\description{

  Implementations of the \code{\link{print}} and \code{\link{summary}}
  methods for type frequency list objects (of class \code{tfl}).
  
}

\usage{

  \method{print}{tfl}(x, all=FALSE, ...)

  \method{summary}{tfl}(object, ...)

}

\arguments{

  \item{x, object}{an object of class \code{tfl}, representing a type
  frequency list}

  \item{all}{if \code{FALSE}, only the twenty most frequent types will
    be shown (default)}

  \item{...}{other arguments passed on from generic method will be ignored}

}

\value{

  \code{NULL}

  Unlike other implementations of the \code{summary} method,
  \code{summary.tfl} only prints a summary on screen and does not return
  a special "summary" object.
  
}

\details{

  \bold{NB:} implementation details and format of the summary are subject to
  change in future releases

  \code{print.tfl} works similar to the standard \code{print} method for
  data frames, but provides additional information about \eqn{N} and
  \eqn{V}.  Unless \code{all} is set to \code{TRUE}, only the twenty
  most frequent types will be shown.

  \code{summary.tfl} gives a concise summary of the most important
  information about the type frequency list.  In addition to showing
  \eqn{N} \eqn{V}, the summary also indicates whether the list is
  incomplete and shows examples of type representations (if present).
  
}

\seealso{

  See the \code{\link{tfl}} manpage for details on \code{tfl} objects.

}

\keyword{ print }

\examples{

## load Brown tfl
data(Brown.tfl)

## summary and print most frequent types
summary(Brown.tfl)
print(Brown.tfl)

## the whole type list (don't try
## this unless you have some time to
## spare)
\dontrun{print(Brown.tfl,all=TRUE)}

}

