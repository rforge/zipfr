\name{print.lnre}
\alias{print.lnre}
\alias{summary.lnre}
\title{Printing LNRE Models (zipfR)}

\description{

  Implementations of the \code{\link{print}} and \code{\link{summary}}
  methods for LNRE models (subclasses of \code{lnre}).
  
}

\usage{

  \method{print}{lnre}(x, ...)

  \method{summary}{lnre}(object, ...)

}

\arguments{

  \item{x, object}{an object of class \code{lnre} or one of its
    subclasses, representing a LNRE model}

  \item{...}{other arguments passed on from generic method will be ignored}

}

\value{

  \code{NULL}

  Unlike other implementations of the \code{summary} method,
  \code{summary.lnre} only prints a summary on screen and does not return
  a special "summary" object.
  
}

\details{

  \bold{NB:} implementation details and format of the summary are
  subject to change in future releases

  In the current implementation, \code{print} and \code{summary} produce
  the same output for LNRE models.

  This summary comprises the type of LNRE model, its parameter values,
  derived parameters such as normalization constants, and the population
  size \eqn{S}.

  If the model parameters have been estimated from an observed frequency
  spectrum, a comparison of the observed and expected frequency spectrum
  is shown, including goodness-of-fit statistics.
  
}

\seealso{

  See the \code{\link{lnre}} manpage for more information on LNRE
  models.

}

\keyword{ print }

\examples{

# load Brown verbs dataset and estimate lnre models
data(BrownVer.spc)
zm <- lnre("zm",BrownVer.spc)
fzm <- lnre("fzm",BrownVer.spc,exact=FALSE)
gigp <- lnre("gigp",BrownVer.spc)

# look at summaries with either summary or print
summary(zm)
print(zm)

summary(fzm)
print(fzm)

summary(gigp)
print(gigp)

}

