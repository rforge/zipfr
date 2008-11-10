\name{print.vgc}
\alias{print.vgc}
\alias{summary.vgc}
\title{Printing Vocabulary Growth Curves (zipfR)}

\description{

  Implementations of the \code{\link{print}} and \code{\link{summary}}
  methods for vocabulary growth curve objects (of class \code{vgc}).
  
}

\usage{

  \method{print}{vgc}(x, all=FALSE, ...)

  \method{summary}{vgc}(object, ...)

}

\arguments{

  \item{x, object}{an object of class \code{vgc}, representing a vocabulary
    growth curve}

  \item{all}{if \code{FALSE}, vocabulary growth data are shown for at
    most 25 sample sizes (default)}

  \item{...}{other arguments passed on from generic method will be ignored}

}

\value{

  \code{NULL}

  Unlike other implementations of the \code{summary} method,
  \code{summary.vgc} only prints a summary on screen and does not return
  a special "summary" object.
  
}

\details{

  \bold{NB:} implementation details and format of the summary are subject to
  change in future releases

  \code{print.vgc} calls the standard \code{print} method for
  data frames internally, but reduces the data set randomly to
  show at most 25 sample sizes (unless \code{all=TRUE}).

  \code{summary.vgc} gives a concise summary of the available vocabulary
  growth data in the \code{vgc} object, including the number and range
  of sample sizes, whether spectrum elements are included, and whether
  variances are included.
  
}

\seealso{

  See the \code{\link{vgc}} manpage for details on \code{vgc} objects.

}

\keyword{ print }

\examples{

## load Brown "informative" prose empirical vgc
data(BrownInform.emp.vgc)

## summary, print (random subset) and print all
summary(BrownInform.emp.vgc)
print(BrownInform.emp.vgc)
print(BrownInform.emp.vgc,all=TRUE)

## load Brown informative prose spectrum
## and get estimate a fzm model
data(BrownInform.spc)
fzm <- lnre("fzm",BrownInform.spc,exact=FALSE)

## obtain expected vgc up to 2M tokens
## with spectrum elements up to V_3
## and variances
fzm.vgc <- lnre.vgc(fzm,(1:100)*2e+4,m.max=3,variances=TRUE)

## summary and print
summary(fzm.vgc)
print(fzm.vgc)
print(fzm.vgc,all=TRUE)

}

