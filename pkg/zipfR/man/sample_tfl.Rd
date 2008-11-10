\name{sample.tfl}
\alias{sample.tfl}
\title{Incremental Samples from a Type Frequency List (zipfR)}

\description{

  Compute incremental random samples from a type frequency list (an
  object of class \code{tfl}).
  
}

\usage{

  sample.tfl(obj, N, force.list=FALSE)

}

\arguments{

  \item{obj}{an object of class \code{tfl}, representing a type
    frequency list}

  \item{N}{a vector of non-negative integers in increasing order, the
    sample sizes for which incremental samples will be generated}

  \item{force.list}{if \code{TRUE}, the return value will always be a
    list of \code{tfl} objects, even if \code{N} is just a single
    integer}

}

\value{
  
  If \code{N} is a single integer (and the \code{force.list} flag is not
  set), a \code{tfl} object representing a random sample of size \eqn{N}
  from the type frequency list \code{obj}.

  If \code{N} is a vector of length greater one, or if
  \code{force.list=TRUE}, a list of \code{tfl} objects representing
  incremental random samples of the specified sizes \eqn{N}.
  \emph{Incremental} means that each sample is a superset of the
  preceding sample.
  
}

\details{

  The current implementation is reasonably efficient, but will be rather
  slow when applied to very large type frequency lists.
  
}

\seealso{

  \code{\link{tfl}} for more information about type frequency lists

  \code{\link{sample.spc}} is an analogous function for frequency
  spectra (objects of class \code{spc})
  
}

\keyword{ manip }
\keyword{ distribution }
  
\examples{

## load Brown tfl
data(Brown.tfl)
summary(Brown.tfl)

## sample a tfl of 100k tokens
MiniBrown.tfl <- sample.tfl(Brown.tfl,1e+5)
summary(MiniBrown.tfl)

## if we repat, we get a different sample
MiniBrown.tfl <- sample.tfl(Brown.tfl,1e+5)
summary(MiniBrown.tfl)

}


}

