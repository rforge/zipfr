\name{merge.tfl}
\alias{merge.tfl}
\title{Merging Type Frequency Lists (zipfR)}

\description{

  Merge two or more type frequency lists. Types from the individual lists are pooled
  and frequencies of types occurring in multiple lists are aggregated.

}

\usage{

\method{merge}{tfl}(x, y, \dots)

}

\arguments{

  \item{x, y}{type frequency lists (i.e. objects of class \code{\link{tfl}})}
  
  \item{\dots}{optional further type frequency lists to be merged}

}

\details{

  All type frequency lists to be merged must contain type labels,
  and none of them may be incomplete.
  
}

\seealso{

  \code{\link{tfl}} for more information about type frequency lists.

}

\keyword{ manip }
