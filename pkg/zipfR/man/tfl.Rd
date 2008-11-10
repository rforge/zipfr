\name{tfl}
\alias{tfl}
\alias{tfl.object}
\title{Type Frequency Lists (zipfR)}
\description{

  In the \code{zipfR} library, \code{tfl} objects are used to represent
  a type frequency list, which specifies the observed frequency of each
  type in a corpus.  For mathematical reasons, expected type frequencies
  are rarely considered.

  With the \code{tfl} constructor function, an object can be initialized
  directly from the specified data vectors.  It is more common to read
  a type frequency list from a disk file with \code{\link{read.tfl}} or,
  in some cases, derive it from an observed frequency spectrum with
  \code{\link{spc2tfl}}. 
  
  \code{tfl} objects should always be treated as read-only.
  
}

\usage{

  tfl(f, k=1:length(f), type=NULL, f.min=min(f), f.max=max(f),
      incomplete=!(missing(f.min) && missing(f.max)), N=NA, V=NA,
      delete.zeros=FALSE)

}

\arguments{

  \item{k}{integer vector of type IDs \eqn{k} (if omitted, natural
    numbers \eqn{1,2,\ldots} are assigned automatically)}

  \item{f}{vector of corresponding type frequencies \eqn{f_k}}

  \item{type}{optional character vector of type representations
    (e.g. word forms or lemmata), used for informational and
    printing purposes only}

  \item{incomplete}{indicates that the type frequency list is
    incomplete, i.e. only contains types in a certain frequency range
    (typically, the lowest-frequency types may be excluded).  Incomplete
    type frequency lists are rarely useful.}

  \item{N, V}{sample size and vocabulary size corresponding to the type
    frequency list have to be specified explicitly for incomplete lists}

  \item{f.min, f.max}{frequency range represented in an incomplete type
    frequency list (see details below)}

  \item{delete.zeros}{if \code{TRUE}, delete types with \eqn{f=0} from
    the type frequency list, \emph{after} assigning type IDs.  This
    operation does \emph{not} make the resulting \code{tfl} object
    incomplete.}

}

\value{

  An object of class \code{tfl} representing the specified type
  frequency list.  This object should be treated as read-only (although
  such behaviour cannot be enforced in \R).
  
}

\details{

  If \code{f.min} and \code{f.max} are not specified, but the list is
  marked as incomplete (with \code{incomplete=TRUE}), they are
  automatically determined from the frequency vector \code{f} (making
  the assumption that all types in this frequency range are listed).
  Explicit specification of either \code{f.min} or \code{f.max} implies
  an incomplete list.  In this case, all types outside the specified
  range will be deleted from the list.  If \code{incomplete=FALSE} is
  explicitly given, \code{N} and \code{V} will be determined
  automatically from the input data (which is assumed to be complete),
  but the resulting type frequency list will still be incomplete.

  If you just want to remove types with \eqn{f=0} without marking the
  type frequency list as incomplete, use the option
  \code{delete.zeros=TRUE}.

  A \code{tfl} object is a data frame with the following variables:

  \describe{

    \item{\code{k}}{integer type ID \eqn{k} }

    \item{\code{f}}{corresponding type frequency \eqn{f_k} }

    \item{\code{type}}{optional: character vector with type
    representations used for printing}

  }

  The data frame always has to be sorted with respect to the \code{k}
  column (ascending order). 
  
  The following attributes are used to store additional information
  about the frequency spectrum:

  \describe{

    \item{\code{N, V}}{sample size \eqn{N} and vocabulary size \eqn{V}
      corresponding to the type frequency list.  For a complete list,
      these values could easily be determined from the \code{f}
      variable, but they are essential for an incomplete list.}

    \item{\code{incomplete}}{if \code{TRUE}, the type frequency list is
      incomplete, i.e. it lists only types in the frequency range given
      by \code{f.min} and \code{f.max}}

    \item{\code{f.min}, \code{f.max}}{range of type frequencies
      represented in the list (should be ignored unless the
      \code{incomplete} flag is set)}

    \item{\code{hasTypes}}{indicates whether or not the \code{type}
      variable is present}
    
  }
  
}

\seealso{

  \code{\link{read.tfl}}, \code{\link{write.tfl}},
 \code{\link{sample.tfl}}, \code{\link{spc2tfl}}, \code{\link{tfl2spc}}

  Generic methods supported by \code{tfl} objects are
  \code{\link{print}}, \code{\link{summary}}, \code{\link{N}},
  \code{\link{V}} and \code{\link{Vm}}.

  Implementation details and non-standard arguments for these methods
  can be found on the manpages \code{\link{print.tfl}},
  \code{\link{summary.tfl}}, \code{\link{N.tfl}}, \code{\link{V.tfl}},
  etc.
  
}

\keyword{ classes }

\examples{

## typically, you will read a tfl from a file
## (see examples in the read.tfl manpage)

## or you can load a ready-made tfl
data(Brown.tfl)
summary(Brown.tfl)
print(Brown.tfl)

## or create it from a spectrum (with different ids and
## no type labels)
data(Brown.spc)

Brown.tfl2 <- spc2tfl(Brown.spc)

## same frequency information as Brown.tfl
## but with different ids and no type labels
summary(Brown.tfl2)
print(Brown.tfl2)

## how to display draw a Zipf's rank/frequency plot
## by extracting frequencies from a tfl
plot(sort(Brown.tfl$f,decreasing=TRUE),log="y",xlab="rank",ylab="frequency")

## simulating a tfl
Zipfian.tfl <- tfl(1000/(1:1000))
plot(Zipfian.tfl$f,log="y")

}

