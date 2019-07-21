\name{read.vgc}
\alias{read.vgc}
\alias{write.vgc}
\title{Loading and Saving Vocabulary Growth Curves (zipfR)}

\description{

  \code{read.vgc} loads vocabulary growth data from \code{.vgc} file

  \code{write.vgc} saves vocabulary growth data in \code{.vgc} file
  
}

\usage{

  read.vgc(file)

  write.vgc(vgc, file)

}

\arguments{

  \item{file}{character string specifying the pathname of a disk file.
    Files with extension \code{.gz} will automatically be compressed/decompressed.
    See section "Format" for a description of the required file format}

  \item{vgc}{a vocabulary growth curve, i.e.\ an object of class
    \code{vgc}}

}

\value{
  
  \code{read.vgc} returns an object of class \code{vgc} (see the
  \code{\link{vgc}} manpage for details)
  
}

\details{

  If the filename \code{file} ends in the extension \code{.gz}, \code{.bz2} or \code{.xz}, 
  the disk file will automatically be decompressed (\code{read.vgc}) or compressed (\code{write.vgc}).

}

\format{
 
  A TAB-delimited text file with column headers but no row names
  (suitable for reading with \code{read.delim}).  The file must contain
  at least the following two columns:
  
  \describe{

    \item{\code{N}}{increasing integer vector of sample sizes \eqn{N} }

    \item{\code{V}}{corresponding observed vocabulary sizes \eqn{V(N)}
      or expected vocabulary sizes \eqn{E[V(N)]} }

  }

  Optionally, columns \code{V1}, \ldots, \code{V9} can be added to
  specify the number of hapaxes (\eqn{V_1(N)}), dis legomena
  (\eqn{V_2(N)}), and further spectrum elements up to \eqn{V_9(N)}.

  It is not necessary to include all 9 columns, but for any \eqn{V_m(N)}
  in the data set, all "lower" spectrum elements \eqn{V_{m'}(N)} (for
  \eqn{m' < m}) must also be present.  For example, it is valid to have
  columns \code{V1 V2 V3}, but not \code{V1 V3 V5} or \code{V2 V3 V4}.

  Variances for expected vocabulary sizes and spectrum elements can be
  given in further columns \code{VV} (for
  \eqn{\mathop{Var}[V(N)]}{Var[V(N)]}), and \code{VV1}, \ldots,
  \code{VV9} (for \eqn{\mathop{Var}[V_m(N)]}{Var[V_m(N)]}).  \code{VV}
  is mandatory in this case, and columns \code{VVm} must be specified
  for exactly the same frequency classes \code{m} as the \code{Vm}
  above.
  
  These columns may appear in any order in the text file.  All other
  columns will be silently ignored.

}

%\references{
%
%  ~~ perhaps reference to Web site for file format specfications ~~
%
%}

\seealso{

  See the \code{\link{vgc}} manpage for details on \code{vgc} objects.
  See \code{\link{read.tfl}} and \code{\link{read.spc}} for
  import/export of other data structures.

}

\keyword{ file }

\examples{
## save Italian ultra- prefix VGC to external text file
fname <- tempfile(fileext=".vgc")
write.vgc(ItaUltra.emp.vgc, fname)
## now <fname> is a TAB-delimited text file with columns N, V and V1

## we ready it back in
New.vgc <- read.vgc(fname)

## same vgc as ItaUltra.emp.vgc, compare:
summary(New.vgc)
summary(ItaUltra.emp.vgc)
head(New.vgc)
head(ItaUltra.emp.vgc)

stopifnot(isTRUE(all.equal(New.vgc, ItaUltra.emp.vgc))) # should be identical
}
