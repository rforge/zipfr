\name{read.tfl}
\alias{read.tfl}
\alias{write.tfl}
\title{Loading and Saving Type Frequency Lists (zipfR)}

\description{

  \code{read.tfl} loads type frequency list from \code{.tfl} file

  \code{write.tfl} saves type frequency list object in \code{.tfl}
  file
  
}

\usage{

  read.tfl(file)

  write.tfl(tfl, file)

}

\arguments{

  \item{file}{character string specifying the pathname of a disk file.
    Files with extension \code{.gz} will automatically be compressed/decompressed.
    See section "Format" for a description of the required file format}

  \item{tfl}{a type frequency list, i.e.\ an object of class \code{tfl}}

}

\value{
  
  \code{read.tfl} returns an object of class \code{tfl} (see the
  \code{\link{tfl}} manpage for details)
  
}

\details{

  If the filename \code{file} ends in the extension \code{.gz}, the disk file
  will automatically be decompressed (\code{read.tfl}) and compressed (\code{write.tfl}).

  The \code{.tfl} file format stores neither the values of \code{N} and
  \code{V} nor the range of type frequencies explicitly.  Therefore,
  incomplete type frequency lists cannot be fully reconstructed from
  disk files (and will not even be recognized as such).  An attempt to
  save such a list will trigger a corresponding warning.
  
}

\format{
  
  A TAB-delimited text file with column headers but no row names
  (suitable for reading with \code{read.delim}), containing the
  following columns:
  
  \describe{

    \item{\code{f}}{type frequencies \eqn{f_k} }

    \item{\code{k}}{optional: the corresponding type IDs \eqn{k}.  If
      missing, increasing non-negative integers are automatically
      assigned as IDs.}

    \item{\code{type}}{optional: type representations (such as word
      forms or lemmas)}

  }

  These columns may appear in any order in the text file.  Only the
  \code{f} column is mandatory and all unrecognized columns will be
  silently ignored.

}

%\references{

%  ~~ reference to format specification, e.g. on the Web site? ~~

%}

\seealso{

  See the \code{\link{tfl}} manpage for details on \code{tfl}
  objects. See \code{\link{read.spc}} and \code{\link{read.vgc}} for
  import/export of other data structures.

}

\keyword{ file }

\examples{

\dontrun{

## examples will not be run during package compilation
## since they would require accessing and writing to
## external files

## load Brown tfl and write it to external file
data(Brown.tfl)
write.tfl(Brown.tfl,"brown.tfl")
## now brown.tfl is external file with fields
## k (an id), f (frequency), type (word)

## read it back in
New.tfl <- read.tfl("brown.tfl")

## same as Brown.tfl
summary(New.tfl)
summary(Brown.tfl)
print(New.tfl)
print(Brown.tfl)
head(New.tfl)
head(Brown.tfl)

## suppose you have a text file with a
## frequency list, one f per line, e.g.:
## f
## 14
## 12
## 31
## ...

## you can import this with read.tfl
MyData.tfl <- read.tfl("mylist.txt")
summary(MyData.tfl)
print(MyData.tfl) # ids in column k added by zipfR

## from this you can generate a spectrum with tfl2spc
MyData.spc <- tfl2spc(MyData.tfl)
summary(MyData.spc)

}

}
