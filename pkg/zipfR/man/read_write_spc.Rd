\name{read.spc}
\alias{read.spc}
\alias{write.spc}
\title{Loading and Saving Frequency Spectra (zipfR)}

\description{

  \code{read.spc} loads frequency spectrum from \code{.spc} file

  \code{write.spc} saves frequency spectrum object in \code{.spc}
  file
  
}

\usage{

  read.spc(file)

  write.spc(spc, file)

}

\arguments{

  \item{file}{character string specifying the pathname of a disk file.
    Files with extension \code{.gz} will automatically be compressed/decompressed.
    See section "Format" for a description of the required file format}

  \item{spc}{a frequency spectrum, i.e.\ an object of class
    \code{spc}}

}

\value{
  
  \code{read.spc} returns an object of class \code{spc} (see the
  \code{\link{spc}} manpage for details)
  
}

\details{

  If the filename \code{file} ends in the extension \code{.gz}, the disk file
  will automatically be decompressed (\code{read.spc}) and compressed (\code{write.spc}).

  The \code{.spc} file format does not store the values of \code{N},
  \code{V} and \code{VV} explicitly.  Therefore, incomplete frequency
  spectra and expected spectra with variances cannot be fully
  reconstructed from disk files.  Saving such frequency spectra (or
  loading a spectrum with variance data) will trigger corresponding
  warnings.
  
}

\format{

  A TAB-delimited text file with column headers but no row names
  (suitable for reading with \code{read.delim}).  The file must contain
  at least the following two columns:
  
  \describe{

    \item{\code{m}}{frequency class \eqn{m} }

    \item{\code{Vm}}{number \eqn{V_m} of types in frequency class
      \eqn{m} (or expected class size \eqn{E[V_m]})}

  }

  An optional column labelled \code{VVm} can be used to specify
  variances of expected class sizes (for a frequency spectrum derived
  from a LNRE model or by binomial interpolation).  

  These columns may appear in any order in the text file.  All other
  columns will be silently ignored.

}


\seealso{

  See the \code{\link{spc}} manpage for details on \code{spc}
  objects. See \code{\link{read.tfl}} and \code{\link{read.vgc}} for
  import/export of other data structures.

}

\keyword{ file }

\examples{

\dontrun{

## examples will not be run during package compilation
## since they would require accessing and writing to
## external files

## load Italian ultra- data
## and write corresponding spectrum to external text file
data(ItaUltra.spc)
write.spc(ItaUltra.spc,"ultra.spc")
## now ultra.spc is a text file with columns m and Vm

## we ready it back in
New.spc <- read.spc("ultra.spc")

## same spectrum as ItaUltra.spc, compare:
summary(New.spc)
summary(ItaUltra.spc)

## DON'T do the following, incomplete spectrum will not be
## restored properly!!!
zm <- lnre("zm",ItaUltra.spc) # estimate model
zm.spc <- lnre.spc(zm,N(zm)) # incomplete spectrum from model
write.spc(zm.spc,"var.spc") # warnings
bad.spc <- read.spc("/Users/baroni/Desktop/var.spc")
## latter is DIFFERENT from zm.spc!!!

}

}
