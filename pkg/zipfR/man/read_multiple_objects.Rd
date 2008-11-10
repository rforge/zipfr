\name{read.multiple.objects}
\alias{read.multiple.objects}
\title{Reading Multiple Objects from Files (zipfR)}

\description{

  \code{read.multiple.objects} constructs a list of \code{\link{spc}},
  \code{\link{vgc}} or \code{\link{tfl}} objects from a set of input
  text files in the specified directory

  \bold{NB}: This function is intended for users that want to run
  advanced experiments (e.g., handling hundreds of spectra generated in
  multiple randomizations experiments). For the standard
  one-object-at-a-time reading functionality, look at the documentation
  of \code{\link{read.spc}}, \code{\link{read.vgc}} and
  \code{\link{read.tfl}}
  
}

\usage{

  read.multiple.objects(directory, prefix, class=c("spc", "vgc", "tfl"))

}

\arguments{

  \item{directory}{character string specifying the directory where the
    target input files reside (absolute path, or path relative to
    current working directory)}

  \item{prefix}{character string specifying prefix that must be shared
  by all target input file names}

  \item{class}{one of \code{spc}, \code{vgc} or \code{tfl} as character
  string, specifying the class of object we are importing (see the
  manpages of \code{\link{spc}}, \code{\link{vgc}} and \code{\link{tfl}}
  for details)}

}

\value{
  
  \code{read.multiple.objects} returns a list of objects of the
  specified class; each object is indexed with the id extracted from the
  corresponding file name (see section "Format")
  
}

\format{

  \code{read.multiple.objects} reads in all files matching the pattern
  \code{prefix.id.class} from the specified directory, where the
  \code{prefix} and \code{class} strings are passed as arguments, and
  \code{id} is an arbitrary string that is used as index of the
  corresponding object in the output list

  \code{read.multiple.objects} calls the \code{read} function
  corresponding to the \code{class} argument. Thus, the input files must
  respect the formatting conventions of the relevant reading functions
  (see documentation of \code{\link{read.spc}}, \code{\link{read.vgc}}
  and \code{\link{read.tfl}})

}


\seealso{

  See the \code{\link{spc}}, \code{\link{vgc}} and \code{\link{tfl}}
  manpages for details on the corresponding objects;
  \code{\link{read.spc}}, \code{\link{read.vgc}} and
  \code{\link{read.tfl}} for the single-file reading functions and input
  format details

}

\keyword{ file }

\examples{

\dontrun{

## examples will not be run during package compilation
## since they would require accessing external files

## suppose that the current working directory contains
## 100 spc files named: rand.1.spc, rand.2.spc, ...,
## rand.100.spc

## read the files in:
spc.list <- read.multiple.objects(".","rand","spc")

## you can access each spc using the id extracted from
## the file name, e.g.:
summary(spc.list[["1"]])

## more usefully, you might want to iterate over the
## whole list, e.g., to calculate mean V:
mean(sapply(spc.list,V))

## notice that ids are arbitrary strings
## e.g., suppose that directory /home/me/animals
## contains sounds.dog.vgc and sounds.elephant.vgc

## we read the vgcs in:
vgc.list <- read.multiple.objects("/home/me/animals","sounds","vgc")

## accessing the elephant vgc:
V(vgc.list[["elephant"]])

## of course, tfl-reading works in the same way (assuming
## that the animals directory also contains some tfl files):
tfl.list <- read.multiple.objects("/home/me/animals","sounds","tfl")



}

}
