\name{N-V-Vm}
\alias{N}
\alias{V}
\alias{Vm}
\title{Access Methods for Observed Frequency Data (zipfR)}

\description{

  \code{N}, \code{V} and \code{Vm} are generic methods that can (and
  should) be used to access observed frequency data for objects of class
  \code{tfl}, \code{spc}, \code{vgc} and \code{lnre}.  The precise
  behaviour of the functions depends on the class of the object, but in
  general \code{N} returns the sample size, \code{V} the vocabulary
  size, and \code{Vm} one or more selected elements of the frequency
  spectrum.

}

\usage{

  N(obj, ...)
  V(obj, ...)
  Vm(obj, m, ...)

}

\arguments{

  \item{obj}{an object of class \code{tfl} (type frequency list),
    \code{spc} (frequency spectrum), \code{vgc} (vocabulary growth
    curve) or \code{lnre} (LNRE model)}

  \item{m}{positive integer value determining the frequency class
    \eqn{m} to be returned (or a vector of such values).}

  \item{...}{additional arguments passed on to the method implementation
    (see respective manpages for details)}
  
}

\details{

  For \code{tfl} and \code{vgc} objects, the \code{Vm} method allows
  only a single value \code{m} to be specified.

}

\value{

  For a frequency spectrum (class \code{spc}), \code{N} returns the
  sample size, \code{V} returns the vocabulary size, and \code{Vm}
  returns individual spectrum elements.  

  For a type frequency list (class \code{tfl}), \code{N} returns the
  sample size and \code{V} returns the vocabulary size corresponding to
  the list.  \code{Vm} returns a single spectrum element from the
  corresponding frequency spectrum, and may only be called with a single
  value \code{m}.

  For a vocabulary growth curve (class \code{vgc}), \code{N} returns the
  vector of sample sizes and \code{V} the vector of vocabulary sizes.
  \code{Vm} may only be called with a single value \code{m} and returns
  the corresponding vector from the \code{vgc} object (if present).

  For a LNRE model (class \code{lnre}) estimated from an observed
  frequency spectrum, the methods \code{N}, \code{V} and \code{Vm}
  return information about this frequency spectrum.
  
}

\seealso{

  For details on the implementations of these methods, see
  \code{\link{N.tfl}}, \code{\link{N.spc}}, \code{\link{N.vgc}}, etc.
  When applied to an LNRE model, the methods return information about
  the observed frequency spectrum from which the model was estimated, so
  the manpages for \code{\link{N.spc}} are relevant in this case.

  Expected vocabulary size and frequency spectrum for a sample of size
  \eqn{N} according to a LNRE model can be computed with the analogous
  methods \code{\link{EV}} and \code{\link{EVm}}.  The corresponding
  variances are obtained with the \code{\link{VV}} and \code{\link{VVm}}
  methods, which can also be applied to expected or interpolated
  frequency spectra and vocabulary growth curves.
  
}

\keyword{ methods }
\keyword{ manip }

\examples{

## load Brown spc and tfl
data(Brown.spc)
data(Brown.tfl)

## you can extract N, V and Vm (for a specific m)
## from either structure
N(Brown.spc)
N(Brown.tfl)

V(Brown.spc)
V(Brown.tfl)

Vm(Brown.spc,1)
Vm(Brown.tfl,1)

## you can extract the same info also from a lnre model estimated
## from these data (NB: these are the observed quantities; for the
## expected values predicted by the model use EV and EVm instead!)
model <- lnre("gigp",Brown.spc)
N(model)
V(model)
Vm(model,1)

## Baayen's P:
Vm(Brown.spc,1)/N(Brown.spc)

## when input is a spectrum (and only then) you can specify a vector
## of m's; e.g., to obtain class sizes of first 5 spectrum elements
## you can write:
Vm(Brown.spc,1:5)

## the Brown vgc
data(Brown.emp.vgc)

## with a vgc as input, N, V and Vm return vectors of the respective
## values for each sample size listed in the vgc
Ns <- N(Brown.emp.vgc)
Vs <- V(Brown.emp.vgc)
V1s <- Vm(Brown.emp.vgc,1)

head(Ns)
head(Vs)
head(V1s)

## since the last sample size in Brown.emp.vgc
## corresponds to the full Brown, the last elements
## of the Ns, Vs and V1s vectors are the same as
## the quantities extracted from the spectrum and
## tfl:
Ns[length(Ns)]
Vs[length(Vs)]
V1s[length(V1s)]

}
