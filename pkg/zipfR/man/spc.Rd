\name{spc}
\alias{spc}
\alias{spc.object}
\title{Frequency Spectra (zipfR)}
\description{

  In the \code{zipfR} library, \code{spc} objects are used to represent
  a word frequency spectrum (either an observed spectrum or the expected
  spectrum of a LNRE model at a given sample size).

  With the \code{spc} constructor function, an object can be initialized
  directly from the specified data vectors.  It is more common to read
  an observed spectrum from a disk file with \code{\link{read.spc}} or
  compute an expected spectrum with \code{\link{lnre.spc}}, though.
  
  \code{spc} objects should always be treated as read-only.
  
}

\usage{

  spc(Vm, m=1:length(Vm), VVm=NULL, N=NA, V=NA, VV=NA,
      m.max=0, expected=!missing(VVm))

}

\arguments{

  \item{m}{integer vector of frequency classes \eqn{m} (if omitted,
    \code{Vm} is assumed to list the first \eqn{k} frequency classes
    \eqn{V_1, \ldots, V_k})}

  \item{Vm}{vector of corresponding class sizes \eqn{V_m} (may be
    fractional for expected frequency spectrum \eqn{E[V_m]})}

  \item{VVm}{optional vector of estimated variances
    \eqn{\mathop{Var}[V_m]}{Var[V_m]} (for expected frequency spectrum
    only)}

  \item{N, V}{total sample size \eqn{N} and vocabulary size \eqn{V} of
    frequency spectrum.  While these values are usually determined
    automatically from \code{m} and \code{Vm}, they are required for an
    incomplete frequency spectrum that does not list all non-empty
    frequency classes.}

  \item{VV}{variance \eqn{\mathop{Var}[V]}{Var[V]} of expected
    vocabulary size.  If \code{VVm} is specified, \code{VV} should
    also be given.}

  \item{m.max}{highest frequency class \eqn{m} listed in incomplete
    spectrum.  If \code{m.max} is set, \code{N} and \code{V} also have
    to be specified, and all non-zero frequency classes up to
    \code{m.max} have to be included in the input vectors.  Frequency
    classes above \code{m.max} in the input will automatically be
    deleted.}
  
  \item{expected}{set to \code{TRUE} if the frequency spectrum
    represents expected values \eqn{E[V_m]} of the class sizes according
    to some LNRE model (this is automatically triggered when the
    \code{VVm} argument is specified).}

}

\value{

  An object of class \code{spc} representing the specified frequency
  spectrum.  This object should be treated as read-only (although such
  behaviour cannot be enforced in \R).
  
}

\details{
  
  A \code{spc} object is a data frame with the following variables:

  \describe{

    \item{\code{m}}{frequency class \eqn{m}, an integer vector}

    \item{\code{Vm}}{class size, i.e. number \eqn{V_m} of types in
      frequency class \eqn{m} (either observed class size from a sample
      or expected class size \eqn{E[V_m]} based on a LNRE model)}

    \item{\code{VVm}}{optional: estimated variance \eqn{V[V_m]} of
      expected class size (only meaningful for expected spectrum derived
      from LNRE model)}

  }

  The following attributes are used to store additional information
  about the frequency spectrum:

  \describe{

    \item{\code{m.max}}{if non-zero, the frequency spectrum is
      incomplete and lists only frequency classes up to \code{m.max}}

    \item{\code{N, V}}{sample size \eqn{N} and vocabulary size \eqn{V}
      of the frequency spectrum.  For a complete frequency spectrum,
      these values could easily be determined from \code{m} and
      \code{Vm}, but they are essential for an incomplete spectrum.}

    \item{\code{VV}}{variance of expected vocabulary size; only present
      if \code{hasVariances} is \code{TRUE}.  Note that \code{VV} may
      have the value \code{NA} is the user failed to specify it.}
    
    \item{\code{expected}}{if \code{TRUE}, frequency spectrum lists
      expected class sizes \eqn{E[V_m]} (rather than observed
      sizes \eqn{V_m}).  Note that the \code{VVm} variable is only
      allowed for an expected frequency spectrum.}

    \item{\code{hasVariances}}{indicates whether or not the \code{VVm}
      variable is present}
    
  }
  
}

\seealso{

  \code{\link{read.spc}}, \code{\link{write.spc}},
  \code{\link{spc.vector}}, \code{\link{sample.spc}},
  \code{\link{spc2tfl}}, \code{\link{tfl2spc}},
  \code{\link{lnre.spc}}, \code{\link{plot.spc}}

  Generic methods supported by \code{spc} objects are
  \code{\link{print}}, \code{\link{summary}}, \code{\link{N}},
  \code{\link{V}}, \code{\link{Vm}}, \code{\link{VV}}, and
  \code{\link{VVm}}.

  Implementation details and non-standard arguments for these methods
  can be found on the manpages \code{\link{print.spc}},
  \code{\link{summary.spc}}, \code{\link{N.spc}}, \code{\link{V.spc}},
  etc.
  
}

\keyword{ classes }

\examples{

## load Brown imaginative prose spectrum and inspect it
data(BrownImag.spc)

summary(BrownImag.spc)
print(BrownImag.spc)

plot(BrownImag.spc)

N(BrownImag.spc)
V(BrownImag.spc)
Vm(BrownImag.spc,1)
Vm(BrownImag.spc,1:5)

## compute ZM model, and generate PARTIAL expected spectrum
## with variances for a sample of 10 million tokens
zm <- lnre("zm",BrownImag.spc)
zm.spc <- lnre.spc(zm,1e+7,variances=TRUE)

## inspect extrapolated spectrum
summary(zm.spc)
print(zm.spc)

plot(zm.spc,log="x")

N(zm.spc)
V(zm.spc)
VV(zm.spc)
Vm(zm.spc,1)
VVm(zm.spc,1)

## generate an artificial Zipfian-looking spectrum
## and take a look at it
zipf.spc <- spc(round(1000/(1:1000)^2))

summary(zipf.spc)
plot(zipf.spc)

## see manpages of lnre, and the various *.spc mapages
## for more examples of spc usage

}

