\name{vgc}
\alias{vgc}
\alias{vgc.object}
\title{Vocabulary Growth Curves (zipfR)}
\description{

  In the \code{zipfR} library, \code{vgc} objects are used to represent
  a vocabulary growth curve (VGC).  This can be an observed VGC from an
  incremental set of sample (such as a corpus), a randomized VGC
  obtained by binomial interpolation, or the expected VGC according to a
  LNRE model.

  With the \code{vgc} constructor function, an object can be initialized
  directly from the specified data vectors.  It is more common to read
  an observed VGC from a disk file with \code{\link{read.vgc}}, generate
  a randomized VGC with \code{\link{vgc.interp}} or compute an expected
  VGC with \code{\link{lnre.vgc}}, though.
  
  \code{vgc} objects should always be treated as read-only.
  
}

\usage{

  vgc(N, V, Vm=NULL, VV=NULL, VVm=NULL, expected=FALSE, check=TRUE)

}

\arguments{

  \item{N}{integer vector of sample sizes \eqn{N} for which vocabulary
    growth data is available}

  \item{V}{vector of corresponding vocabulary sizes \eqn{V(N)}, or
    expected vocabulary sizes \eqn{E[V(N)]} for an interpolated or
    expected VGC.}

  \item{Vm}{optional list of growth vectors for hapaxes \eqn{V_1(N)},
    dis legomena \eqn{V_2(N)}, etc.  Up to 9 growth vectors are accepted
    (i.e.\ \eqn{V_m(N)} for \eqn{m \le 9}).  For an interpolated or
    expected VGC, the vectors represent expected class sizes
    \eqn{E[V_m(N)]}.}

  \item{VV}{optional vector of variances
    \eqn{\mathop{Var}[V(N)]}{Var[V(N)]} for an interpolated or expected
    VGC}

  \item{VVm}{optional list of variance vectors
    \eqn{\mathop{Var}[V_m(N)]}{Var[V_m(N)]} for an expected VGC.  If
    present, these vectors must be defined for exactly the same frequency
    classes \eqn{m} as the vectors in \code{Vm}.}

  \item{expected}{if \code{TRUE}, the object represents an interpolated
    or expected VGC (for informational purposes only)}

  \item{check}{by default, various sanity checks are performed on the
    data supplied to the \code{spc} constructor.  Specify
    \code{check=FALSE} to skip these sanity test, e.g. when
    automatically processing data from external programs that may be
    numerically unstable.}

}

\value{

  An object of class \code{vgc} representing the specified vocabulary
  growth curve.  This object should be treated as read-only (although
  such behaviour cannot be enforced in \R).
  
}

\details{

  If variances (\code{VV} or \code{VVm}) are specified for an expected
  VGC, all relevant vectors must be given.  In other words, \code{VV}
  always has to be present in this case, and \code{VVm} has to be
  present whenever \code{Vm} is specified, and must contain vectors for
  exactly the same frequency classes.

  \code{V} and \code{VVm} are integer vectors for an observed VGC, but
  will usually be fractional for an interpolated or expected VGC.
  
  A \code{vgc} object is a data frame with the following variables:

  \describe{

    \item{\code{N}}{sample size \eqn{N} }

    \item{\code{V}}{corresponding vocabulary size (either observed
      vocabulary size \eqn{V(N)} or expected vocabulary size
      \eqn{E[V(N)]})}

    \item{\code{V1} \ldots \code{V9}}{optional: observed or expected
      spectrum elements (\eqn{V_m(N)} or \eqn{E[V_m(N)]}).  Not all of
      these variables have to be present, but there must not be any
      "gaps" in the spectrum.}

    \item{\code{VV}}{optional: variance of expected vocabulary size,
      \eqn{\mathop{Var}[V(N)]}{Var[V(N)]} }

    \item{\code{VV1} \ldots \code{VV9}}{optional: variances of expected
      spectrum elements, \eqn{\mathop{Var}[V_m(N)]}{Var[V_m(N)]}.  If
      variances are present, they must be available for exactly the same
      frequency classes as the corresponding expected values.}
    
  }

  The following attributes are used to store additional information
  about the vocabulary growth curve:

  \describe{

    \item{\code{m.max}}{if non-zero, the VGC includes spectrum elements
      \eqn{V_m(N)} for \eqn{m} up to \code{m.max}.  For \code{m.max=0},
      no spectrum elements are present.}

    \item{\code{expected}}{if \code{TRUE}, the object represents an
      interpolated or expected VGC, with expected vocabulary size and
      spectrum elements.  Otherwise, the object represents an observed
      VGC.}

    \item{\code{hasVariances}}{indicates whether or not the \code{VV}
      variable is present (as well as \code{VV1}, \code{VV2}, etc., if
      appropriate)}
    
  }
  
}

\seealso{

  \code{\link{read.vgc}}, \code{\link{write.vgc}}, \code{\link{plot.vgc}},
  \code{\link{vgc.interp}}, \code{\link{lnre.vgc}} 

  Generic methods supported by \code{vgc} objects are
  \code{\link{print}}, \code{\link{summary}}, \code{\link{N}},
  \code{\link{V}}, \code{\link{Vm}}, \code{\link{VV}}, and
  \code{\link{VVm}}.

  Implementation details and non-standard arguments for these methods
  can be found on the manpages \code{\link{print.vgc}},
  \code{\link{summary.vgc}}, \code{\link{N.vgc}}, \code{\link{V.vgc}},
  etc.
  
}

\keyword{ classes }

\examples{

## load Dickens' work empirical vgc and take a look at it

data(Dickens.emp.vgc)
summary(Dickens.emp.vgc)
print(Dickens.emp.vgc)

plot(Dickens.emp.vgc,add.m=1)

## vectors of sample sizes in the vgc, and the
## corresponding V and V_1 vectors
Ns <- N(Dickens.emp.vgc)
Vs <- V(Dickens.emp.vgc)
Vm <- V(Dickens.emp.vgc,1)

## binomially interpolated V and V_1 at the same sample sizes
## as the empirical curve
data(Dickens.spc)
Dickens.bin.vgc <- vgc.interp(Dickens.spc,N(Dickens.emp.vgc),m.max=1)

## compare observed and interpolated
plot(Dickens.emp.vgc,Dickens.bin.vgc,add.m=1,legend=c("observed","interpolated"))


## load Italian ultra- prefix data
data(ItaUltra.spc)

## compute zm model
zm <- lnre("zm",ItaUltra.spc)

## compute vgc up to about twice the sample size
## with variance of V
zm.vgc <- lnre.vgc(zm,(1:100)*70, variances=TRUE)

summary(zm.vgc)
print(zm.vgc)

## plot with confidence intervals derived from variance in
## vgc (with larger datasets, ci will typically be almost
## invisible)
plot(zm.vgc)

## for more examples of vgc usages, see manpages of lnre.vgc,
## plot.vgc, print.vgc  and vgc.interp


}
