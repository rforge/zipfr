\name{lnre.vgc}
\alias{lnre.vgc}
\title{Expected Vocabulary Growth Curves of LNRE Model (zipfR)}

\description{

  \code{lnre.vgc} computes expected vocabulary growth curves
  \eqn{E[V(N)]} according to a LNRE model, returning an object of class
  \code{vgc}.  Data points are returned for the specified values of
  \eqn{N}, optionally including estimated variances and/or growth curves
  for the spectrum elements \eqn{E[V_m(N)]}.
  
}

\usage{

  lnre.vgc(model, N, m.max=0, variances=FALSE)

}

\arguments{

  \item{model}{an object belonging to a subclass of \code{lnre},
    representing a LNRE model}

  \item{N}{an increasing sequence of non-negative integers, specifying
    the sample sizes \eqn{N} for which vocabulary growth data should be
    calculated}

  \item{m.max}{if specified, include vocabulary growth curves
    \eqn{E[V_m(N)]} for spectrum elements up to \code{m.max}.  Must be a
    single integer in the range \eqn{1 \ldots 9}.}

  \item{variances}{if \code{TRUE}, include variance estimates for the
    vocabulary size (and the spectrum elements, if applicable)}

}

\value{

  An object of class \code{vgc}, representing the expected vocabulary
  growth curve \eqn{E[V(N)]} of the LNRE model \code{lnre}, with data
  points at the sample sizes \code{N}.

  If \code{m.max} is specified, expected growth curves \eqn{E[V_m(N)]}
  for spectrum elements (\emph{hapax legomena}, \emph{dis legomena},
  etc.) up to \code{m.max} are also computed.

  If \code{variances=TRUE}, the \code{vgc} object includes variance data
  for all growth curves.
  
}

\details{

  ~~ TODO, if any ~~
  
}

\seealso{

  \code{\link{vgc}} for more information about vocabulary growth curves
  and links to relevant functions; \code{\link{lnre}} for more
  information about LNRE models and how to initialize them
  
}

\keyword{ models }
\keyword{ distribution }
  
\examples{

## load Dickens dataset and estimate lnre models
data(Dickens.spc)

zm <- lnre("zm",Dickens.spc)
fzm <- lnre("fzm",Dickens.spc,exact=FALSE)
gigp <- lnre("gigp",Dickens.spc)

## compute expected V and V_1 growth up to 100 million tokens
## in 100 steps of 1 million tokens
zm.vgc <- lnre.vgc(zm,(1:100)*1e6, m.max=1)
fzm.vgc <- lnre.vgc(fzm,(1:100)*1e6, m.max=1)
gigp.vgc <- lnre.vgc(gigp,(1:100)*1e6, m.max=1)

## compare
plot(zm.vgc,fzm.vgc,gigp.vgc,add.m=1,legend=c("ZM","fZM","GIGP"))

## load Italian ultra- prefix data
data(ItaUltra.spc)

## compute zm model
zm <- lnre("zm",ItaUltra.spc)

## compute vgc up to about twice the sample size
## with variance of V
zm.vgc <- lnre.vgc(zm,(1:100)*70, variances=TRUE)

## plot with confidence intervals derived from variance in
## vgc (with larger datasets, ci will typically be almost
## invisible)
plot(zm.vgc)

}

