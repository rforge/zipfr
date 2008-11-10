\name{lnre.spc}
\alias{lnre.spc}
\title{Compute Expected Frequency Spectrum of LNRE Model (zipfR)}

\description{

  \code{lnre.spc} computes the expected frequency spectrum of a LNRE
  model at specified sample size \code{N}, returning an object of class
  \code{spc}.  Since almost all expected spectrum elements are non-zero,
  only an incomplete spectrum can be generated.
  
}

\usage{

  lnre.spc(model, N=NULL, variances=FALSE, m.max=100)

}

\arguments{

  \item{model}{an object belonging to a subclass of \code{lnre},
    representing a LNRE model}

  \item{N}{a single positive integer, specifying the sample size \eqn{N}
    for which the expected frequency spectrum is calculated (defaults to
    same sample size as used for estimating the model)}

  \item{variances}{if \code{TRUE}, include variances for the spectrum
    elements in the \code{spc} object}

  \item{m.max}{number of spectrum elements listed in the frequency
    spectrum.  The default of 100 is chosen to avoid numerical
    problems that certain LNRE models (in particular, GIGP) have for
    higher \eqn{m}.  If variance data is included, the default value
    is automatically reduced to 50.}

}

\value{

  An object of class \code{spc}, representing the incomplete expected
  frequency spectrum of the LNRE model \code{lnre} at sample size
  \code{N}.  If \code{variances=TRUE}, the spectrum also includes
  variance data.
  
}

\details{

  ~~ TODO, if any ~~
  
}

\seealso{

  \code{\link{spc}} for more information about frequency spectra and
  links to relevant functions; \code{\link{lnre}} for more information
  about LNRE models and how to initialize them
  
}

\keyword{ models }
\keyword{ distribution }
  
\examples{

## load Dickens dataset and compute lnre models
data(Dickens.spc)

zm <- lnre("zm",Dickens.spc)
fzm <- lnre("fzm",Dickens.spc, exact=FALSE)
gigp <- lnre("gigp",Dickens.spc)

## calculate the corresponding expected
## frequency spectra at the Dickens size
zm.spc <- lnre.spc(zm,N(Dickens.spc))
fzm.spc <- lnre.spc(fzm,N(Dickens.spc))
gigp.spc <- lnre.spc(gigp,N(Dickens.spc))

## comparative plot
plot(Dickens.spc,zm.spc,fzm.spc,gigp.spc,m.max=10)

## expected spectra at N=100e+8
## and comparative plot
zm.spc <- lnre.spc(zm,1e+8)
fzm.spc <- lnre.spc(fzm,1e+8)
gigp.spc <- lnre.spc(gigp,1e+8)

plot(zm.spc,fzm.spc,gigp.spc,m.max=10)

## with variances
zm.spc <- lnre.spc(zm,1e+8,variances=TRUE)
head(zm.spc)

## asking for more than 50 spectrum elements
## (increasing m.max will eventually lead
## to error, at different threshold for
## the different models)
zm.spc <- lnre.spc(zm,1e+8,m.max=1000)
fzm.spc <- lnre.spc(fzm,1e+8,m.max=1000)
gigp.spc <- lnre.spc(gigp,1e+8,m.max=100) ## gigp breaks first!


}

